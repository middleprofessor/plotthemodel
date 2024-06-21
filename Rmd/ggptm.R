## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
# wrangling
library(data.table)
library(stringr)

# plot function
library(ggplot2)
library(ggpubr)
library(ggforce)
library(insight)
library(cowplot)



## -------------------------------------------------------------------------------------------------------------------------------
remove_parentheses <- function(x){
  if(substr(x, 1, 1) == "("){
    x <- substr(x, 2, nchar(x))
  }
  if(substr(x, nchar(x), nchar(x)) == ")"){
    x <- substr(x, 1, nchar(x)-1)
  }
  return(x)
}


## -------------------------------------------------------------------------------------------------------------------------------

create_model_data <- function(
    data,
    response_label = "response_label",
    factor1_label = "factor1_label",
    factor2_label = "factor2_label",
    block_label = "block_label",
    nest_label = "nest_label",
    two_factors = FALSE,
    include_block = FALSE,
    include_nest = FALSE
){
  y_cols <- c(response_label, factor1_label)
  if(two_factors == TRUE){y_cols <- c(y_cols, factor2_label)}
  if(include_block == TRUE){y_cols <- c(y_cols, block_label)}
  if(include_nest == TRUE){y_cols <- c(y_cols, nest_label)}
  model_data <- data[, y_cols] |>
    data.table()
  
  # make generic columns for easier handling of data
  model_data[, data_set := ifelse(include_nest == TRUE,
                                  "tech_reps",
                                  "exp_reps")]
  model_data[, y := get(response_label)]
  model_data[, factor_1 := get(factor1_label)]
  model_data[, factor_2 := ifelse(two_factors == TRUE,
                                  get(factor2_label),
                                  NA)]
  model_data[, plot_factor := ifelse(two_factors == TRUE,
                                     paste(factor_1, factor_2,
                                           sep = "\n"),
                                     factor_1)]
  model_data[, block_id := ifelse(include_block == TRUE,
                                  get(block_label),
                                  NA)]
  model_data[, nest_id := ifelse(include_nest == TRUE,
                                 get(nest_label),
                                 NA)]
  # reorder factor levels for 2-factor plots
  levels_1 <- levels(model_data$factor_1) |>
    as.character()
  levels_2 <- levels(model_data$factor_2) |>
    as.character()
  levels_table <- expand.grid(levels_1, levels_2, stringsAsFactors = FALSE) |>
    data.table()
  levels_table[, plot_levels := paste(Var1, Var2, sep = "\n")]
  model_data[, plot_factor := factor(plot_factor,
                                     levels = levels_table[, plot_levels])]
  return(model_data)
}



## -------------------------------------------------------------------------------------------------------------------------------

create_plot_data <- function(m1){
  gg_data <- get_data(m1) |>
    data.table()
  response_label <- find_response(m1)
  predictors <- find_predictors(m1)
  predictors_fixed <- predictors$conditional
  factor1_label <- predictors_fixed[1]
  factor2_label <- predictors_fixed[2]
  two_factors <- ifelse(is.na(factor2_label), FALSE, TRUE)

  # create generic columns
  gg_data[, y := get(response_label)]
  gg_data[, factor_1 := get(factor1_label) |>
              factor()]
  if(two_factors == TRUE){
  gg_data[, factor_2 := get(factor2_label) |>
            factor()]
  }
  gg_data[, plot_factor := factor_1]
  if(two_factors == TRUE){
    gg_data[, plot_factor := paste(factor_1, factor_2, sep = "\n")]
  # reorder factor levels for 2-factor plots
    levels_1 <- levels(gg_data$factor_1) |>
      as.character()
    levels_2 <- levels(gg_data$factor_2) |>
      as.character()
    levels_table <- expand.grid(levels_1, levels_2, stringsAsFactors = FALSE) |>
      data.table()
    levels_table[, plot_levels := paste(Var1, Var2, sep = "\n")]
    gg_data[, plot_factor := factor(plot_factor,
                                    levels = levels_table[, plot_levels])]
  }

  gg_data[, plot_factor_id := as.integer(plot_factor) |>
             as.character()]
  return(gg_data)
}



## -------------------------------------------------------------------------------------------------------------------------------
create_emm_data <- function(m1, m1_emm){
  gg_data <- get_data(m1) |>
    data.table()
  response_label <- find_response(m1)
  predictors <- find_predictors(m1)
  predictors_fixed <- predictors$conditional
  factor1_label <- predictors_fixed[1]
  factor2_label <- predictors_fixed[2]
  two_factors <- ifelse(is.na(factor2_label), FALSE, TRUE)

  if(is.data.frame(m1_emm) == TRUE){
    gg_emm <- data.table(m1_emm)
  }else{
    gg_emm <- summary(m1_emm) |>
      data.table()
  }
  # create plot_factor column - this will be horizontal axis
  gg_emm[, plot_factor := get(factor1_label)]
  if(two_factors == TRUE){
    gg_emm[, plot_factor := paste(get(factor1_label), get(factor2_label), sep = "\n")]
    gg_emm[, plot_factor := factor(plot_factor, levels = plot_factor)]
  }
  gg_emm[, plot_factor_id := as.integer(plot_factor) |>
             as.character()]
  # create generic columns for mean and CIs
  if("emmean" %in% names(gg_emm)){
    gg_emm[, mean := emmean]
  }
  if("lower.CL" %in% names(gg_emm)){
    gg_emm[, lo := lower.CL]
    gg_emm[, hi := upper.CL]
  }
  
  return(gg_emm)
}


## -------------------------------------------------------------------------------------------------------------------------------
plot_response <- function(m1, m1_emm,
                          jitter_spread = 0.8,
                          jitter_width = 0.4,
                          y_label = NULL){
  

  # get some palettes
  pal_okabe_ito <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )
  pal_okabe_ito_blue <- pal_okabe_ito[c(5,6,1,2,3,7,4)]
  pal_npg <- pal_npg("nrc")(10)
  pal_aaas <- pal_aaas("default")(10)
  pal_jco <- pal_jco("default")(10)
  pal_frontiers <- pal_frontiers("default")(7)

  if(is.null(y_label)){y_label <- find_response(m1)}

  # create plot data
  gg_data <- create_plot_data(m1)
  gg_emm <- create_emm_data(m1, m1_emm)
  
  gg <- ggplot(data = gg_data,
               aes(x = plot_factor_id,
                   y = y))
 
  # add data points. These are nest means if nested data 
  gg <- gg +
    geom_jitter(data = gg_data,
                aes(x = plot_factor_id,
                    y = y,
                    color = factor_1),
                width = jitter_width,
                size = 2,
                show.legend = FALSE) +
    ylab(y_label) +
    scale_x_discrete(labels = levels(gg_data$plot_factor)) +
    theme_pubr() +
    theme(axis.title.x = element_blank())
  
  # add model means and CI
  gg <- gg +
    geom_errorbar(data = gg_emm,
                  aes(x = plot_factor_id,
                      y = mean,
                      ymin = lo,
                      ymax = hi,
                      width =.1),
                  show.legend = FALSE) +
    geom_point(data = gg_emm,
               aes(x = plot_factor_id,
                   y = mean),
               size = 4,
               show.legend = FALSE)
  
  gg
  
  return(gg)
}


## ----output-as-R-file-----------------------------------------------------------------------------------------------------------
# highlight and run to put update into R folder
# knitr::purl("ggptm.Rmd")

