## ----setup, include=FALSE----------------------------------------------------------
# wrangling
library(data.table)
library(stringr)

# plot function
library(ggplot2)
library(ggpubr)
library(ggforce)
library(insight)
library(cowplot)



## ----palettes----------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------
remove_parentheses <- function(x){
  if(substr(x, 1, 1) == "("){
    x <- substr(x, 2, nchar(x))
  }
  if(substr(x, nchar(x), nchar(x)) == ")"){
    x <- substr(x, 1, nchar(x)-1)
  }
  return(x)
}

pretty_pvalues <- function(p){
  p[p >= 0.1] <- round(p[p > 0.1], 2)
  p[p < 0.1 & p >= 0.01] <- round(p[p < 0.1 & p >= 0.01], 3)
  p[p < 0.01 & p >= 0.001] <- round(p[p < 0.01 & p >= 0.001], 4)
  p_string <- paste0("p=",as.character(p))
  p_string[p < 0.001] <- "p<0.001"
  return(p_string)
}


## ----------------------------------------------------------------------------------

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



## ----------------------------------------------------------------------------------

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



## ----------------------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------
create_pairs_data <- function(m1_pairs, ptm){
  if(is.data.frame(m1_pairs) == TRUE){
    gg_pairs <- data.table(m1_pairs)
  }else{
    gg_pairs <- summary(m1_pairs) %>%
      data.table()
  }
  
  two_factors <- ptm$two_factors
  simple <- ptm$simple
  plot_factor_levels <- ptm$plot_factor_levels
  
  # create group1 and group2 columns (the two groups of contrast)
  groups <- unlist(str_split(gg_pairs$contrast, " - "))
  # remove parentheses if they exist from group cols
  groups <- lapply(groups, remove_parentheses) |>
    unlist()
  i_seq <- 1:length(groups)
  gg_pairs[, group1_label := groups[i_seq%%2 != 0]]
  gg_pairs[, group2_label := groups[i_seq%%2 == 0]]
  if(simple == TRUE){
    simple_group_1 <- names(gg_pairs)[1]
    simple_group_2 <- names(gg_pairs)[2]
    gg_pairs[get(simple_group_1) != ".", group1_label := paste(group1_label, get(simple_group_1))]
    gg_pairs[get(simple_group_1) != ".", group2_label := paste(group2_label, get(simple_group_1))]
    gg_pairs[get(simple_group_2) != ".", group1_label := paste(group1_label, get(simple_group_2))]
    gg_pairs[get(simple_group_2) != ".", group2_label := paste(group2_label, get(simple_group_2))]
  }
  if(two_factors == TRUE){
    gg_pairs[, group1_label := str_replace(group1_label, " ", "\n")]
    gg_pairs[, group2_label := str_replace(group2_label, " ", "\n")]
  }
  gg_pairs[, group1 := match(group1_label, plot_factor_levels)]
  gg_pairs[, group2 := match(group2_label, plot_factor_levels)]
  gg_pairs[, p.print := pretty_pvalues(p.value)]
  gg_pairs[, p.print := format_p(p.value, whitespace = FALSE)]
  
  return(gg_pairs)
}



## ----------------------------------------------------------------------------------
create_nest_data <- function(m1, gg_data, ptm){
  gg_nest_data <- gg_data[, .(y = mean(get(ptm$response_label), na.rm = TRUE)),
                          by = c(ptm$factor1_label, ptm$factor1_label, "factor_1",
                                 "plot_factor", "plot_factor_id", ptm$nest_id)]
  # for prediction to get y, we need factor nest and block labels
  # convert to original column labels
  setnames(gg_nest_data, old = "y", new = ptm$response_label)
  y_hat <- predict(m1, gg_nest_data, type = "response")
  gg_nest_data[, nest_mean := y_hat]
  return(gg_nest_data)
}


## ----------------------------------------------------------------------------------
# need to find maximum y-value from experimental reps, technical reps, or CIs
add_y_pos <- function(gg_pairs, gg_data, gg_emm, gg_nest, ptm){
  if(ptm$nested == FALSE | (ptm$nested == TRUE & ptm$show_nest == TRUE)){
    max_data <- max(gg_data[, y], na.rm = TRUE)
    min_data <- min(gg_data[, y], na.rm = TRUE)
  }else{
    max_data <- NA
    min_data <- NA
  }
  if(ptm$nested == TRUE){
    max_nest <- max(gg_nest[, nest_mean], na.rm = TRUE)
    min_nest <- min(gg_nest[, nest_mean], na.rm = TRUE)
  }else{
    max_nest <- NA
    min_nest <- NA
  }
  max_ci <- max(gg_emm[, hi], na.rm = TRUE)
  min_ci <- min(gg_emm[, lo], na.rm = TRUE)
  max_y <- max(c(max_data, max_nest, max_ci), na.rm = TRUE)
  min_y <- min(c(min_data, min_nest, min_ci), na.rm = TRUE)
  p_increment <- 0.06*(max_y - min_y)
  gg_pairs[, y_pos := max_y + .I * p_increment]
  return(gg_pairs)
}




## ----------------------------------------------------------------------------------
get_ptm_parameters <- function(m1, m1_pairs){
  ptm <- list()
  ptm$response_label <- find_response(m1)
  predictors <- find_predictors(m1)
  predictors_fixed <- predictors$conditional
  ptm$factor1_label <- predictors_fixed[1]
  ptm$factor2_label <- predictors_fixed[2]
  ptm$two_factors <- ifelse(is.na(ptm$factor2_label), FALSE, TRUE)
  random <- find_random(m1)$random
  ptm$random <- ifelse(is.null(random), NA, random)
  
  # nesting or blocked?
  gg_data <- get_data(m1) |>
    data.table()
  if(is.na(ptm$random)){
    ptm$nested <- FALSE
    ptm$nest_id <- NA
  }else{
    counts <- gg_data[!is.na(get(ptm$response)), .(N = .N),
                      by = c(ptm$factor1_label, ptm$factor1_label, ptm$random)]
    ptm$nested <- ifelse(any(counts$N > 1), TRUE, FALSE)
    ptm$nest_id <- ptm$random
  }
  
  # simple effects?
  if(is.data.frame(m1_pairs) == TRUE){
    gg_pairs <- data.table(m1_pairs)
  }else{
    gg_pairs <- summary(m1_pairs) %>%
      data.table()
  }
  ptm$simple <- ifelse(names(gg_pairs)[1] != "contrast", TRUE, FALSE)
  
  return(ptm)
}


## ----------------------------------------------------------------------------------
plot_response <- function(m1,
                          m1_emm,
                          m1_pairs,
                          show_nest_data = FALSE,
                          nest_id = NA, # this is the column containing the cluster
                          jitter_spread = 0.8,
                          jitter_width = 0.2,
                          palette = "pal_ggplot",
                          y_label = NA){
  
  ptm <- get_ptm_parameters(m1, m1_pairs)
  ptm$show_nest <- show_nest_data
  if(!is.na(nest_id)){ptm$nest_id <- nest_id}
  
  if(is.na(y_label)){y_label <- ptm$response_label}
  
  gg_data <- create_plot_data(m1)
  gg_emm <- create_emm_data(m1, m1_emm)
  ptm$plot_factor_levels <- gg_emm[, plot_factor] |> as.character()
  gg_pairs <- create_pairs_data(m1_pairs, ptm)
  if(!is.na(nest_id)){
    gg_nest <- create_nest_data(m1, gg_data, ptm)
  }else{
    gg_nest <- NA
  }
  gg_pairs <- add_y_pos(gg_pairs, gg_data, gg_emm, gg_nest, ptm)
  
  gg <- ggplot(data = gg_data,
               aes(x = plot_factor_id,
                   y = y))
  
  # add data points. If nest, these are gray
  if(ptm$nested == FALSE){
    # experimental reps
    gg <- gg +
      geom_jitter(data = gg_data,
                  aes(x = plot_factor_id,
                      y = y,
                      color = factor_1),
                  width = jitter_width,
                  size = 4,
                  show.legend = FALSE)
  }
  if(ptm$nested == TRUE){
    # nested reps
    if(show_nest_data == TRUE){
      gg <- gg +
        geom_sina(data = gg_data,
                  aes(x = plot_factor_id,
                      y = y,
                      alpha = 1),
                  scale = "width",
                  maxwidth = jitter_width,
                  size = 2,
                  color = "gray",
                  show.legend = FALSE)
    }
  }
  
  # add nest means = experimental reps
  if(ptm$nested == TRUE){
    gg <- gg +
      geom_jitter(data = gg_nest,
                  aes(x = plot_factor_id,
                      y = nest_mean,
                      color = factor_1),
                  width = jitter_width,
                  size = 4,
                  show.legend = FALSE)
  }
  
  
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
  
  # add some color
  if(palette != "pal_ggplot"){
    gg <- gg +
      scale_color_manual(values = get(palette))
  }
  
  # add p-value brackets
  gg <- gg +
    stat_pvalue_manual(gg_pairs,
                       label = "p.print",
                       y.position = "y_pos",
                       # xmin = "minx",
                       # xmax = "maxx",
                       size = 4,
                       tip.length = 0.01)
  
  # add basic graph stuff
  gg <- gg +
    ylab(y_label) +
    scale_x_discrete(labels = levels(gg_data$plot_factor)) +
    theme_pubr() +
    theme(axis.title.x = element_blank())
  
  # gg
  
  return(gg)
}


## ----output-as-R-file--------------------------------------------------------------
# highlight and run to put update into R folder
# knitr::purl("ggptm.Rmd")
