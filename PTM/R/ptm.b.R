
# This file is a generated template, your changes will not be overwritten

PTMClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "PTMClass",
    inherit = PTMBase,
    private = list(
        .run = function() {

          # source_path <- here("R", "ggplot_the_model.R")
          # source(source_path)
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          # table <- self$results$ttest # use this line to find if code gets this far
          # setwd("/Users/walker/Documents/Github/plotthemodel/PTM")
          
          remove_parentheses <- function(x){
            if(substr(x, 1, 1) == "("){
                    x <- substr(x, 2, nchar(x))
                }
                if(substr(x, nchar(x), nchar(x)) == ")"){
                    x <- substr(x, 1, nchar(x)-1)
                }
                return(x)
            }

            # model_options
            two_factors <- ifelse(is.null(self$options$factor2), FALSE, TRUE)
            include_block <- ifelse(is.null(self$options$block), FALSE, TRUE)
            include_nest <- ifelse(is.null(self$options$nest), FALSE, TRUE)
            the_design <- self$options$design
            the_model <- self$options$model
            the_family <- self$options$family
            plot_notes_string <- paste("The 95% confidence intervals are computed from the model error (sigma)",
                                       "and not from the sample standard error of each group.", sep = "\n ")
            if(the_family == "norm"){
              model_notes_string <- paste("The fit model assumes Independent and Identically Distributed (IID) responses",
              "(the Normal, Homogenous variance, Independent assumption) that is, each conditional response",
              "was independently sampled from the same, normal distribution")
            }
            model_notes_string <- ""
            
            # fixed, mixed, or anova
            if(the_model == "aov_4"){
              model_class <- "anova"
            }else{
              model_class <- ifelse(include_block == TRUE | include_nest == TRUE, "mixed", "fixed")
            }
            family_class <- ifelse(the_model == "glm" | the_model == "glmer", "generalized", "general")

            # plot options
            plot_trt.vs.ctrl <- ifelse(self$options$trtvsctrl == TRUE,
                                     "trt.vs.ctrl",
                                     "revpairwise")
            plot_simple <- self$options$simple
            plot_adjust <- self$options$adjust
            show_tech_reps <- ifelse(self$options$tech_reps == TRUE, TRUE, FALSE)
            if(include_nest == FALSE){show_tech_reps <- FALSE}

            # Stats output
            # get formulas
            response_label <- self$options$dep
            factor1_label <- self$options$factor1
            factor2_label <- ifelse(two_factors == TRUE,
                                    self$options$factor2,
                                    NA)
            block_label <- ifelse(include_block == TRUE,
                                    self$options$block,
                                    NA)
            nest_label <- ifelse(include_nest == TRUE,
                                  self$options$nest,
                                  NA)
            subplot_label <- ifelse(self$options$subplot == "subplot.eq.1",
                                    factor1_label,
                                    factor2_label)
            if(two_factors == FALSE){
                fixed_part <- factor1_label
                formula_string <- paste(response_label, "~", fixed_part)
                spec_list <- factor1_label
                if(include_block == FALSE & include_nest == FALSE){
                    if(length(levels(self$data[[self$options$factor1]])) == 2){
                        model_notes_string <- "This LM is equivalent to a Student's (Independent Samples) t-test!"
                    }else{
                        model_notes_string <- "This LM is equivalent to a one-way ANOVA!"
                    }
                }
                total_levels <- length(levels(self$data[[self$options$factor1]]))
            }else{
                fixed_part <- paste(factor1_label, " * ", factor2_label)
                formula_string <- paste(response_label, "~", fixed_part)
                spec_list <- c(factor1_label, factor2_label)
                total_levels <- length(levels(self$data[[self$options$factor1]])) +
                  length(levels(self$data[[self$options$factor2]]))
            }

            # don't allow lmm/aov_4 if there is no block data
            if(the_model == "lmer" & include_block == FALSE & include_nest == FALSE){
                model_notes_string <- "lmer needs a blocking variable or a nesting variable\n and doesn't work with a CRD! Use Model -> lm."
            }
            if(the_model == "aov_4" & include_block == FALSE & include_nest == FALSE){
                model_notes_string <- "aov_4 needs a blocking variable and doesn't work\n with a CRD! Use Model -> lm."
            }
            if(include_nest == FALSE & the_design == "crds"){
              model_notes_string <- "CRDS requires a nesting variable!"
            }

            # formulas with nests and blocks
            if(include_nest == TRUE & include_block == FALSE){
              if(the_design == "crds"){
                    if(model_class == "fixed"){
                        formula_string <- paste0(formula_string, " + ", nest_label)
                        model_notes_string <- "Danger! You have pseudoreplication! Use lmer or aov_4."
                    }
                    if(model_class == "mixed"){
                        formula_string <- paste0(formula_string, " + (1 | ", nest_label, ")")
                        model_notes_string <- "This LMM is equivalent to a Nested ANOVA if there are no missing data!"
                    }
                    if(model_class == "anova"){
                        formula_string <- paste0(formula_string, " + (1 | ", nest_label, ")")
                        model_notes_string <- "This Nested ANOVA is a speciall case of a LMM!"
                    }
                }
            }
            if(include_block == TRUE & include_nest == FALSE){
                if(the_design == "rcbd"){
                    if(the_model == "lm"){
                        formula_string <- paste0(formula_string, " + ", block_label)
                        model_notes_string <- ifelse(total_levels > 2,
                                                     "This is equivalent to a univariate model RM-ANOVA!",
                                                     "This is equivalent to a paired t-test!")
                    }
                    if(the_model == "lmer"){
                        formula_string <- paste0(formula_string, " + (1 | ", block_label, ")")
                        model_notes_string <- ifelse(total_levels > 2,
                                                     "This is equivalent to a univariate model RM-ANOVA!",
                                                     "This is equivalent to a paired t-test!")
                    }
                    if(the_model == "aov_4"){
                        formula_string <- paste0(formula_string, " + (", fixed_part, " | ", block_label, ")")
                        model_notes_string <- ifelse(total_levels > 2,
                                                     "This is equivalent to a multivariate model RM-ANOVA!",
                                                     "This is equivalent to a paired t-test!")
                    }
                }
              if(the_design == "spd"){
                    if(the_model == "lm"){
                        formula_string <- "lm doesn't work with a SPD"
                    }
                    if(the_model == "lmer"){
                        formula_string <- paste0(formula_string, " + (1 | ", block_label, ")")
                    }
                    if(the_model == "aov_4"){
                        formula_string <- paste0(formula_string, " + (", subplot_label, " | ", block_label, ")")
                    }
                }
            }
            if(include_nest == TRUE & include_block == TRUE){
              if(the_design == "rcbds"){
                if(the_model == "lm"){
                  formula_string <- paste0(formula_string, " + ", block_label, " + ", nest_label)
                  model_notes_string <- "Danger! You have pseudoreplication! Use lmer or aov_4."
                }
                if(the_model == "lmer"){
                  formula_string <- paste0(formula_string, " + (", fixed_part, " | ", block_label, ")")
                  model_notes_string <- "This is equivalent to a multivariate model RM-ANOVA!"
                }
                if(the_model == "aov_4"){
                  formula_string <- paste0(formula_string, " + (", fixed_part, " | ", block_label, ")")
                  model_notes_string <- "This is equivalent to a linear mixed model with random intercept and slope"
                }
              }
            }

            # print plot notes
            self$results$plot_notes$setContent(plot_notes_string)
            
            # print model formula
            family_string <- ""
            if(family_class == "generalized"){
              if(the_family == "gamma"){
                family_string <- ', family = Gamma(link = "log")'
              }
            }
            model_string <- paste0(the_model, "(", formula_string, family_string, ")")
            self$results$model$setContent(model_string)

            # print model notes
            self$results$model_notes$setContent(model_notes_string)

            # fit model
            model_formula <- formula_string |>
                as.formula()
            if(the_model == "lm" & the_family == "norm"){
                m1 <- lm(model_formula, self$data)
            }
            if(the_model == "glm" & the_family == "gamma"){
              m1 <- glm(model_formula, family = Gamma(link = "log"), self$data)
            }
            if(the_model == "lmer" & the_family == "norm"){
                m1 <- lmer(model_formula, self$data)
            }
            if(the_model == "glmer" & the_family == "gamma"){
              m1 <- glmer(model_formula, family = Gamma(link = "log"), self$data)
            }
            if(the_model == "aov_4" & the_family == "norm"){
              m1 <- aov_4(model_formula, self$data)
            }

            # model results
            self$results$coef$setContent(coef(summary(m1)))

            if(family_class == "general"){
              m1_emm <- emmeans(m1, specs = spec_list)
            }else{
              m1_emm <- emmeans(m1, specs = spec_list, type = "response")
            }
            # emm results
            self$results$emm$setContent(m1_emm)

            if(two_factors == FALSE){
                if(plot_adjust == "default"){
                    m1_pairs <- contrast(m1_emm,
                                         method = plot_trt.vs.ctrl) |>
                        summary(infer = TRUE)
                }else{
                    m1_pairs <- contrast(m1_emm,
                                         method = plot_trt.vs.ctrl,
                                         adjust = plot_adjust) |>
                        summary(infer = TRUE)
                }
            }else{ # two factors
                if(plot_simple == TRUE){
                    if(plot_adjust == "default"){
                        m1_pairs <- contrast(m1_emm,
                                             method = "revpairwise",
                                             simple = "each",
                                             combine = TRUE) |>
                            summary(infer = TRUE)

                    }else{
                        m1_pairs <- contrast(m1_emm,
                                             method = "revpairwise",
                                             adjust = plot_adjust,
                                             simple = "each",
                                             combine = TRUE) |>
                            summary(infer = TRUE)
                    }
                }else{
                    if(plot_adjust == "default"){
                        m1_pairs <- contrast(m1_emm,
                                             method = "revpairwise") |>
                            summary(infer = TRUE)
                    }else{
                        m1_pairs <- contrast(m1_emm,
                                             method = "revpairwise",
                                             adjust = plot_adjust) |>
                            summary(infer = TRUE)
                    }
                }
            }
            # contrasts results
            self$results$contrasts$setContent(m1_pairs)
            
#            table <- self$results$ttest # use this line to find if code gets this far
            # create the datasets of measurement reps
            # for non-nested data, these are the experimental reps
            # for nested data, these are technical reps
            # then compute the nest means = experimental reps next

            plotData_full <- data.table(
              dataset = "exp_reps", # change this to technical_reps if nested
              y = self$data[[self$options$dep]],
              factor_1 = self$data[[self$options$factor1]]
            )
            if(two_factors == FALSE){
                plotData_full[, factor_2 := factor_1]
                plotData_full[, plot_factor := factor_1]
            }else{
                plotData_full[, factor_2 := self$data[[self$options$factor2]]]
                plotData_full[, plot_factor := paste(factor_1, factor_2,
                                                     sep = "\n")]
                # reorder factor levels for 2-factor plots
                levels_1 <- levels(plotData_full$factor_1) |>
                    as.character()
                levels_2 <- levels(plotData_full$factor_2) |>
                    as.character()
                levels_table <- expand.grid(levels_1, levels_2, stringsAsFactors = FALSE) |>
                    data.table()
                levels_table[, plot_levels := paste(Var1, Var2, sep = "\n")]
                plotData_full[, plot_factor := factor(plot_factor,
                                                 levels = levels_table[, plot_levels])]
            }

            if(include_block == FALSE){
                plotData_full[, block_id := NA]
            }else{
                plotData_full[, block_id := self$data[[self$options$block]]]
            }
            if(include_nest == FALSE){
                plotData_full[, nest_id := NA]
            }else{
                plotData_full[, nest_id := self$data[[self$options$nest]]]
            }
            plotData_full[, mean := as.numeric(NA)]
            plotData_full[, SE := as.numeric(NA)]
            plotData_full[, lo := as.numeric(NA)]
            plotData_full[, hi := as.numeric(NA)]

            # compute nest means if nested
            if(include_nest == TRUE){
                nest_means <- plotData_full[, .(y = mean(y, na.rm = TRUE)),
                                            by = .(factor_1, factor_2, plot_factor, block_id, nest_id)]
                # for prediction to get y, we need factor nest and block labels
                new_data <- copy(nest_means)
                setnames(new_data,
                         old = c("factor_1", "nest_id"),
                         new = c(factor1_label, nest_label))
                if(two_factors == TRUE){
                  setnames(new_data,
                           old = "factor_2",
                           new = factor2_label)
                }
                if(include_block == TRUE){
                  setnames(new_data,
                           old = "block_id",
                           new = block_label)
                }
                
                y_hat <- predict(m1, new_data)
                if(family_class == "generalized"){
                  y_hat <- predict(m1, new_data, type = "response")
                }
                plotData_nest_means <- data.table(
                    dataset = "exp_reps",
                    y = y_hat,
                    factor_1 = nest_means[, factor_1],
                    factor_2 = nest_means[, factor_2],
                    plot_factor = nest_means[, plot_factor],
                    block_id = nest_means[, block_id],
                    nest_id = nest_means[, nest_id],
                    mean = NA,
                    SE = NA,
                    lo = NA,
                    hi = NA
                )
                plotData_full[, dataset := "tech_reps"]
            }

            # m1_emm and m1_pairs have variable columns and column names so here I fit
            # and 2X data flattened (all combos in a single treatment factor) using 
            # lm (even if the specified model is mixed or generalized). This is because
            # I want the x-axis labels to be those of the flattened model. So all of
            # this just get's me the plot_factor column - the means, CIs, p-values
            # are all from the specified model fit.
            
            # these are the tables needed to send to ggplot
            m1_emm_dt <- m1_emm |>
              summary() |>
              data.table()
            m1_pairs_dt <- m1_pairs |>
              data.table()
            # these are the tables that get the plot_factor column correct
            m1_plot <- lm(y ~ plot_factor, plotData_full)
            emm_plot <- emmeans(m1_plot, specs = "plot_factor")
            pairs_plot <- contrast(emm_plot,
                                   method = "revpairwise",
                                   adjust = "none") |>
              summary(infer = TRUE) |>
              data.table()
            if(two_factors == FALSE){
              plot_factor_names <- summary(emm_plot)[,1] # aov_4 can modify factor names so this returns it to original
              colnames(m1_emm_dt) <- c("plot_factor", "mean", "SE", "df", "lo", "hi")
              m1_emm_dt[, plot_factor := plot_factor_names]
              m1_pairs_dt[, contrast := pairs_plot[, 1]]
            }else{
                if(plot_simple == TRUE){
                    # keep simple effects only
                    levels_1 <- levels(plotData_full$factor_1) |>
                        as.character()
                    levels_2 <- levels(plotData_full$factor_2) |>
                        as.character()
                    p1 <- length(levels_1)
                    p2 <- length(levels_2)
                    pp <- p1*p2
                    rows_list <- numeric(pp)
                    j <- 0
                    for(i in 1:(pp/2)){
                        j <- j + 1
                        a <- i
                        b <- (pp * (pp - 1))/2 - i + 1
                        rows_list[j] <- a
                        j <- j + 1
                        rows_list[j] <- b
                    }
                    pairs_plot <- pairs_plot[rows_list,]
                }
                new_contrast_col <- pairs_plot[, contrast]
                m1_pairs_dt[, contrast := new_contrast_col]
                
                # replace flattened names in m1_emm
                colnames(m1_emm_dt) <- c("factor1", "factor2", "mean", "SE", "df", "lo", "hi")
                m1_emm_dt[, plot_factor := paste(factor1, factor2, sep = "\n")]
                m1_emm_dt <- m1_emm_dt[, .SD, .SDcols = c("plot_factor", "mean", "SE", "df", "lo", "hi")]
            }
            # unify colnames for different kinds of emm and pairs tables
            # colnames(emm_plot)[2,5,6] <- c("mean", "lo", "hi")
            setnames(m1_pairs_dt, old = "SE", new = "SED")
            

            # fill out summary table for means and error bars
            y_cols <- c("dataset", "y", "factor_1", "factor_2", "plot_factor",
                        "block_id", "nest_id", "mean", "SE", "lo", "hi")
            plotData_summary <- data.table(
                dataset = "emmeans",
                y = as.numeric(NA),
                factor_1 = NA,
                factor_2 = NA,
                block_id = NA,
                nest_id = NA,
                m1_emm_dt
            )
            plotData_summary <- plotData_summary[, .SD, .SDcols = y_cols]

            # combine summary and full data because ggplot in jmv can
            # only take a single dataset
            plotData <- rbind(plotData_summary, plotData_full)
            if(include_nest){
                plotData <- rbind(plotData, plotData_nest_means)
            }

            # for plot, I need integer for xpos of groups since
            # jmv isn't correctly ordering factor levels
            plotData[, plot_factor_id := as.integer(plot_factor) |>
                         as.character()]
            plot_factor_levels <- levels(plotData$plot_factor)

            # p-value table
            groups <- unlist(str_split(m1_pairs_dt$contrast, " - "))
            # remove parentheses if they exist from group cols
            groups <- lapply(groups, remove_parentheses) |>
                unlist()
            i_seq <- 1:length(groups)
            m1_pairs_dt[, group1 := groups[i_seq%%2 != 0] |>
                           factor(levels = levels(plotData_full$plot_factor))]
            m1_pairs_dt[, group2 := groups[i_seq%%2 == 0] |>
                           factor(levels = levels(plotData_full$plot_factor))]
            m1_pairs_dt[, minx := as.integer(group1) |> as.character()]
            m1_pairs_dt[, maxx := as.integer(group2) |> as.character()]
            m1_pairs_dt[, p.print := ifelse(p.value > 0.1,
                                           p_format(round(p.value, digits = 2),
                                                    add.p = TRUE, leading.zero = FALSE, space = FALSE),
                                           p_format(round(p.value, digits = 3),
                                                    add.p = TRUE, leading.zero = FALSE, space = FALSE))]
            # get y position of p-value brackets
            if(show_tech_reps == TRUE){
                maxy <- max(plotData[dataset == "tech_reps", y], na.rm = TRUE)
                miny <- min(plotData[dataset == "tech_reps", y], na.rm = TRUE)
            }else{
                maxy <- max(plotData[dataset == "exp_reps", y], na.rm = TRUE)
                miny <- min(plotData[dataset == "exp_reps", y], na.rm = TRUE)
            }
            p_increment <- 0.05*(maxy - miny)
            m1_pairs_dt[, y_pos := maxy + .I * p_increment ]

            # add pairs_plot to plotData
            n_rows <- nrow(plotData) - nrow(pairs_plot)
            n_col <- ncol(m1_pairs_dt)
            add_na <- matrix(nrow = n_rows, ncol = n_col)
            colnames(add_na) <- colnames(m1_pairs_dt)
            m1_pairs_dt <- rbind(m1_pairs_dt, add_na)

            plotData <- cbind(plotData, m1_pairs_dt)

            image <- self$results$plot
            image$setState(plotData)
            
            image_check <- self$results$check
            image_check$setState(m1)
            
        },
        .plot=function(image, ...) {
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
          
          plotData <- image$state |>
            data.table()
          
          # plot options
          # tech replicates
          show_tech_reps <- ifelse(self$options$tech_reps == TRUE, TRUE, FALSE)
          if(is.null(self$options$nest)){show_tech_reps <- FALSE}
          # mods for joining blocks
          include_block <- ifelse(is.null(self$options$block), FALSE, TRUE)
          join_blocks <- ifelse(include_block == TRUE, self$options$join_blocks, FALSE)
          # # if join, then don't jitter
          jitter_spread <- ifelse(join_blocks == TRUE, 0, 0.2)
          line_color <- ifelse(join_blocks == TRUE, "gray", "white")
          # y-axis label
          y_label <- ifelse(is.null(self$options$y_label), self$options$dep, self$options$y_label)
          y_label <- ifelse(self$options$y_label == "", self$options$dep, self$options$y_label)
          
          # plot it!
          plot <- ggplot(plotData,
                         aes(x = plot_factor_id,
                             y = mean))
          if(join_blocks == TRUE){
            plot <- plot +
              geom_line(aes(x = plot_factor_id,
                            y = y,
                            group = block_id),
                        color = line_color
              )
          }
          if(show_tech_reps == TRUE){
            plot <- plot +
              geom_sina(data = plotData[dataset == "tech_reps"],
                        aes(x = plot_factor_id, y = y),
                        scale = "width",
                        maxwidth = jitter_spread,
                        size = 1,
                        color = "gray",
                        show.legend = FALSE)
          }
          plot <- plot +
            geom_jitter(data = plotData[dataset == "exp_reps"],
                        aes(x = plot_factor_id, y = y, color = factor_1),
                        width = jitter_spread,
                        size = 2,
                        show.legend = FALSE) +
            geom_errorbar(aes(ymin = lo, ymax = hi, width=.1),
                          show.legend = FALSE) +
            geom_point(aes(x = plot_factor_id, y = mean),
                       size = 4,
                       show.legend = FALSE) +
            ylab(y_label) +
            scale_x_discrete(labels = levels(plotData$plot_factor)) +
            theme_pubr() +
            theme(axis.title.x = element_blank())
          plot <- plot +
            stat_pvalue_manual(plotData,
                               label = "p.print",
                               y.position = "y_pos",
                               xmin = "minx",
                               xmax = "maxx",
                               size = 4,
                               tip.length = 0.01)
          if(self$options$pal != "pal_ggplot"){
            plot <- plot +
              scale_color_manual(values = get(self$options$pal))
          }
          print(plot)
          TRUE
        },
        .plot_check=function(image_check, ...){
          m1 <- image_check$state
          # check the model
          check_m1 <- simulateResiduals(fittedModel = m1,
                                        n = 250,
                                        refit = FALSE)
          g <- plot(check_m1)
          print(g)
          TRUE
        })
)
