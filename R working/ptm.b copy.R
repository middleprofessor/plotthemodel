
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

            # two factors?
            two_factors <- ifelse(is.null(self$options$factor2), FALSE, TRUE)

            # create the plot dataset
            plotData_full <- data.table(
                y := self$data[[self$options$dep]],
                factor_1 = self$data[[self$options$factor1]]
            )
            if(two_factors == FALSE){
                plotData_full[, factor_2 := factor_1]
                plotData_full[, plot_factor := factor_1]
            }else{
                plotData_full[, factor_2 := self$data[[self$options$factor1]]]
                plotData_full[, plot_factor := paste(self$data[[self$options$factor1]],
                                                     self$data[[self$options$factor2]],
                                                     sep = "\n")]
            }

            # get formulas
            if(two_factors == FALSE){
                formula <- paste(self$options$dep, "~", self$options$factor1) |>
                    as.formula()
                spec_list <- self$options$factor1

            }else{
                formula <- paste(self$options$dep, "~",
                                 self$options$factor1, " * ",
                                 self$options$factor2) |>
                    as.formula()
                spec_list <- c(self$options$factor1,
                               self$options$factor2)

            }

            m1 <- lm(formula, self$data)

            # model results
            self$results$coef$setContent(coef(summary(m1)))

            m1_emm <- emmeans(m1, specs = spec_list)
            # emm results
            self$results$emm$setContent(m1_emm)

            if(two_factors == FALSE){
                m1_pairs <- contrast(m1_emm,
                                     method = "revpairwise",
                                     adjust = "none") |>
                    summary(infer = TRUE)
            }else{
                m1_pairs <- contrast(m1_emm,
                                     method = "revpairwise",
                                     adjust = "none",
                                     simple = "each",
                                     combine = TRUE) |>
                    summary(infer = TRUE)
            }
            # contrasts results
            self$results$contrasts$setContent(m1_pairs)

            # for plot, if two-factor, redo flattened
            if(two_factors == TRUE){
                plotData_full <- data.table(
                    factor_1 = paste(self$data[[self$options$factor1]],
                                     self$data[[self$options$factor2]],
                                     sep = "\n"),
                    factor_color = self$data[[self$options$factor1]],
                    y = self$data[[self$options$dep]]
                )
                m1 <- lm(y ~ factor_1, data = plotData_full)
                m1_emm <- emmeans(m1,
                                  specs = "factor_1")
            }

            # fill out summary table for means and error bars
            y_cols <- c("factor_1", "factor_color", "mean", "SE", "lo", "hi")
            plotData_summary <- summary(m1_emm) |>
                data.table()
 #           if(two_factors == FALSE){
                colnames(plotData_summary) <- c("factor_1", "mean", "SE", "df", "lo", "hi")
                setnames(plotData_summary,
                         old = colnames(plotData_summary),
                         new = c("factor_1", "mean", "SE", "df", "lo", "hi"))
                plotData_summary[, factor_color := factor_1]
 #           }else{
 #               colnames(plotData_summary) <- c("factor_1", "factor_color", "mean", "SE", "df", "lo", "hi")
 #           }
            plotData_summary <- plotData_summary[, .SD,
                                                 .SDcols = y_cols]
            plotData_summary$y <- as.numeric(NA)

            # fill out full table for stripchart
            y <- self$data[[self$options$dep]]
            plotData_full <- data.table(
                factor_1 = self$data[[self$options$factor1]],
                factor_color = self$data[[self$options$factor1]],
                mean = as.numeric(NA),
                SE = as.numeric(NA),
                lo = as.numeric(NA),
                hi = as.numeric(NA),
                y = y
            )
            # if(two_factors == TRUE){
            #     plotData_full[, factor_color := self$data[[self$options$factor2]]]
            #     plotData_full[, factor_1 := paste(factor_1, factor_color, sep = "\n")]
            # }
            plotData <- rbind(plotData_summary, plotData_full)

            image <- self$results$plot
            image$setState(plotData)

        },
        .plot=function(image, ...) {
            plotData <- image$state
            plot <- ggplot(plotData,
                           aes(x = factor_1,
                               y = mean)) +
                geom_errorbar(aes(ymin = lo, ymax = hi, width=.1),
                              show.legend = FALSE) +
                geom_point(aes(x = factor_1, y = mean),
                           size = 2,
                           show.legend = FALSE) +
                geom_jitter(aes(x = factor_1, y = y),
                            width = 0.1,
                            color = "gray",
                            show.legend = FALSE) +
                ylab(self$options$dep) +
                theme_pubr() +
                theme(axis.title.x = element_blank())
            print(plot)
            TRUE
        })
)
