
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
      
      mb_notes_str <- ""
      plot_notes_string <- ""
      model_notes_string <- ""
      # print model
      
      # model data
      two_factors <- ifelse(is.null(self$options$factor2), FALSE, TRUE)
      include_block <- ifelse(is.null(self$options$block), FALSE, TRUE)
      include_nest <- ifelse(is.null(self$options$nest_id), FALSE, TRUE)
      response_label <- self$options$dep
      factor1_label <- self$options$factor1
      factor2_label <- ifelse(two_factors == TRUE,
                              self$options$factor2,
                              NA)
      block_label <- ifelse(include_block == TRUE,
                            self$options$block,
                            NA)
      nest_label <- ifelse(include_nest == TRUE,
                           self$options$nest_id,
                           NA)
      subplot_item <- self$options$subplot
      # **** need to create subplot_label for SPD to work!
      
      # model_options
      the_design <- self$options$design
      the_model <- self$options$model
      model_family <- self$options$family
      mb_hints_all <- ifelse(self$options$mb_hints == TRUE, FALSE, TRUE)
      check_the_model <- ifelse(self$options$check_the_model == TRUE, TRUE, FALSE)
      
      # plot options
      plot_trt.vs.ctrl <- ifelse(self$options$trtvsctrl == TRUE,
                                 "trt.vs.ctrl",
                                 "revpairwise")
      plot_simple <- self$options$simple
      plot_adjust <- self$options$adjust
      show_tech_reps <- ifelse(self$options$tech_reps == TRUE, TRUE, FALSE)
      if(include_nest == FALSE){show_tech_reps <- FALSE}
      
      plot_notes_string <- paste("The 95% confidence intervals are computed from the model error (sigma)",
                                 "and not from the sample standard error of each group.", sep = "\n ")
      if(model_family == "norm"){
        model_notes_string <- paste("The fit model assumes Independent and Identically Distributed (IID) responses",
                                    "(the Normal, Homogenous variance, Independent assumption) that is, each conditional response",
                                    "was independently sampled from the same, normal distribution")
      }
      model_notes_string <- ""
      
      # fixed, mixed, or anova
      if(the_model == "lm" | the_model == "glm"){
        model_class <- "fixed"
      }
      if(the_model == "aov_4"){
        model_class <- "anova"
      }
      if(the_model == "lmer" | the_model == "glmer"){
        model_class <- "mixed"
      }
      family_class <- ifelse(the_model == "glm" | the_model == "glmer", "generalized", "general")
      
      # response variable type
      y <- self$data[, response_label]
      y_type <- "character"
      if(length(unique(y)) == 2){
        y_type <- "binary"
      }
      if(y_type != "binary"){
        if(min(y) >= 0 & max(y) <= 1){
          y_type <- "proportion"
        }
        if(all(round(y, 0) == y) & min(y) >= 0){
          y_type <- "count"
          y_family_long <- "Negative Binomial or Quasipoisson"
          y_family <- "negbin"
        }
        if(any(round(y, 0) != y) & min(y) > 0 & max(y) > 1){
          y_type <- "positive continuous"
          y_family_long <- "Gamma"
          y_family <- "gamma"
        }
        if(any(round(y, 0) != y) & min(y) <= 0){
          y_type <- "continuous"
        }
      }
      
      # model builder notes
      mb_notes_level <- 2 # 1 is corrective, 2 is all
      mb_notes_str <- "no notes"
      working_model <- TRUE
      if(is.null(response_label) & is.null(factor1_label)){
        mb_notes_str <- "A response variable and a factor variable need to be chosen"
        working_model <- FALSE
      }
      if(is.null(response_label) & !is.null(factor1_label)){
        mb_notes_str <- "A response variable needs to be chosen"
        working_model <- FALSE
      }
      if(!is.null(response_label) & is.null(factor1_label)){
        mb_notes_str <- "A factor variable needs to be chosen"
        working_model <- FALSE
      }
      if(working_model == TRUE){
        if(include_nest == FALSE & include_block == FALSE){
          mb_notes_str <- paste("This is a Completely Randomized Design with a",
                                y_type,
                                "response.")
          # mb_notes_str <- paste(mb_notes_str,
          #                                     "\n 1. Choose CRDS in the Experimental Design picker.")
          if (mb_hints_all == TRUE | model_class != "fixed"){
            add_str <- "\n -- Model this with lm or glm in the Model picker."
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(model_class != "fixed"){
              working_model <- FALSE
            }
          }
          if(mb_hints_all == TRUE | (family_class == "general" & model_family != "norm")){
            add_str <- "\n -- lm requires Model Family: Normal in the Model picker."
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(family_class == "general" & model_family != "norm"){
              working_model <- FALSE}
          }
          if(mb_hints_all == TRUE | (family_class == "generalized" & model_family != y_family)){
            add_str <- paste("\n -- glm should use Model Family:",
                             y_family_long,
                             "in the Model picker.")
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(family_class == "generalized" & model_family != y_family){
              working_model <- FALSE}
          }
          the_design <- "crd"
        }
        if(include_nest == TRUE & include_block == FALSE){
          mb_notes_str <- paste("This is a subsampled Completely Randomized Design with a",
                                y_type,
                                "response.")
          # mb_notes_str <- paste(mb_notes_str,
          #                                     "\n 1. Choose CRDS in the Experimental Design picker.")
          if (mb_hints_all == TRUE | model_class == "fixed"){
            add_str <- "\n -- Model this with lmer, aov_4, or glmer in the Model picker."
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(model_class == "fixed"){
              working_model <- FALSE
            }
          }
          if(mb_hints_all == TRUE | (family_class == "general" & model_family != "norm")){
            add_str <- "\n -- lmer / aov_4 require Model Family: Normal in the Model picker."
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(family_class == "general" & model_family != "norm"){
              working_model <- FALSE}
          }
          if(mb_hints_all == TRUE | (family_class == "generalized" & model_family != y_family)){
            add_str <- paste("\n -- glmer should use Model Family:",
                             y_family_long,
                             "in the Model picker.")
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(family_class == "generalized" & model_family != y_family){
              working_model <- FALSE}
          }
          the_design <- "crds"
        }
        if(include_nest == FALSE & include_block == TRUE){
          mb_notes_str <- paste("This is a Randomized Complete Block Design with a",
                                y_type,
                                "response.")
          # mb_notes_str <- paste(mb_notes_str,
          #                                     "\n 1. Choose CRDS in the Experimental Design picker.")
          if (mb_hints_all == TRUE | model_class == "fixed"){
            add_str <- "\n -- Model this with lmer, aov_4, or glmer in the Model picker."
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(model_class == "fixed"){
              working_model <- FALSE
            }
          }
          if(mb_hints_all == TRUE | (family_class == "general" & model_family != "norm")){
            add_str <- "\n -- lmer / aov_4 require Model Family: Normal in the Model picker."
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(family_class == "general" & model_family != "norm"){
              working_model <- FALSE}
          }
          if(mb_hints_all == TRUE | (family_class == "generalized" & model_family != y_family)){
            add_str <- paste("\n -- glmer should use Model Family:",
                             y_family_long,
                             "in the Model picker.")
            mb_notes_str <- paste(mb_notes_str, add_str)
            if(family_class == "generalized" & model_family != y_family){
              working_model <- FALSE}
          }
          the_design <- "rcbd"
        }
      }
      
      if(working_model == TRUE){
        
        model_data <- create_model_data(self$data,
                                        response_label,
                                        factor1_label,
                                        factor2_label,
                                        block_label,
                                        nest_label,
                                        two_factors,
                                        include_block,
                                        include_nest
        )
      }
      
      # Stats output
      if(working_model == TRUE){
        # get formulas
        if(two_factors == FALSE){
          fixed_part <- factor1_label
          formula_string <- paste(response_label, "~", fixed_part)
          spec_list <- factor1_label
          if(include_block == FALSE & include_nest == FALSE){
            if(length(levels(model_data$factor_1)) == 2){
              model_notes_string <- "This LM is equivalent to a Student's (Independent Samples) t-test!"
            }else{
              model_notes_string <- "This LM is equivalent to a one-way ANOVA!"
            }
          }
          total_levels <- length(levels(model_data$factor_1))
        }else{
          fixed_part <- paste(factor1_label, " * ", factor2_label)
          formula_string <- paste(response_label, "~", fixed_part)
          spec_list <- c(factor1_label, factor2_label)
          total_levels <- length(levels(model_data$factor_1)) +
            length(levels(model_data$factor_2))
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
        # print model formula
        family_string <- ""
        if(family_class == "generalized"){
          if(model_family == "gamma"){
            family_string <- ', family = Gamma(link = "log")'
          }
        }
        model_string <- paste0(the_model, "(", formula_string, family_string, ")")
      }
      
      
      # fit model
      if(working_model == TRUE){
        model_formula <- formula_string |>
          as.formula()
        if(the_model == "lm" & model_family == "norm"){
          m1 <- lm(model_formula, model_data)
        }
        if(the_model == "glm" & model_family == "gamma"){
          m1 <- glm(model_formula, family = Gamma(link = "log"), model_data)
        }
        if(the_model == "lmer" & model_family == "norm"){
          m1 <- lmer(model_formula, model_data)
        }
        if(the_model == "glmer" & model_family == "gamma"){
          m1 <- glmer(model_formula, family = Gamma(link = "log"), model_data)
        }
        if(the_model == "aov_4" & model_family == "norm"){
          m1 <- aov_4(model_formula, model_data)
        }
        
        if(family_class == "general"){
          m1_emm <- emmeans(m1, specs = spec_list)
        }else{
          m1_emm <- emmeans(m1, specs = spec_list, type = "response")
        }
        
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
        
      }
      
      # print model builder notes
      self$results$model_builder_notes$setContent(mb_notes_str)
      # print plot notes
      self$results$plot_notes$setContent(plot_notes_string)
      # print model notes
      self$results$model_notes$setContent(model_notes_string)
      if(working_model == TRUE){
        # print model
        self$results$model$setContent(model_string)
        # model results
        self$results$coef$setContent(coef(summary(m1)))
        # emm results
        self$results$emm$setContent(m1_emm)
        # contrasts results
        self$results$contrasts$setContent(m1_pairs)
      }
      
      #            table <- self$results$ttest # use this line to find if code gets this far


      if(working_model == TRUE){
        image <- self$results$plot
        # image$setState(plotData)
        model_list <- list(m1, m1_emm, m1_pairs)
        image$setState(model_list)
      }else{
        model_list <- NULL
      }
      
      # prepare m1_check model for check_the_model
      # use nest means if there are subsampled reps
      m1_check <- NULL
      if(working_model == TRUE){
        if(check_the_model == TRUE){
          m1_check <- copy(m1)
        }else{
          m1_check <- NULL
        }
        if(model_class == "anova"){m1_check <- NULL}
        do_model_check <- TRUE
        if(do_model_check == TRUE){
          image_check <- self$results$check
          image_check$setState(m1_check)
        }
      }
      
      ### DEBUG ONLY remove this when 
      m1_check <- NULL
      
    },
    .plot=function(image, ...) {
      model_list <- image$state
      if(!is.null(model_list)){
        m1 <- model_list[[1]]
        m1_emm <- model_list[[2]]
        m1_pairs <- model_list[[3]]
        
        # plot features
        join_blocks_val <- ifelse(self$options$join_blocks == TRUE, TRUE, FALSE)
        show_nest_data_val <- ifelse(self$options$tech_reps == TRUE, TRUE, FALSE)
        nest_id_val <- ifelse(is.null(self$options$nest_id), NA, self$options$nest_id)
        palette_val <- self$options$pal
        jitter_width_val <- ifelse(join_blocks_val == TRUE, 0, 0.2)
        
        # get the plot!
        gg <- plot_response(m1,
                            m1_emm,
                            m1_pairs,
                            show_nest_data = show_nest_data_val,
                            join_blocks = join_blocks_val,
                            nest_id = nest_id_val,
                            jitter_width = jitter_width_val,
                            palette = palette_val)
        print(gg)
        TRUE       
      }
    },
    .plot_check=function(image_check, ...){
      # check the model
      m1_check <- image_check$state
      if(!is.null(m1_check)){
        check_m1 <- simulateResiduals(fittedModel = m1_check,
                                      n = 250,
                                      refit = FALSE)
        g <- plot(check_m1)
        print(g)
        TRUE
      }
    })
)
