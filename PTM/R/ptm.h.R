
# This file is automatically generated, you probably don't want to edit this

PTMOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "PTMOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            factor1 = NULL,
            factor2 = NULL,
            block = NULL,
            nest_id = NULL,
            covariate_id = NULL,
            offset = FALSE,
            log_covariate = FALSE,
            mb_hints = FALSE,
            design = "crd",
            subplot = "subplot.eq.1",
            model = "lm",
            family = "norm",
            check_the_model = FALSE,
            trtvsctrl = FALSE,
            simple = TRUE,
            adjust = "default",
            hide_pairs = NULL,
            pal = "pal_okabe_ito",
            tech_reps = FALSE,
            join_blocks = FALSE,
            plot_width = 400,
            plot_height = 300,
            font_size = 12,
            y_label = NULL,
            y_units = NULL,
            x_label = NULL,
            rescale = 1, ...) {

            super$initialize(
                package="PTM",
                name="PTM",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep)
            private$..factor1 <- jmvcore::OptionVariable$new(
                "factor1",
                factor1)
            private$..factor2 <- jmvcore::OptionVariable$new(
                "factor2",
                factor2)
            private$..block <- jmvcore::OptionVariable$new(
                "block",
                block)
            private$..nest_id <- jmvcore::OptionVariable$new(
                "nest_id",
                nest_id)
            private$..covariate_id <- jmvcore::OptionVariable$new(
                "covariate_id",
                covariate_id)
            private$..offset <- jmvcore::OptionBool$new(
                "offset",
                offset,
                default=FALSE)
            private$..log_covariate <- jmvcore::OptionBool$new(
                "log_covariate",
                log_covariate,
                default=FALSE)
            private$..mb_hints <- jmvcore::OptionBool$new(
                "mb_hints",
                mb_hints,
                default=FALSE)
            private$..design <- jmvcore::OptionList$new(
                "design",
                design,
                options=list(
                    "crd",
                    "crds",
                    "rcbd",
                    "rcbds",
                    "grcbd",
                    "grcbds",
                    "spd",
                    "gspd"),
                default="crd")
            private$..subplot <- jmvcore::OptionList$new(
                "subplot",
                subplot,
                options=list(
                    "subplot.eq.1",
                    "subplot.eq.2"),
                default="subplot.eq.1")
            private$..model <- jmvcore::OptionList$new(
                "model",
                model,
                options=list(
                    "lm",
                    "lmer",
                    "aov_4",
                    "glm",
                    "glmer"),
                default="lm")
            private$..family <- jmvcore::OptionList$new(
                "family",
                family,
                options=list(
                    "norm",
                    "qpoisson",
                    "negbin",
                    "gamma",
                    "qbinom",
                    "binom"),
                default="norm")
            private$..check_the_model <- jmvcore::OptionBool$new(
                "check_the_model",
                check_the_model,
                default=FALSE)
            private$..trtvsctrl <- jmvcore::OptionBool$new(
                "trtvsctrl",
                trtvsctrl,
                default=FALSE)
            private$..simple <- jmvcore::OptionBool$new(
                "simple",
                simple,
                default=TRUE)
            private$..adjust <- jmvcore::OptionList$new(
                "adjust",
                adjust,
                options=list(
                    "default",
                    "none",
                    "holm",
                    "fdr"),
                default="default")
            private$..hide_pairs <- jmvcore::OptionString$new(
                "hide_pairs",
                hide_pairs)
            private$..pal <- jmvcore::OptionList$new(
                "pal",
                pal,
                options=list(
                    "pal_ggplot",
                    "pal_okabe_ito",
                    "pal_okabe_ito_blue",
                    "pal_npg",
                    "pal_aaas",
                    "pal_jco",
                    "pal_frontiers"),
                default="pal_okabe_ito")
            private$..tech_reps <- jmvcore::OptionBool$new(
                "tech_reps",
                tech_reps,
                default=FALSE)
            private$..join_blocks <- jmvcore::OptionBool$new(
                "join_blocks",
                join_blocks,
                default=FALSE)
            private$..plot_width <- jmvcore::OptionInteger$new(
                "plot_width",
                plot_width,
                default=400)
            private$..plot_height <- jmvcore::OptionInteger$new(
                "plot_height",
                plot_height,
                default=300)
            private$..font_size <- jmvcore::OptionInteger$new(
                "font_size",
                font_size,
                default=12)
            private$..y_label <- jmvcore::OptionString$new(
                "y_label",
                y_label)
            private$..y_units <- jmvcore::OptionString$new(
                "y_units",
                y_units)
            private$..x_label <- jmvcore::OptionString$new(
                "x_label",
                x_label)
            private$..rescale <- jmvcore::OptionNumber$new(
                "rescale",
                rescale,
                default=1)

            self$.addOption(private$..dep)
            self$.addOption(private$..factor1)
            self$.addOption(private$..factor2)
            self$.addOption(private$..block)
            self$.addOption(private$..nest_id)
            self$.addOption(private$..covariate_id)
            self$.addOption(private$..offset)
            self$.addOption(private$..log_covariate)
            self$.addOption(private$..mb_hints)
            self$.addOption(private$..design)
            self$.addOption(private$..subplot)
            self$.addOption(private$..model)
            self$.addOption(private$..family)
            self$.addOption(private$..check_the_model)
            self$.addOption(private$..trtvsctrl)
            self$.addOption(private$..simple)
            self$.addOption(private$..adjust)
            self$.addOption(private$..hide_pairs)
            self$.addOption(private$..pal)
            self$.addOption(private$..tech_reps)
            self$.addOption(private$..join_blocks)
            self$.addOption(private$..plot_width)
            self$.addOption(private$..plot_height)
            self$.addOption(private$..font_size)
            self$.addOption(private$..y_label)
            self$.addOption(private$..y_units)
            self$.addOption(private$..x_label)
            self$.addOption(private$..rescale)
        }),
    active = list(
        dep = function() private$..dep$value,
        factor1 = function() private$..factor1$value,
        factor2 = function() private$..factor2$value,
        block = function() private$..block$value,
        nest_id = function() private$..nest_id$value,
        covariate_id = function() private$..covariate_id$value,
        offset = function() private$..offset$value,
        log_covariate = function() private$..log_covariate$value,
        mb_hints = function() private$..mb_hints$value,
        design = function() private$..design$value,
        subplot = function() private$..subplot$value,
        model = function() private$..model$value,
        family = function() private$..family$value,
        check_the_model = function() private$..check_the_model$value,
        trtvsctrl = function() private$..trtvsctrl$value,
        simple = function() private$..simple$value,
        adjust = function() private$..adjust$value,
        hide_pairs = function() private$..hide_pairs$value,
        pal = function() private$..pal$value,
        tech_reps = function() private$..tech_reps$value,
        join_blocks = function() private$..join_blocks$value,
        plot_width = function() private$..plot_width$value,
        plot_height = function() private$..plot_height$value,
        font_size = function() private$..font_size$value,
        y_label = function() private$..y_label$value,
        y_units = function() private$..y_units$value,
        x_label = function() private$..x_label$value,
        rescale = function() private$..rescale$value),
    private = list(
        ..dep = NA,
        ..factor1 = NA,
        ..factor2 = NA,
        ..block = NA,
        ..nest_id = NA,
        ..covariate_id = NA,
        ..offset = NA,
        ..log_covariate = NA,
        ..mb_hints = NA,
        ..design = NA,
        ..subplot = NA,
        ..model = NA,
        ..family = NA,
        ..check_the_model = NA,
        ..trtvsctrl = NA,
        ..simple = NA,
        ..adjust = NA,
        ..hide_pairs = NA,
        ..pal = NA,
        ..tech_reps = NA,
        ..join_blocks = NA,
        ..plot_width = NA,
        ..plot_height = NA,
        ..font_size = NA,
        ..y_label = NA,
        ..y_units = NA,
        ..x_label = NA,
        ..rescale = NA)
)

PTMResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "PTMResults",
    inherit = jmvcore::Group,
    active = list(
        model_builder_notes = function() private$.items[["model_builder_notes"]],
        plot = function() private$.items[["plot"]],
        plot_notes = function() private$.items[["plot_notes"]],
        model = function() private$.items[["model"]],
        model_notes = function() private$.items[["model_notes"]],
        coef = function() private$.items[["coef"]],
        emm = function() private$.items[["emm"]],
        contrasts = function() private$.items[["contrasts"]],
        check = function() private$.items[["check"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Plot The Model")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="model_builder_notes",
                title="Model Builder Hints"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Plot the Model",
                width=400,
                height=300,
                renderFun=".plot"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="plot_notes",
                title="Plot Notes"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="model",
                title="Model"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="model_notes",
                title="Model Notes"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="coef",
                title="Model coefficients"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="emm",
                title="Estimated Model Means"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="contrasts",
                title="Model Contrasts"))
            self$add(jmvcore::Image$new(
                options=options,
                name="check",
                title="Check the Model",
                width=500,
                height=300,
                renderFun=".plot_check"))}))

PTMBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "PTMBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "PTM",
                name = "PTM",
                version = c(1,0,0),
                options = options,
                results = PTMResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Plot The Model
#'
#' 
#' @param data .
#' @param dep .
#' @param factor1 .
#' @param factor2 .
#' @param block .
#' @param nest_id .
#' @param covariate_id .
#' @param offset .
#' @param log_covariate .
#' @param mb_hints .
#' @param design .
#' @param subplot .
#' @param model .
#' @param family .
#' @param check_the_model .
#' @param trtvsctrl .
#' @param simple .
#' @param adjust .
#' @param hide_pairs .
#' @param pal .
#' @param tech_reps .
#' @param join_blocks .
#' @param plot_width .
#' @param plot_height .
#' @param font_size .
#' @param y_label .
#' @param y_units .
#' @param x_label .
#' @param rescale .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model_builder_notes} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot_notes} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$model} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$model_notes} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$coef} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$emm} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$contrasts} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$check} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' @export
PTM <- function(
    data,
    dep,
    factor1,
    factor2,
    block,
    nest_id,
    covariate_id,
    offset = FALSE,
    log_covariate = FALSE,
    mb_hints = FALSE,
    design = "crd",
    subplot = "subplot.eq.1",
    model = "lm",
    family = "norm",
    check_the_model = FALSE,
    trtvsctrl = FALSE,
    simple = TRUE,
    adjust = "default",
    hide_pairs,
    pal = "pal_okabe_ito",
    tech_reps = FALSE,
    join_blocks = FALSE,
    plot_width = 400,
    plot_height = 300,
    font_size = 12,
    y_label,
    y_units,
    x_label,
    rescale = 1) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("PTM requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(factor1)) factor1 <- jmvcore::resolveQuo(jmvcore::enquo(factor1))
    if ( ! missing(factor2)) factor2 <- jmvcore::resolveQuo(jmvcore::enquo(factor2))
    if ( ! missing(block)) block <- jmvcore::resolveQuo(jmvcore::enquo(block))
    if ( ! missing(nest_id)) nest_id <- jmvcore::resolveQuo(jmvcore::enquo(nest_id))
    if ( ! missing(covariate_id)) covariate_id <- jmvcore::resolveQuo(jmvcore::enquo(covariate_id))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(factor1), factor1, NULL),
            `if`( ! missing(factor2), factor2, NULL),
            `if`( ! missing(block), block, NULL),
            `if`( ! missing(nest_id), nest_id, NULL),
            `if`( ! missing(covariate_id), covariate_id, NULL))


    options <- PTMOptions$new(
        dep = dep,
        factor1 = factor1,
        factor2 = factor2,
        block = block,
        nest_id = nest_id,
        covariate_id = covariate_id,
        offset = offset,
        log_covariate = log_covariate,
        mb_hints = mb_hints,
        design = design,
        subplot = subplot,
        model = model,
        family = family,
        check_the_model = check_the_model,
        trtvsctrl = trtvsctrl,
        simple = simple,
        adjust = adjust,
        hide_pairs = hide_pairs,
        pal = pal,
        tech_reps = tech_reps,
        join_blocks = join_blocks,
        plot_width = plot_width,
        plot_height = plot_height,
        font_size = font_size,
        y_label = y_label,
        y_units = y_units,
        x_label = x_label,
        rescale = rescale)

    analysis <- PTMClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

