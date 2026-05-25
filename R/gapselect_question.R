#' @include simple_question.R
NULL

#' @export
GapselectQuestion <- R6::R6Class(
    "GapselectQuestion",
    inherit = SimpleQuestion,
    public = list(
        initialize = function(text,
                              data        = quote({}),
                              hidden_data = quote({}),
                              seed        = NULL,
                              hidden_seed = NULL,
                              feedback    = NULL,
                              answer      = NULL,
                              tags        = NULL,
                              options     = list()) {
            super$initialize(text,
                             data        = data,
                             hidden_data = hidden_data,
                             seed        = seed,
                             hidden_seed = hidden_seed,
                             feedback    = feedback,
                             answer      = answer,
                             tags        = tags,
                             options     = options)
            self$type <- "gapselect"
        }
    )
)
