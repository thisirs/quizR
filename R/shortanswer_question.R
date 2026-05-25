#' @include simple_question.R
NULL

#' @export
ShortanswerQuestion <- R6::R6Class(
    "ShortanswerQuestion",
    inherit = SimpleQuestion,
    public = list(
        dist = 0,

        initialize = function(text,
                              data        = quote({}),
                              hidden_data = quote({}),
                              seed        = NULL,
                              hidden_seed = NULL,
                              feedback    = NULL,
                              answer      = NULL,
                              dist        = 0,
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
            self$type <- "shortanswer"
            self$dist <- dist
        }
    )
)
