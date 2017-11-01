#' @include simple_question.R
NULL

#' @export
GapselectQuestion <-
    R6::R6Class(
            "Gapselect",
            inherit = SimpleQuestion,
            public = list(
                type = "gapselect",

                initialize = function(text,
                                      data = quote({}),
                                      hidden_data = quote({}),
                                      seed = NULL,
                                      hidden_seed = NULL,
                                      feedback = NULL,
                                      answer = NULL) {
                    super$initialize(text,
                                     data = data,
                                     hidden_data = hidden_data,
                                     seed = seed,
                                     hidden_seed = hidden_seed,
                                     feedback = feedback,
                                     answer = answer)
                }

            )
        )
