#' @include simple_question.R
NULL

#' @export
ShortanswerQuestion <- R6::R6Class(
    "Shortanswer",
    inherit = SimpleQuestion,
    public = list(
        type = "shortanswer",
        dist = 0,

        initialize = function(text,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              dist = 0,
                              tag = NULL) {
            super$initialize(text,
                             data = data,
                             hidden_data = hidden_data,
                             seed = seed,
                             hidden_seed = hidden_seed,
                             feedback = feedback,
                             answer = answer,
                             tag = tag)
            self$dist <- dist
        },

        get_inst_cookie = function(opts, info) {
            answer <- get_evaluated_answer2(opts, info)
            sprintf("{%s:NM:=%s}", 1, answer)
        }
    )
)
