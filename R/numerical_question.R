#' @include simple_question.R
NULL

#' @export
NumericalQuestion <- R6::R6Class(
    "NumericalQuestion",
    inherit = SimpleQuestion,
    public = list(
        tolerance      = 1e-3,
        tolerance_type = "relative",
        distractors    = NULL,

        initialize = function(text,
                              data           = quote({}),
                              hidden_data    = quote({}),
                              seed           = NULL,
                              hidden_seed    = NULL,
                              feedback       = NULL,
                              answer         = NULL,
                              tolerance_type = "relative",
                              tolerance      = 1e-3,
                              distractors    = NULL,
                              tags           = NULL,
                              options        = list()) {
            super$initialize(text,
                             data        = data,
                             hidden_data = hidden_data,
                             seed        = seed,
                             hidden_seed = hidden_seed,
                             feedback    = feedback,
                             answer      = answer,
                             tags        = tags,
                             options     = options)
            self$type          <- "numerical"
            self$tolerance_type <- tolerance_type
            self$tolerance     <- tolerance
            self$distractors   <- if (is.null(distractors)) NULL
                                  else if (is.language(distractors)) distractors
                                  else as.list(distractors)
        },

        rename_answer = function(prefix, names = self$hidden_data_names()) {
            super$rename_answer(prefix, names)
            if (!is.null(self$distractors)) {
                self$distractors <- if (is.language(self$distractors))
                    prefix_object(prefix, names, self$distractors)
                else
                    lapply(self$distractors, function(d) prefix_object(prefix, names, d))
            }
            invisible(self)
        },

        copy = function() {
            Question(self$text,
                     type           = self$type,
                     seed           = self$seed,
                     hidden_seed    = self$hidden_seed,
                     hidden_data    = self$hidden_data,
                     data           = self$data,
                     answer         = self$answer,
                     feedback       = self$feedback,
                     distractors    = self$distractors,
                     tolerance      = self$tolerance,
                     tolerance_type = self$tolerance_type,
                     options        = self$options)
        }
    )
)
