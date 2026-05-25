#' @include simple_question.R
NULL

formats <- c(
    "multiplechoice", "multichoice", "mc",
    "multichoice_v", "mcv",
    "multichoice_h", "mch",
    "multiresponse", "mr",
    "multiresponse_h", "mrh",
    "multichoice_s", "mcs",
    "multichoice_vs", "mcvs",
    "multichoice_hs", "mchs",
    "multiresponse_s", "mrs",
    "multiresponse_hs", "mrhs"
)

single_formats <- c(
    "multiplechoice", "multichoice", "mc",
    "multichoice_v", "mcv",
    "multichoice_h", "mch",
    "multiresponse",
    "multichoice_s", "mcs",
    "multichoice_vs", "mcvs",
    "multichoice_hs", "mchs"
)

cookies_keyword <- list(
    "multiplechoice"   = "MC",  "multichoice"    = "MC",  "mc"    = "MC",
    "multichoice_v"    = "MCV", "mcv"            = "MCV",
    "multichoice_h"    = "MCH", "mch"            = "MCH",
    "multiresponse"    = "MR",  "mr"             = "MR",
    "multiresponse_h"  = "MRH", "mrh"            = "MRH",
    "multichoice_s"    = "MCS", "mcs"            = "MCS",
    "multichoice_vs"   = "MCVS","mcvs"           = "MCVS",
    "multichoice_hs"   = "MCHS","mchs"           = "MCHS",
    "multiresponse_s"  = "MRS", "mrs"            = "MRS",
    "multiresponse_hs" = "MRHS","mrhs"           = "MRHS"
)

#' @export
MultipleChoice <- R6::R6Class(
    "MultipleChoice",
    inherit = SimpleQuestion,
    public = list(
        items            = NULL,
        answer_feedbacks = NULL,
        shuffle_answers  = FALSE,
        single           = FALSE,
        format           = NULL,

        initialize = function(text,
                              data             = quote({}),
                              hidden_data      = quote({}),
                              seed             = NULL,
                              hidden_seed      = NULL,
                              feedback         = NULL,
                              answer           = NULL,
                              items            = NULL,
                              answer_feedbacks = NULL,
                              shuffle_answers  = NULL,
                              single           = FALSE,
                              format           = "mc",
                              tags             = NULL,
                              options          = list()) {
            super$initialize(text,
                             data        = data,
                             hidden_data = hidden_data,
                             seed        = seed,
                             hidden_seed = hidden_seed,
                             feedback    = feedback,
                             answer      = answer,
                             tags        = tags,
                             options     = options)
            self$type <- "multichoice"

            if (is.null(format)) format <- "mc"
            if (!tolower(format) %in% formats)
                stop("Unknown format: ", sQuote(format))
            self$format <- tolower(format)

            last_char           <- substr(self$format, nchar(self$format), nchar(self$format))
            self$shuffle_answers <- (last_char == "s")
            self$single          <- (self$format %in% single_formats)

            if (is.language(items)) {
                self$items           <- items
                self$answer_feedbacks <- answer_feedbacks
            } else {
                self$items <- as.list(items)

                if (is.null(self$answer)) {
                    n           <- length(self$items)
                    answers     <- lapply(seq_len(n), function(i) self$items[[i]][[1]])
                    items       <- lapply(seq_len(n), function(i) self$items[[i]][[2]])
                    self$answer <- answers
                    self$items  <- items
                }

                n <- length(self$items)
                if (is.null(answer_feedbacks)) {
                    self$answer_feedbacks <- as.list(rep("", n))
                } else {
                    if (length(answer_feedbacks) != n)
                        stop("Wrong number of answer feedbacks")
                    self$answer_feedbacks <- answer_feedbacks
                }
            }
        },

        rename_text = function(prefix, names = self$hidden_data_names()) {
            self$text <- prefix_object(prefix, names, self$text)
            self$items <- if (is.language(self$items))
                prefix_object(prefix, names, self$items)
            else
                lapply(self$items, function(item) prefix_object(prefix, names, item))
            invisible(self)
        },

        rename_answer = function(prefix, names = self$hidden_data_names()) {
            self$answer <- if (is.language(self$answer))
                prefix_object(prefix, names, self$answer)
            else
                lapply(self$answer, function(a) prefix_object(prefix, names, a))
            invisible(self)
        },

        copy = function() {
            MultipleChoice$new(self$text,
                               seed             = self$seed,
                               hidden_seed      = self$hidden_seed,
                               hidden_data      = self$hidden_data,
                               items            = self$items,
                               answer_feedbacks = self$answer_feedbacks,
                               data             = self$data,
                               answer           = self$answer,
                               feedback         = self$feedback,
                               shuffle_answers  = self$shuffle_answers,
                               single           = self$single,
                               format           = self$format,
                               options          = self$options)
        }
    )
)
