Question_classes <- list(
    "cloze" = ClozeQuestion,
    "numerical" = NumericalQuestion,
    "nm" = NumericalQuestion,
    "shortanswer" = ShortanswerQuestion,
    "sa" = ShortanswerQuestion,
    "multiplechoice" = MultipleChoice,
    "multichoice" = MultipleChoice,
    "mc" = MultipleChoice,
    "multichoice_v" = MultipleChoice,
    "mcv" = MultipleChoice,
    "multichoice_h" = MultipleChoice,
    "mch" = MultipleChoice,
    "multiresponse" = MultipleChoice,
    "mr" = MultipleChoice,
    "multiresponse_h" = MultipleChoice,
    "mrh" = MultipleChoice,
    "multichoice_s" = MultipleChoice,
    "mcs" = MultipleChoice,
    "multichoice_vs" = MultipleChoice,
    "mcvs" = MultipleChoice,
    "multichoice_hs" = MultipleChoice,
    "mchs" = MultipleChoice,
    "multiresponse_s" = MultipleChoice,
    "mrs" = MultipleChoice,
    "multiresponse_hs" = MultipleChoice,
    "mrhs" = MultipleChoice
)

#' Factory class that creates questions object give their type
#'
#' @param ... A list of named arguments
#' @export
Question <- function(...) {
    args <- list(...)
    if (is.null(args$type))
        NumericalQuestion$new(...)
    else {
        name <- tolower(args$type)
        class <- Question_classes[[name]]
        if (is.null(class))
            stop("Unknown question type: ", sQuote(args$type))

        if(identical(class, MultipleChoice))
            args$format <- name

        args$type <- NULL
        do.call(class$new, args, quote = TRUE)
    }
}
