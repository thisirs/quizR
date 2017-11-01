Question_classes <- list(
    "cloze" = ClozeQuestion,
    "numerical" = NumericalQuestion,
    "nm" = NumericalQuestion,
    "shortanswer" = ShortanswerQuestion,
    "sa" = ShortanswerQuestion,
    "multiplechoice" = MultipleChoice,
    "multichoice" = MultipleChoice,
    "mc" = MultipleChoice
)

#' Factory class that creates questions object give their type
#' @export
Question <- function(...) {
    args <- list(...)
    if (is.null(args$type))
        NumericalQuestion$new(...)
    else {
        name <- tolower(gsub("[_-]", " ", args$type))
        class <- Question_classes[[name]]
        if (is.null(class))
            stop("Unknown question type: ", sQuote(args$type))
        args$type <- NULL
        do.call(class$new, args, quote = TRUE)
    }
}
