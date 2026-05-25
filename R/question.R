Question_classes <- list(
    "cloze"            = ClozeQuestion,
    "numerical"        = NumericalQuestion,
    "nm"               = NumericalQuestion,
    "shortanswer"      = ShortanswerQuestion,
    "sa"               = ShortanswerQuestion,
    "multiplechoice"   = MultipleChoice,
    "multichoice"      = MultipleChoice,
    "mc"               = MultipleChoice,
    "multichoice_v"    = MultipleChoice,
    "mcv"              = MultipleChoice,
    "multichoice_h"    = MultipleChoice,
    "mch"              = MultipleChoice,
    "multiresponse"    = MultipleChoice,
    "mr"               = MultipleChoice,
    "multiresponse_h"  = MultipleChoice,
    "mrh"              = MultipleChoice,
    "multichoice_s"    = MultipleChoice,
    "mcs"              = MultipleChoice,
    "multichoice_vs"   = MultipleChoice,
    "mcvs"             = MultipleChoice,
    "multichoice_hs"   = MultipleChoice,
    "mchs"             = MultipleChoice,
    "multiresponse_s"  = MultipleChoice,
    "mrs"              = MultipleChoice,
    "multiresponse_hs" = MultipleChoice,
    "mrhs"             = MultipleChoice
)

#' Factory function that creates question objects given their type.
#'
#' @param ... Named arguments forwarded to the appropriate question class.
#' @export
Question <- function(...) {
    args <- list(...)
    if (is.null(args$type))
        return(NumericalQuestion$new(...))

    name  <- tolower(args$type)
    klass <- Question_classes[[name]]
    if (is.null(klass))
        stop("Unknown question type: ", sQuote(args$type))

    if (identical(klass, MultipleChoice))
        args$format <- name

    args$type <- NULL
    do.call(klass$new, args, quote = TRUE)
}
