question.xml <- "<question type=\"%s\">
  <name>
    <text>%s</text>
  </name>
  <questiontext format=\"%s\">
    <text><![CDATA[%s]]></text>
  </questiontext>
  <answer fraction=\"100\" format=\"plain_text\">
    <text>*</text>
  </answer>
  <generalfeedback format=\"html\">
    <text><![CDATA[%s]]></text>
  </generalfeedback>
  <hidden>0</hidden>
</question>"

group.xml <- "<question type=\"category\">
  <category>
    <text>$course$/%s</text>
  </category>
</question>"

quiz.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<quiz>
%s
</quiz>"

render_HTML <- function(text, data) {
    if (missing(data)) data <- quote({})
    data_chunk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", answerstr(data))

    tmpfile <- tempfile("question", fileext = ".Rmd")
    write(data_chunk, tmpfile)
    write(text, tmpfile, append = TRUE)
    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment())
    to_string(output)
}

to_XML <- function(obj, ...)
{
    UseMethod("to_XML")
}

to_XML.default <- function(obj, ...)
{
    print("You screwed up. I do not know how to handle this object.")
    return(obj)
}

#' @export
to_XML.Quiz <- function(obj, ...)
{
    args <- list(...)
    if (is.null(args$data)) stop("Missing data when calling to_XML.Group")

    ## Call to_XML on each group with args
    gs <- lapply(obj$groups, function(g) {
        args0 <- c(list(g, quiz = obj), args)
        do.call(to_XML, args0, quote = TRUE)
    })
    gs <- paste(gs, collapse = "\n")
    output <- sprintf(quiz.xml, add_spaces_left(gs, 2))
    return(output)
}

#' @export
to_XML.Group <- function(obj, ...)
{
    args <- list(...)
    if (is.null(args$quiz)) stop("Missing quiz when calling to_XML.Group")
    if (is.null(args$data)) stop("Missing data when calling to_XML.Group")

    if (obj$type == "identifier")
        args$feedback <- FALSE

    title <- paste(args$quiz$title, "/", obj$title, sep = "")
    qs <- lapply(obj$questions, function(q) {
        args0 <- c(list(q), args)
        do.call(to_XML, args0, quote = TRUE) })
    qs <- paste(qs, collapse = "\n")
    return(paste(sprintf(group.xml, title), qs, sep = "\n"))
}

#' @export
to_XML.Question <- function(obj, ...) {
    args <- list(...)
    if (is.null(args$quiz)) stop("Missing quiz when calling to_XML.Group")
    if (is.null(args$data)) stop("Missing data when calling to_XML.Group")

    set.seed(obj$quiz$seed)

    ## Set environment corresponding to quiz data
    env <- cleanenv()
    eval(args$data, env)

    ## Block defining data of quiz
    data_s <- answerstr(args$data)
    md_qdata_blk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", data_s)

    ## Block defining hidden data
    hdata <- obj$get_hdata()
    if (length(hdata) == 1 && hdata[[1]] == as.name("{")) {
        md_hdata_blk <- NULL
    } else {
        hdata_s <- answerstr(hdata)
        md_hdata_blk <- sprintf("```{r include=FALSE}\n%s\n```\n", hdata_s)
    }

    # Get title of question
    if (is.null(args$title))
        title <- "-"
    else
        title <- args$title

    # Get HTML of question body
    md_question <- paste0(md_qdata_blk, md_hdata_blk, obj$text)
    HTML_question <- paste0("<!-- Q(", obj$id, ") -->", render_HTML(md_question))

    # Get HTML of feedback
    if (obj$type == "description" | !args$feedback) {
        md_feedback <- ""
    } else if (is.function(obj$feedback)) {
        md_feedback <- obj$feedback(NULL, obj, env, numbered = FALSE, eval = args$eval, question.body = FALSE)
    } else stop("Unhandled feedback type")

    md_feedback <- paste0(md_qdata_blk, md_feedback)
    HTML_feedback <- render_HTML(md_feedback)

    # Return XML
    return(sprintf(question.xml, obj$type, title, "html", HTML_question, HTML_feedback))
}

#' Generate XML Moodle quiz file and data file
#'
#' @param quiz Quiz
#' @param data.name Name of data file
#' @param quiz.name Name of XML file
#' @param language Additionnal language for validation
#'
#' @export
generate_files <- function(quiz,
                           data.name = paste0(quiz$title, "-data.R"),
                           quiz.name = paste0(quiz$title, "-quiz.xml"),
                           language) {
    if (missing(language)) language <- NULL

    # Instantiate random data with quiz$hidden.seed
    unrandomize_data(quiz)

    validate_quiz(quiz, language)

    data <- get_recursive_language(quiz)
    data0 <- merge_languages(language, data)

    # Write XML Moodle file
    if (!is.null(quiz.name))
        write(to_XML(quiz, data = data0), quiz.name)

    ## Write data file if any
    if (!is.null(data.name) & length(data0) > 1) {
        write(answerstr(data0), data.name)
    }
}


#' Generate XML Moodle quiz file
#'
#' @param quiz Quiz
#' @param quiz.name Name of generated file
#' @param language Additionnal language for validation
#' @param feedback Whether to add feedback in XML
#' @param eval Whether to evaluate answers in feedback
#'
#' @export
generate_XML <- function(quiz,
                         quiz.name = paste0(quiz$title, "-quiz.xml"),
                         language = NULL,
                         feedback = TRUE,
                         eval = feedback) {
    # Instantiate random data with quiz$hidden.seed
    unrandomize_data(quiz)

    validate_quiz(quiz, language)

    data <- get_recursive_language(quiz)
    data0 <- merge_languages(language, data)

    # Write XML Moodle file
    write(to_XML(quiz, data = data0, feedback = feedback, eval = eval), quiz.name)
}

#' Generate data file
#'
#' @param quiz Quiz
#' @param data.name Name of the data file
#' @param language Additionnal language for validation
#'
#' @export
generate_data_file <- function(quiz,
                               data.name = paste0(quiz$title, "-data.R"),
                               language = NULL) {
    # Instantiate random data with quiz$hidden.seed
    unrandomize_data(quiz)

    validate_quiz(quiz, language)

    data <- get_recursive_language(quiz)

    ## Write data file if any
    if (length(data) > 1) {
        write(answerstr(data), data.name)
    }
}
