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

renderHTML <- function(text, data) {
    if(missing(data)) data <- quote({})
    data_chunk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", paste(deparse(data), collapse = "\n"))

    tmpfile <- tempfile("question", fileext = ".Rmd")
    write(data_chunk, tmpfile)
    write(text, tmpfile, append = T)
    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment())
    to_string(output)
}

toXML <- function(obj, ...)
{
    UseMethod("toXML")
}

toXML.default <- function(obj, ...)
{
    print("You screwed up. I do not know how to handle this object.")
    return(obj)
}

#' @export
toXML.Quiz <- function(obj, ...)
{
    args <- list(...)
    if(is.null(args$data)) stop("Missing data when calling toXML.Group")

    ## Call toXML on each group with args
    gs <- lapply(obj$groups, function(g) { do.call(toXML, c(list(g, quiz = obj), args)) })
    gs <- paste(gs, collapse = "\n")
    output <- sprintf(quiz.xml, add_spaces_left(gs, 2))
    return(output)
}

#' @export
toXML.Group <- function(obj, ...)
{
    args <- list(...)
    if(is.null(args$quiz)) stop("Missing quiz when calling toXML.Group")
    if(is.null(args$data)) stop("Missing data when calling toXML.Group")

    title <- paste(args$quiz$title, "/", obj$title, sep = "")
    qs <- lapply(obj$questions, function(q) { do.call(toXML, c(list(q), args)) })
    qs <- paste(qs, collapse = "\n")
    return(paste(sprintf(group.xml, title), qs, sep = "\n"))
}

#' @export
toXML.Question <- function(obj, ...) {
    args <- list(...)
    if(is.null(args$quiz)) stop("Missing quiz when calling toXML.Group")
    if(is.null(args$data)) stop("Missing data when calling toXML.Group")

    ## Add chunk containing data of quiz
    md_data <- sprintf("```{r include=FALSE}\n%s\n```\n\n", paste(deparse(args$data), collapse = "\n"))
    md_hdata <- sprintf("```{r include=FALSE}\n%s\n```\n\n", paste(deparse(obj$get_hdata()), collapse = "\n"))
    title <- "-"

    md_question <- paste0(md_data, md_hdata, obj$text)
    HTML_question <- paste0("<!-- Q(", obj$id, ") -->", renderHTML(md_question))

    if(obj$type == "description" | args$feedback == FALSE) {
        return(sprintf(question.xml, obj$type, title, "html", HTML_question, ""))
    } else if(is.character(obj$feedback)) {
        t_answers <- if(is.list(obj$answer)) obj$answer else list(obj$answer)
        r_answers <- replace_answers(t_answers, obj$get_hdata())

        if(obj$type == "cloze")
            md_answer <- sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n\n", answerstr(r_answers))
        else
            md_answer <- sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n\n", answerstr(r_answers[[1]]))

        md_feedback <- paste0(md_data, md_hdata, md_answer, obj$feedback)
        HTML_feedback <- renderHTML(md_feedback)
        return(sprintf(question.xml, obj$type, title, "html", HTML_question, HTML_feedback))
    } else if(is.function(obj$feedback)) {
        md_feedback <- paste0(md_data, obj$feedback(NULL, obj, NULL, numbered = FALSE, eval = FALSE, question.body = FALSE))
        HTML_feedback <- renderHTML(md_feedback)
        return(sprintf(question.xml, obj$type, title, "html", HTML_question, HTML_feedback))
    } else stop("Unhandled feedback type")
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
    if(missing(language)) language <- NULL
    validate_quiz(quiz, language)

    data <- get_recursive_language(quiz)
    data0 <- merge_languages(language, data)

    # Write XML Moodle file
    if(!is.null(quiz.name))
        write(toXML(quiz, data = data0), quiz.name)

    ## Write data file if any
    if(!is.null(data.name) & length(data0) > 1) {
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
    validate_quiz(quiz, language)

    data <- get_recursive_language(quiz)
    data0 <- merge_languages(language, data)

    # Write XML Moodle file
    write(toXML(quiz, data = data0, feedback = feedback, eval = eval), quiz.name)
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
    validate_quiz(quiz, language)

    data <- get_recursive_language(quiz)
    data0 <- merge_languages(language, data)

    ## Write data file if any
    if(length(data0) > 1) {
        write(answerstr(data0), data.name)
    }
}
