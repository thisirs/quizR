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
    data_chunk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", paste(deparse(data), collapse="\n"))

    tmpfile <- tempfile("question", fileext=".Rmd")
    write(data_chunk, tmpfile)
    write(text, tmpfile, append=T)
    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment())
    toString(output)
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
    gs <- lapply(obj$groups, function(g) { toXML(g, obj) })
    gs <- do.call(paste, c(gs, sep="\n"))
    output <- sprintf(quiz.xml, addSpacesLeft(gs, 2))
    return(output)
}

#' @export
toXML.Group <- function(obj, ...)
{
    quiz <- list(...)[[1]]
    title <- paste(quiz$title, "/", obj$title, sep="")
    qs <- do.call(paste, c(lapply(obj$questions, function(q) { toXML(q) }), sep="\n"))
    return(paste(sprintf(group.xml, title), qs, sep="\n"))
}

#' @export
toXML.Question <- function(obj, ...)
{
    title <- "-"
    body <- paste0("<!-- Q(", obj$id, ") -->", renderHTML(obj$text, obj$get_hdata()))
    if(is.function(obj$feedback)) {
        feedback <- obj$feedback(NULL, obj, NULL, numbered=FALSE, eval=FALSE, question.body=FALSE)
    } else {
        feedback <- obj$feedback
    }
    rdr_feedback <- renderHTML(feedback, obj$get_hdata())
    return(sprintf(question.xml, obj$type, title, "html", body, rdr_feedback))
}

#' Generate XML Moodle quiz file and data file
#'
#' @param quiz Quiz
#' @param data.name Name of data file
#' @param quiz.name Name of XML file
#'
#' @export
generateFiles <- function(quiz, data.name=paste0(quiz$title, "-data.R"), quiz.name=paste0(quiz$title, "-quiz.xml")) {
    stopifnot(uniqueIDs(quiz))
    stopifnot(distinct_data(quiz))

    if(!is.null(quiz.name)) write(toXML(quiz), quiz.name)
    if(!is.null(data.name)) {
        l <- getRecursiveLanguage(quiz)
        if(!is.null(l))
            write(deparse(l), data.name)
    }
}

