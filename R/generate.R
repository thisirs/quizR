generateFiles <- function(quiz, data.name=paste0(quiz$title, "-data"), quiz.name=paste0(quiz$title, "-quiz")) {
    if(!is.null(quiz.name)) write(toXML(quiz), paste0(quiz.name, ".xml"))
    if(!is.null(data.name)) {
        l <- getRecursiveLanguage(quiz)
        write(deparse(l), paste0(data.name, ".R"))
    }
}

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

toXML <- function(obj, ...)
{
    UseMethod("toXML")
}

toXML.default <- function(obj, ...)
{
    print("You screwed up. I do not know how to handle this object.")
    return(obj)
}

toXML.Quiz <- function(obj, ...)
{
    gs <- lapply(obj$groups, function(g) { toXML(g, obj) })
    gs <- do.call(paste, c(gs, sep="\n"))
    output <- sprintf(quiz.xml, addSpacesLeft(gs, 2))
    return(output)
}

renderHTML <- function(text) {
    tmpfile <- tempfile("question", fileext=".md")
    write(text, tmpfile)
    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment())
    return(paste(readLines(output), collapse="\n"))
}

toXML.Question <- function(obj, ...)
{
    title <- "-"
    body <- paste0("<!-- Q(", obj$id, ") -->", renderHTML(obj$text))
    feedback <- renderHTML(obj$feedback)
    return(sprintf(question.xml, obj$type, title, "html", body, feedback))
}

toXML.Group <- function(obj, ...)
{
    quiz <- list(...)[[1]]
    title <- paste(quiz$title, "/", obj$title, sep="")
    qs <- do.call(paste, c(lapply(obj$questions, function(q) { toXML(q) }), sep="\n"))
    return(paste(sprintf(group.xml, title), qs, sep="\n"))
}

getMapping <- function(qs.text, questions) {
    map <- rep(0, length(qs.text))
    for(i in 1:length(qs.text)) {
        # Extract ID from the question's body
        text <- qs.text[i]
        id <- stringr::str_match(text, "\\(Q([A-Za-z0-9]+)\\)")[1, 2]
        if(is.na(id)) stop("`id' not found")

        # Look for corresponding id in list of questions
        index <- Position(function(q){ id == q$id}, questions)
        if(is.na(index)) stop("No corresponding `id' found for ", q$text)
        map[i] <- index
    }
    return(map)
}
