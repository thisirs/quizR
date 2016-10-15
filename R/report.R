#' @export
generate_correction <- function(quiz, output, lang) {
    validate_quiz(quiz)

    if(missing(output)) output <- paste0(quiz$title, ".pdf")
    if(missing(lang)) lang <- quote({})

    soutput <- paste0("# ", quiz$title, "\n\n")

    data <- get_recursive_language(quiz)
    data0 <- merge_languages(lang, data)

    ## env <- cleanenv()
    env <- new.env(parent=.GlobalEnv)
    eval(data0, env)
    data_chunk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", paste(deparse(data0), collapse="\n"))

    soutput <- c(soutput, data_chunk)

    for (g in quiz$groups) {
        if(g$type == "identifier") next
        if(g$type == "random")
            soutput <- c(soutput,
                         sprintf("\n## %s (aléatoire %d parmi %d)\n\n", g$title, g$num, length(g$questions)))
        else if(g$type == "sequential")
            soutput <- c(soutput, sprintf("\n## %s \n\n", g$title))
        else
            stop("Unhandled group type")

        qno <- 0
        for (q in g$questions) {
            if(q$type == "description") {
                soutput <- c(soutput, paste0(q$text, "\n"))
            } else {
                qno <- qno + 1
                if(is.function(q$feedback)) {
                    body <- q$feedback(qno, q, env)
                } else if(is.character(q$feedback)) {
                    t_answers <- if(is.list(q$answer)) q$answer else list(q$answer)
                    r_answers <- replace_answers(t_answers, q$get_hdata())

                    hdata <- paste(deparse(q$get_hdata()), collapse="\n")
                    answer <- paste(deparse(r_answers[[1]]), collapse="\n")

                    body <- sprintf("```{r include=FALSE}\n%s\n```\n\n", hdata)
                    body <- c(body, sprintf("**Question %d.** %s\n\n", qno, q$text))
                    body <- c(body, sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n\n", answer))
                    body <- c(body, "**Réponse:**\n")
                    body <- c(body, q$feedback)
                } else stop("Unhandled feedback")
                soutput <- c(soutput, paste0("\n", body, "\n"))
            }
        }
    }

    soutput <- paste(soutput, collapse="")
    tmpfile <- tempfile("quiz", fileext=".Rmd")
    write(soutput, tmpfile)

    rmarkdown::render(input=tmpfile,
                      output_dir=dirname(output),
                      output_file=basename(output),
                      "pdf_document")
}

cloze_regex <- "\\{(\\d+):(SHORTANSWER|SA|MW|SHORTANSWER_C|SAC|MWC|NUMERICAL|NM|MULTICHOICE|MC|MULTICHOICE_V|MCV|MULTICHOICE_H|MCH):=[^\\}]*\\}"

replace_cloze_fields <- function(text) {
    num <- get_cloze_num(text)
    for(i in 1:num) {
        text <- stringi::stri_replace_first_regex(text, cloze_regex, paste0("(", i, ")"))
    }
    return(text)
}

## Taken from RCurl
merge.list <- function (x, y)
{
    if (length(x) == 0)
        return(y)
    if (length(y) == 0)
        return(x)
    i = match(names(y), names(x))
    i = is.na(i)
    if (any(i))
        x[names(y)[which(i)]] = y[which(i)]
    x
}

feedback_args <- c("numbered", "eval", "question.body", "alt.answer", "header")

general_feedback <- function(...) {
    args <- list(...)
    stopifnot(length(setdiff(names(args), feedback_args)) == 0)

    function(qno, question, env, ...) {
        override_args <- list(...)
        stopifnot(length(setdiff(names(override_args), feedback_args)) == 0)
        f.args <- merge.list(override_args, args)

        if(is.null(qno) & f.args$numbered) stop("Cannot number without numbers")
        if(is.null(qno)) f.args$numbered <- FALSE

        if(is.null(env) & f.args$eval) stop("Cannot eval with a null environment")
        if(is.null(env)) f.args$eval <- FALSE

        if(question$type == "cloze") {
            body <- replace_cloze_fields(question$text)

            hdata <- answerstr(question$get_hdata())

            paste0(c(
                sprintf("```{r include=FALSE}\n%s\n```\n", hdata),
                if(f.args$numbered) sprintf("**Question %d.** ", qno),
                if(f.args$question.body) trimws(body),
                "\n\n**Réponse:**\n",
                if(!is.null(f.args$header)) f.args$header,
                sapply(1:get_cloze_num(question$text), function(i) {
                    answer <- if(is.list(question$answer[[i]])) question$answer[[i]] else list(question$answer[[i]])
                    answer <- replace_answers(answer, question$get_hdata())
                    answer <- answerstr(answer[[1]])

                    alt_answer <- if(is.null(f.args$alt.answer))
                                      if(f.args$eval)
                                          sprintf("```{r}\n%s\n```\n", answer)
                                      else sprintf("```r\n%s\n```\n", answer)
                                  else {
                                      alt_answer <- if(is.list(f.args$alt.answer[[i]])) f.args$alt.answer[[i]] else list(f.args$alt.answer[[i]])
                                      if(is.character(alt_answer[[1]]))
                                          paste0(trimws(alt_answer[[1]]), "\n")
                                      else if(f.args$eval)
                                          sprintf("```{r}\n%s\n```\n", answerstr(alt_answer[[1]]))
                                      else sprintf("```r\n%s\n```\n", answerstr(alt_answer[[1]]))
                                  }

                    c(if(f.args$eval) sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer),
                      sprintf("%d. ", i),
                      alt_answer,
                      "\n",
                      if(f.args$eval) "La réponse est: $`r answer`$\n")
                })), collapse="")
        } else {
            answer <- if(is.list(question$answer)) question$answer else list(question$answer)
            answer <- replace_answers(answer, question$get_hdata())
            answer <- answerstr(answer[[1]])
            alt_answer <- if(is.null(f.args$alt.answer))
                              if(f.args$eval)
                                  sprintf("```{r}\n%s\n```\n", answer)
                              else sprintf("```r\n%s\n```\n", answer)
                          else {
                              alt_answer <- if(is.list(f.args$alt.answer)) f.args$alt.answer else list(f.args$alt.answer)
                              if(is.character(alt_answer[[1]]))
                                  paste0(trimws(alt_answer[[1]]), "\n")
                              else if(f.args$eval)
                                  sprintf("```{r}\n%s\n```\n", answerstr(alt_answer[[1]]))
                              else sprintf("```r\n%s\n```\n", answerstr(alt_answer[[1]]))
                          }

            hdata <- answerstr(question$get_hdata())

            paste0(c(
                sprintf("```{r include=FALSE}\n%s\n```\n", hdata),
                if(f.args$numbered) sprintf("**Question %d.** ", qno),
                if(f.args$question.body) trimws(question$text),
                "\n\n**Réponse:**\n",
                if(!is.null(f.args$header)) f.args$header,
                if(f.args$eval) sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer),
                alt_answer,
                "\n",
                if(f.args$eval) "La réponse est: $`r answer`$"), collapse="")
        }
    }
}

## general_feedback <- function(...) {
##     args <- list(...)
##     stopifnot(length(setdiff(names(args), feedback_args)) == 0)

##     function(qno, question, env, ...) {
##         override_args <- list(...)
##         stopifnot(length(setdiff(names(override_args), feedback_args)) == 0)
##         f.args <- merge.list(override_args, args)

##         if(is.null(qno) & f.args$numbered) stop("Cannot number without numbers")
##         if(is.null(qno)) f.args$numbered <- FALSE

##         if(is.null(env) & f.args$eval) stop("Cannot eval with a null environment")
##         if(is.null(env)) f.args$eval <- FALSE

##         answer <- if(is.list(question$answer)) question$answer else list(question$answer)
##         answer <- replace_answers(answer, question$get_hdata())
##         answer <- answerstr(answer[[1]])
##         alt_answer <- if(is.null(f.args$alt.answer)) answer else answerstr(f.args$alt.answer)

##         hdata <- answerstr(question$get_hdata())

##         paste0(c(
##             sprintf("```{r include=FALSE}\n%s\n```\n", hdata),
##             if(f.args$numbered) sprintf("**Question %d.** ", qno),
##             if(f.args$question.body) trimws(question$text),
##             "\n\n**Réponse:**\n",
##             if(f.args$eval) sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer),
##             if(f.args$eval)
##                 sprintf("```{r}\n%s\n```\n", alt_answer)
##             else sprintf("```r\n%s\n```\n", alt_answer),
##             if(f.args$eval) "La réponse est: $`r answer`$"), collapse="")
##     }
## }

#' @export
answer_feedback <- general_feedback(eval=TRUE, numbered=TRUE, question.body=TRUE)

#' @export
alt_feedback <- function(expr) {
    general_feedback(eval=TRUE, numbered=TRUE, question.body=TRUE, alt.answer=expr)
}

#' @export
header_feedback <- function(header, alt.answer) {
    general_feedback(eval=TRUE, numbered=TRUE, question.body=TRUE, header=header, alt.answer=alt.answer)
}

answerstr <- function(answer) {
    if(is.null(answer)) return("")
    type <- typeof(answer)
    switch(type,
           closure={
               d <- deparse(body(answer))
               lines <- d[-c(1, length(d))]
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse="\n")
           },
           language={
               d <- deparse(answer)
               lines <- if (length(d) >=3) d[-c(1, length(d))] else d
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse="\n")
           },
           symbol={deparse(answer)},
           double={deparse(answer)},
           character={answer},
           stop("Unhandled type in ", sQuote("answerstr"), ": ", type))
}
