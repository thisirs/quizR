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

feedback_defaults <- list(numbered=TRUE, eval=TRUE, question.body=TRUE, alt.answer=NULL, header=NULL)

feedback_args <- c("numbered", "eval", "question.body", "alt.answer", "header")

#' @export
general_feedback <- function(...) {
    args <- list(...)
    stopifnot(length(setdiff(names(args), feedback_args)) == 0)

    general_feedback0 <- function(qno, question, env, ...) {
        override_args <- list(...)
        stopifnot(length(setdiff(names(override_args), feedback_args)) == 0)
        f.args <- merge.list(override_args, args)

        all_args <- c(list(qno, question, env), f.args)
        do.call(automatic_feedback, all_args, quote=TRUE)
    }
    return(general_feedback0)
}

#' @export
header_feedback <- function(header, alt.answer) {
    general_feedback(eval=TRUE, numbered=TRUE, question.body=TRUE, header=header, alt.answer=alt.answer)
}

#' @export
alt_feedback <- function(alt_answer) {
    general_feedback(alt.answer=alt_answer)
}

#' @export
automatic_feedback <- function(qno, question, env, ...) {
    args <- list(...)

    stopifnot(length(setdiff(names(args), feedback_args)) == 0)
    args <- merge.list(args, feedback_defaults)

    if(is.null(qno) & args$numbered) stop("Cannot number without numbers")
    if(is.null(qno)) args$numbered <- FALSE

    if(is.null(env) & args$eval) stop("Cannot eval with a null environment")
    if(is.null(env)) args$eval <- FALSE

    if(question$type == "cloze") {
        do.call(automatic_cloze_feedback, c(list(qno, question, env), args), quote=TRUE)
    } else {
        do.call(automatic_normal_feedback, c(list(qno, question, env), args), quote=TRUE)
    }
}


automatic_cloze_feedback <- function(qno, question, env, ...) {
    args <- list(...)

    ## Replace cloze fields by numbers enclosed in parens
    body <- replace_cloze_fields(question$text)

    ## Blocks defining hidden data
    hdata <- answerstr(question$get_hdata())
    md_hdata_blk <- sprintf("```{r include=FALSE}\n%s\n```\n", hdata)

    paste0(c(
        md_hdata_blk,
        if(args$numbered) sprintf("**Question %d.** ", qno),
        if(args$question.body) trimws(body),
        "\n\n**Réponse:**\n",
        if(!is.null(args$header)) args$header,
        sapply(1:get_cloze_num(question$text), function(i) {
            answer <- if(is.list(question$answer[[i]])) question$answer[[i]] else list(question$answer[[i]])
            answer <- replace_answers(answer, question$get_hdata())
            answer <- answerstr(answer[[1]])
            if(args$eval)
                md_answer_blk <- sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer)
            else
                md_answer_blk <- "```{r include=FALSE}\nanswer <- \"undefined\"\n```\n"

            ## Real answer: alt_answer if any, answer otherwise
            if(is.null(args$alt.answer)) {
                if(args$eval)
                    md_answer <- sprintf("```{r}\n%s\n```\n", answer)
                else
                    md_answer <- sprintf("```r\n%s\n```\n", answer)
            } else {
                alt_answer <- if(is.list(args$alt.answer[[i]])) args$alt.answer[[i]] else list(args$alt.answer[[i]])
                alt_answer <- replace_answers(alt_answer, question$get_hdata())
                alt_answer <- alt_answer[[1]]
                if(is.character(alt_answer)) {
                    md_answer <- paste0(trimws(alt_answer), "\n")
                } else if(args$eval)
                    md_answer <- sprintf("```{r}\n%s\n```\n", answerstr(alt_answer))
                else
                    md_answer <- sprintf("```r\n%s\n```\n", answerstr(alt_answer))
            }

            c(md_answer_blk,
              sprintf("%d. ", i),
              md_answer,
              "\n",
              if(args$eval) "La réponse est: $`r answer`$\n")
        })), collapse="")
}

automatic_normal_feedback <- function(qno, question, env, ...) {
    args <- list(...)

    ## Blocks defining hidden data and answer
    hdata <- answerstr(question$get_hdata())
    md_hdata_blk <- sprintf("```{r include=FALSE}\n%s\n```\n", hdata)

    ## Listify answer, replace by hidden data and select first
    ## possible answer
    answer <- if(is.list(question$answer)) question$answer else list(question$answer)
    answer <- replace_answers(answer, question$get_hdata())
    answer <- answerstr(answer[[1]])
    if(args$eval)
        md_answer_blk <- sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer)
    else
        md_answer_blk <- "```{r include=FALSE}\nanswer <- \"undefined\"\n```\n"

    ## Real answer: alt_answer if any, answer otherwise
    if(is.null(args$alt.answer)) {
        if(args$eval)
            md_answer <- sprintf("```{r}\n%s\n```\n", answer)
        else
            md_answer <- sprintf("```r\n%s\n```\n", answer)
    } else {
        alt_answer <- if(is.list(args$alt.answer)) args$alt.answer else list(args$alt.answer)
        alt_answer <- replace_answers(alt_answer, question$get_hdata())
        alt_answer <- alt_answer[[1]]
        if(is.character(alt_answer)) {
            md_answer <- paste0(trimws(alt_answer), "\n")
        } else if(args$eval)
            md_answer <- sprintf("```{r}\n%s\n```\n", answerstr(alt_answer))
        else
            md_answer <- sprintf("```r\n%s\n```\n", answerstr(alt_answer))
    }

    ## Concatenating for final markdown
    paste0(c(
        md_hdata_blk,
        if(args$numbered) sprintf("**Question %d.** ", qno),
        if(args$question.body) trimws(question$text),
        "\n\n**Réponse:**\n",
        md_answer_blk,
        md_answer,
        "\n",
        if(args$eval) "La réponse est: $`r answer`$"), collapse="")
}

## eval_noeval <- function(feedback_eval, feedback_noeval) {
##     function(qno, question, env, ...) {
##         args <- list(...)
##         if(is.null(args$eval))
##             if(is.character(feedback_eval))

##                 else

##             feedback_noeval(qno, question, env, ...)
##         else
##             feedback_eval(qno, question, env, ...)
##     }
## }

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
