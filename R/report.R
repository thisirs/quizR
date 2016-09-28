getCharacterFromAnswer <- function(answer) {
    if(is.function(answer)) {
        d <- deparse(body(answer))
        lines <- d[-c(1, length(d))]
        indent <- min(attr(regexpr("^ +", lines), "match.length"))
        paste(substring(lines, indent + 1), collapse="\n")
    } else as.character(answer)
}


getReport <- function(allResults) {
    lapply(allResults, function(quizResult) {

    })
}


correctionForIdentifier <- function(quiz, identifier=NULL, formatter=full_answer_frmt) {
    stopifnot(length(quiz$groups) > 0)
    env <- new.env(globalenv())
    if(quiz$groups[[1]]$type == "identifier") {
        stopifnot(length(quiz$groups) > 1)
        groups0 <- quiz$groups[-1]
        warning("No identifier given but identifying question present, defaulting to \"quizR\"")
        identifier <- "quizR"
        assign('identifiant', identifier, env)
    } else groups0 <- groups
    eval(getRecursiveLanguage(quiz), env)

    qno <- 0
    report <- ""
    for (g in groups0) {
        stopifnot(g$type != "identifier")
        report <- c(report, paste("Partie", g$title, "\n\n"))
        for (q in g$questions) {
            qno <- qno + 1
            t_answers <- if(is.list(q$answer)) q$answer else list(q$answer)
            e_answers <- evalAnswers(t_answers, env)
            report <- c(report, formatter(qno, q, e_answers, t_answers))
        }
    }
    paste0(report, collapse="")
}


answerstr <- function(answer) {
    type <- typeof(answer)
    switch(type,
           closure={
               d <- deparse(body(answer))
               lines <- d[-c(1, length(d))]
               print(lines)
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse="\n")
           },
           language={
               d <- deparse(answer)
               lines <- if (length(lines) >=3) d[-c(1, length(d))] else d
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse="\n")
           },
           default=stop("Unhandled"))
}

feedback_frmt <- function(qno, question, e_answers, t_answers) {
    feedback <- question$feedback

    e_ans <- gsub("@E_ANS@", paste(e_answers, collapse=" ou"), feedback)
    t_ans <- paste(sapply(t_answers, function (a) {
        paste0("```r\n", answerstr(a), "\n```")
    }), collapse="\nou\n")

    feedback <- gsub("@E_ANS@", paste(e_answers, collapse=" ou"), feedback)
    feedback <- gsub("@T_ANS@", t_ans, feedback)
    sprintf("%02d. %s\n\n%s\n\n", qno, question$text, feedback)
}

eval_answer_frmt <- function(qno, question, e_answers, t_answers) {
    sprintf("%02d. %s\nRéponse: %s\n\n", qno, question$text, paste(e_answers, collapse=" ou "))
}

full_answer_frmt <- function(qno, question, e_answers, t_answers) {
    sprintf("%02d. %s\n\n%s\n\nRéponse: %s\n\n", qno, question$text,
            paste(t_answers, collapse=" ou "),
            paste(e_answers, collapse=" ou "))
}
