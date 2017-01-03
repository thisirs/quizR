markdown_question <- function(q, qno, env, eval) {
    if (q$type == "description") {
        paste0(q$text, "\n")
    } else {
        if (is.function(q$feedback)) {
            body <- q$feedback(qno, q, env, eval = eval)
        } else if (is.character(q$feedback)) {
            t_answers <- if (is.list(q$answer)) q$answer else list(q$answer)
            r_answers <- replace_answers(t_answers, q$get_hdata())

            hdata <- answerstr(q$get_hdata())
            answer <- answerstr(r_answers[[1]])

            body <- sprintf("```{r include=FALSE}\n%s\n```\n\n", hdata)
            body <- c(body, sprintf("**Question %d.** %s\n\n", qno, q$text))
            body <- c(body, sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n\n", answer))
            body <- c(body, "**Réponse:**\n")
            body <- c(body, q$feedback)
        } else stop("Unhandled feedback")
        paste0("\n", body, "\n")
    }
}

markdown_title <- function(title) {
    sprintf("---
title: \"%s\"
---\n\n", title)
}

markdown_group <- function(g, env, eval) {
    ## Set title of group
    if (g$type == "random")
        title <- sprintf("\n## %s (aléatoire %d parmi %d)\n\n", g$title, g$num, length(g$questions))
    else if (g$type == "sequential")
        title <- sprintf("\n## %s \n\n", g$title)
    else
        stop("Unhandled group type")

    soutput <- c(title)
    qno <- 0
    for (q in g$questions) {
        if (q$type != "description") {
            qno <- qno + 1
            soutput <- c(soutput, markdown_question(q, qno, env, eval))
        }
    }
    soutput
}

quiz_environment <- function(quiz, lang) {
    data <- get_recursive_language(quiz)
    data0 <- merge_languages(lang, data)

    ## Setting seed to evaluate data0
    set.seed(quiz$seed)

    env <- cleanenv()
    eval(data0, env)

    list(env = env, data = data0)
}

#' @export
generate_correction <- function(quiz, output, lang, eval = TRUE) {
    # Instantiate random data with quiz$hidden.seed
    unrandomize_data(quiz)

    if (missing(output)) output <- paste0(quiz$title, ".pdf")
    if (missing(lang)) lang <- quote({})

    validate_quiz(quiz, lang)

    title_chunk <- markdown_title(quiz$title)

    quiz_env <- quiz_environment(quiz, lang)

    data_chunk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", answerstr(quiz_env$data))

    groups_chunks <- unlist(sapply(quiz$groups, function(g) {
        if (g$type == "identifier")
            NULL
        else
            markdown_group(g, env = quiz_env$env, eval = eval)
    }))

    markdown <- paste(c(title_chunk, data_chunk, groups_chunks), collapse = "")

    tmpfile <- tempfile("quiz", fileext = ".Rmd")
    write(markdown, tmpfile)

    ## Setting seed to evaluate data0
    set.seed(quiz$seed)

    rmarkdown::render(input = tmpfile,
                      output_dir = dirname(output),
                      output_file = basename(output),
                      "pdf_document")
}

#' @export
generate_corrections <- function(quiz, input, lang) {
    unrandomize_data(quiz)

    validate_quiz(quiz, lang)

    data <- utils::read.csv(input, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "-")

    ## No factor, numeric or character vector
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)

    ## Check that number of real questions in quiz and data match
    num_questions <- get_number_of_questions(quiz)

    ncolq <- ncol(data) - 10            # first 10 columns are info like name, email,...
    stopifnot((ncolq == num_questions) | (ncolq == 2*num_questions))

    ## Is data containing bodies of questions
    is_with_question_body <- ncolq == 2*num_questions

    ident_enabled <- quiz$groups[[1]]$type == "identifier"
    stopifnot(ident_enabled)

    if (is_with_question_body) {
        identifier_list <- data[, 12]
    } else {
        identifier_list <- data[, 11]
    }

    identifier_list <- identifier_list[!is.na(identifier_list)]
    identifier_list <- unique(identifier_list)
    for (identifier in identifier_list) {
        output <- paste0(quiz$title, "_correction_", identifier, ".pdf")
        generate_correction(quiz,
                            output = output,
                            lang = bquote({ identifiant <- .(identifier)}),
                            eval = TRUE)
    }
}

cloze_regex <- "\\{(\\d+):(SHORTANSWER|SA|MW|SHORTANSWER_C|SAC|MWC|NUMERICAL|NM|MULTICHOICE|MC|MULTICHOICE_V|MCV|MULTICHOICE_H|MCH):=([^\\}]+)\\}"

replace_cloze_fields <- function(text) {
    num <- get_cloze_num(text)
    for (i in 1:num) {
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

feedback_defaults <- list(numbered = TRUE, eval = TRUE, question.body = TRUE, alt.answer = NULL, header = NULL, eval_feedback = NULL, noeval_feedback = NULL)

feedback_args <- names(feedback_defaults)

#' @export
general_feedback <- function(...) {
    args <- list(...)
    stopifnot(length(setdiff(names(args), feedback_args)) == 0)

    general_feedback0 <- function(qno, question, env, ...) {
        override_args <- list(...)
        stopifnot(length(setdiff(names(override_args), feedback_args)) == 0)
        f.args <- merge.list(override_args, args)

        all_args <- c(list(qno, question, env), f.args)
        do.call(automatic_feedback, all_args, quote = TRUE)
    }
    return(general_feedback0)
}

#' @export
header_feedback <- function(header, alt.answer) {
    general_feedback(eval = TRUE, numbered = TRUE, question.body = TRUE, header = header, alt.answer = alt.answer)
}

#' @export
alt_feedback <- function(alt_answer) {
    general_feedback(alt.answer = alt_answer)
}

#' @export
automatic_feedback <- function(qno, question, env, ...) {
    args <- list(...)

    stopifnot(length(setdiff(names(args), feedback_args)) == 0)
    args <- merge.list(args, feedback_defaults)

    if (is.null(qno) & args$numbered) stop("Cannot number without numbers")
    if (is.null(qno)) args$numbered <- FALSE

    if (is.null(env) & args$eval) stop("Cannot eval with a null environment")
    if (is.null(env)) args$eval <- FALSE

    if (question$type == "cloze") {
        do.call(automatic_cloze_feedback, c(list(qno, question, env), args), quote = TRUE)
    } else {
        do.call(automatic_normal_feedback, c(list(qno, question, env), args), quote = TRUE)
    }
}

automatic_cloze_feedback <- function(qno, question, env, ...) {
    args <- list(...)

    ## Replace cloze fields by numbers enclosed in parens
    body <- replace_cloze_fields(question$text)

    ## Block defining hidden data
    hdata <- question$get_hdata()
    if (length(hdata) == 1 && hdata[[1]] == as.name("{")) {
        md_hdata_blk <- NULL
    } else {
        hdata_s <- answerstr(hdata)
        md_hdata_blk <- sprintf("```{r include=FALSE}\n%s\n```\n", hdata_s)
    }

    paste0(c(
        md_hdata_blk,
        if (args$numbered) sprintf("**Question %d.** ", qno),
        if (args$question.body) trimws(body),
        "\n\n**Réponse:**\n\n",
        if (!is.null(args$header)) args$header,
        sapply(1:get_cloze_num(question$text), function(i) {
            answers <- if (is.list(question$answer[[i]])) question$answer[[i]] else list(question$answer[[i]])
            answers_rep <- replace_answers(answers, question$get_hdata())
            answers_eval <- eval_answers(answers_rep, env)

            # First answer in raw form, as a string and evaluated
            answer_lang <- answers_rep[[1]]
            answer_str <- answerstr(answer_lang)
            answer_eval <- answers_eval[[1]]

            if (args$eval)
                md_answer_blk <- sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer_str)
            else
                md_answer_blk <- "```{r include=FALSE}\nanswer <- \"undefined\"\n```\n"

            ## Setting md_answer
            if (args$eval) {
                ## Selecting the feedback to display
                if (!is.null(args$eval_feedback))
                    feedback <- args$eval_feedback[[i]]
                else if (!is.null(args$alt.answer))
                    feedback <- args$alt.answer[[i]]
                else if (!is.null(args$noeval_feedback))
                    feedback <- args$noeval_feedback[[i]]
                else feedback <- sprintf("```{r}\n%s\n```\n", answer_str)

                ## Listify, replace with hdata and extract first
                feedback <- if (is.list(feedback)) feedback else list(feedback)
                feedback <- replace_answers(feedback, question$get_hdata())
                feedback <- feedback[[1]]

                if (is.character(feedback))
                    md_answer <- paste0(trimws(feedback), "\n")
                else
                    md_answer <- sprintf("```{r}\n%s\n```\n", answerstr(feedback))
            } else {
                if (!is.null(args$noeval_feedback))
                    feedback <- args$noeval_feedback[[i]]
                else if (!is.null(args$alt.answer))
                    feedback <- args$alt.answer[[i]]
                else feedback <- sprintf("```r\n%s\n```\n", answer_str)

                feedback <- if (is.list(feedback)) feedback else list(feedback)
                feedback <- replace_answers(feedback, question$get_hdata())
                feedback <- feedback[[1]]

                if (is.character(feedback))
                    md_answer <- paste0(trimws(feedback), "\n")
                else
                    md_answer <- sprintf("```r\n%s\n```\n", answerstr(feedback))
            }

            c(md_answer_blk,
              sprintf("%d. ", i),
              if (args$eval) {
                  if (is.character(answer_eval))
                      "La réponse est: ``r answer``"
                  else
                      "La réponse est: $`r answer`$"
              },
              "\n",
              md_answer,
              "\n")
        })), collapse = "")
}

automatic_normal_feedback <- function(qno, question, env, ...) {
    args <- list(...)

    ## Block defining hidden data
    hdata <- question$get_hdata()
    if (length(hdata) == 1 && hdata[[1]] == as.name("{")) {
        md_hdata_blk <- NULL
    } else {
        hdata_s <- answerstr(hdata)
        md_hdata_blk <- sprintf("```{r include=FALSE}\n%s\n```\n", hdata_s)
    }

    ## Listify answer, replace by hidden data and eval in env
    answers <- if (is.list(question$answer)) question$answer else list(question$answer)
    answers_rep <- replace_answers(answers, question$get_hdata())
    answers_eval <- eval_answers(answers_rep, env)

    # First answer in raw form, as a string and evaluated
    answer_lang <- answers_rep[[1]]
    answer_str <- answerstr(answer_lang)
    answer_eval <- answers_eval[[1]]

    if (args$eval)
        md_answer_blk <- sprintf("```{r include=FALSE}\nanswer <- {%s}\n```\n", answer_str)
    else
        md_answer_blk <- "```{r include=FALSE}\nanswer <- \"undefined\"\n```\n"

    ## Setting md_answer
    if (args$eval) {
        ## Selecting the feedback to display
        if (!is.null(args$eval_feedback))
            feedback <- args$eval_feedback
        else if (!is.null(args$alt.answer))
            feedback <- args$alt.answer
        else if (!is.null(args$noeval_feedback))
            feedback <- args$noeval_feedback
        else feedback <- sprintf("```{r}\n%s\n```\n", answer_str)

        ## Listify, replace with hdata and extract first
        feedback <- if (is.list(feedback)) feedback else list(feedback)
        feedback <- replace_answers(feedback, question$get_hdata())
        feedback <- feedback[[1]]

        if (is.character(feedback))
            md_answer <- paste0(trimws(feedback), "\n")
        else
            md_answer <- sprintf("```{r}\n%s\n```\n", answerstr(feedback))
    } else {
        if (!is.null(args$noeval_feedback))
            feedback <- args$noeval_feedback
        else if (!is.null(args$alt.answer))
            feedback <- args$alt.answer
        else feedback <- sprintf("```r\n%s\n```\n", answer_str)

        feedback <- if (is.list(feedback)) feedback else list(feedback)
        feedback <- replace_answers(feedback, question$get_hdata())
        feedback <- feedback[[1]]

        if (is.character(feedback))
            md_answer <- paste0(trimws(feedback), "\n")
        else
            md_answer <- sprintf("```r\n%s\n```\n", answerstr(feedback))
    }

    ## Concatenating for final markdown
    paste0(c(
        md_hdata_blk,
        md_answer_blk,
        if (args$numbered) sprintf("**Question %d.** ", qno),
        if (args$question.body) trimws(question$text),
        "\n\n**Réponse:**",
        if (args$eval) {
            if (is.character(answer_eval))
                " ``r answer``"
            else
                " $`r answer`$"
        },
        "\n\n",
        md_answer,
        "\n"),
        collapse = "")
}

answerstr <- function(answer) {
    if (is.null(answer)) return("")
    type <- typeof(answer)
    switch(type,
           closure = {
               d <- deparse(body(answer), width.cutoff = 120)
               lines <- d[-c(1, length(d))]
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse = "\n")
           },
           language = {
               d <- deparse(answer, width.cutoff = 120)
               lines <- if (length(d) >=3) d[-c(1, length(d))] else d
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse = "\n")
           },
           symbol = {deparse(answer, width.cutoff = 120)},
           double = {deparse(answer, width.cutoff = 120)},
           character = {answer},
           stop("Unhandled type in ", sQuote("answerstr"), ": ", type))
}
