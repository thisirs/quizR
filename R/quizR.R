#' Create a quiz object
#'
#' @param title Title of the quiz
#' @param groups List of groups
#' @param data Language object defining the data
#' @param hidden.data Language object defining the hidden data
#' @param seed The seed used to generate data
#' @param hidden.seed The seed used to generate hidden data
#'
#' @return A quiz object
#' @export
Quiz <- function(title, groups, data, hidden.data, seed = NULL, hidden.seed = NULL) {
    if (missing(title)) stop("Quiz needs a title")
    if (missing(seed)) stop("Quiz needs a seed to generate its data")
    if (missing(groups)) groups <- list()
    if (missing(data)) data <- quote({})
    if (missing(hidden.data)) hidden.data <- quote({})

    .hdata <- NULL
    set_hdata <- function(hdata) {
        .hdata <<- hdata
    }
    get_hdata <- function() .hdata

    me <- list(
        title = title,
        groups = groups,
        data = data,
        hidden.data = hidden.data,
        seed = seed,
        hidden.seed = hidden.seed,
        get_hdata = get_hdata,
        set_hdata = set_hdata
    )

    class(me) <- append(class(me), "Quiz")
    return(me)
}

#' Validate a quiz object
#'
#' @param quiz A quiz object
#' @param lang Additionnal language object
#'
#' @return Return TRUE if quiz is a valid one
validate_quiz <- function(quiz, lang) {
    stopifnot(length(quiz$groups) > 0)

    # Only one question in identifier group if any
    if (quiz$groups[[1]]$type == "identifier") {
        stopifnot(length(quiz$groups[[1]]$questions) == 1)
        groups <- quiz$groups[-1]
    }
    else
        groups <- quiz$groups

    ## Not only an identifier group
    stopifnot(length(groups) > 0)

    ## No other identifier group
    stopifnot(!"identifier" %in% lapply(groups, function(g) {g$type}))

    ## Check that IDs are unique
    stopifnot(unique_IDs(quiz))

    ## Check that hidden seed is specified if hidden data somewhere
    if (is.null(quiz$hidden.seed)) {
        if (paste0(deparse(quiz$hidden.data), collapse = "") != "{}")
            stop("Hidden seed should be specified when using hidden data")
        for (g in quiz$groups) {
            if ((paste0(deparse(g$hidden.data), collapse = "") != "{}") & (is.null(g$hidden.seed)))
                stop("Hidden seed should be specified when using hidden data")
            for (q in g$questions) {
                if ((paste0(deparse(q$hidden.data), collapse = "") != "{}") & (is.null(q$hidden.seed)))
                    stop("Hidden seed should be specified when using hidden data")
            }
        }
    }

    ## Data is distinct
    stopifnot(distinct_data(quiz, lang))

    lapply(quiz$groups, validate_group)
}

#' Compute grades for given quiz and results
#'
#' @param quiz Quiz
#' @param filename CSV file of results
#' @param lang Chunk of code to pass validation
#'
#' @return A data frame of name, firstname, email and grade
#' @export
compute_grades <- function(quiz, filename = NULL, lang = NULL) {
    stopifnot(is.character(filename))
    data <- utils::read.csv(filename, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "-")
    results <- compute_results_from_data(quiz, data, lang)
    grades <- sapply(results, function(e) { e$grade })
    data.frame(Nom                   = data[, 1],
               `Prénom`              = data[, 2],
               `Adresse de courriel` = data[, 5],
               note                  = grades)
}

#' Compute full results of given quiz
#'
#' @param quiz Quiz
#' @param filename CSV file of results
#' @param lang Chunk of code to pass validation
#'
#' @return Detailed results of quiz
#' @export
compute_results <- function(quiz, filename = NULL, lang = NULL) {
    stopifnot(is.character(filename))

    ## Loading answers (there is a header, do not modify col names)
    data <- utils::read.csv(filename, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "-")

    return(compute_results_from_data(quiz, data, lang))
}

#' Write grades from answers in csv file
#'
#' @param quiz Quiz
#' @param input CSV file of results
#' @param output CSV file of grades
#' @param lang Chunk of code to pass validation
#' @export
write_grades <- function(quiz, input = NULL, output = NULL, lang = NULL) {
    grades <- compute_grades(quiz, filename = input, lang = lang)
    if (missing(output)) {
        output <- paste0(tools::file_path_sans_ext(input), "_grades.csv")
    }
    write.csv(grades, file = output, na = "-", row.names = FALSE)
}

#' Add group to quiz
#'
#' @param quiz Quiz
#' @param group Group
#' @export
add_group <- function(quiz, group) {
    quiz$groups[[length(quiz$groups) + 1]] <- group
    return(quiz)
}

#' Create a group object
#'
#' @param title Title of group
#' @param type Type of group
#' @param num Number of questions of group if of random type
#' @param data Language object defining the data
#' @param hidden.data Language object defining the hidden data
#' @param questions List of questions
#' @export
Group <- function(title, type, num, data, hidden.data, hidden.seed = NULL, questions) {
    if (missing(hidden.data)) hidden.data <- quote({}) else stopifnot(is.language(hidden.data))
    if (missing(data)) data <- quote({}) else stopifnot(is.language(data))
    if (missing(num) && type == "random") stop("Missing `num' argument for random group")
    if (!missing(num) && type != "random") stop("Ignored `num' argument for non-random group")
    match.arg(type, c("random", "sequential", "identifier"))
    num <- if (missing(num)) NA else num
    if (missing(questions)) questions <- list()

    .hdata <- NULL
    set_hdata <- function(hdata) {
        .hdata <<- hdata
    }
    get_hdata <- function() .hdata

    me <- list(
        title = title,
        type = type,
        num = num,
        questions = questions,
        data = data,
        hidden.data = hidden.data,
        hidden.seed = hidden.seed,
        get_hdata = get_hdata,
        set_hdata = set_hdata
    )
    class(me) <- append(class(me), "Group")
    return(me)
}

validate_group <- function(group) {
    if (length(group$questions) == 0) {
        stop(sprintf("Group %s is empty", sQuote(group$title)))
    }
    lapply(group$questions, validate_question)
}

#' Add question to group
#'
#' @param group Group object
#' @param question Question to add
#' @export
add_question <- function(group, question) {
    group$questions[[length(group$questions) + 1]] <- question
    return(group)
}

#' Number of real questions (except description questions)
#'
#' @param group Group object
get_num <- function(group) {
    if (group$type == "random")
        return(group$num)
    else {
        return(sum(unlist(lapply(group$questions, function(q) q$type != "description"))))
    }
}

#' @export
question_types <- c("shortanswer", "description", "cloze")

#' Create a question object
#'
#' @param text Body of question
#' @param type Type of question
#' @param answer Answer of question
#' @param hidden.data Language object defining the hidden data
#' @param data Language object defining the data
#' @param feedback Feedback of question
#' @param points Number of points of question
#' @param dist Tolerance of answer for character string answer
#' @param epsilon Relative error for numeric answer
#' @export
Question <- function(text,
                     type = question_types,
                     answer = NULL,
                     hidden.data = quote({}),
                     hidden.seed = NULL,
                     data = quote({}),
                     feedback = automatic_feedback,
                     points = if (type == "cloze")
                                  sum(cloze_field_points_text(text))
                              else
                                  1,
                     dist = 0,
                     epsilon = 1e-3) {
    stopifnot(is.character(text))
    type <- match.arg(type)

    .hdata <- NULL
    set_hdata <- function(hdata) {
        .hdata <<- hdata
    }
    get_hdata <- function() .hdata

    me <- list(
        text = text,
        type = type,
        hidden.data = hidden.data,
        hidden.seed = hidden.seed,
        data = data,
        answer = answer,
        feedback = feedback,
        points = points,
        dist = dist,
        epsilon = epsilon)

    me$id <- hexa_hash(me)

    me$get_hdata <- get_hdata
    me$set_hdata <- set_hdata

    class(me) <- append(class(me), "Question")
    return(me)
}

validate_question <- function(question) {
    if (question$type == "cloze") {
        if (get_cloze_num(question$text) != length(question$answer)) {
            stop("Number of answers and number of detected cloze fields are not the same")
        }
    }
}

#' Replace data in list of answers
#'
#' @param answers List of answers
#' @param data Data to replace
#'
#' @return A list of answers where some data is replaced
replace_answers <- function(answers, data) {
    env <- cleanenv()
    eval(data, env)
    lapply(answers, function(e) {
        call <- substitute(substitute(e, as.list(env, all.names = TRUE)), list(e = e))
        eval(call)
    })
}

#' Evaluate list of answers in environment
#'
#' @param answers List of answers
#' @param env Environment in which to evaluate
eval_answers <- function(answers, env) {
    lapply(answers, function(answer) {
        type <- typeof(answer)
        switch(type,
               closure = {
                   environment(answer) <- env
                   answer()
               },
               language = {
                   eval(answer, env)
               },
               symbol = {
                   eval(answer, env)
               },
               character = {
                   answer
               },
               double = {
                   answer
               },
               stop("Unhandled type in ", sQuote("eval_answers"), ": ", type))
    })
}

#' Match guess with list of evaluated answers
#'
#' @param eval_answers List of evaluated answers
#' @param guess A guess
#' @param dist Maximum edit distance for a match
#' @param epsilon Relative error for numeric answers
match_answers <- function(eval_answers, guess, dist, epsilon) {
    sapply(eval_answers, function(answer) {
        if (is.na(guess)){
            FALSE
        } else if (is.numeric(answer)) {
            # Fix if comma instead of dot
            if (is.character(guess))
                guess <- chartr(",", ".", guess)
            guess <- suppressWarnings(as.numeric(guess))
            !is.na(guess) & abs(answer - guess) <= epsilon * abs(answer)
        } else if (is.character(answer)) {
            guess <- suppressWarnings(as.character(guess))
            !is.na(guess) & utils::adist(answer, guess) <= dist
        } else {
            stop("Unhandled answer type")
        }
    })
}

#' Correct question
#'
#' @param question A question
#' @param env The environment in which to evaluate answers
#' @param guess A guess
correct_question <- function(question, env, guess) {
    if (question$type == "cloze") {
        correct_question_cloze(question, env, guess)
    } else {
        # Possibly several right answers, listify them
        answers <- if (is.list(question$answer)) question$answer else list(question$answer)
        ra <- replace_answers(answers, question$get_hdata())
        ea <- eval_answers(ra, env)
        match <- match_answers(ea, guess, question$dist, question$epsilon)

        if (any(match)) {
            is_correct <- TRUE
            points <- question$points
            right_answer <- ra[match][[1]]
            right_answer_eval <- ea[match][[1]]
        } else {
            is_correct <- FALSE
            points <- 0
            right_answer <- ra[[1]]
            right_answer_eval <- ea[[1]]
        }
        list(question = question,
             type = "shortanswer",
             is_correct = is_correct,
             points = points,
             guess = guess,
             right_answer_eval = right_answer_eval,
             right_answer = right_answer)
    }
}

expression_to_lang <- function(expr) {
    ls <- as.list(expr)
    do.call(call, c("{", ls), quote = TRUE)
}

unrandomize <- function(lang) {
    if (is.null(lang) | length(lang) == 1) return(quote({}))
    env <- cleanenv()
    eval(lang, env)
    tmpfile <- tempfile("data", fileext = ".R")

    vars <- ls(env, all.names = TRUE)
    if (length(vars) == 0) return(quote({}))

    # dump appears to obey to deparse.max.lines
    dml <- options(deparse.max.lines = NULL)
    on.exit(options(deparse.max.lines = dml))

    # Remove keepInteger from default dump option
    dump(vars, tmpfile, envir = env, control = c("quoteExpressions",
                                                 "showAttributes",
                                                 "useSource",
                                                 "warnIncomplete",
                                                 "keepNA"))

    expression_to_lang(parse(tmpfile))
}

#' @export
unrandomize_data <- function(obj)
{
    UseMethod("unrandomize_data", obj)
}

#' @export
unrandomize_data.Quiz <- function(obj) {
    if (!is.null(obj$hidden.seed))
        set.seed(obj$hidden.seed)
    obj$set_hdata(unrandomize(obj$hidden.data))
    lapply(obj$groups, unrandomize_data)
}

#' @export
unrandomize_data.Group <- function(obj) {
    if (!is.null(obj$hidden.seed))
        set.seed(obj$hidden.seed)
    obj$set_hdata(unrandomize(obj$hidden.data))
    lapply(obj$questions, unrandomize_data)
}

#' @export
unrandomize_data.Question <- function(obj) {
    if (!is.null(obj$hidden.seed))
        set.seed(obj$hidden.seed)
    obj$set_hdata(unrandomize(obj$hidden.data))
}

get_local_language <- function(obj) {
    hidden.data.env <- cleanenv()
    eval(obj$get_hdata(), hidden.data.env)
    l <- pryr::substitute_q(obj$data, as.list(hidden.data.env, all.names = TRUE))
    if (length(l) == 1 && l[[1]] == as.name("{"))
        return(NULL)
    else
        return(l)
}

get_recursive_language <- function(obj)
{
    UseMethod("get_recursive_language", obj)
}

#' @export
get_recursive_language.Quiz <- function(obj) {
    ll <- get_local_language(obj)
    rlgs <- lapply(obj$groups, get_recursive_language)
    ls <- unlist(c(ll, rlgs))
    if (is.null(ls)) NULL else merge_languages(ls)
}

#' @export
get_recursive_language.Group <- function(obj) {
    ll <- get_local_language(obj)
    rlqs <- lapply(obj$questions, get_local_language)
    ls <- unlist(c(ll, rlqs))
    if (is.null(ls)) NULL else merge_languages(ls)
}

#' Return TRUE if all questions' ID are unique
#'
#' @param quiz The quiz
unique_IDs <- function(quiz) {
    allQuestions <- do.call(c, lapply(quiz$groups, function(g) { g$questions }))
    !any(duplicated(sapply(allQuestions, function(q) { q$id })))
}

#' Check if languages are commutative chunks of code
#'
#' @param lang1 First chunk of code
#' @param lang2 Second chunk of code
#'
#' @return Return TRUE if the two chunks of code are commutative
distinct_language <- function (lang1, lang2) {
    env1 <- cleanenv()
    env2 <- cleanenv()
    eval(lang1, env1)
    eval(lang2, env2)
    all(sapply(intersect(ls(env1), ls(env2)), function(e) identical(get(e, envir = env1), get(e, envir = env2))))
}

#' Check that data do not overlap
#'
#' @param quiz A quiz object
#' @param lang Optional additionnal language object
#'
#' @return Return TRUE if data do not overlap
distinct_data <- function(quiz, lang) {
    if (missing(lang)) lang <- quote({})

    languages <- list(merge_languages(lang, get_local_language(quiz)))
    for (g in quiz$groups) {
        languages <- c(merge_languages(lang, get_local_language(g)), languages)
        for (q in g$questions) {
            languages <- c(merge_languages(lang, get_local_language(q)), languages)
        }
    }
    sapply(languages, fails)
    length(languages) < 2 || all(utils::combn(languages, 2, function(args) do.call(distinct_language, args, quote = TRUE)))
}

#' Check that all given answers are consistent
noerror_in_answers <- function(quiz, env) {
    if (missing(env)) env <- new.env(parent = baseenv())
    if (is.language(env)) {
        lang <- env
        env <- new.env(parent = baseenv())
        eval(lang, env)
    }

    l_global <- get_recursive_language(quiz)
    l_quiz <- get_local_language(quiz)
    for (g in quiz$groups) {
        if (g$type == 'identifier') next
        l_group <- get_local_language(g)
        for (q in g$questions) {
            l_question <- get_local_language(q)
            l_branch <- merge_languages(l_quiz, l_group, l_question)

            parent.env(env) <- cleanenv()

            env_branch <- new.env(parent = env)
            eval(l_branch, env_branch)

            env_global <- new.env(parent = env)
            eval(l_global, env_global)

            tryCatch(eval(q$answer, env_branch),
                     error = function(e) {
                         stop(paste("Error in question \"", substring(q$text, 1, 16), "...\": ", e))
                     })

            tryCatch(eval(q$answer, env_global),
                     error = function(e) {
                         stop(paste("Error in question \"", substring(q$text, 1, 16), "...\": ", e))
                     })
        }
    }
}

get_mapping <- function(res.text, questions) {
    text <- res.text[1]
    id <- stringr::str_match(text, "\\(Q([A-Za-z0-9]+)\\)")[1, 2]
    if (is.na(id)) {
        # No id present, matching text directly
        res.text0 <- gsub("[^a-zA-Z0-9]", "", res.text)

        qs.text <- gsub("[^a-zA-Z0-9]", "", sapply(questions, function(q) {
            q$text
        }))
        d <- adist(res.text0, qs.text)
        map <- apply(d, 1, which.min)
        stopifnot(any(!duplicated(map)))
        return(map)
    } else {
        map <- rep(0, length(res.text))

        for (i in 1:length(qs.text)) {
            # Extract ID from the question's body
            text <- qs.text[i]
            id <- stringr::str_match(text, "\\(Q([A-Za-z0-9]+)\\)")[1, 2]
            if (is.na(id)) stop("`id' not found")

            # Look for corresponding id in list of questions
            index <- Position(function(q){ id == q$id}, questions)
            if (is.na(index)) stop("No corresponding `id' found for ", q$text)
            map[i] <- index
        }
        return(map)
    }
}

get_number_of_questions <- function(quiz) {
    sum(sapply(quiz$groups, get_num))
}

compute_results_from_data <- function(quiz, data, lang) {
    unrandomize_data(quiz)

    validate_quiz(quiz, lang)

    ## No factor, numeric or character vector
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)

    ## Check that number of real questions in quiz and data match
    num_questions <- get_number_of_questions(quiz)

    ncolq <- ncol(data) - 10            # first 10 columns are info like name, email,...
    stopifnot((ncolq == num_questions) | (ncolq == 2*num_questions))

    ## Is data containing bodies of questions
    is_with_question_body <- ncolq == 2*num_questions

    ## Strip off eventual first identifier group
    quiz0 <- quiz
    ident_enabled <- quiz$groups[[1]]$type == "identifier"
    if (ident_enabled) {
        quiz0$groups <- quiz$groups[-1]
    }

    all_results <- list()
    for (i in 1:nrow(data)) {
        full_record <- as.list(data[i, ])
        record <- as.list(data[i, 11:ncol(data)])

        # Preparing an environment
        env <- cleanenv()
        if (ident_enabled) {
            if (is_with_question_body) {
                identifier <- record[[2]]
                record <- record[-c(1, 2)]
            } else {
                identifier <- record[[1]]
                record <- record[-1]
            }
            assign('identifiant', identifier, env)
        }
        eval(get_recursive_language(quiz0), env)

        # Correcting the student's quiz
        res <- correct_record(quiz0, record, env, is_with_question_body)

        # Adding some metadata
        res$lastname <- full_record$Nom
        res$firstname <- full_record$Prénom
        res$email <- full_record$Adresse.de.courriel
        res$identifier <- if (exists("identifier")) identifier else NULL

        # Appending to other results
        all_results[[length(all_results) + 1]] <- res
    }
    return(all_results)
}

correct_record <- function(quiz0, record, env, is_with_question_body) {
    result_quiz <- list(grade = 0)
    for (g in quiz0$groups) {
        numq <- get_num(g)
        num <- if (is_with_question_body) 2*numq else numq
        recordg <- record[1:num]
        record <- record[-seq(1, num)]
        resultg <- correct_record_group(g, recordg, env, is_with_question_body)
        result_quiz$grade <- result_quiz$grade + resultg$points
        result_quiz$groups[[length(result_quiz$groups) + 1]] <- resultg
    }
    return(result_quiz)
}

matching_id <- function(qs.text, questions) {
    # Extract id from questions body
    bodies_id <- sapply(qs.text, function(text) {
        stringr::str_match(text, "\\(Q([A-Za-z0-9]+)\\)")[1, 2]
    })

    # If no id in questions body, return OK
    if (all(is.na(bodies_id)))
        return(TRUE)

    if (any(is.na(bodies_id)))
        stop("Some questions do not have an id")

    qs_id <- sapply(questions, function(q) q$id)

    mapply(function(i, q_id, b_id) {
        if (q_id != b_id)
            stop("Id do not match")
    },
    1:length(questions), qs_id, bodies_id)
}

correct_record_group <- function(group, record, env, is_with_question_body) {
    stopifnot(!group$type == "random" | is_with_question_body)

    # Set mapping of questions with records
    if (group$type == "random") {
        if (is_with_question_body) {
            qs.text <- as.character(record[c(TRUE, FALSE)])
            qs.answer <- record[c(FALSE, TRUE)]
            map <- get_mapping(qs.text, group$questions)
        } else {
            stop("Cannot get mapping with no question body")
        }
    } else {
        if (is_with_question_body) {
            qs.answer <- record[c(FALSE, TRUE)]
            qs.text <- as.character(record[c(TRUE, FALSE)])

            ## Check that ids are the same
            matching_id(qs.text, group$questions)
        } else {
            qs.answer <- record
        }

        map <- 1:get_num(group)
    }

    resultg <- list(group = group,
                    points = 0,
                    questions = list())

    for (i in 1:length(qs.answer)) {
        q.answer <- qs.answer[[i]]
        q <- group$questions[[map[i]]]
        result <- correct_question(q, new.env(parent = env), q.answer)

        ## Add body of question to result
        result$body <- if (is_with_question_body) qs.text[i] else paste0("Q", i)

        resultg$points <- resultg$points + result$points
        resultg$questions[[length(resultg$questions) + 1]] <- result
    }
    return(resultg)
}

#' @export
generate_student_correction <- function
(
    quiz,
    quiz_result,
    output = sprintf("%s_%s_%s.pdf", quiz$title, quiz_result$lastname, quiz_result$firstname),
    lang) {

    # Instantiate random data with quiz$hidden.seed
    unrandomize_data(quiz)

    if (missing(lang)) lang <- quote({})

    validate_quiz(quiz, lang)

    title <- paste(quiz_result$lastname, quiz_result$firstname)
    yaml_chunk <- yaml_header(sprintf("%s %s", quiz$title, title))

    identifier <- quiz_result$identifier
    lang <- bquote({ identifiant <- .(identifier)})
    quiz_env <- quiz_environment(quiz, lang)

    data_chunk <- sprintf("```{r include=FALSE}\n%s\n```\n\n", answerstr(quiz_env$data))

    identifier_chunk <- sprintf("\nIdentifiant: `%s`\n\n", identifier)

    groups_chunks <- paste0(sapply(quiz_result$groups, function(g) {
        title <- sprintf("\n## %s\n\n", g$group$title)
        qno <- 0
        qs <- paste0(sapply(g$questions, function(q) {
            qno <- qno + 1
            markdown_question(q$question, qno, quiz_env$env, eval = TRUE, guess = q)
        }), collapse = "\n")
        paste0(title, qs)
    }), collapse = "\n")

    markdown <- paste(c(yaml_chunk, data_chunk, identifier_chunk, groups_chunks), collapse = "")

    tmpfile <- tempfile("quiz", fileext = ".Rmd")
    write(markdown, tmpfile)

    ## Setting seed to evaluate data0
    set.seed(quiz$seed)

    rmarkdown::render(input = tmpfile,
                      output_dir = dirname(output),
                      output_file = basename(output),
                      "pdf_document")
}

generate_student_corrections <- function
(
    quiz,
    results,
    output.dir = "corrections",
    lang
) {
    for (student_result in results) {
        output <- sprintf("%s/%s_%s_%s.pdf",
                          output.dir,
                          quiz$title,
                          student_result$lastname,
                          student_result$firstname)
        generate_student_correction(quiz, student_result, output, lang)
    }
}
