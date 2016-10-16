#' Create a quiz object
#'
#' @param title Title of the quiz
#' @param groups List of groups
#' @param data Language object defining the data
#' @param hidden.data Language object defining the hidden data
#' @param seed The seed used to generate hidden data
#'
#' @return A quiz object
#' @export
Quiz <- function(title, groups, data, hidden.data, seed=NULL) {
    if(missing(title)) stop("Quiz needs a title")
    if(missing(groups)) groups <- list()
    if(missing(data)) data <- quote({})
    if(missing(hidden.data)) hidden.data <- quote({})

    .hdata <- NULL
    set_hdata <- function(hdata) {
        .hdata <<- hdata
    }
    get_hdata <- function() .hdata

    me <- list(
        title=title,
        groups=groups,
        data=data,
        hidden.data=hidden.data,
        seed=seed,
        get_hdata=get_hdata,
        set_hdata=set_hdata
    )

    class(me) <- append(class(me), "Quiz")
    return(me)
}

#' Validate a quiz object
#'
#' @param quiz A quiz object
#'
#' @return Return TRUE if quiz is a valid one
validate_quiz <- function(quiz, lang) {
    stopifnot(length(quiz$groups) > 0)

    # Only one question in identifier group if any
    if(quiz$groups[[1]]$type == 'identifier') {
        stopifnot(length(quiz$groups[[1]]$questions) == 1)
        groups <- quiz$groups[-1]
    }
    else
        groups <- quiz$groups

    ## Not only an identifier group
    stopifnot(length(groups) > 0)

    ## No other identifier group
    stopifnot(!"identifier" %in% lapply(groups, function(g) {g$type}))

    ## No group with no question
    for(g in groups)
        stopifnot(length(g$questions) > 0)

    ## Check that IDs are unique
    stopifnot(uniqueIDs(quiz))

    ## Check that seed is present if hidden data present
    if(is.null(quiz$seed)) {
        if(paste0(deparse(quiz$hidden.data), collapse="") != "{}")
            stop("Seed should be specified when using hidden data")
        for(g in quiz$groups) {
            if(paste0(deparse(g$hidden.data), collapse="") != "{}")
                stop("Seed should be specified when using hidden data")
            for(q in g$questions) {
                if(paste0(deparse(q$hidden.data), collapse="") != "{}")
                    stop("Seed should be specified when using hidden data")
            }
        }
    }

    ## Data is distinct
    stopifnot(distinct_data(quiz, lang))

}


#' Compute grades for given quiz and results
#'
#' @param quiz Quiz
#' @param filename CSV file of results
#' @export
compute_grades <- function(quiz, filename=NULL) {
    stopifnot(is.character(filename))
    data <- utils::read.csv(filename, header=T, check.names=F, stringsAsFactors=F)
    results <- compute_results_from_data(quiz, data)
    grades <- sapply(results, function(e) { e$grade })
    data.frame(Nom=data[,1], `Prénom`=data[,2], note=grades)
}

#' Compute full results of given quiz
#'
#' @param quiz Quiz
#' @param filename CSV file of results
#' @export
compute_results <- function(quiz, filename=NULL) {
    stopifnot(is.character(filename))

    ## Loading answers (there is a header, do not modify col names)
    data <- utils::read.csv(filename, header=T, check.names=F, stringsAsFactors=F)

    return(compute_results_from_data(quiz, data))
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
#' @param questions List of questions
#' @param data Language object defining the data
#' @param hidden.data Language object defining the hidden data
#' @export
Group <- function(title, type, num, data, hidden.data, questions) {
    if(missing(hidden.data)) hidden.data <- quote({}) else stopifnot(is.language(hidden.data))
    if(missing(data)) data <- quote({}) else stopifnot(is.language(data))
    if(missing(num) && type == 'random') stop("Missing `num' argument for random group")
    if(!missing(num) && type != "random") stop("Ignored `num' argument for non-random group")
    match.arg(type, c("random", "sequential", "identifier"))
    num <- if(missing(num)) NA else num
    if(missing(questions)) questions <- list()

    .hdata <- NULL
    set_hdata <- function(hdata) {
        .hdata <<- hdata
    }
    get_hdata <- function() .hdata

    me <- list(
        title=title,
        type=type,
        num=num,
        questions=questions,
        data=data,
        hidden.data=hidden.data,
        get_hdata=get_hdata,
        set_hdata=set_hdata
    )
    class(me) <- append(class(me), "Group")
    return(me)
}

#' Add question to group
#'
#' @param group Group object
#' @param question Question to add
#' @export
add_question <- function(group, question)
{
    group$questions[[length(group$questions) + 1]] <- question
    return(group)
}

#' Number of real questions (except description questions)
#'
#' @param group Group object
get_num <- function(group) {
    if(group$type == "random")
        return(group$num)
    else {
        return(sum(unlist(lapply(group$questions, function(q) q$type != "description"))))
    }
}

#' Number of fields in cloze question
#'
#' @param text Some character string
get_cloze_num <- function(text) {
    stringi::stri_count_regex(text, cloze_regex)
}

questionTypes <- c("shortanswer", "description", "cloze")


#' Create a question object
#'
#' @param text Body of question
#' @param type Type of question
#' @param answer Answer of question
#' @param data Language object defining the data
#' @param hidden.data Language object defining the hidden data
#' @param feedback Feedback of question
#' @param points Number of points of question
#' @param dist Tolerance of answer for character string answer
#' @param epsilon Relative error for numeric answer
#' @export
Question <- function(text, type=NULL, answer=NULL, hidden.data=quote({}), data=quote({}), feedback=automatic_feedback, points=1, dist=2, epsilon=1e-4) {
    stopifnot(is.character(text))
    type <- ifelse(is.null(type), "shortanswer", type)
    match.arg(type, questionTypes)

    .hdata <- NULL
    set_hdata <- function(hdata) {
        .hdata <<- hdata
    }
    get_hdata <- function() .hdata

    me <- list(
        text=text,
        type=type,
        hidden.data=hidden.data,
        data=data,
        answer=answer,
        feedback=feedback,
        points=points,
        dist=dist,
        epsilon=epsilon)

    me$id <- hexa_hash(me)

    me$get_hdata <- get_hdata
    me$set_hdata <- set_hdata

    class(me) <- append(class(me), "Question")
    return(me)
}

#' Replace data in list of answers
#'
#' @param answers List of answers
#' @param data Data to replace
replace_answers <- function(answers, data) {
    env <- new.env(parent=.GlobalEnv)
    eval(data, env)
    lapply(answers, function(e) {
        call <- substitute(substitute(e, env), list(e = e))
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
               closure={
                   environment(answer) <- new.env(parent=env)
                   answer()
               },
               language={
                   eval(answer, new.env(parent=env))
               },
               symbol={
                   eval(answer, new.env(parent=env))
               },
               character={answer},
               double={answer},
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
        if(is.numeric(answer)) {
            guess <- as.numeric(guess)
            !is.na(guess) && abs(answer - guess) <= epsilon * abs(answer)
        } else if(is.character(answer)) {
            guess <- as.character(guess)
            utils::adist(answer, guess) <= dist
        } else {
            stop("Unhandled answer type")
        }
    })
}

#' Return coefficients of cloze fields over their sum
#'
#' @param question A question
cloze_coefficients <- function(question) {
    stopifnot(question$type == "cloze")
    coeffs <- as.numeric(stringi::stri_match_all_regex(question$text, cloze_regex)[[1]][,2])
    coeffs / sum(coeffs)
}


correct_question <- function(question, env, guess) {
    if(question$type == "cloze") {
        stopifnot(is.list(question$answer))
        num <- get_cloze_num(question$text)
        stopifnot(length(question$answer) == num)
        guesses <- split_cloze_guesses(num, guess)
        stopifnot(length(guesses) == num)

        cloze_points <- rep(0, num)
        right_answers <- vector(mode="list", length=num)

        for (i in 1:num) {
            guess <- guesses[i]
            answer_raw <- question$answer[[i]]

            # Possibly several right answers, listify them
            answers <- if(is.list(answer_raw)) answer_raw else list(answer_raw)
            ea <- eval_answers(answers, env)
            match <- match_answers(ea, guess, question$dist, question$epsilon)

            if(any(match)) {
                cloze_points[i] <- 1
                right_answers[[i]] <- answers[match][[1]]
            } else {
                cloze_points[i] <- 0
                right_answers[[i]] <- answers[[1]]
            }
        }

        cloze_coefficients <- cloze_coefficients(question)
        weighted_points <- cloze_points * cloze_coefficients
        total_points <- question$points * sum(weighted_points)

        list(type="cloze",
             points=total_points,
             cloze.points=cloze_points,
             cloze.coeffs=cloze_coefficients,
             guesses=guesses,
             right_answers=right_answers)
    } else {
        # Possibly several right answers, listify them
        answers <- if(is.list(question$answer)) question$answer else list(question$answer)
        ra <- replace_answers(answers, question$get_hdata())
        ea <- eval_answers(ra, env)
        match <- match_answers(ea, guess, question$dist, question$epsilon)

        if(any(match)) {
            points <- question$points
            right_answer <- answers[match][[1]]
        } else {
            points <- 0
            right_answer <- answers[[1]]
        }
        list(type="shortanswer",
             points=points,
             guess=guess,
             right_answer=right_answer)
    }
}

expression_to_lang <- function(expr) {
    ls <- as.list(expr)
    do.call(call, c("{", ls), quote=TRUE)
}

unrandomize <- function(lang) {
    if(is.null(lang) | length(lang) == 1) return(quote({}))
    env <- new.env(parent=.GlobalEnv)
    eval(lang, env)
    tmpfile <- tempfile("data", fileext=".R")

    # Remove keepInteger from default dump option
    dump(ls(env), tmpfile, envir=env, control=c("quoteExpressions",
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
    if(!is.null(obj$seed))
        set.seed(obj$seed)
    obj$set_hdata(unrandomize(obj$hidden.data))
    lapply(obj$groups, unrandomize_data)
}

#' @export
unrandomize_data.Group <- function(obj) {
    obj$set_hdata(unrandomize(obj$hidden.data))
    lapply(obj$questions, unrandomize_data)
}

#' @export
unrandomize_data.Question <- function(obj) {
    obj$set_hdata(unrandomize(obj$hidden.data))
}

get_local_language <- function(obj) {
    hidden.data.env <- new.env(parent=.GlobalEnv)
    eval(obj$get_hdata(), hidden.data.env)
    l <- pryr::substitute_q(obj$data, as.list(hidden.data.env))
    if(length(l) == 1)
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
    if(is.null(ls)) NULL else merge_languages(ls)
}

#' @export
get_recursive_language.Group <- function(obj) {
    ll <- get_local_language(obj)
    rlqs <- lapply(obj$questions, get_local_language)
    ls <- unlist(c(ll, rlqs))
    if(is.null(ls)) NULL else merge_languages(ls)
}

#' Return TRUE if all questions' ID are unique
#'
#' @param quiz The quiz
uniqueIDs <- function(quiz) {
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
    all(sapply(intersect(ls(env1), ls(env2)), function(e) identical(get(e, envir=env1), get(e, envir=env2))))
}

#' Check that data do not overlap
#'
#' @param quiz A quiz object
#' @param lang Optional additionnal language object
#'
#' @return Return TRUE if data do not overlap
distinct_data <- function(quiz, lang) {
    if(missing(lang)) lang <- quote({})

    languages <- list(merge_languages(lang, get_local_language(quiz)))
    for(g in quiz$groups) {
        languages <- c(merge_languages(lang, get_local_language(g)), languages)
        for(q in g$questions) {
            languages <- c(merge_languages(lang, get_local_language(q)), languages)
        }
    }
    errors <- sapply(languages, fails)
    if(any(errors)) stop("Some errors in data code chunks")
    languages <- languages[!errors]
    length(languages) < 2 || all(utils::combn(languages, 2, function(args) do.call(distinct_language, args, quote=TRUE)))
}

#' Check that all given answers are consistent
#' @export
noerror_in_answers <- function(quiz, env) {
    if(missing(env)) env <- new.env(parent=baseenv())
    if(is.language(env)) {
        lang <- env
        env <- new.env(parent=baseenv())
        eval(lang, env)
    }

    l_global <- get_recursive_language(quiz)
    l_quiz <- get_local_language(quiz)
    for (g in quiz$groups) {
        if(g$type == 'identifier') next
        l_group <- get_local_language(g)
        for (q in g$questions) {
            l_question <- get_local_language(q)
            l_branch <- merge_languages(l_quiz, l_group, l_question)

            parent.env(env) <- cleanenv()

            env_branch <- new.env(parent=env)
            eval(l_branch, env_branch)

            env_global <- new.env(parent=env)
            eval(l_global, env_global)

            tryCatch(eval(q$answer, env_branch),
                     error=function(e) {
                         stop(paste("Error in question \"", substring(q$text, 1, 16), "...\": ", e))
                     })

            tryCatch(eval(q$answer, env_global),
                     error=function(e) {
                         stop(paste("Error in question \"", substring(q$text, 1, 16), "...\": ", e))
                     })
        }
    }
}

get_mapping <- function(qs.text, questions) {
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

compute_results_from_data <- function(quiz, data) {
    validate_quiz(quiz, data)

    ## No factor, numeric or character vector
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)

    ## Check that number of real questions in quiz and data match
    numQuestions <- 0
    for(g in quiz$groups)
        numQuestions <- numQuestions + get_num(g)

    ncolq <- ncol(data) - 10            # first 10 columns are info like name, email,...
    stopifnot((ncolq == numQuestions) | (ncolq == 2*numQuestions))

    ## Is data containing bodies of questions
    isWithQuestionBody <- ncolq == 2*numQuestions

    ## Strip off eventual first identifier group
    quiz0 <- quiz
    identEnabled <- quiz$groups[[1]]$type == "identifier"
    if(identEnabled) {
        quiz0$groups <- quiz$groups[-1]
    }

    allResults <- list()
    for(i in 1:nrow(data)) {
        record <- as.list(data[i, 11:ncol(data)])
        env <- cleanenv()
        if(identEnabled) {
            if(isWithQuestionBody) {
                identifier <- record[[2]]
                record <- record[-c(1, 2)]
            } else {
                identifier <- record[[1]]
                record <- record[-1]
            }
            assign('identifiant', identifier, env)
        }
        eval(get_recursive_language(quiz0), env)
        res <- correctRecord(quiz0, record, env, isWithQuestionBody)
        allResults[[length(allResults) + 1]] <- res
    }
    return(allResults)
}


correctRecord <- function(quiz0, record, env, isWithQuestionBody) {
    resultQuiz <- list(grade=0)
    for(g in quiz0$groups) {
        numq <- get_num(g)
        num <- if(isWithQuestionBody) 2*numq else numq
        recordg <- record[1:num]
        record <- record[-seq(1, num)]
        resultg <- correct_record_group(g, recordg, env, isWithQuestionBody)
        resultQuiz$grade <- resultQuiz$grade + resultg$grade
        resultQuiz$groups[[length(resultQuiz$groups) + 1]] <- resultg
    }
    return(resultQuiz)
}

correct_record_group <- function(group, record, env, isWithQuestionBody) {
    stopifnot(!group$type == "random" | isWithQuestionBody)

    if(isWithQuestionBody) {
        qs.answer <- record[c(F, T)]
        qs.text <- as.character(record[c(T, F)])
    } else
        qs.answer <- record

    if(group$type == "random") {
        map <- get_mapping(qs.text, group$questions)
    } else {
        map <- 1:get_num(group)
    }

    resultg <- list(points=0,
                    questions=list())

    for(i in 1:length(qs.answer)) {
        q.answer <- qs.answer[[i]]
        q <- group$questions[[map[i]]]
        result <- correct_question(q, new.env(parent=env), q.answer)

        ## Add body of question to result
        result$body <- if(isWithQuestionBody) qs.text[i] else paste0("Q", i)

        resultg$points <- resultg$points + result$points
        resultg$questions[[length(resultg$questions) + 1]] <- result
    }
    return(resultg)
}

split_cloze_guesses <- function(num, s_answers) {
    prefix <- "partie (\\d+) : "
    numbers <- as.numeric(stringi::stri_match_all_regex(s_answers, prefix)[[1]][,2])
    stopifnot(numbers == (1:num))

    raw_answers <- stringi::stri_split_regex(s_answers, prefix, omit_empty=TRUE)[[1]]
    stopifnot(length(raw_answers) == num)
    answers0 <- trimws(raw_answers)

    ## Remove trailing semicolon
    sub(";$", "", answers0)
}
