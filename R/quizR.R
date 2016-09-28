#' Create a quiz object
#'
#' @param title Title of the quiz
#' @param groups List of groups
#'
#' @return A quiz object
#' @export
Quiz <- function(title, groups, data, hidden.data) {
    if(missing(title)) stop("Quiz needs a title")
    if(missing(groups)) groups <- list()
    if(missing(data)) data <- quote({})
    if(missing(hidden.data)) hidden.data <- quote({})

    me <- list(
        title=title,
        groups=groups,
        data=data,
        hidden.data=hidden.data
    )

    class(me) <- append(class(me), "Quiz")
    return(me)
}

#' Compute grades for given quiz and results
#'
#' @param quiz Quiz
#' @param filename CSV file of results
#' @export
computeGrades <- function(quiz, filename=NULL) {
    stopifnot(is.character(filename))
    data <- utils::read.csv(filename, header=T, check.names=F, stringsAsFactors=F)
    results <- computeResultsFromData(quiz, data)
    grades <- sapply(results, function(e) { e$grade })
    data.frame(Nom=data[,1], `Prénom`=data[,2], note=grades)
}

#' Compute full results of given quiz
#'
#' @param quiz Quiz
#' @param filename CSV file of results
#' @export
computeResults <- function(quiz, filename=NULL) {
    stopifnot(is.character(filename))

    ## Loading answers (there is a header, do not modify col names)
    data <- utils::read.csv(filename, header=T, check.names=F, stringsAsFactors=F)

    return(computeResultsFromData(quiz, data))
}

#' Add group to quiz
#'
#' @param quiz Quiz
#' @param group Group
#' @export
addGroup <- function(quiz, group) {
    quiz$groups[[length(quiz$groups) + 1]] <- group
    return(quiz)
}

#' Create a group object
#'
#' @param title Title of group
#' @param type Type of group
#' @param num Number of questions of group if of random type
#' @export
Group <- function(title, type, num, data, hidden.data, questions) {
    if(missing(hidden.data)) hidden.data <- quote({}) else stopifnot(is.language(hidden.data))
    if(missing(data)) data <- quote({}) else stopifnot(is.language(data))
    if(missing(num) && type == 'random') stop("Missing `num' argument for random group")
    if(!missing(num) && type != "random") stop("Ignored `num' argument for non-random group")
    match.arg(type, c("random", "sequential", "identifier"))
    num <- if(missing(num)) NA else num
    if(missing(questions)) questions <- list()
    me <- list(
        title=title,
        type=type,
        num=num,
        questions=questions,
        data=data,
        hidden.data=hidden.data
    )
    class(me) <- append(class(me), "Group")
    return(me)
}

#' Add question to group
#'
#' @param group Group
#' @param question Question
#' @export
addQuestion <- function(group, question)
{
    group$questions[[length(group$questions) + 1]] <- question
    return(group)
}

getNum <- function(group) {
    if(group$type == "random")
        return(group$num)
    else {
        return(sum(unlist(lapply(group$questions, function(q) q$type != "description"))))
    }
}


questionTypes <- c("shortanswer", "description")


#' Create a question object
#'
#' @param text Body of question
#' @param type Type of question
#' @param id Id of question
#' @param answer Answer of question
#' @export
Question <- function(text, type=NULL, id=hexaHash(text), answer=NULL, hidden.data=quote({}), data=quote({}), feedback="", points=1, dist=2, epsilon=1e-4) {
    stopifnot(is.character(text))
    type <- ifelse(is.null(type), "shortanswer", type)
    match.arg(type, questionTypes)

    me <- list(
        id=id,
        text=text,
        type=type,
        hidden.data=hidden.data,
        data=data,
        answer=answer,
        feedback=feedback,
        points=points,
        dist=dist,
        epsilon=epsilon)

    class(me) <- append(class(me), "Question")
    return(me)
}

evalAnswers <- function(answers, env) {
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
               default=stop("Unhandled answer type"))
    })
}

matchAnswers <- function(evalAnswers, guess, dist, epsilon) {
    sapply(evalAnswers, function(answer) {
        if(is.numeric(answer)) {
            guess <- as.numeric(guess)
            !is.na(guess) && abs(answer - guess) <= epsilon * abs(answer)
        } else if(is.character(answer)) {
            guess <- as.character(guess)
            utils::adist(answer, guess) <= dist
        } else {
            browser()
            stop("Unhandled answer type")
        }
    })
}

isCorrect <- function(question, env, guess) {
    answers <- if(is.list(question$answer)) question$answer else list(question$answer)
    ea <- evalAnswers(answers, env)
    match <- matchAnswers(ea, guess, question$dist, question$epsilon)

    if(any(is.na(match))) browser()
    if(any(match)) {
        manswer <- ea[match][[1]]
        return(list(ok=TRUE, answer=manswer))
    } else {
        return(list(ok=FALSE, answer=ea))
    }
}


getLocalLanguage <- function(obj) {
    hidden.data.env <- new.env(parent=globalenv())
    eval(obj$hidden.data, hidden.data.env)
    l <- pryr::substitute_q(obj$data, hidden.data.env)
    if(length(l) == 1)
        return(NULL)
    else
        return(l)
}

getRecursiveLanguage <- function(obj)
{
    UseMethod("getRecursiveLanguage")
}

getRecursiveLanguage.Quiz <- function(obj) {
    ll <- getLocalLanguage(obj)
    rlgs <- lapply(obj$groups, getRecursiveLanguage)
    ls <- unlist(c(ll, rlgs))
    if(is.null(ls)) NULL else do.call(call, c("{", ls), quote=TRUE)
}

getRecursiveLanguage.Group <- function(obj) {
    ll <- getLocalLanguage(obj)
    rlqs <- lapply(obj$questions, getLocalLanguage)
    ls <- unlist(c(ll, rlqs))
    if(is.null(ls)) NULL else do.call(call, c("{", ls), quote=TRUE)
}

checkId <- function(quiz) {
    allQuestions <- do.call(c, lapply(quiz$groups, function(g) { g$questions }))
    !any(duplicated(sapply(allQuestions, function(q) { q$id })))
}

computeResultsFromData <- function(quiz, data) {
    stopifnot(length(quiz$groups) > 0)

    ## No factor, numeric or character vector
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)

    ## Check that number of real questions in quiz and data match
    numQuestions <- 0
    for(g in quiz$groups)
        numQuestions <- numQuestions + getNum(g)

    ncolq <- ncol(data) - 10            # first 10 columns are info
    # like name, email,...

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
        env <- new.env(baseenv())
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
        eval(getRecursiveLanguage(quiz0), env)
        res <- correctRecord(quiz0, record, env, isWithQuestionBody)
        allResults[[length(allResults) + 1]] <- res
    }
    return(allResults)
}


correctRecord <- function(quiz0, record, env, isWithQuestionBody) {
    resultQuiz <- list(grade=0)
    for(g in quiz0$groups) {
        numq <- getNum(g)
        num <- if(isWithQuestionBody) 2*numq else numq
        recordg <- record[1:num]
        record <- record[-seq(1, num)]
        resultg <- correctRecordGroup(g, recordg, env, isWithQuestionBody)
        resultQuiz$grade <- resultQuiz$grade + resultg$grade
        resultQuiz$groups[[length(resultQuiz$groups) + 1]] <- resultg
    }
    return(resultQuiz)
}

correctRecordGroup <- function(group, record, env, isWithQuestionBody) {
    stopifnot(!group$type == "random" | isWithQuestionBody)

    if(isWithQuestionBody) {
        qs.answer <- record[c(F, T)]
        qs.text <- as.character(record[c(T, F)])
    } else
        qs.answer <- record

    if(group$type == "random") {
        map <- getMapping(qs.text, group$questions)
    } else {
        map <- 1:getNum(group)
    }

    resultg <- list(grade=0)
    for(i in 1:length(qs.answer)) {
        q.answer <- qs.answer[[i]]
        q <- group$questions[[map[i]]]
        result <- isCorrect(q, new.env(parent=env), q.answer)
        if(result$ok)
            resultg$grade <- resultg$grade + q$points
        resultq <- list(body=if(isWithQuestionBody) qs.text[i] else paste0("Q", i),
                        truth=q$answer,
                        answer=result$answer,
                        guess=q.answer,
                        result=result)
        resultg$questions[[length(resultg$questions) + 1]] <- resultq
    }
    return(resultg)
}
