hexa_hash <- function(s) {
    substr(digest::digest(s), 0, 7)
}

add_spaces_left <- function(s, spaces) {
    indent <- paste(rep(" ", spaces), collapse = "")
    gsub("(?m)^", indent, s, perl = TRUE)
}

merge_languages <- function(...) {
    ls <- unlist(list(...))
    if (length(ls) == 1) return(ls[[1]])
    if (is.null(ls))
        quote({})
    else {
        langs <- sapply(ls, function(l) {
            if (is.symbol(l))
                return(list(l))
            else if (l[[1]] == as.name("{"))
                if (length(l) == 1)     # Empty {}
                    return(NULL)
                else                    # Remove {}
                    return(as.list(l)[-1])
            else return(list(l))
        })
        do.call(call, c(list("{"), unlist(langs)), quote = TRUE)
    }
}

fails <- function(language) {
    tryCatch({
        eval(language, cleanenv())
    },
    error = function(e) stop("Some errors in data code chunks:\n", e))
}

cleanenv <- function() {
    new.env(parent = parent.env(.GlobalEnv))
}

#' Helper function to specify list of languages
#'
#' Helper function 
#' @param Expressions
#'
#' @return A quoted expression or a list of quoted expression
#' @export
expr <- function(...) {
    l <- as.list(match.call(expand.dots = TRUE)[-1])
    if (length(l) == 1) l[[1]] else l
}

to_string <- function(filename) {
    paste(readLines(filename), collapse = "\n")
}

aliases_from_question <- function(question) {
    hdata <- question$hidden.data
    env <- cleanenv()
    eval(hdata, env)
    if (length(ls(env)) >= 1) {
        prefix <- sprintf("data%s", question$id)
        varnames <- paste0(prefix, ls(env))
        aliases <- lapply(as.list(varnames), as.name)
        names(aliases) <- ls(env)
        aliases
    } else list()
}

clozify <- function(...) {
    questions <- list(...)

    aliases_list <- lapply(questions, aliases_from_question)

    texts <- mapply(function(question, aliases) {
        if (question$type == "cloze") {
            replace_text(question$text, aliases)
        } else if (question$type == "shortanswer") {
            paste(replace_text(question$text, aliases), sprintf("{%d:SA:=*}", question$points))
        } else stop("Unhandler question type")
    }, questions, aliases_list)
    text <- paste(texts, collapse = "\n\n")

    hidden.data <- do.call(merge_languages, mapply(function(question, aliases) {
        replace_language(question$hidden.data, aliases)
    }, questions, aliases_list), quote = TRUE)

    data <- do.call(merge_languages, mapply(function(question, aliases) {
        replace_language(question$data, aliases)
    }, questions, aliases_list), quote = TRUE)

    answers <- mapply(function(question, aliases) {
        if (question$type == "cloze") {
            replace_language(question$answer, aliases)
        } else {
            list(replace_language(question$answer, aliases))
        }
    }, questions, aliases_list)

    points <- sapply(questions, function(question) {
        if (question$type == "cloze") {
            sum(cloze_field_points_text(question$text))
        } else {
            question$points
        }
    })

    Question(text,
             type = "cloze",
             answer = answers,
             hidden.data = hidden.data,
             data = data,
             feedback = automatic_feedback,
             points = sum(points))
}
