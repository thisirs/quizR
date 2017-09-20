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

translate_hidden_data <- function(question) {
    hdata <- question$get_hdata()
    env <- cleanenv()
    eval(hdata, env)
    if (length(ls(env)) >= 1) {
        prefix <- sprintf("data%s", question$id)
        varnames <- paste0(prefix, ls(env))
        aliases <- lapply(as.list(varnames), as.name)
        names(aliases) <- ls(env)
        as.environment(aliases)
    } else cleanenv()
}

clozify <- function(...) {
    questions <- list(...)

    ## Prefix all defined variables in hidden.data
    env_list <- lapply(questions, translate_hidden_data)

    texts <- mapply(function(question, env) {
        if (question$type == "cloze") {
            replace_text_env(question$get_text(), env)
        } else if (question$type == "shortanswer") {
            paste(replace_text_env(question$get_text(), env), sprintf("{%d:SA:=*}", question$points))
        } else if (question$type == "numerical") {
            paste(replace_text_env(question$get_text(), env), sprintf("{%d:NM:=*}", question$points))
        } else stop("Unhandled question type")
    }, questions, env_list)
    text <- paste(texts, collapse = "\n\n")

    hidden.data <- do.call(merge_languages, mapply(function(question, env) {
        replace_language_env(question$get_hdata(), env)
    }, questions, env_list), quote = TRUE)

    data <- do.call(merge_languages, mapply(function(question, env) {
        replace_language_env(question$get_data(), env)
    }, questions, env_list), quote = TRUE)

    answers <- unlist(mapply(function(question, env) {
        if (question$type == "cloze") {
            if (is.list(question$get_answer())) {
                lapply(question$get_answer(), function(ans) {
                    if (is.list(ans)) {
                        lapply(ans, replace_language_env, env)
                    } else {
                        replace_language_env(ans, env)
                    }
                })
            } else {
                stop("Answer field in cloze question should be a list")
            }
        } else {
            if (is.list(question$get_answer())) {
                list(lapply(question$get_answer(), replace_language_env, env))
            } else {
                list(replace_language_env(question$get_answer(), env))
            }
        }
    }, questions, env_list, SIMPLIFY = FALSE), recursive = FALSE)

    points <- sapply(questions, function(question) {
        if (question$type == "cloze") {
            sum(cloze_field_points_text(question$get_text()))
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
