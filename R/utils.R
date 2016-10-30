hexa_hash <- function(s) {
    substr(digest::digest(s), 0, 7)
}

add_spaces_left <- function(s, spaces) {
    indent <- paste(rep(" ", spaces), collapse="")
    gsub("(?m)^", indent, s, perl=T)
}

merge_languages <- function(...) {
    ls <- unlist(list(...))
    if(length(ls) == 1) return(ls[[1]])
    if(is.null(ls))
        quote({})
    else {
        langs <- sapply(ls, function(l) {
            if(length(l) > 1 && l[[1]] == as.name("{"))
                return(as.list(l)[-1])
            else
                return(list(l))
        })
        do.call(call, c(list("{"), unlist(langs)), quote=TRUE)
    }
}

fails <- function(language) {
    tryCatch({eval(language, cleanenv()); FALSE},
             error=function(e) TRUE)
}

cleanenv <- function() {
    new.env(parent=parent.env(.GlobalEnv))
}

#' @export
expr <- function(...) {
    l <- as.list(match.call(expand.dots=TRUE)[-1])
    if(length(l) == 1) l[[1]] else l
}

to_string <- function(filename) {
    paste(readLines(filename), collapse="\n")
}
