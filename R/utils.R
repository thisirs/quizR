hexaHash <- function(s) {
    substr(digest::digest(s), 0, 7)
}

addSpacesLeft <- function(s, spaces) {
    indent <- paste(rep(" ", spaces), collapse="")
    gsub("(?m)^", indent, s, perl=T)
}

# Concatenate languages
merge_languages <- function(...) {
    ls <- unlist(list(...))
    if(length(ls) == 1) return(ls[[1]])
    if(is.null(ls)) quote({}) else do.call(call, c("{", ls), quote=TRUE)
}

fails <- function(language) {
    result <- NULL
    tryCatch({eval(language, cleanenv()); FALSE},
             error=function(e) TRUE)
}

cleanenv <- function() {
    new.env(parent=parent.env(.GlobalEnv))
}
toString <- function(filename) {
    paste(readLines(filename), collapse="\n")
}
