hexaHash <- function(s) {
    substr(digest::digest(s), 0, 7)
}

addSpacesLeft <- function(s, spaces) {
    indent <- paste(rep(" ", spaces), collapse="")
    gsub("(?m)^", indent, s, perl=T)
}
# TODO Verify return value
fails <- function(language) {
    oldseed <- .Random.seed # Save seed because evaluating potential set.seed
    result <- NULL
    tryCatch({eval(language, cleanenv()); FALSE},
             error=function(e) TRUE,
             finally=.Random.seed <- oldseed)
}

