hexaHash <- function(s) {
    substr(digest::digest(s), 0, 7)
}

addSpacesLeft <- function(s, spaces) {
    indent <- paste(rep(" ", spaces), collapse="")
    gsub("(?m)^", indent, s, perl=T)
}
