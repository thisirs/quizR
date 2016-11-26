#' Number of fields in cloze question
#'
#' @param text Some character string
get_cloze_num <- function(text) {
    stopifnot(is.character(text))
    stringi::stri_count_regex(text, cloze_regex)
}

#' Return coefficients of cloze fields over their sum
#'
#' @param question A question
cloze_coefficients <- function(question) {
    stopifnot(question$type == "cloze")
    coeffs <- as.numeric(stringi::stri_match_all_regex(question$text, cloze_regex)[[1]][,2])
    coeffs / sum(coeffs)
}

split_cloze_guesses <- function(num, s_answers) {
    prefix <- "partie (\\d+) :"
    numbers <- as.integer(stringi::stri_match_all_regex(s_answers, prefix)[[1]][, 2])
    stopifnot(identical(numbers, 1:num))

    raw_answers <- strsplit(s_answers, "; ")[[1]]
    raw_answers <- gsub(prefix, "", raw_answers)

    stopifnot(length(raw_answers) == num)
    answers0 <- trimws(raw_answers)
}

correct_question_cloze <- function(question, env, guess) {
    stopifnot(is.list(question$answer))
    num <- get_cloze_num(question$text)
    stopifnot(length(question$answer) == num)
    guesses <- split_cloze_guesses(num, guess)
    stopifnot(length(guesses) == num)

    cloze_points <- rep(0, num)
    right_answers <- vector(mode = "list", length = num)

    for (i in 1:num) {
        guess <- guesses[i]
        answer_raw <- question$answer[[i]]

        # Possibly several right answers, listify them
        answers <- if (is.list(answer_raw)) answer_raw else list(answer_raw)
        ea <- eval_answers(answers, env)
        match <- match_answers(ea, guess, question$dist, question$epsilon)

        if (any(match)) {
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

    list(type = "cloze",
         points = total_points,
         cloze.points = cloze_points,
         cloze.coeffs = cloze_coefficients,
         guesses = guesses,
         right_answers = right_answers)
}
