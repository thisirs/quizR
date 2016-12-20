#' Number of fields in cloze question
#'
#' @param text Some character string
get_cloze_num <- function(text) {
    stopifnot(is.character(text))
    stringi::stri_count_regex(text, cloze_regex)
}

#' Return list of cloze fields points
#'
#' @param question A question
cloze_field_points <- function(question) {
    stopifnot(question$type == "cloze")
    cloze_field_points_text(question$text)
}

cloze_field_points_text <- function(text) {
    as.numeric(stringi::stri_match_all_regex(text, cloze_regex)[[1]][,2])
}

split_cloze_guesses <- function(num, s_answers) {
    if (is.na(s_answers)) {
        return(rep(NA, num))
    } else {
        prefix <- "partie (\\d+) :"
        numbers <- as.integer(stringi::stri_match_all_regex(s_answers, prefix)[[1]][, 2])
        stopifnot(identical(numbers, 1:num))

        # Get locations prefix to take them out
        locs <- stringi::stri_locate_all_regex(s_answers, prefix)[[1]]

        # Starts and ends of answers
        cuts <- c(as.vector(t(locs)) + c(-1, 1), nchar(s_answers))[-1]

        # Extract answers
        raw_answers <- substring(s_answers, cuts[c(T, F)], cuts[c(F, T)])

        # Remove possible trailing "; "
        raw_answers <- gsub("; $", "", raw_answers)

        stopifnot(length(raw_answers) == num)
        return(trimws(raw_answers))
    }
}

correct_question_cloze <- function(question, env, guess) {
    stopifnot(is.list(question$answer))
    num <- get_cloze_num(question$text)
    stopifnot(length(question$answer) == num)
    guesses <- split_cloze_guesses(num, guess)
    stopifnot(length(guesses) == num)

    cloze_good <- rep(FALSE, num)
    right_answers <- vector(mode = "list", length = num)

    for (i in 1:num) {
        guess <- guesses[i]
        answer_raw <- question$answer[[i]]

        # Possibly several right answers, listify them
        answers <- if (is.list(answer_raw)) answer_raw else list(answer_raw)
        ea <- eval_answers(answers, env)
        match <- match_answers(ea, guess, question$dist, question$epsilon)

        if (any(match)) {
            cloze_good[i] <- TRUE
            right_answers[[i]] <- answers[match][[1]]
        } else {
            cloze_good[i] <- FALSE
            right_answers[[i]] <- answers[[1]]
        }
    }

    cloze_total_points <- cloze_field_points(question)

    cloze_points <- cloze_total_points
    cloze_points[!cloze_good] <- 0

    points <- sum(cloze_points) / sum(cloze_total_points) * question$points

    list(type = "cloze",
         points = points,
         cloze.points = cloze_points,
         cloze.total.points = cloze_total_points,
         guesses = guesses,
         right_answers = right_answers)
}
