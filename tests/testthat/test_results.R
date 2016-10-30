get_rand_string <- function(len = 12)
    return(paste0(sample(c(0:9, letters[1:6]), len, replace = TRUE), collapse = ""))

get_bullshit <- function() {
    list(NA, "3.14", "fuck R", 42)[[sample.int(4, 1)]]
}

get_right_answers <- function(quiz, identifier = NULL) {

}


get_record <- function(...) {
    record <- c(rep(NA, 10), list(...))
}

context("Computing results")

test_that("body and result of quiz", {
    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = 42)))))

    r <- get_record("Q_1", 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")


    r <- get_record("Q_1", 43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")

    r <- get_record(42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q1")

    r <- get_record(43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q1")
})

test_that("Checking Results, no identifier, 2 questions", {
    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = "blah"),
                               Question("Q2", type = "shortanswer", answer = 42)))))

    r <- get_record("Q_1", "blah", "Q_2", 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")

    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$body, "Q_2")

    r <- get_record("Q_1", "blahfoo", "Q_2", 43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")

    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 0)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$body, "Q_2")
})

test_that("Checking Results, with identifier", {

    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G0", type = "identifier",
                           questions = list(
                               Question("Q0", type = "shortanswer")
                           )),
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = 42)))))

    r <- get_record("Q_0", "ident", "Q_1", 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)


    r <- get_record("Q_0", "ident", "Q_1", 43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)

    r <- get_record("ident", 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q1")
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)

    r <- get_record("ident", 43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q1")
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)

})

test_that("Checking Results, with identifier, 2 questions", {
    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G0", type = "identifier",
                           questions = list(
                               Question("Q0", type = "shortanswer")
                           )),
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = "blah"),
                               Question("Q2", type = "shortanswer", answer = 42)))))

    r <- get_record("Q_0", "ident", "Q_1", "blah", "Q_2", 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")

    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$body, "Q_2")

    r <- get_record("Q_0", "ident", "Q_1", "blahfoo", "Q_2", 43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$body, "Q_1")

    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 0)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$body, "Q_2")
})

test_that("Multiple answers", {
    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = list(42, 43))))))

    r <- get_record("Q_1", 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)

    r <- get_record("Q_1", 43)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)

    r <- get_record("Q_1", 45)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 0)
})

test_that("Expression in answers", {
    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = quote({42})),
                               Question("Q2", type = "shortanswer", answer = quote({20+22}))))))

    r <- get_record(42, 42)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 1)
})

test_that("Expression in answers + context", {
    quiz <- Quiz("quiz1",
                 data = quote({a <- 1; b <- 2}),
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = quote(a)),
                               Question("Q2", type = "shortanswer", answer = quote(b))))))

    r <- get_record(1, 2)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 1)


    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G1",
                           data = quote({a <- 1; b <- 2}),
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = quote(a)),
                               Question("Q2", type = "shortanswer", answer = quote(b))))))

    r <- get_record(1, 2)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 1)


    quiz <- Quiz("quiz1",
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1",
                                        data = quote({a <- 1}),
                                        type = "shortanswer", answer = quote(a)),
                               Question("Q2",
                                        data = quote({b <- 2}),
                                        type = "shortanswer", answer = quote(b))))))

    r <- get_record(1, 2)
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[2]]$points, 1)
})


## Generate a data.frame of 100 records
generateRandomRecord <- function(quiz, N, with.question = T, right.answers) {
    numQuestions <- get_num(quiz)
    data <- data.frame(matrix(NA, N, 2*numQuestions + 10), stringsAsFactors = FALSE)

    for(i in 1:N) {
        # Random name and family name
        data[i, 1] <- get_rand_string()
        data[i, 2] <- get_rand_string()
        record <- list()
        qno <- 0
        for(group in quiz$groups) {
            switch(group$type,
                   identifier = {
                       identifier <- get_rand_string()
                       record <- list("Ident", identifier)
                   },
                   description = next,
                   default = {
                       ## Get questions asked
                       qs <- if(group$type == "random") {
                                 group$questions[sample(1:length(group$questions), group$num)]
                             } else {
                                 group$questions
                             }

                       for(j in 1:length(qs)) {
                           qno <- qno + 1
                           q <- qs[[j]]
                           env <- new.env(globalenv())
                           assign('identifiant', identifier, env)
                           eval(get_recursive_language(quiz), env)
                           if ((i %% 6) + 1 <= qno) {
                               ans <- evalAnswerInEnv(q$answer, env)
                               if(is.numeric(ans)) ans <- ans + 1e-6
                           } else
                               ans <- get_bullshit()

                           record <- c(record, paste('(Q', q$id, ') ', q$text, sep = ""), ans)
                       }
                   })
        }
        data[i, 11:ncol(data)] <- record
    }
    return(data)
}


test_that("compute_results_from_data works with random data", {
    quiz <- Quiz("quiz1",
                 data = quote({
                     set.seed(1234)
                     a <- runif(1)
                 }),
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           data = quote({b <- runif(1)}),
                           questions = list(
                               Question("Q1",
                                        type = "shortanswer",
                                        data = quote({c <- runif(1)}),
                                        answer = quote({a + b + c}))))))

    tmpfile <- tempfile("data", fileext = ".R")
    generate_files(quiz, quiz.name = NULL, data.name = tmpfile)

    answer <- local({
        source(tmpfile, local = TRUE)
        a + b + c
    })
    r <- get_record("Q1", answer)
    data <- as.data.frame(r)

    results <- compute_results_from_data(quiz, data)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
})

test_that("compute_results_from_data works with random data and hidden data", {
    quiz <- Quiz("quiz1",
                 seed = 1,
                 hidden.data = quote({
                     a <- runif(1)
                 }),
                 data = quote({
                     b <- a
                 }),
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1",
                                        type = "shortanswer",
                                        answer = quote({b}))))))

    unrandomize_data(quiz)

    tmpfile <- tempfile("data", fileext = ".R")
    generate_files(quiz, quiz.name = NULL, data.name = tmpfile)

    # What students do
    answer <- local({
        source(tmpfile, local = TRUE)
        b
    })
    r <- get_record("Q1", answer)
    data <- as.data.frame(r)

    results <- compute_results_from_data(quiz, data)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)
})
