context("Testing cloze questions")

test_that("renaming functions are working correctly", {
    q1 <- ClozeQuestion$new("foo `r a` {1:NM:=}", hidden_seed = 1,
                            hidden_data = quote({a <- 1}))
    q1$rename("data_")

    expect_equal(q1$text, "foo `r data_a`\n{1:NM:=}")

    q1 <- ClozeQuestion$new(questions = list(
                                Question("blah",
                                         hidden_seed = 1,
                                         hidden_data = quote({a <- 1}))))
    q1$rename("data_")

    expect_equal(q1$hidden_data, quote({data_data1_a <- 1}))
})

test_that(paste("property", sQuote("type"), "is working correctly"), {
    q1 <- ClozeQuestion$new("blah {1:NM:=}")
    expect_identical(q1$type, "cloze")
})

test_that(paste("property", sQuote("text"), "is working correctly"), {
    q1 <- ClozeQuestion$new("blah {1:NM:=}")
    expect_identical(q1$text, "blah\n{1:NM:=}")

    q1 <- Question(type = "cloze",
                   questions = list(
                       Question("sq1"),
                       Question("sq2", type = "shortanswer")))
    expect_identical(q1$text, "sq1\n{1:NM:=}\n\nsq2\n{1:SA:=}")

    q1 <- ClozeQuestion$new(questions = list(Question("blah"), Question("foo")))
    q1$text <- "blah {1:SA:=}"
    expect_equal(q1$text, "blah\n{1:SA:=}")

    q1 <- ClozeQuestion$new("blah {1:SA:=}")
    q1$subquestions <- list(Question("blah"), Question("foo"))
    expect_equal(q1$text, "blah\n{1:NM:=}\n\nfoo\n{1:NM:=}")
})

test_that(paste("property", sQuote("num"), "is working correctly"), {
    q1 <- ClozeQuestion$new("blah {1:SA:=}")
    expect_equal(q1$num, 1)

    q1 <- ClozeQuestion$new("blah {1:SA:=} {2:SA:=}")
    expect_equal(q1$num, 2)

    q1 <- ClozeQuestion$new(questions = list(Question("blah")))
    expect_equal(q1$num, 1)

    q1 <- ClozeQuestion$new(questions = list(Question("blah"), Question("foo")))
    expect_equal(q1$num, 2)

    q1 <- ClozeQuestion$new(questions = list(Question("blah"), Question("foo")))
    q1$text <- "blah {1:SA:=}"
    expect_equal(q1$num, 1)
    q1$text <- "blah {1:SA:=} foo {1:SA:=}"
    expect_equal(q1$num, 2)

    q1 <- ClozeQuestion$new("blah {1:SA:=}")
    q1$subquestions <- list(Question("blah"), Question("foo"))
    expect_equal(q1$num, 2)
})

test_that(paste("property", sQuote("total_num"), "is working correctly"), {
    q1 <- ClozeQuestion$new("blah1 {1:SA:=} foo1 {1:SA:=}")
    qc <- ClozeQuestion$new(questions = list(q1))
    expect_equal(qc$total_num, 2)

    q1 <- ClozeQuestion$new("blah1 {1:SA:=} foo1 {1:SA:=}")
    q2 <- ClozeQuestion$new("blah2 {1:SA:=} foo2 {1:SA:=}")
    qc <- ClozeQuestion$new(questions = list(q1, q2))
    expect_equal(qc$total_num, 4)

    q1 <- Question("blah", answer = 1)
    q2 <- ClozeQuestion$new("blah2 {1:SA:=} foo2 {1:SA:=}")
    qc <- ClozeQuestion$new(questions = list(q1, q2))
    expect_equal(qc$total_num, 3)
})

test_that(paste("property", sQuote("data"), "is working correctly"), {
    q1 <- ClozeQuestion$new("foo {1:NM:=}", data = quote(1))
    expect_equal(q1$data, quote({1}))

    q1 <- ClozeQuestion$new(data = quote(1), questions = list(
                                                 Question("foo")))
    expect_equal(q1$data, quote({1}))

    q1 <- ClozeQuestion$new(data = quote(1), questions = list(
                                                 Question("foo", data = quote({2}))))
    expect_equal(q1$data, quote({1; 2}))

    q1 <- ClozeQuestion$new(data = quote(a <- 1),
                            questions = list(
                                Question("{1:NM:=}",
                                         type = "cloze",
                                         data = quote(a <- 2))))
    expect_equal(q1$data, quote({a <- 1; a <- 2}))



})

test_that(paste("property", sQuote("hidden_data"), "is working correctly"), {
    q1 <- Question(type = "cloze",
                   questions = list(
                       Question("sq1", hidden_seed = 1,
                                hidden_data = quote(a <- 1))))
    expect_equal(q1$hidden_data, quote({data1_a <- 1}))

    q1 <- Question(type = "cloze", hidden_seed = 1,
                   questions = list(
                       Question("sq1", hidden_data = quote(a <- 1)),
                       Question("sq2", hidden_data = quote(a <- 2))))
    expect_equal(q1$hidden_data, quote({data1_a <- 1; data2_a <- 2}))

    q1 <- Question(type = "cloze", hidden_seed = 1,
                   hidden_data = quote({a <- 1}),
                   questions = list(Question("sq1", hidden_data = quote(a <- 1))))
    expect_equal(q1$hidden_data, quote({a <- 1; data1_a <- 1}))

    q1 <- Question(type = "cloze", hidden_seed = 1,
                   questions = list(
                       Question("sq1", hidden_data = quote(a <- 1))))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$hidden_data, quote({a <- 2; data1_a <- 1}))
})

test_that(paste("property", sQuote("hidden_data_list"), "is working correctly"), {
    q1 <- Question(type = "cloze",
                   hidden_seed = 1,
                   questions = list(
                       Question("sq1", hidden_data = quote(a <- 1))))
    expect_equal(q1$hidden_data_list, list())
    expect_equal(q1$subquestions[[1]]$hidden_data_list, list(data1_a = 1))

    q1 <- Question(type = "cloze",
                   hidden_seed = 1,
                   questions = list(
                       Question("sq1", hidden_data = quote(a <- 1)),
                       Question("sq2", hidden_data = quote(a <- 2))))
    expect_equal(q1$hidden_data_list, list())
    expect_equal(q1$subquestions[[1]]$hidden_data_list, list(data1_a = 1))
    expect_equal(q1$subquestions[[2]]$hidden_data_list, list(data2_a = 2))

    q1 <- Question(type = "cloze",
                   hidden_seed = 1,
                   hidden_data = quote({a <- 0}),
                   questions = list(
                       Question("sq1", hidden_data = quote(a <- 1)),
                       Question("sq2", hidden_data = quote(a <- 2))))
    expect_equal(q1$hidden_data_list, list(a = 0))
    expect_equal(q1$subquestions[[1]]$hidden_data_list, list(data1_a = 1, a = 0))
    expect_equal(q1$subquestions[[2]]$hidden_data_list, list(data2_a = 2, a = 0))

})

test_that(paste("property", sQuote("answer"), "is working correctly"), {
    q1 <- ClozeQuestion$new(questions = list(Question("blah", answer = 1), Question("foo", answer = 2)))
    expect_identical(q1$answer, list(1, 2))

    q1 <- ClozeQuestion$new("foo {1:SA:=} {1:SA:=}", answer = list(1, 2))
    expect_identical(q1$answer, list(1, 2))

    q1 <- ClozeQuestion$new(answer = list(3, 4),
                            questions = list(Question("blah", answer = 1), Question("foo", answer = 2)))
    expect_identical(q1$answer, list(3, 4))

    q1 <- Question(type = "cloze",
                   hidden_seed = 1,
                   questions = list(
                       Question("sq1", hidden_data = quote(a <- 1), answer = quote(a)),
                       Question("sq2", hidden_data = quote(a <- 2), answer = quote(a))))
    expect_identical(q1$answer, list(quote(data1_a), quote(data2_a)))
})

test_that(paste("property", sQuote("feedback"), "is working correctly"), {
    fb <- function(...) {
        args <- list(...)
        list(header = NULL,
             feedbacks = lapply(args, function(arg) {
                 list(text = arg, noeval_text = NULL, eval = TRUE, indent = 2)
             }))
    }

    fbh <- function(...) {
        args <- list(...)
        list(header = args[[1]],
             feedbacks = lapply(args[-1], function(arg) {
                 list(text = arg, noeval_text = NULL, eval = TRUE, indent = 2)
             }))
    }

    q1 <- ClozeQuestion$new(questions = list(Question("blah", feedback = 1)))
    expect_equal(q1$feedback, fb(1))
    q1$feedback <- list(2)
    expect_equal(q1$feedback, fb(2))
    q1$feedback <- list(header = "foo", feedbacks = list(2))
    expect_equal(q1$feedback, fbh("foo", 2))

    q1 <- ClozeQuestion$new("foo {1:SA:=} {1:SA:=}", feedback = list(1, 2))
    expect_identical(q1$feedback, fb(1, 2))
    q1$feedback <- list(3, 4)
    expect_equal(q1$feedback, fb(3, 4))
    q1$feedback <- list(header = "foo", feedbacks = list(5, 6))
    expect_equal(q1$feedback, fbh("foo", 5, 6))

    q1 <- ClozeQuestion$new(feedback = list(3, 4),
                            questions = list(Question("blah", feedback = 1), Question("foo", feedback = 2)))
    expect_identical(q1$feedback, fb(3, 4))
    q1$feedback <- list(5, 6)
    expect_equal(q1$feedback, fb(5, 6))

    q1 <- ClozeQuestion$new(feedback = list(header = "foo", feedbacks = list(3, 4)),
                            questions = list(Question("blah", feedback = 1), Question("foo", feedback = 2)))
    expect_identical(q1$feedback, fbh("foo", 3, 4))
    q1$feedback <- list(3, 4)
    expect_equal(q1$feedback, fb(3, 4))

    q1 <- ClozeQuestion$new(feedback = list(header = "foo"),
                            questions = list(Question("blah", feedback = 1), Question("foo", feedback = 2)))
    expect_identical(q1$feedback, fbh("foo", 1, 2))

    q1 <- ClozeQuestion$new("blah {1:NM:=} foo {1:NM:=}", feedback = list(header = "foo", feedbacks = list(3, 4)))
    expect_identical(q1$feedback, fbh("foo", 3, 4))

    q1 <- ClozeQuestion$new(questions = list(
                                ClozeQuestion$new(questions = list(
                                                      Question("q1", answer = 1),
                                                      Question("q2", answer = 2))),
                                ClozeQuestion$new(questions = list(
                                                      Question("q3", answer = 3),
                                                      Question("q4", answer = 4)))))
    expect_identical(q1$feedback, fb(1, 2, 3, 4))

    q1 <- ClozeQuestion$new(questions = list(
                                ClozeQuestion$new("blah1 {1:NM:=} foo1 {1:NM:=}", feedback = list(1, 2)),
                                ClozeQuestion$new("blah2 {1:NM:=} foo2 {1:NM:=}", feedback = list(3, 4))))
    expect_identical(q1$feedback, fb(1, 2, 3, 4))

    q1 <- ClozeQuestion$new(questions = list(
                                ClozeQuestion$new("blah1 {1:NM:=} foo1 {1:NM:=}", answer = list(1, 2)),
                                ClozeQuestion$new("blah2 {1:NM:=} foo2 {1:NM:=}", answer = list(3, 4))))
    expect_identical(q1$feedback, fb(1, 2, 3, 4))


})

test_that(paste("property", sQuote("instantiated_text"), "is working correctly"), {
    q1 <- ClozeQuestion$new("`r a` {1:NM:=}", hidden_data = quote({a <- 1}), hidden_seed = 1)
    expect_identical(q1$instantiated_text, "`r 1`\n{1:NM:=}")

    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$instantiated_text, "`r 2`\n{1:NM:=}")

    q1 <- ClozeQuestion$new("```{r}\na\n``` {1:NM:=}", hidden_data = quote({a <- 1}), hidden_seed = 1)
    expect_identical(q1$instantiated_text, "```{r}\n1\n```\n{1:NM:=}")

    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$instantiated_text, "```{r}\n2\n```\n{1:NM:=}")

    q1 <- ClozeQuestion$new(hidden_seed = 1,
                            questions = list(Question("`r a`",
                                                      hidden_data = quote({a <- 1}))))
    expect_identical(q1$text, "`r data1_a`\n{1:NM:=}")
    expect_identical(q1$instantiated_text, "`r 1`\n{1:NM:=}")

    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$text, "`r data1_a`\n{1:NM:=}")
    expect_identical(q1$instantiated_text, "`r 1`\n{1:NM:=}")

    q1 <- ClozeQuestion$new(hidden_seed = 1,
                            questions = list(Question("```{r}\na\n```",
                                                      hidden_data = quote({a <- 1}))))
    expect_identical(q1$text, "```{r}\ndata1_a\n```\n{1:NM:=}")
    expect_identical(q1$instantiated_text, "```{r}\n1\n```\n{1:NM:=}")

    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$instantiated_text, "```{r}\n1\n```\n{1:NM:=}")
})

test_that(paste("property", sQuote("instantiated_answer"), "is working correctly"), {
    q1 <- ClozeQuestion$new("blah {1:SA:=}", hidden_data = quote({a <- 1}), hidden_seed = 1, answer = list("`r a`"))
    expect_identical(q1$instantiated_answer, list("`r 1`"))

    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$instantiated_answer, list("`r 2`"))

    q1 <- ClozeQuestion$new(hidden_seed = 1,
                            questions = list(Question("blah",
                                                      hidden_data = quote({a <- 1}),
                                                      answer = quote({a}))))
    expect_equal(q1$instantiated_answer, list(quote({1})))
})

test_that("clozify cloze questions works", {
    q1 <- ClozeQuestion$new("blah1 {1:SA:=} foo1 {1:SA:=}")
    q2 <- ClozeQuestion$new("blah2 {1:SA:=} foo2 {1:SA:=}")
    qc <- ClozeQuestion$new(questions = list(q1, q2))

    expect_equal(qc$text, "blah1\n{1:SA:=}\n\nfoo1\n{1:SA:=}\n\nblah2\n{1:SA:=}\n\nfoo2\n{1:SA:=}")

    q1 <- ClozeQuestion$new("blah1 {1:SA:=} foo1 {1:SA:=}", answer = list(1, 2))
    q2 <- ClozeQuestion$new("blah2 {1:SA:=} foo2 {1:SA:=}", answer = list(3, 4))
    qc <- ClozeQuestion$new(questions = list(q1, q2))

    expect_equal(qc$answer, list(1, 2, 3, 4))

    q1 <- Question("blah", answer = 1)
    q2 <- ClozeQuestion$new("blah2 {1:SA:=} foo2 {1:SA:=}", answer = list(2, 3))
    qc <- ClozeQuestion$new(questions = list(q1, q2))

    expect_equal(qc$answer, list(1, 2, 3))

    q1 <- ClozeQuestion$new("blah1 {1:SA:=} foo1 {1:SA:=}",
                            hidden_data = quote(a <- 1),
                            answer = list(quote(a), quote(a^2)))
    q2 <- ClozeQuestion$new("blah2 {1:SA:=} foo2 {1:SA:=}",
                            hidden_data = quote(a <- 2),
                            answer = list(quote(a), quote(a^2)))
    qc <- ClozeQuestion$new(hidden_seed = 1,
                            questions = list(q1, q2))

    expect_equal(qc$answer, list(quote(data1_a),
                                 quote(data1_a^2),
                                 quote(data2_a),
                                 quote(data2_a^2)))


    q1 <- ClozeQuestion$new("$`r a`$ ? {1:NM:=*} {1:NM:=*}",
                            hidden_data = quote({a <- 1}),
                            data = quote({foo1 <- a^2}),
                            answer = list(
                                quote({a + foo1}),
                                quote({a + 2*foo1})))

    q2 <- ClozeQuestion$new("$`r a`$ ? {1:NM:=*} {1:NM:=*}",
                            hidden_data = quote({a <- 2}),
                            data = quote({foo2 <- a^2}),
                            answer = list(
                                quote({a + foo2}),
                                quote({a + 2*foo2})))

    qc <- ClozeQuestion$new(hidden_seed = 1,
                            questions = list(q1, q2))
    expect_equal(qc$text, "$`r data1_a`$ ?\n{1:NM:=}\n\n\n{1:NM:=}\n\n$`r data2_a`$ ?\n{1:NM:=}\n\n\n{1:NM:=}")
    expect_equal(qc$instantiated_text, "$`r 1`$ ?\n{1:NM:=}\n\n\n{1:NM:=}\n\n$`r 2`$ ?\n{1:NM:=}\n\n\n{1:NM:=}")

    expect_equal(qc$answer, list(quote({data1_a + foo1}),
                                 quote({data1_a + 2*foo1}),
                                 quote({data2_a + foo2}),
                                 quote({data2_a + 2*foo2})))

    expect_equal(qc$instantiated_answer, list(quote({1 + foo1}),
                                              quote({1 + 2*foo1}),
                                              quote({2 + foo2}),
                                              quote({2 + 2*foo2})))


    fb <- function(...) {
        args <- list(...)
        list(header = NULL,
             feedbacks = lapply(args, function(arg) {
                 list(text = arg, noeval_text = NULL, eval = TRUE, indent = 2)
             }))
    }

})
