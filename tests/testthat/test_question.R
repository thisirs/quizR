context("Question")

test_that("renaming functions are working correctly", {
    q1 <- Question("blah", hidden_seed = 1, hidden_data = quote(a <- 1))
    q1$rename("data_")
    expect_equal(q1$hidden_data, quote(data_a <- 1))

    q1 <- Question("blah `r a`", hidden_seed = 1, hidden_data = quote(a <- 1))
    q1$rename("data_")
    expect_equal(q1$text, "blah `r data_a`")

    q1 <- Question("blah `r a`", hidden_seed = 1, hidden_data = quote(a <- 1),
                   answer = quote(a))
    q1$rename("data_")
    expect_equal(q1$answer, quote(data_a))

    q1 <- Question("blah `r a`", hidden_seed = 1, hidden_data = quote(a <- 1),
                   feedback = quote(a))
    q1$rename("data_")
    expect_equal(q1$feedback$text, quote(data_a))
})

test_that(paste(sQuote("data"), "and", sQuote("instantiated_data"), "work correctly"), {
    q1 <- Question("blah", data = quote({1}))
    expect_equal(q1$data, quote({1}))

    q1 <- Question("foo", hidden_seed = 1, hidden_data = quote({a <- 1}), data = quote({a + b}))
    expect_equal(q1$data, quote({a + b}))
    expect_equal(q1$instantiated_data, quote({1 + b}))
    q1$data <- quote({a - b})
    expect_equal(q1$data, quote({a - b}))
    expect_equal(q1$instantiated_data, quote({1 - b}))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$data, quote({a - b}))
    expect_equal(q1$instantiated_data, quote({2 - b}))
})

test_that(paste(sQuote("text"), "and", sQuote("instantiated_text"), "work correctly"), {
    q1 <- Question("blah")
    expect_identical(q1$text, "blah")

    q1 <- Question("`r a + b`", hidden_seed = 1, hidden_data = quote({a <- 1}))
    expect_identical(q1$text, "`r a + b`")
    expect_identical(q1$instantiated_text, "`r 1 + b`")
    q1$text <- "`r a - b`"
    expect_identical(q1$text, "`r a - b`")
    expect_identical(q1$instantiated_text, "`r 1 - b`")
    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$text, "`r a - b`")
    expect_identical(q1$instantiated_text, "`r 2 - b`")

    q1 <- Question("```{r}\na + b\n```", hidden_seed = 1, hidden_data = quote({a <- 1}))
    expect_identical(q1$text, "```{r}\na + b\n```")
    expect_identical(q1$instantiated_text, "```{r}\n1 + b\n```")
    q1$text <- "```{r}\na - b\n```"
    expect_identical(q1$text, "```{r}\na - b\n```")
    expect_identical(q1$instantiated_text, "```{r}\n1 - b\n```")
    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$text, "```{r}\na - b\n```")
    expect_identical(q1$instantiated_text, "```{r}\n2 - b\n```")
})

test_that(paste(sQuote("answer"), "and", sQuote("instantiated_answer"), "work correctly"), {
    q1 <- Question("blah", answer = quote({1}))
    expect_equal(q1$answer, quote({1}))

    q1 <- Question("foo", hidden_seed = 1, hidden_data = quote({a <- 1}), answer = "`r a + b`")
    expect_identical(q1$answer, "`r a + b`")
    expect_identical(q1$instantiated_answer, "`r 1 + b`")
    q1$answer <- "`r a - b`"
    expect_identical(q1$answer, "`r a - b`")
    expect_identical(q1$instantiated_answer, "`r 1 - b`")
    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$answer, "`r a - b`")
    expect_identical(q1$instantiated_answer, "`r 2 - b`")

    q1 <- Question("foo", hidden_seed = 1, hidden_data = quote({a <- 1}), answer = "```{r}\na + b\n```")
    expect_identical(q1$answer, "```{r}\na + b\n```")
    expect_identical(q1$instantiated_answer, "```{r}\n1 + b\n```")
    q1$answer <- "```{r}\na - b\n```"
    expect_identical(q1$answer, "```{r}\na - b\n```")
    expect_identical(q1$instantiated_answer, "```{r}\n1 - b\n```")
    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$answer, "```{r}\na - b\n```")
    expect_identical(q1$instantiated_answer, "```{r}\n2 - b\n```")

    q1 <- Question("foo", hidden_seed = 1, hidden_data = quote({a <- 1}), answer = quote(a + b))
    expect_identical(q1$answer, quote(a + b))
    expect_identical(q1$instantiated_answer, quote(1 + b))
    q1$answer <- quote(a - b)
    expect_identical(q1$answer, quote(a - b))
    expect_identical(q1$instantiated_answer, quote(1 - b))
    q1$hidden_data <- quote({a <- 2})
    expect_identical(q1$answer, quote(a - b))
    expect_identical(q1$instantiated_answer, quote(2 - b))
})

test_that(paste(sQuote("hidden_data"), "works correctly"), {
    q1 <- Question("blah", hidden_data = quote({1}))
    expect_equal(q1$hidden_data, quote({1}))
})

test_that(paste(sQuote("feedback"), "and", sQuote("instantiated_feedback"), "work correctly"), {
    q1 <- Question("blah", feedback = quote({1}))
    expect_equal(q1$feedback, list(text = quote({1}), noeval_text = NULL, eval = TRUE, indent = 2))

    q1 <- Question("blah", hidden_seed = 1, hidden_data = quote({a <- 1}), feedback = quote({a + b}))
    expect_equal(q1$feedback, list(text = quote({a + b}), noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = quote({1 + b}), eval = TRUE, indent = 2))
    q1$feedback <- quote({a - b})
    expect_equal(q1$feedback, list(text = quote({a - b}), noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = quote({1 - b}), eval = TRUE, indent = 2))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$feedback, list(text = quote({a - b}), noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = quote({2 - b}), eval = TRUE, indent = 2))

    q1 <- Question("blah", hidden_seed = 1, hidden_data = quote({a <- 1}), feedback = "`r a + b`")
    expect_equal(q1$feedback, list(text = "`r a + b`", noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = "`r 1 + b`", eval = TRUE, indent = 2))
    q1$feedback <- "`r a - b`"
    expect_equal(q1$feedback, list(text = "`r a - b`", noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = "`r 1 - b`", eval = TRUE, indent = 2))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$feedback, list(text = "`r a - b`", noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = "`r 2 - b`", eval = TRUE, indent = 2))

    q1 <- Question("blah", hidden_seed = 1, hidden_data = quote({a <- 1}), feedback = "```{r}\na + b\n```")
    expect_equal(q1$feedback, list(text = "```{r}\na + b\n```", noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = "```{r}\n1 + b\n```", eval = TRUE, indent = 2))
    q1$feedback <- "```{r}\na - b\n```"
    expect_equal(q1$feedback, list(text = "```{r}\na - b\n```", noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = "```{r}\n1 - b\n```", eval = TRUE, indent = 2))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$feedback, list(text = "```{r}\na - b\n```", noeval_text = NULL, eval = TRUE, indent = 2))
    expect_equal(q1$instantiated_feedback, list(text = "```{r}\n2 - b\n```", eval = TRUE, indent = 2))
})

test_that(paste(sQuote("hidden_data_list"), "works correctly"), {
    q1 <- Question("blah", hidden_seed = 1, hidden_data = quote({a <- 1}))
    expect_equal(q1$hidden_data_list, list(a = 1))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$hidden_data_list, list(a = 2))
})
