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




test_that(paste(sQuote("hidden_data"), "works correctly"), {
    q1 <- Question("blah", hidden_data = quote({1}))
    expect_equal(q1$hidden_data, quote({1}))
})


test_that(paste(sQuote("hidden_data_list"), "works correctly"), {
    q1 <- Question("blah", hidden_seed = 1, hidden_data = quote({a <- 1}))
    expect_equal(q1$hidden_data_list, list(a = 1))
    q1$hidden_data <- quote({a <- 2})
    expect_equal(q1$hidden_data_list, list(a = 2))
})
