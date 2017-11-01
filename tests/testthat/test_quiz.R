context("Quiz")

test_that("quiz properties are working correctly", {
    quiz <- Quiz$new("test")

    expect_identical(quiz$quiz, quiz)
    expect_identical(quiz$title, "test")
    expect_null(quiz$ancestor)
    expect_identical(quiz$children, list())
})

test_that("group properties are working correctly", {
    quiz <- Quiz$new("quiz")
    group1 <- Group$new("group1")

    expect_identical(group1$title, "group1")
    expect_identical(group1$children, list())
    expect_identical(group1$type, "sequential")
    expect_null(group1$ancestor)
    expect_null(group1$quiz)

    quiz$add_child(group1)

    expect_identical(quiz$children, list(group1))
    expect_identical(group1$quiz, quiz)
    expect_identical(group1$ancestor, quiz)
    expect_identical(group1$children, list())

    group2 <- Group$new("group2")
    quiz$add_child(group2)
    expect_identical(quiz$children, list(group1, group2))
    expect_identical(group2$quiz, quiz)
    expect_identical(group1$children, list())
})

test_that("question properties are working correctly", {
    q1 <- SimpleQuestion$new("q1")
    expect_identical(q1$type, "abstract")
    expect_identical(q1$title, "q1")

    quiz <- Quiz$new("quiz")
    group1 <- Group$new("group1")
    q1 <- SimpleQuestion$new("q1")
    group1$add_child(q1)

    quiz$add_child(group1)
    expect_identical(q1$ancestor, group1)
    expect_identical(q1$quiz, quiz)

    group2 <- Group$new("group2")
    q2 <- SimpleQuestion$new("q2")
    group2$add_child(q2)
    quiz$add_child(group2)
    expect_identical(q2$ancestor, group2)
    expect_identical(q2$quiz, quiz)
})

test_that(paste(sQuote("validate_data"), "is working correctly"), {
    quiz <- Quiz$new("quiz", seed = 1,
                     children = list(Group$new("group1", data = quote({n <- 1})),
                                     Group$new("group2", data = quote({n <- 1}))))
    expect_error(quiz$validate_data(), NA)

    quiz <- Quiz$new("quiz", seed = 1,
                     children = list(Group$new("group1", data = quote({n <- 1})),
                                     Group$new("group2", data = quote({n <- 2}))))
    expect_error(quiz$validate_data())

    quiz <- Quiz$new("quiz", seed = 1,
                     children = list(Group$new("group1", data = quote({m <- 1})),
                                     Group$new("group2", data = quote({n <- 2}))))
    expect_error(quiz$validate_data(), NA)

    quiz <- Quiz$new("quiz", seed = 1, data = quote({m <- 1}),
                     children = list(Group$new("group1", data = quote({m <- 2}))))
    expect_error(quiz$validate_data(), NA)

    quiz <- Quiz$new("quiz", seed = 1, data = quote({a <- 1}),
                     children = list(Group$new("group1", data = quote({a <- 2}),
                                               children = list(
                                                   Question("q1", data = quote({a <- 3}))))))
    expect_error(quiz$validate_data(), NA)

    quiz <- Quiz$new("quiz", seed = 1,
                     children = list(Group$new("group1",
                                               children = list(
                                                   Question("q1", data = quote({a <- 1})))),
                                     Group$new("group2",
                                               children = list(
                                                   Question("q1", data = quote({a <- 2}))))))
    expect_error(quiz$validate_data())
})
