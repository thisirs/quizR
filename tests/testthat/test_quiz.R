context("Quiz")

test_that("groups are correctly added", {
    quiz <- Quiz("test")
    expect_identical(quiz$groups, list())

    g1 <- Group("group1", type="sequential")
    g2 <- Group("group2", type="sequential")
    quiz <- add_group(quiz, g1)
    quiz <- add_group(quiz, g2)
    expect_identical(quiz$groups, list(g1, g2))

    quiz <- Quiz("test", groups=list(g1, g2))
    expect_identical(quiz$groups, list(g1, g2))
})

test_that("identifiers are unique", {
    gen_quiz <- function(id1, id2, id3) {
        Quiz("test",
             groups=list(
                 Group("g1", type="sequential",
                       questions=list(
                           Question(id1),
                           Question(id2))),
                 Group("g2", type="sequential",
                       questions=list(
                           Question(id3)))))
    }
    expect_identical(uniqueIDs(gen_quiz("a", "b", "c")), TRUE)
    expect_identical(uniqueIDs(gen_quiz("a", "a", "b")), FALSE)
    expect_identical(uniqueIDs(gen_quiz("a", "b", "b")), FALSE)
})

context("Answers")


test_that("replace_answers is correctly working", {
    answers <- list(quote(a), quote(b), "blah", 42)
    data <- quote({ a <- 1; b <- 2})
    expect_identical(replace_answers(answers, data), list(1, 2, "blah", 42))
})


test_that("eval_answers is properly evaluating", {
    expect_identical(eval_answers(list("a"), new.env()), list("a"))

    expect_identical(eval_answers(list(42), new.env()), list(42))

    expect_identical(eval_answers(list(quote({42})), new.env()), list(42))
    expect_identical(eval_answers(list(quote({a <- 1; b <- 3; a + b})), new.env()), list(4))

    expect_identical(eval_answers(list(function() 42), new.env()), list(42))

    expect_identical(eval_answers(list(function() 42, "a"), new.env()), list(42, "a"))

    env <- new.env(parent=baseenv())
    env$a <- 2
    expect_identical(eval_answers(list(function() a), env), list(2))
})
