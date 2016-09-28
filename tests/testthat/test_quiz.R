context("Quiz")

test_that("Add groups to quiz", {
    quiz <- Quiz("test")
    expect_identical(quiz$groups, list())

    g1 <- Group("group1", type="sequential")
    g2 <- Group("group2", type="sequential")
    quiz <- addGroup(quiz, g1)
    quiz <- addGroup(quiz, g2)
    expect_identical(quiz$groups, list(g1, g2))

    quiz <- Quiz("test", groups=list(g1, g2))
    expect_identical(quiz$groups, list(g1, g2))
})

test_that("Check that identifiers are unique", {
    gen_quiz <- function(id1, id2, id3) {
        Quiz("test",
             groups=list(
                 Group("g1", type="sequential",
                       questions=list(
                           Question("q1", id=id1),
                           Question("q2", id=id2))),
                 Group("g2", type="sequential",
                       questions=list(
                           Question("q3", id=id3)))))
    }
    expect_identical(checkId(gen_quiz("a", "b", "c")), TRUE)
    expect_identical(checkId(gen_quiz("a", "a", "b")), FALSE)
    expect_identical(checkId(gen_quiz("a", "b", "b")), FALSE)
})


context("Answers")

test_that("Answers", {
    expect_identical(evalAnswers(list("a"), new.env()), list("a"))

    expect_identical(evalAnswers(list(42), new.env()), list(42))

    expect_identical(evalAnswers(list(quote({42})), new.env()), list(42))
    expect_identical(evalAnswers(list(quote({a <- 1; b <- 3; a + b})), new.env()), list(4))

    expect_identical(evalAnswers(list(function() 42), new.env()), list(42))

    expect_identical(evalAnswers(list(function() 42, "a"), new.env()), list(42, "a"))


    env <- new.env(parent=baseenv())
    env$a <- 2
    expect_identical(evalAnswers(list(function() a), env), list(2))
})
