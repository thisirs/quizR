library(stringi)

getRecord <- function(...) {
    record <- c(rep(NA, 10), list(...))
}

context("Cloze questions")

test_that("split_cloze_guesses splits answer correctly", {
    expect_identical(split_cloze_guesses(3, "partie 1 : blah; partie 2 : foo; partie 3 : bar"),
                     c("blah", "foo", "bar"))

    expect_identical(split_cloze_guesses(3, "partie 1 : blah; partie 2 : foo; partie 3 : bar0"),
                     c("blah", "foo", "bar0"))
})

test_that("cloze_coefficients properly extracts the coefficients", {
    q1 <- Question("{2:SA:=*}", type="cloze", answer=list(quote(a)))
    expect_identical(cloze_coefficients(q1), 1)

    q2 <- Question("{2:SA:=*} foo {1:SHORTANSWER:=*}", type="cloze", answer=list(quote(a)))
    expect_identical(cloze_coefficients(q2), c(2, 1)/3)
})

test_that("getClozeNum retrieves the right number of cloze questions", {
    q1 <- Question("{2:SA:=*}", type="cloze", answer=list(quote(a)))
    expect_identical(getClozeNum(q1), as.integer(1))

    q2 <- Question("{2:SA:=*} foo {1:SHORTANSWER:=*}", type="cloze", answer=list(quote(a)))
    expect_identical(getClozeNum(q2), as.integer(2))
})

test_that("correct_question returns the right structure", {
    q1 <- Question("{2:SA:=*}", type="cloze", answer=list("blah"))
    res <- correct_question(q1, new.env(), "partie 1 : blah")
    expect_identical(res$points, 1)

    res <- correct_question(q1, new.env(), "partie 1 : blahfoo")
    expect_identical(res$points, 0)

    q1 <- Question("{2:SA:=*}, {1:SA:=*}", type="cloze", answer=list("blah", "foo"))
    res <- correct_question(q1, new.env(), "partie 1 : blah; partie 2 : foo")
    expect_identical(res$cloze.points, c(1, 1))
    expect_identical(res$points, 1)

    q1 <- Question("{3:SA:=*}, {1:SA:=*}", type="cloze", answer=list("blah", "foo"))
    res <- correct_question(q1, new.env(), "partie 1 : blah; partie 2 : foobar")
    expect_identical(res$cloze.points, c(1, 0))
    expect_identical(res$points, 3/4)
})


test_that("computeResultsFromData returns Cloze question record", {
    quiz <- Quiz("quiz1",
                 groups=list(
                     Group("G1",
                           type="sequential",
                           questions=list(
                               Question("{1:SA:=*}, {2:SA:=*}, {1:SA:=*}",
                                        type="cloze", answer=expr("blah", "foo", "bar"))))))

    r <- getRecord("partie 1 : blah; partie 2 : foo; partie 3 : bar")
    data <- as.data.frame(r)
    results <- computeResultsFromData(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$points, 1)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 1)

})
