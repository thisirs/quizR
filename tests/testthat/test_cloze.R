library(stringi)

get_record <- function(...) {
    record <- c(rep(NA, 10), list(...))
}

context("Cloze questions")

test_that("split_cloze_guesses splits answer correctly", {
    expect_identical(split_cloze_guesses(3, "partie 1 : blah; partie 2 : foo; partie 3 : bar"),
                     c("blah", "foo", "bar"))

    expect_identical(split_cloze_guesses(3, "partie 1 : blah; partie 2 : foo; partie 3 : bar"),
                     c("blah", "foo", "bar"))

    expect_identical(split_cloze_guesses(3, "partie 1 : ; partie 2 : ; partie 3 :"),
                     c("", "", ""))

    expect_identical(split_cloze_guesses(3, "partie 1 : blah; boo; partie 2 : ; partie 3 :"),
                     c("blah; boo", "", ""))
})

test_that("cloze_field_points properly extracts the points", {
    q1 <- Question("{2:SA:=*}", type = "cloze", answer = list(quote(a)))
    expect_identical(cloze_field_points(q1), 2)

    q2 <- Question("{2:SA:=*} foo {1:SHORTANSWER:=*}", type = "cloze", answer = list(quote(a)))
    expect_identical(cloze_field_points(q2), c(2, 1))
})

test_that("get_cloze_num retrieves the right number of cloze questions", {
    text <- "{2:SA:=*}"
    expect_identical(get_cloze_num(text), as.integer(1))

    text <- "{2:SA:=*} foo {1:SHORTANSWER:=*}"
    expect_identical(get_cloze_num(text), as.integer(2))
})

test_that("correct_question returns the right structure", {
    q1 <- Question("{2:SA:=*}", type = "cloze", answer = list("blah"))
    res <- correct_question(q1, new.env(), "partie 1 : blah")
    expect_identical(res$points, 2)

    res <- correct_question(q1, new.env(), "partie 1 : blahfoo")
    expect_identical(res$points, 0)

    q1 <- Question("{2:SA:=*}, {1:SA:=*}", type = "cloze", answer = list("blah", "foo"))
    res <- correct_question(q1, new.env(), "partie 1 : blah; partie 2 : foo")
    expect_identical(res$cloze.points, c(2, 1))
    expect_identical(res$points, 3)

    q1 <- Question("{3:SA:=*}, {1:SA:=*}", type = "cloze", answer = list("blah", "foo"))
    res <- correct_question(q1, new.env(), "partie 1 : blah; partie 2 : foobar")
    expect_identical(res$cloze.points, c(3, 0))
    expect_identical(res$points, 3)
})


test_that("compute_results_from_data returns Cloze question record", {
    quiz <- Quiz("quiz1",
                 seed = 1,
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("{1:SA:=*}, {2:SA:=*}, {1:SA:=*}",
                                        type = "cloze", answer = expr("blah", "foo", "bar"))))))

    r <- get_record("partie 1 : blah; partie 2 : foo; partie 3 : bar")
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data, NULL)

    expect_identical(results[[1]]$groups[[1]]$points, 4)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 4)

})

test_that("replace_cloze_fields is correctly replacing", {
    expect_identical(replace_cloze_fields("blah {1:SA:=*}"), "blah (1)")
    expect_identical(replace_cloze_fields("foo {1:SA:=bar}"), "foo (1)")
    expect_identical(replace_cloze_fields("foo {1:SA:=bar} blah {22:SHORTANSWER:=blah} bar"), "foo (1) blah (2) bar")
})

test_that("environment is the same for all cloze fields", {
    quiz <- Quiz("quiz1",
                 seed = 1,
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("{1:SA:=*}, {2:SA:=*}, {1:SA:=*}",
                                        type = "cloze",
                                        answer = expr(
                                            a <- 2,
                                            {
                                                tmp <- 2
                                                a**2 + tmp
                                            },
                                            tmp + 1))))))

    r <- get_record("partie 1 : 2; partie 2 : 6; partie 3 : 3")
    data <- as.data.frame(r)
    results <- compute_results_from_data(quiz, data)

    expect_identical(results[[1]]$groups[[1]]$points, 4)
    expect_identical(results[[1]]$groups[[1]]$questions[[1]]$points, 4)
})

## quiz <- Quiz("quiz1",
##              groups=list(
##                  Group("G1",
##                        type="sequential",
##                        questions=list(
##                            Question("Que vaut $`r b`$  et $`r a`$ {1:SA:=*}, {2:SA:=*}, {1:SA:=*}",
##                                     hidden.data=quote({a <- rnorm(1)}),
##                                     data=quote({ b <- pi}),
##                                     type="cloze", answer=list(quote(a+b), quote("foo"), quote("bar")))))))
## unrandomize_data(quiz)
## generate_correction(quiz, output="foo.pdf")
