context("Clozify")

q1 <- Question("Q1", type = "shortanswer", answer = quote(a))
q2 <- Question("Q2", type = "shortanswer", answer = quote(a))
q3 <- Question("Q3", type = "shortanswer", answer = list(quote(a), quote(b)),
               data = quote({a <- 1}))
q4 <- Question("blah `r a` and `r fun(3)`", type = "shortanswer", answer = quote(a),
               hidden.data = quote({ a <- 42; fun <- function(a) a + 1}))

test_that("replace_language_data is working correctly", {
    expect_equal(replace_language_data(quote({ a <- 1}), quote({a = as.name("b")})),
                 quote({ b <- 1}))
    expect_equal(replace_language_data(quote({ a <- 1 }), list()),
                 quote({ a <- 1 }))
    expect_equal(replace_language_data(quote({ a <- 1 }), NULL),
                 quote({ a <- 1 }))
})

test_that("replace_text_data is working correctly", {
    text <- "blah foo"
    lang <- quote({blah = as.name("foo")})
    expect_equal(replace_text_data(text, lang), "blah foo")
    expect_equal(replace_text_data(text, NULL), "blah foo")
    expect_equal(replace_text_data(text, list()), "blah foo")

    text <- "`r blah`"
    data <- quote({blah = as.name("foo")})
    expect_equal(replace_text_data(text, data), "`r foo`")

    text <- "`r blah <- 1`"
    data <- quote({blah = as.name("foo")})
    expect_equal(replace_text_data(text, data), "`r foo <- 1`")

    text <- "blah `r func(args)` foo"
    data <- quote({func = as.name("myfunc"); args = as.name("myargs")})
    expect_equal(replace_text_data(text, data), "blah `r myfunc(myargs)` foo")
})

## test_that("aliases_from_question is working correctly", {
##     expect_equal(aliases_from_question(q1), list())

##     sym_a <- as.name(sprintf("a%s", q1$id))
##     sym_fun <- as.name(sprintf("fun%s", q1$id))
##     blah <- list(a = sym_a, fun = sym_fun)
##     expect_identical(aliases_from_question(q4), blah)
## })



test_that("clozify works with one question", {

    merge <- clozify(q4)
    ## expect_equal(merge$text, "blah `r data3d352a` and `r data7d3d352fun(3)` {1:SA:=*}")
    expect_equal(merge$points, 1)
})

test_that("Merge 2 shortanswer questions", {
    merge <- clozify(q1, q2)
    expect_equal(merge$get_text(), "Q1 {1:SA:=*}\n\nQ2 {1:SA:=*}")
    expect_equal(merge$get_answer(), list(quote(a), quote(a)))

    merge <- clozify(q1, q3)
    expect_equal(merge$get_text(), "Q1 {1:SA:=*}\n\nQ3 {1:SA:=*}")
    expect_equal(merge$get_answer(), list(quote(a), list(quote(a), quote(b))))
    expect_equal(merge$get_data(), quote({a <- 1}))

})

## test_that("Merge text with hidden data", {
##     q1 <- Question("Q1 `r lambda`", type = "shortanswer", answer = quote(a),
##                    hidden.data = quote(lambda <- 3))

##     merge <- clozify(q1)
##     expect_equal(q1$text, "Q1 `r data111lambda` {1:SA:=*}")


##     ## q2 <- Question("Q2", type = "shortanswer", answer = quote(a),
##     ##                hidden.data = quote(lambda <- 4))

##     ## merge <- clozify(q1, q2)
##     ## expect_equal(merge$get_hdata()),
## })
