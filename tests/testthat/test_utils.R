context("Utils functions")

test_that("fails is working", {
    expect_true(fails(quote({stop("Error")})))
    expect_false(fails(quote({a <- 1})))
})

test_that("merge_languages is working", {
    expect_deparse <- function(a, b) {
        expect_identical(paste0(deparse(a), collapse="\n"), b)
    }
    expect_deparse(merge_languages(), "{\n}")
    expect_deparse(merge_languages(NULL), "{\n}")
    expect_deparse(merge_languages(NULL, NULL), "{\n}")
    expect_deparse(merge_languages(quote({})), "{\n}")
    expect_deparse(merge_languages(quote(a)), "a")
    expect_deparse(merge_languages(quote({a <- 1}), NULL), "{\n    a <- 1\n}")
    expect_deparse(merge_languages(quote({a <- 1}), quote({b <- 1})), "{\n    a <- 1\n    b <- 1\n}")
    expect_deparse(merge_languages(quote(a <- 1), quote(b <- 1)), "{\n    a <- 1\n    b <- 1\n}")
    expect_deparse(merge_languages(quote({a <- 1; b <- 1})), "{\n    a <- 1\n    b <- 1\n}")


})

