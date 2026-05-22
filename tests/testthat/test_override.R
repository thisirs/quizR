context("Per-question rendering override and preproc")

test_that("add_override replaces markdown text rendering", {
    q <- Question("hello world", type = "shortanswer", answer = "x")
    q$add_override("text", "markdown", function(text) toupper(text))

    out <- q$to_markdown(opts = list(feedback = FALSE, numbered = FALSE))
    expect_true(grepl("HELLO WORLD", out))
})

test_that("add_override replaces XML question text rendering", {
    q <- Question("hello world", type = "shortanswer", answer = "x")
    q$add_override("text", "xml", function(text) "<b>hello world</b>")

    out <- q$to_XML(opts = list(feedback = FALSE))
    expect_true(grepl("<b>hello world</b>", out, fixed = TRUE))
})

test_that("add_preproc transforms markdown text before use", {
    q <- Question("hello world", type = "shortanswer", answer = "x")
    q$add_preproc("text", "markdown", function(text) paste0("**", text, "**"))

    out <- q$to_markdown(opts = list(feedback = FALSE, numbered = FALSE))
    expect_true(grepl("**hello world**", out, fixed = TRUE))
})

test_that("add_override returns self invisibly for chaining", {
    q <- Question("hello", type = "shortanswer", answer = "x")
    result <- q$add_override("text", "markdown", toupper)
    expect_identical(result, q)
})

test_that("add_preproc returns self invisibly for chaining", {
    q <- Question("hello", type = "shortanswer", answer = "x")
    result <- q$add_preproc("text", "markdown", toupper)
    expect_identical(result, q)
})

test_that("override takes precedence over preproc when both defined", {
    q <- Question("hello", type = "shortanswer", answer = "x")
    q$add_preproc("text", "markdown", function(text) paste0("preproc:", text))
    q$add_override("text", "markdown", function(text) paste0("override:", text))

    out <- q$to_markdown(opts = list(feedback = FALSE, numbered = FALSE))
    expect_true(grepl("override:", out))
    expect_false(grepl("preproc:", out))
})

test_that("unknown kind raises an error", {
    q <- Question("hello", type = "shortanswer", answer = "x")
    expect_error(q$add_override("unknown_kind", "xml", identity), "Unknown kind")
    expect_error(q$add_preproc("text", "pdf", identity), "Unknown kind")
})

test_that("overrides on one question do not affect another", {
    q1 <- Question("hello", type = "shortanswer", answer = "x")
    q2 <- Question("hello", type = "shortanswer", answer = "x")
    q1$add_override("text", "markdown", function(text) "OVERRIDDEN")

    out1 <- q1$to_markdown(opts = list(feedback = FALSE, numbered = FALSE))
    out2 <- q2$to_markdown(opts = list(feedback = FALSE, numbered = FALSE))

    expect_true(grepl("OVERRIDDEN", out1))
    expect_false(grepl("OVERRIDDEN", out2))
})
