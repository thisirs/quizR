context("Whisker XML rendering")

# ---- helpers ----------------------------------------------------------------

xml_has <- function(xml, pattern) grepl(pattern, xml, fixed = TRUE)

# ---- SimpleQuestion / ShortanswerQuestion -----------------------------------

test_that("to_XML_whisker produces correct structure for shortanswer", {
    q <- Question("What is 2+2?", type = "shortanswer", answer = "4")
    out <- q$to_XML_whisker(opts = list(feedback = FALSE))

    expect_true(xml_has(out, '<question type="shortanswer">'))
    expect_true(xml_has(out, "What is 2+2?"))
    expect_true(xml_has(out, "<text>4</text>"))
    expect_true(xml_has(out, "<hidden>0</hidden>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML_whisker includes feedback when present", {
    q <- Question("Q", type = "shortanswer", answer = "A",
                  feedback = "Great job")
    out <- q$to_XML_whisker(opts = list(feedback = TRUE))

    expect_true(xml_has(out, "<generalfeedback"))
})

test_that("to_XML_whisker suppresses feedback when disabled", {
    q <- Question("Q", type = "shortanswer", answer = "A",
                  feedback = "Great job")
    out <- q$to_XML_whisker(opts = list(feedback = FALSE))

    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML_whisker and to_XML agree on question text and answer", {
    q <- Question("Explain recursion", type = "shortanswer", answer = "base case")
    old <- q$to_XML(opts = list(feedback = FALSE))
    new <- q$to_XML_whisker(opts = list(feedback = FALSE))

    expect_true(xml_has(new, "Explain recursion"))
    expect_true(xml_has(new, "base case"))
    expect_true(xml_has(old, "Explain recursion"))
    expect_true(xml_has(old, "base case"))
})

# ---- NumericalQuestion ------------------------------------------------------

test_that("to_XML_whisker produces correct structure for numerical", {
    q <- Question("What is pi?", type = "numerical",
                  answer = quote(pi), tolerance = 0.01,
                  tolerance_type = "absolute", hidden_seed = 1)
    out <- q$to_XML_whisker(opts = list(feedback = FALSE))

    expect_true(xml_has(out, '<question type="numerical">'))
    expect_true(xml_has(out, "<tolerance>0.010000</tolerance>"))
    expect_true(xml_has(out, "<tolerancetype>absolute</tolerancetype>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML_whisker omits tolerance block when tolerance is NULL", {
    q <- Question("Q", type = "numerical", answer = 42,
                  tolerance = NULL, tolerance_type = NULL, hidden_seed = 1)
    out <- q$to_XML_whisker(opts = list(feedback = FALSE))

    expect_false(xml_has(out, "<tolerance>"))
    expect_false(xml_has(out, "<tolerancetype>"))
})

test_that("to_XML_whisker and to_XML agree on numerical question structure", {
    q <- Question("What is 6*7?", type = "numerical",
                  answer = 42, tolerance = 0.001, hidden_seed = 1)
    old <- q$to_XML(opts = list(feedback = FALSE))
    new <- q$to_XML_whisker(opts = list(feedback = FALSE))

    expect_true(xml_has(new, "What is 6*7?"))
    expect_true(xml_has(new, "<tolerance>"))
    expect_true(xml_has(old, "<tolerance>"))
})

# ---- build_xml_context ------------------------------------------------------

test_that("build_xml_context returns expected keys for shortanswer", {
    q <- Question("Q", type = "shortanswer", answer = "A")
    ctx <- q$build_xml_context(list(feedback = FALSE, export = "xml",
                                    numbered = FALSE, quiet = FALSE), list())
    expect_true(all(c("type", "title", "question_text", "answer") %in% names(ctx)))
    expect_equal(ctx$type, "shortanswer")
})

test_that("build_xml_context returns expected keys for numerical", {
    q <- Question("Q", type = "numerical", answer = 1,
                  tolerance = 0.05, hidden_seed = 1)
    ctx <- q$build_xml_context(list(feedback = FALSE, export = "xml",
                                    numbered = FALSE, quiet = FALSE), list())
    expect_true("tolerance" %in% names(ctx))
    expect_true("tolerance_type" %in% names(ctx))
    expect_equal(ctx$tolerance, "0.050000")
})
