context("Whisker XML rendering")

# ---- helpers ----------------------------------------------------------------

xml_has <- function(xml, pattern) grepl(pattern, xml, fixed = TRUE)

# ---- SimpleQuestion / ShortanswerQuestion -----------------------------------

test_that("to_XML produces correct structure for shortanswer", {
    q <- Question("What is 2+2?", type = "shortanswer", answer = "4")
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_true(xml_has(out, '<question type="shortanswer">'))
    expect_true(xml_has(out, "What is 2+2?"))
    expect_true(xml_has(out, "<text>4</text>"))
    expect_true(xml_has(out, "<hidden>0</hidden>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML includes feedback when present", {
    q <- Question("Q", type = "shortanswer", answer = "A",
                  feedback = "Great job")
    out <- q$to_XML(opts = list(feedback = TRUE))

    expect_true(xml_has(out, "<generalfeedback"))
})

test_that("to_XML suppresses feedback when disabled", {
    q <- Question("Q", type = "shortanswer", answer = "A",
                  feedback = "Great job")
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_false(xml_has(out, "<generalfeedback"))
})

# ---- NumericalQuestion ------------------------------------------------------

test_that("to_XML produces correct structure for numerical", {
    q <- Question("What is pi?", type = "numerical",
                  answer = quote(pi), tolerance = 0.01,
                  tolerance_type = "absolute", hidden_seed = 1)
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_true(xml_has(out, '<question type="numerical">'))
    expect_true(xml_has(out, "<tolerance>0.010000</tolerance>"))
    expect_true(xml_has(out, "<tolerancetype>absolute</tolerancetype>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML omits tolerance block when tolerance is NULL", {
    q <- Question("Q", type = "numerical", answer = 42,
                  tolerance = NULL, tolerance_type = NULL, hidden_seed = 1)
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_false(xml_has(out, "<tolerance>"))
    expect_false(xml_has(out, "<tolerancetype>"))
})

# ---- MultipleChoice ---------------------------------------------------------

test_that("to_XML produces correct structure for mc question", {
    q <- Question("Pick one", type = "mc",
                  items = c("alpha", "beta", "gamma"),
                  answer = c(TRUE, FALSE, FALSE))
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_true(xml_has(out, '<question type="multichoice">'))
    expect_true(xml_has(out, "Pick one"))
    expect_true(xml_has(out, "<single>true</single>"))
    expect_true(xml_has(out, "<hidden>0</hidden>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML correct fractions for single-choice mc", {
    q <- Question("Q", type = "mc",
                  items = c("right", "wrong"),
                  answer = c(TRUE, FALSE))
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_true(xml_has(out, 'fraction="100.0000"'))
    expect_true(xml_has(out, 'fraction="-100.0000"'))
})

test_that("to_XML correct fractions for multi-response mr", {
    q <- Question("Q", type = "mr",
                  items = c("a", "b", "c"),
                  answer = c(TRUE, FALSE, TRUE))
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_true(xml_has(out, "<single>false</single>"))
    expect_true(xml_has(out, 'fraction="50.0000"'))
})

test_that("to_XML emits all answer items for mc", {
    q <- Question("Q", type = "mc",
                  items = c("yes", "no1", "no2"),
                  answer = c(TRUE, FALSE, FALSE))
    out <- q$to_XML(opts = list(feedback = FALSE))

    expect_true(xml_has(out, "yes"))
    expect_true(xml_has(out, "no1"))
    expect_true(xml_has(out, "no2"))
})

test_that("to_XML includes feedback block for mc when enabled", {
    q <- Question("Q", type = "mc",
                  items = c("a", "b"),
                  answer = c(TRUE, FALSE),
                  feedback = "Correct!")
    out <- q$to_XML(opts = list(feedback = TRUE))

    expect_true(xml_has(out, "<generalfeedback"))
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

test_that("build_xml_context returns answers list for mc", {
    q <- Question("Q", type = "mc",
                  items = c("a", "b"),
                  answer = c(TRUE, FALSE))
    ctx <- q$build_xml_context(list(feedback = FALSE, export = "xml",
                                    numbered = FALSE, quiet = FALSE), list())
    expect_true("answers" %in% names(ctx))
    expect_equal(length(ctx$answers), 2)
    expect_true("fraction" %in% names(ctx$answers[[1]]))
    expect_true("text" %in% names(ctx$answers[[1]]))
    expect_equal(ctx$single, "true")
})
