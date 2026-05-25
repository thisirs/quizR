context("Whisker XML rendering")

# ---- helpers ----------------------------------------------------------------

xml_has <- function(xml, pattern) grepl(pattern, xml, fixed = TRUE)

render_xml <- function(q, ...) XMLExporter$new()$render(instantiate(q), list(...))

# ---- SimpleQuestion / ShortanswerQuestion -----------------------------------

test_that("to_XML produces correct structure for shortanswer", {
    q <- Question("What is 2+2?", type = "shortanswer", answer = "4")
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, '<question type="shortanswer">'))
    expect_true(xml_has(out, "What is 2+2?"))
    expect_true(xml_has(out, "<text>4</text>"))
    expect_true(xml_has(out, "<hidden>0</hidden>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML includes feedback when present", {
    q <- Question("Q", type = "shortanswer", answer = "A",
                  feedback = "Great job")
    out <- render_xml(q, feedback = TRUE)

    expect_true(xml_has(out, "<generalfeedback"))
})

test_that("to_XML suppresses feedback when disabled", {
    q <- Question("Q", type = "shortanswer", answer = "A",
                  feedback = "Great job")
    out <- render_xml(q, feedback = FALSE)

    expect_false(xml_has(out, "<generalfeedback"))
})

# ---- NumericalQuestion ------------------------------------------------------

test_that("to_XML produces correct structure for numerical", {
    q <- Question("What is pi?", type = "numerical",
                  answer = quote(pi), tolerance = 0.01,
                  tolerance_type = "absolute", hidden_seed = 1)
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, '<question type="numerical">'))
    expect_true(xml_has(out, "<tolerance>0.010000</tolerance>"))
    expect_true(xml_has(out, "<tolerancetype>absolute</tolerancetype>"))
    expect_false(xml_has(out, "<generalfeedback"))
})

test_that("to_XML omits tolerance block when tolerance is NULL", {
    q <- Question("Q", type = "numerical", answer = 42,
                  tolerance = NULL, tolerance_type = NULL, hidden_seed = 1)
    out <- render_xml(q, feedback = FALSE)

    expect_false(xml_has(out, "<tolerance>"))
    expect_false(xml_has(out, "<tolerancetype>"))
})

# ---- MultipleChoice ---------------------------------------------------------

test_that("to_XML produces correct structure for mc question", {
    q <- Question("Pick one", type = "mc",
                  items = c("alpha", "beta", "gamma"),
                  answer = c(TRUE, FALSE, FALSE))
    out <- render_xml(q, feedback = FALSE)

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
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, 'fraction="100.0000"'))
    expect_true(xml_has(out, 'fraction="-100.0000"'))
})

test_that("to_XML correct fractions for multi-response mr", {
    q <- Question("Q", type = "mr",
                  items = c("a", "b", "c"),
                  answer = c(TRUE, FALSE, TRUE))
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, "<single>false</single>"))
    expect_true(xml_has(out, 'fraction="50.0000"'))
})

test_that("to_XML emits all answer items for mc", {
    q <- Question("Q", type = "mc",
                  items = c("yes", "no1", "no2"),
                  answer = c(TRUE, FALSE, FALSE))
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, "yes"))
    expect_true(xml_has(out, "no1"))
    expect_true(xml_has(out, "no2"))
})

test_that("to_XML includes feedback block for mc when enabled", {
    q <- Question("Q", type = "mc",
                  items = c("a", "b"),
                  answer = c(TRUE, FALSE),
                  feedback = "Correct!")
    out <- render_xml(q, feedback = TRUE)

    expect_true(xml_has(out, "<generalfeedback"))
})

# ---- XML context fields via rendered output ---------------------------------

test_that("shortanswer XML contains type, title, question text, and answer", {
    q <- Question("Q", type = "shortanswer", answer = "A")
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, 'type="shortanswer"'))
    expect_true(xml_has(out, "<text>A</text>"))
})

test_that("numerical XML contains tolerance fields", {
    q <- Question("Q", type = "numerical", answer = 1,
                  tolerance = 0.05, hidden_seed = 1)
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, "<tolerance>0.050000</tolerance>"))
    expect_true(xml_has(out, "<tolerancetype>"))
})

test_that("mc XML contains answers list and single flag", {
    q <- Question("Q", type = "mc",
                  items = c("a", "b"),
                  answer = c(TRUE, FALSE))
    out <- render_xml(q, feedback = FALSE)

    expect_true(xml_has(out, 'fraction="100.0000"'))
    expect_true(xml_has(out, 'fraction="-100.0000"'))
    expect_true(xml_has(out, "<single>true</single>"))
})
