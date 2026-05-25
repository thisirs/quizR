context("XML export")

xml <- function(q, ...) XMLExporter$new()$render(instantiate(q), list(...))

# ── NumericalQuestion ─────────────────────────────────────────────────────────

test_that("NumericalQuestion XML has correct type and answer", {
    q <- Question("What is 2+2?", type = "numerical", answer = quote(4))
    result <- xml(q)
    expect_match(result, 'type="numerical"')
    expect_match(result, "<text>4</text>")
})

test_that("NumericalQuestion XML includes tolerance block", {
    q <- NumericalQuestion$new("Q", answer = quote(3.14), tolerance = 0.01)
    result <- xml(q)
    expect_match(result, "<tolerance>")
    expect_match(result, "0.010000")
})

test_that("NumericalQuestion answer from hidden_data is evaluated in XML output", {
    q <- Question("What is n?", type = "numerical",
                  hidden_seed = 1,
                  hidden_data = quote({ n <- 42 }),
                  answer      = quote(n))
    result <- xml(q)
    expect_match(result, "<text>42</text>")
    expect_no_match(result, "quote")
    expect_no_match(result, "[Ll]anguage")
})

test_that("NumericalQuestion answer using symbol-variable pattern is evaluated in XML", {
    q <- Question("Q", type = "numerical",
                  hidden_data = quote({
                      ds_sym   <- as.symbol("MYDATA")
                      local_ds <- c(1, 2, 3)
                  }),
                  data        = quote({ ds_sym <- local_ds }),
                  answer      = quote(sum(ds_sym^2)))
    result <- xml(q)
    expect_match(result, "<text>14</text>")  # 1^2 + 2^2 + 3^2 = 14
})

# ── MultipleChoice ────────────────────────────────────────────────────────────

test_that("MC single-choice XML uses type multichoice and single=true", {
    q <- Question("Pick one", type = "mc",
                  items  = c("right", "wrong"),
                  answer = c(TRUE, FALSE))
    result <- xml(q)
    expect_match(result, 'type="multichoice"')
    expect_match(result, "<single>true</single>")
})

test_that("MC multi-response XML has single=false", {
    q <- Question("Pick all", type = "mr",
                  items  = c("a", "b", "c"),
                  answer = c(TRUE, FALSE, TRUE))
    result <- xml(q)
    expect_match(result, "<single>false</single>")
})

test_that("MC XML correct answer has positive fraction, wrong answers negative", {
    q <- Question("Pick one", type = "mc",
                  items  = c("right", "wrong"),
                  answer = c(TRUE, FALSE))
    result <- xml(q)
    expect_match(result, 'fraction="100.0000"')
    expect_match(result, 'fraction="-100.0000"')
})

test_that("MC XML item text is rendered", {
    q <- Question("Q", type = "mc",
                  items  = c("alpha", "beta"),
                  answer = c(TRUE, FALSE))
    result <- xml(q)
    expect_match(result, "alpha")
    expect_match(result, "beta")
})

test_that("MC XML with randomized items evaluates inline R", {
    q <- Question("What is `r n`?", type = "mc",
                  hidden_seed = 1,
                  hidden_data = quote({ n <- 7 }),
                  items       = c("`r n`", "`r n + 1`"),
                  answer      = c(TRUE, FALSE))
    result <- xml(q)
    expect_match(result, "7")
    expect_match(result, "8")
})

# ── ShortanswerQuestion ───────────────────────────────────────────────────────

test_that("ShortanswerQuestion XML has type shortanswer and correct answer", {
    q <- Question("Spell it", type = "shortanswer", answer = "hello")
    result <- xml(q)
    expect_match(result, 'type="shortanswer"')
    expect_match(result, "hello")
})

# ── Quiz / Group ──────────────────────────────────────────────────────────────

test_that("Quiz XML wraps output in XML declaration and quiz tags", {
    q    <- Question("Q", type = "mc", items = c("a", "b"), answer = c(TRUE, FALSE))
    quiz <- Quiz$new("myquiz", children = list(q))
    result <- XMLExporter$new()$render(instantiate(quiz), list())
    expect_match(result, "<\\?xml")
    expect_match(result, "<quiz>")
    expect_match(result, "</quiz>")
})

test_that("Group produces a category question in Quiz XML", {
    q     <- Question("Q", type = "mc", items = c("a", "b"), answer = c(TRUE, FALSE))
    group <- Group$new("MyGroup", children = list(q))
    quiz  <- Quiz$new("myquiz", children = list(group))
    result <- XMLExporter$new()$render(instantiate(quiz), list())
    expect_match(result, 'type="category"')
    expect_match(result, "MyGroup")
})

test_that("generate_XML writes a file with valid quiz XML", {
    q    <- Question("Q", type = "mc", items = c("x", "y"), answer = c(TRUE, FALSE))
    quiz <- Quiz$new("testquiz", children = list(q))
    outfile <- tempfile("quiz_xml_", fileext = ".xml")
    on.exit(unlink(outfile), add = TRUE)
    quiz$generate_XML(output = outfile)
    expect_true(file.exists(outfile))
    content <- paste(readLines(outfile), collapse = "\n")
    expect_match(content, "<quiz>")
    expect_match(content, 'type="multichoice"')
})
