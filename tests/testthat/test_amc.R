context("AMC export")

amc <- function(q, ...) AMCExporter$new()$render(instantiate(q), list(...))

# ── MultipleChoice ────────────────────────────────────────────────────────────

test_that("MultipleChoice to_amc uses \\begin{question} for single-choice format", {
    q <- Question("Choose one", type = "mc",
                  items = c("alpha", "beta", "gamma"),
                  answer = c(FALSE, TRUE, FALSE))

    result <- amc(q)

    expect_match(result, "\\\\begin\\{question\\}")
    expect_match(result, "\\\\end\\{question\\}")
    expect_no_match(result, "questionmult")
})

test_that("MultipleChoice to_amc uses \\begin{questionmult} for multi-response format", {
    q <- Question("Select all that apply", type = "mr",
                  items = c("a", "b", "c"),
                  answer = c(TRUE, FALSE, TRUE))

    result <- amc(q)

    expect_match(result, "\\\\begin\\{questionmult\\}")
    expect_match(result, "\\\\end\\{questionmult\\}")
})

test_that("MultipleChoice to_amc emits \\correctchoice and \\wrongchoice", {
    q <- Question("Pick correct", type = "mc",
                  items = c("right", "wrong1", "wrong2"),
                  answer = c(TRUE, FALSE, FALSE))

    result <- amc(q)

    expect_match(result, "\\\\correctchoice\\{right\\}")
    expect_match(result, "\\\\wrongchoice\\{wrong1\\}")
    expect_match(result, "\\\\wrongchoice\\{wrong2\\}")
})

test_that("MultipleChoice to_amc handles multiple correct answers", {
    q <- Question("Multi correct", type = "mr",
                  items = c("yes1", "no1", "yes2"),
                  answer = c(TRUE, FALSE, TRUE))

    result <- amc(q)

    expect_match(result, "\\\\correctchoice\\{yes1\\}")
    expect_match(result, "\\\\wrongchoice\\{no1\\}")
    expect_match(result, "\\\\correctchoice\\{yes2\\}")
})

test_that("MultipleChoice to_amc sanitizes the question title for the ID", {
    q <- Question("My Question!", type = "mc",
                  items = c("a", "b"),
                  answer = c(TRUE, FALSE))

    result <- amc(q)

    # Spaces and punctuation become hyphens; leading/trailing hyphens stripped
    expect_match(result, "\\{My-Question\\}")
})

test_that("MultipleChoice to_amc includes num in the ID when provided", {
    q <- Question("Simple", type = "mc",
                  items = c("a", "b"),
                  answer = c(TRUE, FALSE))

    result <- amc(q, num = 3)

    expect_match(result, "\\{q3-Simple\\}")
})

test_that("MultipleChoice to_amc works with randomized hidden data", {
    q <- Question("What is `r n`?", type = "mc",
                  hidden_seed = 1,
                  hidden_data = quote({ n <- 7 }),
                  items = c("`r n - 1`", "`r n`", "`r n + 1`"),
                  answer = c(FALSE, TRUE, FALSE))

    result <- amc(q)

    expect_match(result, "\\\\correctchoice\\{7\\}")
    expect_match(result, "\\\\wrongchoice\\{6\\}")
    expect_match(result, "\\\\wrongchoice\\{8\\}")
})

# ── NumericalQuestion ─────────────────────────────────────────────────────────

test_that("NumericalQuestion to_amc errors when distractors are missing", {
    q <- Question("What is 2+2?", type = "numerical", answer = quote(4))

    expect_error(amc(q), "distractors")
})

test_that("NumericalQuestion to_amc generates correct LaTeX structure", {
    q <- Question("What is 1+1?", type = "numerical",
                  answer = quote(2),
                  distractors = c(1, 3, 4))

    result <- amc(q)

    expect_match(result, "\\\\begin\\{question\\}")
    expect_match(result, "\\\\correctchoice\\{\\$2\\$\\}")
    expect_match(result, "\\\\wrongchoice\\{\\$1\\$\\}")
    expect_match(result, "\\\\wrongchoice\\{\\$3\\$\\}")
    expect_match(result, "\\\\wrongchoice\\{\\$4\\$\\}")
})

test_that("NumericalQuestion to_amc errors when a distractor equals the answer", {
    q <- Question("Compute", type = "numerical",
                  answer = quote(5),
                  distractors = c(5, 3, 4))

    expect_error(amc(q), "equal to correct answer")
})

test_that("NumericalQuestion to_amc errors on duplicate distractors", {
    q <- Question("Compute", type = "numerical",
                  answer = quote(5),
                  distractors = c(2, 2, 4))

    expect_error(amc(q), "Duplicate")
})

test_that("NumericalQuestion to_amc works with randomized answer and distractors", {
    q <- Question("What is n?", type = "numerical",
                  hidden_seed = 42,
                  hidden_data = quote({ n <- 10 }),
                  answer = quote(n),
                  distractors = list(quote(n - 1), quote(n + 1), quote(n + 2)))

    result <- amc(q)

    expect_match(result, "\\\\correctchoice\\{\\$10\\$\\}")
    # format() may pad single-digit numbers with a leading space when the
    # vector contains wider values, so allow an optional leading space
    expect_match(result, "\\\\wrongchoice\\{\\$ *9\\$\\}")
    expect_match(result, "\\\\wrongchoice\\{\\$11\\$\\}")
    expect_match(result, "\\\\wrongchoice\\{\\$12\\$\\}")
})

# ── Unsupported types ─────────────────────────────────────────────────────────

test_that("ShortanswerQuestion to_amc raises an error", {
    q <- Question("Short answer?", type = "shortanswer", answer = "foo")
    expect_error(amc(q), "No AMC renderer")
})

# ── Quiz / Group ──────────────────────────────────────────────────────────────

test_that("Quiz render concatenates children AMC output", {
    q1 <- Question("Q1", type = "mc",
                   items = c("a", "b"),
                   answer = c(TRUE, FALSE))
    q2 <- Question("Q2", type = "mc",
                   items = c("c", "d"),
                   answer = c(FALSE, TRUE))

    quiz <- Quiz$new("myquiz", children = list(q1, q2))

    result <- AMCExporter$new()$render(instantiate(quiz), list())

    expect_match(result, "\\\\correctchoice\\{a\\}")
    expect_match(result, "\\\\correctchoice\\{d\\}")
})

test_that("generate_AMC writes a file to disk", {
    q <- Question("Pick one", type = "mc",
                  items = c("x", "y"),
                  answer = c(TRUE, FALSE))
    quiz <- Quiz$new("testquiz", children = list(q))

    outfile <- tempfile("quiz_amc_", fileext = ".tex")
    on.exit(unlink(outfile), add = TRUE)

    quiz$generate_AMC(output = outfile)

    expect_true(file.exists(outfile))
    content <- paste(readLines(outfile), collapse = "\n")
    expect_match(content, "\\\\correctchoice\\{x\\}")
})
