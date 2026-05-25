context("Instantiator")

# ── build_env ─────────────────────────────────────────────────────────────────

test_that("build_env substitutes var_list symbols into the data block", {
    # sym maps to the symbol TARGET; after substitution data becomes
    # { TARGET <- 42 }, so env$TARGET must be 42 (not env$sym)
    env <- quizR:::build_env(
        list(sym = as.symbol("TARGET")),
        data = quote({ sym <- 42 }),
        seed = NULL
    )
    expect_equal(env$TARGET, 42)
})

# ── NumericalQuestion answer evaluation ───────────────────────────────────────

test_that("NumericalQuestion answer referencing a hidden_data variable is evaluated to a number", {
    q <- Question("What is n squared?", type = "numerical",
                  hidden_seed = 1,
                  hidden_data = quote({ n <- 5 }),
                  answer      = quote(n^2),
                  distractors = list(1, 2, 3))
    inode <- instantiate(q)
    expect_equal(inode$answer, 25)
})

test_that("NumericalQuestion answer using symbol-variable pattern evaluates to a number", {
    # Regression for the ds_sym -> DATA021 pattern:
    # hidden_data injects ds_sym = as.symbol("MYDATA") and local_ds;
    # data block assigns ds_sym <- local_ds which, after var_list substitution,
    # becomes MYDATA <- local_ds, defining MYDATA in env.
    # answer = quote(sum(ds_sym^2)) becomes sum(MYDATA^2) after instantiate_object,
    # then is evaluated in env where MYDATA is defined.
    q <- Question("Q", type = "numerical",
                  hidden_data = quote({
                      ds_sym   <- as.symbol("MYDATA")
                      local_ds <- c(1, 2, 3, 4, 5)
                  }),
                  data        = quote({ ds_sym <- local_ds }),
                  answer      = quote(sum(ds_sym^2)),
                  distractors = list(1, 2, 3))
    inode <- instantiate(q)
    expect_equal(inode$answer, 55)  # 1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 55
})

# ── ClozeQuestion data propagation to subquestions ───────────────────────────

test_that("ClozeQuestion hidden_data variables are accessible to subquestion answers", {
    cloze <- ClozeQuestion$new(
        hidden_data = quote({ x <- 7 }),
        questions   = list(
            Question("What is x?", type = "numerical",
                     answer      = quote(x),
                     distractors = list(1, 2, 3))
        )
    )
    inode <- instantiate(cloze)
    expect_equal(inode$subquestions[[1]]$answer, 7)
})

test_that("ClozeQuestion data-block variables are accessible to subquestion answers", {
    # Regression: instantiate_cloze previously passed only hidden_data vars
    # (var_list) to subquestions; data-block vars lived only in the cloze env
    # and were invisible to subquestion instantiation.
    cloze <- ClozeQuestion$new(
        data      = quote({ x <- 7 }),
        questions = list(
            Question("What is x?", type = "numerical",
                     answer      = quote(x),
                     distractors = list(1, 2, 3))
        )
    )
    inode <- instantiate(cloze)
    expect_equal(inode$subquestions[[1]]$answer, 7)
})

test_that("ClozeQuestion data-block variables are accessible in subquestion text", {
    cloze <- ClozeQuestion$new(
        data      = quote({ label <- "hello" }),
        questions = list(
            Question("Value is `r label`", type = "numerical",
                     answer      = quote(1),
                     distractors = list(2, 3, 4))
        )
    )
    inode <- instantiate(cloze)
    expect_match(inode$subquestions[[1]]$text, "hello")
})

# ── collect_data_exprs ────────────────────────────────────────────────────────

test_that("collect_data_exprs collects from children, not just the quiz root", {
    # Regression: generate_data_file previously only wrote the quiz-level
    # data_expr, ignoring all child data_exprs.
    q1 <- Question("q1", type = "numerical",
                   seed        = 1,
                   data        = quote({ x <- 42 }),
                   answer      = quote(x),
                   distractors = list(1, 2, 3))
    q2 <- Question("q2", type = "numerical",
                   seed        = 2,
                   data        = quote({ y <- 99 }),
                   answer      = quote(y),
                   distractors = list(1, 2, 3))
    quiz  <- Quiz$new("test", children = list(q1, q2))
    inode <- instantiate(quiz)
    exprs <- quizR:::collect_data_exprs(inode)
    # quiz root has no seed/data → empty; q1 and q2 each have seed+data
    expect_equal(length(exprs), 2)
})

test_that("collect_data_exprs recurses into cloze subquestions", {
    subq <- Question("sq", type = "numerical",
                     seed        = 3,
                     data        = quote({ z <- 7 }),
                     answer      = quote(z),
                     distractors = list(1, 2, 3))
    cloze <- ClozeQuestion$new(questions = list(subq))
    quiz  <- Quiz$new("test", children = list(cloze))
    inode <- instantiate(quiz)
    exprs <- quizR:::collect_data_exprs(inode)
    expect_true(length(exprs) >= 1)
})

test_that("generate_data_file writes content from child questions", {
    q <- Question("q", type = "numerical",
                  data        = quote({ ans <- 7 }),
                  answer      = quote(ans),
                  distractors = list(1, 2, 3))
    quiz    <- Quiz$new("myquiz", children = list(q))
    outfile <- tempfile(fileext = ".R")
    on.exit(unlink(outfile), add = TRUE)
    quiz$generate_data_file(outfile)
    expect_true(file.exists(outfile))
    content <- paste(readLines(outfile), collapse = "\n")
    expect_match(content, "ans")
    expect_match(content, "7")
})
