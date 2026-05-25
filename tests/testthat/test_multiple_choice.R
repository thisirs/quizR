context("Testing multiple choice questions")

# ── Basic MC properties ───────────────────────────────────────────────────────

test_that("MC question single=TRUE for mc type", {
    q <- Question("Q", type = "mc", items = c("a", "b"), answer = c(TRUE, FALSE))
    expect_true(q$single)
})

test_that("MC question single=FALSE for mr type", {
    q <- Question("Q", type = "mr", items = c("a", "b"), answer = c(TRUE, TRUE))
    expect_false(q$single)
})

test_that("MC items and answer stored correctly", {
    q <- Question("Q", type = "mc",
                  items = c("stat 1", "stat 2"),
                  answer = c(TRUE, FALSE))
    expect_equal(q$items, list("stat 1", "stat 2"))
    expect_equal(q$answer, c(TRUE, FALSE))
})

# ── MC cookie in cloze XML ────────────────────────────────────────────────────

test_that("XMLExporter cloze embeds MC cookie with = prefix on correct item", {
    q1 <- ClozeQuestion$new(questions = list(
                                Question("Q", type = "mc",
                                         items = c("stat 1", "stat 2"),
                                         answer = c(TRUE, FALSE))))
    out <- XMLExporter$new()$render(instantiate(q1), list(feedback = FALSE))

    expect_true(grepl("MC:=stat 1~stat 2", out, fixed = TRUE))
})

test_that("XMLExporter cloze MC cookie marks second item correct", {
    q1 <- ClozeQuestion$new(questions = list(
                                Question("Q", type = "mc",
                                         items = c("stat 1", "stat 2"),
                                         answer = c(FALSE, TRUE))))
    out <- XMLExporter$new()$render(instantiate(q1), list(feedback = FALSE))

    expect_true(grepl("MC:stat 1~=stat 2", out, fixed = TRUE))
})

test_that("XMLExporter cloze MC cookie marks multiple correct items", {
    q1 <- ClozeQuestion$new(questions = list(
                                Question("Q", type = "mc",
                                         items = c("stat 1", "stat 2", "stat 3"),
                                         answer = c(FALSE, TRUE, TRUE))))
    out <- XMLExporter$new()$render(instantiate(q1), list(feedback = FALSE))

    expect_true(grepl("MC:stat 1~=stat 2~=stat 3", out, fixed = TRUE))
})
