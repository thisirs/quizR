context("Comprehensive tests for sample_questions")

# ── helpers ───────────────────────────────────────────────────────────────────
make_qs <- function(n) lapply(seq_len(n), function(i) Question(paste0("q", i), answer = i))

ds_name_of <- function(q) {
    env <- new.env(parent = baseenv())
    eval(q$hidden_data, envir = env)
    env$ds_name
}

ds_questions <- function(n) {
    lapply(seq_len(n), function(i)
        Question(paste0("q", i), type = "SA", answer = quote(ds_name)))
}

# ── 1. Input coercion ─────────────────────────────────────────────────────────

test_that("single SimpleQuestion is coerced to list", {
    q <- Question("q1", answer = 1)
    result <- sample_questions(q, n = 3)
    expect_equal(length(result), 3)
    expect_true(all(sapply(result, function(r) !inherits(r, "list"))))
})

# ── 2. n ──────────────────────────────────────────────────────────────────────

test_that("output length equals n", {
    qs <- make_qs(5)
    expect_equal(length(sample_questions(qs, n = 1)), 1)
    expect_equal(length(sample_questions(qs, n = 5)), 5)
    expect_equal(length(sample_questions(qs, n = 20)), 20)
})

# ── 3. group_by = NULL ────────────────────────────────────────────────────────

test_that("group_by=NULL gives each question its own group", {
    qs <- make_qs(5)
    # n_per_group=1: pick 1 group → 1 question, not a cloze
    result <- sample_questions(qs, n = 10, group_by = NULL, n_per_group = 1)
    expect_equal(length(result), 10)
    expect_true(all(sapply(result, function(q) q$type != "cloze")))
})

test_that("group_by=NULL with n_per_group=k picks k distinct questions per cloze", {
    qs <- make_qs(7)
    k <- 3
    result <- sample_questions(qs, n = 5, group_by = NULL, n_per_group = k)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == k)))
})

# ── 4. group_by = "tags" ──────────────────────────────────────────────────────

test_that("group_by='tags' groups questions with the same tag together", {
    qs <- list(
        Question("q1", answer = 1, tags = "A"),
        Question("q2", answer = 2, tags = "A"),
        Question("q3", answer = 3, tags = "B"),
        Question("q4", answer = 4, tags = "B"))
    # 2 groups A and B; n_per_group=2 picks both groups → cloze with 2
    result <- sample_questions(qs, n = 5, group_by = "tags", n_per_group = 2)
    expect_equal(length(result), 5)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == 2)))
})

test_that("group_by='tags' gives untagged questions random singleton groups", {
    qs <- list(
        Question("q1"),  # no tag → singleton
        Question("q2"),  # no tag → singleton
        Question("q3", tags = "A"))
    # Each question in its own group → n_per_group=3 picks all three
    result <- sample_questions(qs, n = 2, group_by = "tags", n_per_group = 3,
                               max_uses = 2)
    expect_equal(length(result), 2)
    expect_true(all(sapply(result, function(q) q$num == 3)))
})

test_that("group_by='tags' preserves factor level order of first appearance", {
    qs <- list(
        Question("q1", tags = "B"),
        Question("q2", tags = "A"),
        Question("q3", tags = "A"))
    # groups must be in order B, A (first appearance)
    # n_per_group=c(1,1) (vector) refers to groups by index: B→1, A→1
    result <- sample_questions(qs, n = 4, group_by = "tags",
                               n_per_group = c(1, 1))
    expect_equal(length(result), 4)
    expect_true(all(sapply(result, function(q) q$num == 2)))
})

# ── 5. group_by = vector ──────────────────────────────────────────────────────

test_that("group_by vector assigns groups in first-appearance level order", {
    qs <- make_qs(4)
    # c("B","A","A","B") → 2 levels: B (idx 1,4), A (idx 2,3)
    result <- sample_questions(qs, n = 5,
                               group_by = c("B", "A", "A", "B"),
                               n_per_group = c(1, 1))
    expect_equal(length(result), 5)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == 2)))
})

test_that("group_by vector requires length matching questions", {
    qs <- make_qs(4)
    expect_error(sample_questions(qs, group_by = c("A", "B")))
})

# ── 6. n_per_group = scalar 1 (default) ───────────────────────────────────────

test_that("n_per_group=1 returns a single question per iteration (no cloze wrapping)", {
    qs <- make_qs(5)
    result <- sample_questions(qs, n = 10, group_by = NULL, n_per_group = 1)
    expect_true(all(sapply(result, function(q) q$type != "cloze")))
})

# ── 7. n_per_group = scalar k ─────────────────────────────────────────────────

test_that("n_per_group scalar k=1 with tags yields single question from one group", {
    qs <- list(
        Question("q1", tags = "A"), Question("q2", tags = "A"),
        Question("q3", tags = "B"), Question("q4", tags = "B"))
    result <- sample_questions(qs, n = 10, group_by = "tags", n_per_group = 1)
    expect_true(all(sapply(result, function(q) q$type != "cloze")))
})

test_that("n_per_group scalar k=2 picks one question from each of 2 sampled groups", {
    qs <- list(
        Question("q1", tags = "A"), Question("q2", tags = "A"),
        Question("q3", tags = "B"), Question("q4", tags = "B"),
        Question("q5", tags = "C"))
    # 3 groups; n_per_group=2 → sample 2 groups → cloze with 2 questions
    result <- sample_questions(qs, n = 8, group_by = "tags", n_per_group = 2)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == 2)))
})

# ── 8. n_per_group = vector (length > 1) ─────────────────────────────────────

test_that("n_per_group vector picks specified count from each group", {
    qs <- list(
        Question("q1", tags = "A"), Question("q2", tags = "A"), Question("q3", tags = "A"),
        Question("q4", tags = "B"), Question("q5", tags = "B"))
    # 2 groups: A (3 questions), B (2 questions)
    # n_per_group=c(2, 1) → 2 from A, 1 from B → cloze with 3
    result <- sample_questions(qs, n = 5, group_by = "tags",
                               n_per_group = c(2, 1))
    expect_equal(length(result), 5)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == 3)))
})

test_that("n_per_group vector requires length matching number of groups", {
    qs <- list(
        Question("q1", tags = "A"),
        Question("q2", tags = "B"))
    expect_error(sample_questions(qs, group_by = "tags", n_per_group = c(1, 1, 1)))
})

# ── 9. n_per_group = named list ───────────────────────────────────────────────

test_that("n_per_group named list picks count by group name", {
    qs <- list(
        Question("q1", tags = "A"), Question("q2", tags = "A"),
        Question("q3", tags = "B"),
        Question("q4", tags = "C"), Question("q5", tags = "C"))
    result <- sample_questions(qs, n = 4, group_by = "tags",
                               n_per_group = list(A = 1, B = 1, C = 2))
    expect_equal(length(result), 4)
    expect_true(all(sapply(result, function(q) q$num == 4)))
})

test_that("n_per_group named list requires names matching group levels", {
    qs <- list(Question("q1", tags = "A"), Question("q2", tags = "B"))
    expect_error(
        sample_questions(qs, group_by = "tags", n_per_group = list(A = 1, X = 1)))
})

# ── 10. max_uses = Inf ────────────────────────────────────────────────────────

test_that("max_uses=Inf allows sampling beyond question count", {
    q <- Question("q1", answer = 1)
    result <- sample_questions(q, n = 100, max_uses = Inf)
    expect_equal(length(result), 100)
})

# ── 11. max_uses scalar (finite) ──────────────────────────────────────────────

test_that("finite max_uses limits number of uses per question", {
    q <- Question("q1", type = "SA", answer = quote(ds_name))
    batch <- 3
    result <- sample_questions(q, n = batch, max_uses = batch, seed = "S")
    expect_equal(length(result), batch)
    ds_names <- sapply(result, ds_name_of)
    expect_equal(length(unique(ds_names)), batch)
})

test_that("exceeding max_uses raises an error", {
    q <- Question("q1", answer = 1)
    expect_error(sample_questions(q, n = 4, max_uses = 3))
})

test_that("max_uses exhaustion is tracked per question", {
    # n_per_group=c(1,1): always pick 1 from each of 2 groups → both exhausted together
    qs <- list(
        Question("q1", tags = "A", answer = 1),
        Question("q2", tags = "B", answer = 2))
    result <- sample_questions(qs, n = 2, group_by = "tags",
                               n_per_group = c(1, 1), max_uses = 2)
    expect_equal(length(result), 2)
    expect_error(
        sample_questions(qs, n = 3, group_by = "tags",
                         n_per_group = c(1, 1), max_uses = 2))
})

# ── 12. max_uses = per-question vector ────────────────────────────────────────

test_that("per-question max_uses vector applies different limits per question", {
    qs <- list(
        Question("q1", tags = "A", answer = 1),
        Question("q2", tags = "B", answer = 2))
    # Both groups always picked (n_per_group=c(1,1)); limits c(2,2)
    result <- sample_questions(qs, n = 2, group_by = "tags",
                               n_per_group = c(1, 1), max_uses = c(2, 2))
    expect_equal(length(result), 2)
    expect_error(
        sample_questions(qs, n = 3, group_by = "tags",
                         n_per_group = c(1, 1), max_uses = c(2, 2)))
})

test_that("per-question max_uses vector requires length matching questions", {
    qs <- make_qs(3)
    expect_error(sample_questions(qs, max_uses = c(1, 2)))
})

# ── 13. seed = single string ──────────────────────────────────────────────────

test_that("single seed generates unique ds_names for each sampled version", {
    q <- Question("q1", type = "SA", answer = quote(ds_name))
    batch <- 5
    result <- sample_questions(q, n = batch, max_uses = batch, seed = "DATA")
    ds_names <- sapply(result, ds_name_of)
    expect_equal(length(unique(ds_names)), batch)
    expect_true(all(grepl("^DATA", ds_names)))
})

test_that("single seed generates distinct seeds across multiple questions", {
    qs <- ds_questions(3)
    # n_per_group=3 → always sample all 3; max_uses=1 → exactly one cloze
    result <- sample_questions(qs, n = 1, group_by = NULL, n_per_group = 3,
                               max_uses = 1, seed = "S")
    answers <- unlist(result[[1]]$instantiated_answer)
    expect_equal(length(unique(answers)), 3)
})

# ── 14. seed = per-group vector ───────────────────────────────────────────────

test_that("per-group seed vector prefixes ds_names by group", {
    qs <- list(
        Question("q1", type = "SA", tags = "A", answer = quote(ds_name)),
        Question("q2", type = "SA", tags = "A", answer = quote(ds_name)),
        Question("q3", type = "SA", tags = "B", answer = quote(ds_name)))
    # n_per_group=c(1,1): always sample 1 from A and 1 from B
    result <- sample_questions(qs, n = 4, group_by = "tags",
                               n_per_group = c(1, 1), max_uses = 10,
                               seed = c("GRPA", "GRPB"))
    all_answers <- unlist(lapply(result, function(q) q$instantiated_answer))
    expect_true(all(grepl("^GRPA|^GRPB", all_answers)))
    expect_true(any(grepl("^GRPA", all_answers)))
    expect_true(any(grepl("^GRPB", all_answers)))
})

# ── 15. seed = per-question vector ────────────────────────────────────────────

test_that("per-question seed vector maps seeds directly (n_questions > n_groups)", {
    # 4 questions in 2 groups: seed length=4 != n_groups=2 → per-question branch
    qs <- list(
        Question("q1", type = "SA", tags = "A", answer = quote(ds_name)),
        Question("q2", type = "SA", tags = "A", answer = quote(ds_name)),
        Question("q3", type = "SA", tags = "B", answer = quote(ds_name)),
        Question("q4", type = "SA", tags = "B", answer = quote(ds_name)))
    seeds <- c("P", "Q", "R", "S")
    # n_per_group=c(2,2): picks all 4 questions → one cloze with 4 sub-questions
    result <- sample_questions(qs, n = 1, group_by = "tags",
                               n_per_group = c(2, 2), max_uses = 1, seed = seeds)
    answers <- unlist(result[[1]]$instantiated_answer)
    expect_true(setequal(answers, paste0(seeds, "1")))
})

# ── 16/17/18. wrap_as_cloze ──────────────────────────────────────────────────

test_that("wrap_as_cloze=TRUE with 1 sampled question returns the question directly", {
    qs <- make_qs(5)
    result <- sample_questions(qs, n = 5, n_per_group = 1, wrap_as_cloze = TRUE)
    expect_true(all(sapply(result, function(q) q$type != "cloze")))
})

test_that("wrap_as_cloze=TRUE with k>1 sampled questions returns a ClozeQuestion", {
    qs <- make_qs(5)
    result <- sample_questions(qs, n = 5, group_by = NULL, n_per_group = 3,
                               wrap_as_cloze = TRUE)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == 3)))
})

test_that("wrap_as_cloze=FALSE returns a list of question lists per iteration", {
    qs <- make_qs(5)
    result <- sample_questions(qs, n = 4, group_by = NULL, n_per_group = 2,
                               wrap_as_cloze = FALSE)
    expect_equal(length(result), 4)
    expect_true(all(sapply(result, is.list)))
    expect_true(all(sapply(result, length) == 2))
})

test_that("wrap_as_cloze=FALSE with n_per_group=1 returns a list of single-question lists", {
    qs <- make_qs(5)
    result <- sample_questions(qs, n = 6, n_per_group = 1, wrap_as_cloze = FALSE)
    expect_true(all(sapply(result, length) == 1))
})

# ── 19. Group versioning ──────────────────────────────────────────────────────

test_that("Group input is versioned: each clone has unique ds_name", {
    group <- Group$new("gp",
                       children = list(
                           Question("q1", type = "SA"),
                           Question("q2", type = "SA")))
    N <- 5
    result <- sample_questions(list(group), n = N, max_uses = N, seed = "DS")
    expect_equal(length(result), N)
    expect_true(all(sapply(result, inherits, "Group")))
    ds_names <- sapply(result, ds_name_of)
    expect_equal(length(unique(ds_names)), N)
    expect_true(all(grepl("^DS", ds_names)))
})

test_that("Group clones retain all children and do not modify the original", {
    group <- Group$new("gp",
                       children = list(
                           Question("q1", type = "SA"),
                           Question("q2", type = "SA"),
                           Question("q3", type = "SA")))
    result <- sample_questions(list(group), n = 3, max_uses = 3, seed = "DS")
    expect_true(all(sapply(result, function(g) length(g$children) == 3)))
    expect_equal(length(group$children), 3)
})

# ── 20. ds_name injection in instantiated answers ─────────────────────────────

test_that("ds_name is injected into sub-question answers inside a ClozeQuestion", {
    qs <- lapply(1:5, function(i)
        Question(paste0("q", i), type = "SA", answer = quote(ds_name)))
    result <- sample_questions(qs, n = 5, group_by = NULL, n_per_group = 3,
                               max_uses = 5, seed = "SEED")
    all_answers <- unlist(lapply(result, function(q) q$instantiated_answer))
    expect_true(all(grepl("^SEED", all_answers)))
    expect_equal(length(unique(all_answers)), length(all_answers))
})

# ── 21. Group mode ────────────────────────────────────────────────────────────

test_that("Group mode: output length equals n", {
    group <- Group$new("g",
                       children = list(
                           Question("q1", answer = 1),
                           Question("q2", answer = 2),
                           Question("q3", answer = 3)))
    result <- sample_questions(group, n = 5, group_by = NULL, n_per_group = 2)
    expect_equal(length(result), 5)
})

test_that("Group mode: each output is a ClozeQuestion with correct number of sub-questions", {
    group <- Group$new("g",
                       children = lapply(1:6, function(i) Question(paste0("q", i), answer = i)))
    result <- sample_questions(group, n = 8, group_by = NULL, n_per_group = 3)
    expect_true(all(sapply(result, function(q) q$type == "cloze")))
    expect_true(all(sapply(result, function(q) q$num == 3)))
})

test_that("Group mode: each cloze carries a unique ds_name in cloze_hidden_data", {
    group <- Group$new("g",
                       children = lapply(1:4, function(i) Question(paste0("q", i), answer = i)))
    N <- 6
    result <- sample_questions(group, n = N, group_by = NULL,
                               n_per_group = 2, seed = "RL")
    ds_names <- sapply(result, function(q) {
        env <- new.env(parent = baseenv())
        eval(q$cloze_hidden_data, envir = env)
        env$ds_name
    })
    expect_equal(length(unique(ds_names)), N)
    expect_true(all(grepl("^RL", ds_names)))
})

test_that("Group mode: all sub-questions in a cloze share the same ds_name", {
    group <- Group$new("g",
                       children = lapply(1:4, function(i)
                           Question(paste0("q", i), type = "SA", answer = quote(ds_name))))
    result <- sample_questions(group, n = 3, group_by = NULL,
                               n_per_group = 2, seed = "DS")
    for (cloze in result) {
        answers <- unlist(cloze$instantiated_answer)
        expect_equal(length(unique(answers)), 1)
    }
})

test_that("Group mode: ds_names differ across cloze questions", {
    group <- Group$new("g",
                       children = lapply(1:4, function(i)
                           Question(paste0("q", i), type = "SA", answer = quote(ds_name))))
    N <- 4
    result <- sample_questions(group, n = N, group_by = NULL,
                               n_per_group = 2, seed = "DS")
    all_ds_names <- sapply(result, function(q) unlist(q$instantiated_answer)[[1]])
    expect_equal(length(unique(all_ds_names)), N)
})

test_that("Group mode: group_by='tags' partitions children correctly", {
    group <- Group$new("g",
                       children = list(
                           Question("q1", tags = "easy"), Question("q2", tags = "easy"),
                           Question("q3", tags = "easy"),
                           Question("q4", tags = "hard"), Question("q5", tags = "hard")))
    result <- sample_questions(group, n = 5, group_by = "tags",
                               n_per_group = c(2, 1))
    expect_true(all(sapply(result, function(q) q$num == 3)))
})

test_that("Group mode: group's hidden_data is included in cloze_hidden_data", {
    group <- Group$new("g",
                       hidden_data = quote({ base_val <- 42 }),
                       children = lapply(1:3, function(i) Question(paste0("q", i), answer = i)))
    result <- sample_questions(group, n = 2, group_by = NULL,
                               n_per_group = 2, seed = "S")
    for (cloze in result) {
        env <- new.env(parent = baseenv())
        eval(cloze$cloze_hidden_data, envir = env)
        expect_equal(env$base_val, 42)
    }
})

test_that("Group mode: group's data is carried onto each cloze", {
    group <- Group$new("g",
                       data = quote({ x <- 7 }),
                       children = lapply(1:3, function(i) Question(paste0("q", i), answer = i)))
    result <- sample_questions(group, n = 2, group_by = NULL,
                               n_per_group = 1, seed = "S")
    for (cloze in result)
        expect_equal(cloze$cloze_data, quote({ x <- 7 }))
})

test_that("Group mode: max_uses limits child usage and errors when exceeded", {
    group <- Group$new("g",
                       children = list(
                           Question("q1", tags = "A"), Question("q2", tags = "B")))
    # n_per_group=c(1,1) always picks both; max_uses=2 allows 2 iterations
    result <- sample_questions(group, n = 2, group_by = "tags",
                               n_per_group = c(1, 1), max_uses = 2)
    expect_equal(length(result), 2)
    expect_error(
        sample_questions(group, n = 3, group_by = "tags",
                         n_per_group = c(1, 1), max_uses = 2))
})

test_that("Group mode: original group is not modified", {
    group <- Group$new("g",
                       children = lapply(1:4, function(i) Question(paste0("q", i), answer = i)))
    original_hd <- group$hidden_data
    sample_questions(group, n = 5, group_by = NULL, n_per_group = 2, seed = "S")
    expect_identical(group$hidden_data, original_hd)
    expect_equal(length(group$children), 4)
})

test_that("Group mode: header is carried onto each cloze question", {
    group <- Group$new("g",
                       header   = "context text",
                       children = lapply(1:4, function(i) Question(paste0("q", i), answer = i)))
    result <- sample_questions(group, n = 3, group_by = NULL, n_per_group = 2, seed = "S")
    expect_true(all(sapply(result, function(q) q$header == "context text")))
})

test_that("Group mode: NULL header produces NULL on cloze", {
    group <- Group$new("g",
                       children = lapply(1:4, function(i) Question(paste0("q", i), answer = i)))
    result <- sample_questions(group, n = 3, group_by = NULL, n_per_group = 2, seed = "S")
    expect_true(all(sapply(result, function(q) is.null(q$header))))
})

test_that("ds_sym is set as symbol of ds_name in hidden_data", {
    q <- Question("q1", type = "SA", answer = quote(ds_name))
    result <- sample_questions(q, n = 1, max_uses = 1, seed = "S")
    env <- new.env(parent = baseenv())
    eval(result[[1]]$hidden_data, envir = env)
    expect_equal(as.character(env$ds_sym), env$ds_name)
})
