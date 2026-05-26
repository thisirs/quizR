context("Testing clozify functions")

test_that(paste(sQuote("sample_questions"), " is working correctly"), {

    # Only one question
    qs <- sample_questions(Question("q1", answer = 1),
                           sample_size = 3)
    expect_true(length(qs) == 3)
    expect_true(all(sapply(qs, function(q) q$type) != "cloze"))

    qs <- sample_questions(list(Question("q1", answer = 1)),
                           sample_size = 3)
    expect_true(length(qs) == 3)

    qs <- sample_questions(list(Question("q1", answer = 1)),
                           clozify = TRUE,
                           sample_size = 3)
    expect_true(length(qs) == 3)
    expect_true(all(sapply(qs, function(q) q$type) != "cloze"))


    qs <- list(
        Question("q1", answer = 1),
        Question("q2", answer = 2),
        Question("q3", answer = 3),
        Question("q4", answer = 4),
        Question("q5", answer = 5),
        Question("q6", answer = 6),
        Question("q7", answer = 7))

    q_list <- sample_questions(qs, sample_size = 4, group_sizes = 2)
    expect_equal(length(q_list), 4)
    expect_true(all(sapply(q_list, function(q) q$type) == "cloze"))


    q_list <- sample_questions(qs, sample_size = 4,
                               group_sizes = 2,
                               clozify = FALSE)
    expect_true(all(sapply(q_list, length) == 2))
    expect_equal(length(q_list), 4)
})


test_that("Sampler injects unique ds_name per version", {
    q <- Question("q1", type = "SA", hidden_seed = 1, answer = quote(ds_name))
    batch <- 5
    sampler <- Sampler$new(q, batch_size = batch, seed = "SEED")
    versions <- lapply(seq_len(batch), function(i) sampler$sample())
    expect_equal(length(versions), batch)
    ds_names <- sapply(versions, function(v) {
        env <- new.env(parent = baseenv())
        eval(v$hidden_data, envir = env)
        env$ds_name
    })
    expect_equal(length(unique(ds_names)), batch)
    expect_true(all(grepl("^SEED", ds_names)))
})

test_that("sample_questions produces cloze questions from grouped questions", {
    questions <- lapply(1:7, function(i) Question(paste0("q", i), answer = i))
    n <- 3
    N <- 10
    q_list <- sample_questions(questions, sample_size = N, group_sizes = n, group_by = NULL)
    expect_equal(length(q_list), N)
    expect_true(all(sapply(q_list, function(q) q$type) == "cloze"))
    expect_true(all(sapply(q_list, function(q) q$num) == n))
})

test_that("sample_questions with batch_size injects ds_name into sub-questions", {
    questions <- lapply(1:7, function(i)
        Question(paste0("q", i), type = "SA", hidden_seed = 1, answer = quote(ds_name)))
    N <- 10
    k <- 3
    batch_size <- 10
    q_list <- sample_questions(questions, sample_size = N, group_sizes = k,
                               group_by = NULL, batch_size = batch_size, seed = "SEED")
    expect_equal(length(q_list), N)
    expect_true(all(sapply(q_list, function(q) q$type) == "cloze"))
    expect_true(all(sapply(q_list, function(q) q$num) == k))
    expect_true(all(sapply(q_list, function(q)
        all(grepl("SEED[0-9]+", unlist(q$instantiated_answer))))))
})

test_that("sample_questions versions a Group via Sampler", {
    seed <- "TEST"
    N <- 5
    group <- Group$new("gp1",
                       children = list(
                           Question("q1", type = "SA"),
                           Question("q2", type = "SA")))
    v_groups <- sample_questions(list(group), sample_size = N, batch_size = N,
                                 seed = seed, group_by = NULL)
    expect_equal(length(v_groups), N)
    expect_true(all(sapply(v_groups, inherits, "Group")))
    ds_names <- sapply(v_groups, function(g) {
        env <- new.env(parent = baseenv())
        eval(g$hidden_data, envir = env)
        env$ds_name
    })
    expect_equal(length(unique(ds_names)), N)
    expect_true(all(grepl(paste0("^", seed), ds_names)))
})


test_that(paste(sQuote("merge_groups"), " is working correctly"), {
    g1 <- Group$new("g1",
                    hidden_seed = 1,
                    hidden_data = quote({a <- 1}),
                    seed = 2,
                    data = quote({b <- a}),
                    children = list(
                        Question("q1", type = "SA")))

    g2 <- Group$new("g2",
                    hidden_seed = 3,
                    hidden_data = quote({a <- 1}),
                    seed = 4,
                    data = quote({b <- a}),
                    children = list(
                        Question("q2", type = "SA")))

    g <- merge_groups(list(g1, g2), "blah")

    expect_equal(g$hidden_data, quote({
        g1_a <- 1
        g2_a <- 1
    }))

    g1 <- Group$new("g1",
                    header = "foo",
                    hidden_seed = 1,
                    hidden_data = quote({a <- 1}),
                    seed = 2,
                    data = quote({b <- a}),
                    children = list(
                        Question("q1", type = "SA")))

    g2 <- Group$new("g2",
                    header = "bar",
                    hidden_seed = 3,
                    hidden_data = quote({a <- 1}),
                    seed = 4,
                    data = quote({b <- a}),
                    children = list(
                        Question("q2", type = "SA")))

    g <- merge_groups(list(g1, g2), "blah")

    expect_equal(length(g$children), 2)
    expect_equal(g$children[[1]]$text, "foo\n\nq1")
    expect_equal(g$children[[2]]$text, "bar\n\nq2")
})


