context("Testing clozify functions")

test_that(paste(sQuote("sample_questions"), " is working correctly"), {
    qs <- list(
        Question("q1", answer = 1),
        Question("q2", answer = 2),
        Question("q3", answer = 3),
        Question("q4", answer = 4),
        Question("q5", answer = 5),
        Question("q6", answer = 6),
        Question("q7", answer = 7))

    q_list <- sample_questions(qs, sample_size = 4)
    expect_equal(length(q_list), 4)
    expect_true(all(sapply(q_list, function(q) q$type) == "cloze"))

    q_list <- sample_questions(qs, sample_size = 4, clozify = FALSE)
    expect_true(all(sapply(q_list, length) == 2))
    expect_equal(length(q_list), 4)
})


test_that(paste(sQuote("versionize_questions"), " is working correctly"), {
    qs <- list(
        Question("q1", answer = 1),
        Question("q2", answer = 2),
        Question("q3", answer = 3),
        Question("q4", answer = 4),
        Question("q5", answer = 5),
        Question("q6", answer = 6),
        Question("q7", answer = 7))

    nver <- c(1:7)
    q_list <- versionize_questions(qs, "SEED", nver)

    expect_equal(sapply(q_list, length), nver)
})

test_that(paste(sQuote("clozify_group"), " is working correctly"), {

    group <-
        Group$new("group",
                  children = list(
                      Question("q1", answer = 1),
                      Question("q2", answer = 2),
                      Question("q3", answer = 3),
                      Question("q4", answer = 4),
                      Question("q5", answer = 5),
                      Question("q6", answer = 6),
                      Question("q7", answer = 7)))

    n <- 3
    N <- 10
    cl_group <- clozify_group(group,
                              sample_size = N,
                              group_sizes = n)

    expect_equal(length(cl_group$children), N)

    expect_true(all(sapply(cl_group$children, function(q) q$type) == "cloze"))
    expect_true(all(sapply(cl_group$children, function(q) q$num) == n))
})


test_that(paste(sQuote("clozify_independant_questions"), " is working correctly"), {

    questions <- list(
        Question("q1", answer = 1),
        Question("q2", answer = 2),
        Question("q3", answer = 3),
        Question("q4", answer = 4),
        Question("q5", answer = 5),
        Question("q6", answer = 6),
        Question("q7", answer = 7))

    N <- 10
    nver <- 10
    k <- 3

    q_list <- clozify_independant_questions(questions, "SEED", N, k, nver)

    expect_equal(length(q_list), N)
    expect_true(all(sapply(q_list, function(q) q$type) == "cloze"))
    expect_true(all(sapply(q_list, function(q) q$num) == k))

    questions <- list(
        Question("q1", type = "SA", hidden_seed = 1, answer = quote(ds_name)),
        Question("q2", type = "SA", hidden_seed = 1, answer = quote(ds_name)),
        Question("q3", type = "SA", hidden_seed = 1, answer = quote(ds_name)),
        Question("q4", type = "SA", hidden_seed = 1, answer = quote(ds_name)),
        Question("q5", type = "SA", hidden_seed = 1, answer = quote(ds_name)),
        Question("q6", type = "SA", hidden_seed = 1, answer = quote(ds_name)),
        Question("q7", type = "SA", hidden_seed = 1, answer = quote(ds_name)))

    q_list <- clozify_independant_questions(questions, "SEED", N, k, nver)

    expect_true(all(sapply(q_list, function(q) grepl("SEED[0-9]{3}", q$instantiated_answer))))
})


test_that(paste(sQuote("versionize_group"), " is working correctly"), {
    seed <- "TEST"
    N <- 10
    group <- Group$new("gp1",
                       hidden_data = quote({
                           ds_sym <- rnorm(1)
                       }),
                       children = list(
                           Question("q1", type = "SA"),
                           Question("q2", type = "SA"),
                           Question("q3", type = "SA"),
                           Question("q4", type = "SA"),
                           Question("q5", type = "SA"),
                           Question("q6", type = "SA"),
                           Question("q7", type = "SA")))

    v_group <- versionize_group(group, seed, N)

    expect_true(all(sapply(v_group, function(g) grepl("v[0-9]{3}$", g$title))))

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


