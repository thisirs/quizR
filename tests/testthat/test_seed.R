context("Setting the seed ")

test_that("get_recursive_language is idempotent", {
    quiz <- Quiz("quiz1",
                 seed = 1,
                 hidden.data = quote({a <- rnorm(1)}),
                 data = quote({b <- a}),
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = quote(b))))))

    unrandomize_data(quiz)

    env1 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz), env1)
    res1 <- evalq(b, env1)

    env2 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz), env2)
    res2 <- evalq(b, env2)

    expect_identical(res1, res2)

    env3 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz), env3)
    res3 <- evalq(b, env3)

    expect_identical(res1, res3)
})


test_that("unrandomize_data is idempotent", {
    quiz <- Quiz("quiz1",
                 seed = 1,
                 hidden.seed = 2,
                 hidden.data = quote({a <- rnorm(1)}),
                 data = quote({b <- a}),
                 groups = list(
                     Group("G1",
                           type = "sequential",
                           questions = list(
                               Question("Q1", type = "shortanswer", answer = quote(b))))))

    unrandomize_data(quiz)

    env1 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz), env1)
    res1 <- evalq(b, env1)

    unrandomize_data(quiz)

    env2 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz), env2)
    res2 <- evalq(b, env2)

    unrandomize_data(quiz)

    env3 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz), env3)
    res3 <- evalq(b, env3)

    expect_identical(res1, res2)
    expect_identical(res1, res3)
})

test_that("same seed for hidden data yields same hidden data", {
    common_seed <- 1234

    quiz1 <- Quiz("quiz1",
                  seed = 1,
                  hidden.seed = common_seed,
                  hidden.data = quote({a <- rnorm(1)}),
                  data = quote({b <- a}),
                  groups = list(
                      Group("G1",
                            type = "sequential",
                            questions = list(
                                Question("Q1", type = "shortanswer", answer = quote(b))))))

    quiz2 <- Quiz("quiz2",
                  seed = 2,
                  hidden.seed = common_seed,
                  hidden.data = quote({aa <- rnorm(1)}),
                  data = quote({bb <- aa}),
                  groups = list(
                      Group("G1",
                            type = "sequential",
                            questions = list(
                                Question("Q1", type = "shortanswer", answer = quote(bb))))))

    unrandomize_data(quiz1)
    unrandomize_data(quiz2)

    env1 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz1), env1)
    res1 <- evalq(b, env1)

    env2 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz2), env2)
    res2 <- evalq(bb, env2)

    expect_identical(res1, res2)

})

test_that("different hidden.seed yields different hidden data", {

    quiz1 <- Quiz("quiz1",
                  seed = 1,
                  hidden.seed = 1,
                  hidden.data = quote({a <- rnorm(1)}),
                  data = quote({
                      set.seed(0)
                      b <- a
                  }),
                  groups = list(
                      Group("G1",
                            type = "sequential",
                            questions = list(
                                Question("Q1", type = "shortanswer", answer = quote(b))))))

    unrandomize_data(quiz1)

    runif(1)

    quiz2 <- Quiz("quiz2",
                  seed = 2,
                  hidden.seed = 2,
                  hidden.data = quote({a <- rnorm(1)}),
                  data = quote({
                      set.seed(0)
                      b <- a
                  }),
                  groups = list(
                      Group("G1",
                            type = "sequential",
                            questions = list(
                                Question("Q1", type = "shortanswer", answer = quote(b))))))

    unrandomize_data(quiz2)

    env1 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz1), env1)
    res1 <- evalq(b, env1)

    env2 <- new.env(parent = .GlobalEnv)
    eval(get_recursive_language(quiz2), env2)
    res2 <- evalq(b, env2)

    expect_false(res1 == res2)
})
