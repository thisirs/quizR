test_that("fails is working", {
    expect_true(fails(quote({stop("Error")})))
    expect_false(fails(quote({ a <- 1})))
})
