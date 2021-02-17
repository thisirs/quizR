context("Testing multiple choice questions")

test_that(paste(sQuote("get_inst_cookie"), "is working correctly"), {
    q1 <- Question("q1", type = "mc", items = c("stat 1", "stat 2"), answer = c(T, F))
    expect_equal(q1$get_inst_cookie(list(), list(data = quote({}), env = new.env())), "{1:MC:=stat 1~stat 2}")

    q1 <- Question("q1", type = "mc", items = c("stat 1", "stat 2"), answer = c(F, T))
    expect_equal(q1$get_inst_cookie(list(), list(data = quote({}), env = new.env())), "{1:MC:stat 1~=stat 2}")

    q1 <- Question("q1", type = "mc", items = c("stat 1", "stat 2", "stat 3"), answer = c(F, T, T))
    expect_equal(q1$get_inst_cookie(list(), list(data = quote({}), env = new.env())), "{1:MC:stat 1~=stat 2~=stat 3}")
})
