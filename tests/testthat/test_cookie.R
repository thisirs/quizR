context("Testing cookie-related functions")

test_that(paste("cookies in numerical questions are working correctly"), {
    q1 <- Question("q1", answer = 1)
    expect_equal(q1$get_inst_cookie(list(), list()), "{1:NM:=1:0.001}")
    expect_equal(q1$get_inst_text_and_cookie(list(), list()), "q1\n\n{1:NM:=1:0.001}")
})

test_that(paste("cookies in cloze questions are working correctly"), {
    q1 <- ClozeQuestion$new(questions = list(
                            Question("sq1", answer = 1)))
    expect_equal(q1$get_inst_text(list(export = "xml"), list()), "sq1\n\n{1:NM:=1:0.001}")
    expect_equal(q1$get_inst_text(list(export = "markdown"), list()), "sq1 (1)")

    q1 <- ClozeQuestion$new(questions = list(
                                Question("sq1", answer = 1),
                                Question("sq2", answer = 2)))
    expect_equal(q1$get_inst_text(list(export = "xml"), list()), "sq1\n\n{1:NM:=1:0.001}\n\nsq2\n\n{1:NM:=2:0.002}")
    expect_equal(q1$get_inst_text(list(export = "markdown"), list()), "sq1 (1)\n\nsq2 (2)")
})
