context("Reporting features")

test_that("automatic_feedback return correct markdown", {
    question <- Question("Q1", type = "shortanswer", answer = quote(1))
    unrandomize_data(question)
    env <- new.env(parent = .GlobalEnv)
    expect_identical(automatic_feedback(3, question, env), "```{r include=FALSE}
answer <- {1}
```
**Question 3.** Q1

**Réponse:** $`r answer`$

```{r}
1
```

")})

test_that("automatic_feedback return correct markdown", {
    question <- Question("Q1", type = "shortanswer", hidden.data = quote({a <- 1}), answer = quote(1))
    unrandomize_data(question)
    env <- new.env(parent = .GlobalEnv)
    expect_identical(automatic_feedback(3, question, env), "```{r include=FALSE}
a <- 1
```
```{r include=FALSE}
answer <- {1}
```
**Question 3.** Q1

**Réponse:** $`r answer`$

```{r}
1
```

")})

test_that("answer_cloze_feedback return correct markdown", {
    question <- Question("blah {1:SA:=*}", type = "cloze", hidden.data = quote({a <- 1}), answer = quote(1))
    unrandomize_data(question)
    env <- new.env(parent = .GlobalEnv)
    res <- automatic_feedback(3, question, env)
    expect_identical(res, "```{r include=FALSE}
a <- 1
```
**Question 3.** blah (1)

**Réponse:**

```{r include=FALSE}
answer <- {1}
```
1. La réponse est: $`r answer`$
```{r}
1
```

")})

test_that("answer_cloze_feedback return correct markdown", {
    question <- Question("blah {1:SA:=*} foo {2:SA:=*}", type = "cloze", hidden.data = quote({a <- 1}), answer = list(1, 2))
    unrandomize_data(question)
    env <- new.env(parent = .GlobalEnv)
    res <- automatic_feedback(3, question, env)
    expect_identical(res, "```{r include=FALSE}
a <- 1
```
**Question 3.** blah (1) foo (2)

**Réponse:**

```{r include=FALSE}
answer <- {1}
```
1. La réponse est: $`r answer`$
```{r}
1
```

```{r include=FALSE}
answer <- {2}
```
2. La réponse est: $`r answer`$
```{r}
2
```

")})

test_that("general_feedback with alt.answer", {
    question <- Question("Q1", type = "shortanswer", answer = quote(1))
    unrandomize_data(question)
    env <- new.env(parent = .GlobalEnv)
    expect_identical((general_feedback(alt.answer = "blah"))(3, question, env), "```{r include=FALSE}
answer <- {1}
```
**Question 3.** Q1

**Réponse:** $`r answer`$

blah

")})


test_that("answerstr correctly returns a string from a language", {
    expect_identical(answerstr(quote(blah)), "blah")
    expect_identical(answerstr(function() {
        a <- 1
        b <- 2
    }),
    "a <- 1\nb <- 2")
    expect_identical(answerstr(quote({
        a <- 1
        b <- 2
    })),
    "a <- 1\nb <- 2")

    expect_identical(answerstr(2), "2")
    expect_identical(answerstr(1.34), "1.34")
    expect_identical(answerstr("foo"), "foo")

    expect_error(answerstr(new.env()))
})
