test_that("answer_feedback return correct markdown", {
    question <- Question("Q1", type="shortanswer", answer=quote(1))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    expect_identical(answer_feedback(3, question, env), trimws("
```{r include=FALSE}
{
}
```
**Question 3.** Q1

**Réponse:**
```{r include=FALSE}
answer <- {1}
```
```{r}
1
```

La réponse est: $`r answer`$
"))})

test_that("answer_feedback return correct markdown", {
    question <- Question("Q1", type="shortanswer", hidden.data=quote({a <- 1}), answer=quote(1))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    expect_identical(answer_feedback(3, question, env), trimws("
```{r include=FALSE}
a <- 1
```
**Question 3.** Q1

**Réponse:**
```{r include=FALSE}
answer <- {1}
```
```{r}
1
```

La réponse est: $`r answer`$
"))})

test_that("answer_cloze_feedback return correct markdown", {
    question <- Question("blah {1:SA:=}", type="cloze", hidden.data=quote({a <- 1}), answer=quote(1))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    res <- answer_feedback(3, question, env)
    expect_identical(res, "```{r include=FALSE}
a <- 1
```
**Question 3.** blah (1)

**Réponse:**
```{r include=FALSE}
answer <- {1}
```
1. ```{r}
1
```

La réponse est: $`r answer`$
")})

test_that("answer_cloze_feedback return correct markdown", {
    question <- Question("blah {1:SA:=} foo {2:SA:=}", type="cloze", hidden.data=quote({a <- 1}), answer=list(1, 2))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    res <- answer_feedback(3, question, env)
    expect_identical(res, "```{r include=FALSE}
a <- 1
```
**Question 3.** blah (1) foo (2)

**Réponse:**
```{r include=FALSE}
answer <- {1}
```
1. ```{r}
1
```

La réponse est: $`r answer`$
```{r include=FALSE}
answer <- {2}
```
2. ```{r}
2
```

La réponse est: $`r answer`$
")})
