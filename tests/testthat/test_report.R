context("Reporting features")

test_that("automatic_feedback return correct markdown", {
    question <- Question("Q1", type="shortanswer", answer=quote(1))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    expect_identical(automatic_feedback(3, question, env), trimws("
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

test_that("automatic_feedback return correct markdown", {
    question <- Question("Q1", type="shortanswer", hidden.data=quote({a <- 1}), answer=quote(1))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    expect_identical(automatic_feedback(3, question, env), trimws("
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
    res <- automatic_feedback(3, question, env)
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
    res <- automatic_feedback(3, question, env)
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

test_that("general_feedback with alt.answer", {
    question <- Question("Q1", type="shortanswer", answer=quote(1))
    unrandomize_data(question)
    env <- new.env(parent=.GlobalEnv)
    expect_identical((general_feedback(alt.answer="blah"))(3, question, env), trimws("
```{r include=FALSE}
{
}
```
**Question 3.** Q1

**Réponse:**
```{r include=FALSE}
answer <- {1}
```
blah

La réponse est: $`r answer`$
"))})


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

quiz <- Quiz("Quiz",
             data=quote({
                 set.seed(0)
             }),
             groups=list(
                 Group("G1", type="sequential",
                       questions=list(
                           q0 <- Question(
                               "Le jeu de données `df` contient des mesures sur une même grandeur à l'aide de 6 capteurs différents: A, B, C, D, E, F. On souhaite remplacer le capteur ayant le plus grand écart inter-quartiles, quel est-il ?",
                               hidden.data=quote({
                                   lvls <- c("A", "B", "C", "D", "E", "F")
                                   c_name <- factor(rep(lvls, 100))
                                   measures <- rnorm(600, mean=c(1.1, 1, 1.3, 0.8, 1, 1.2), sd=c(1, 1.2, 1.3, 0.9, 1.1, 2))
                                   df0 <- data.frame(measures, c_name)
                                   rm(c_name, measures, lvls) # On évite de remplacer lvls, c_name et measures
                               }),
                               data=quote({
                                   ## On définit df en remplacant par .df
                                   df <- df0
                                   ## On permute les levels
                                   ## df$c_name <- sample(levels(df$c_name))[df$c_name]
                                   ## levels(df$c_name) <- lvls

                                   ## On permute les levels
                                   levels(df$c_name) <- levels(df$c_name)[sample(1:nlevels(df$c_name))]
                                   df$c_name <- factor(df$c_name, levels=sort(levels(df$c_name)))
                               }),
                               answer=quote({
                                   b <- boxplot(measures~c_name, data=df, plot=FALSE)
                                   levels(df$c_name)[which.max(diff(b$stats[c(2, 4),]))]
                               }),
                               feedback="
On fait un boxplot de `measures` en fonction de `c_name`.
```{r}
boxplot(measures~c_name, data=df)
```

La réponse est la réponse `r answer`.")))))

## unrandomize_data(quiz)

## generate_correction(quiz, "/home/sylvain/CloudStation/Sylvain/enseignements/programs/quizR/tests/testthat/blah.pdf")
