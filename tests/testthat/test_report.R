test_that("answerstr correctly return as string from language", {
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
