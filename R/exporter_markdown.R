# Markdown / PDF correction-sheet exporter.

#' @export
MarkdownExporter <- R6::R6Class(
    "MarkdownExporter",
    public = list(

        render = function(inode, opts = list()) {
            switch(inode$class_name,
                Quiz              = self$render_quiz(inode, opts),
                Group             = self$render_group(inode, opts),
                ClozeQuestion     = self$render_cloze(inode, opts),
                NumericalQuestion = ,
                ShortanswerQuestion = ,
                MultipleChoice    = self$render_leaf(inode, opts),
                stop("No Markdown renderer for class: ", sQuote(inode$class_name))
            )
        },

        # ── Quiz ─────────────────────────────────────────────────────────────

        render_quiz = function(inode, opts) {
            title_block <- private$quiz_title_block(inode, opts)
            data_chunk  <- private$data_chunk(inode)

            child_parts <- lapply(seq_along(inode$children), function(i) {
                child_opts      <- opts
                child_opts$num  <- i
                self$render(inode$children[[i]], child_opts)
            })

            parts <- c(list(title_block),
                       if (!is.null(data_chunk)) list(data_chunk),
                       Filter(Negate(is.null), child_parts))
            paste(parts, collapse = "\n\n")
        },

        # ── Group ─────────────────────────────────────────────────────────────

        render_group = function(inode, opts) {
            header <- if (inode$type == "random")
                sprintf("## %s (aléatoire %d parmi %d)",
                        inode$title, inode$num, length(inode$children))
            else
                sprintf("## %s", inode$title)

            child_parts <- lapply(seq_along(inode$children), function(i) {
                child_opts     <- opts
                child_opts$num <- i
                self$render(inode$children[[i]], child_opts)
            })

            c(list(header), Filter(Negate(is.null), child_parts)) |>
                paste(collapse = "\n\n")
        },

        # ── Leaf questions ────────────────────────────────────────────────────

        render_leaf = function(inode, opts) {
            num  <- opts$num
            text <- if (!is.null(num) && isTRUE(opts$numbered))
                paste0("**Question ", num, " :** ", inode$text)
            else
                inode$text

            if (inherits_class(inode, "MultipleChoice")) {
                answers_md <- private$mc_answers(inode, opts)
                text <- paste(text, answers_md, sep = "\n\n")
            }

            feedback_md <- private$render_feedback(inode, opts)

            parts <- c(list(text),
                       if (!is.null(feedback_md)) list(feedback_md))
            paste(parts, collapse = "\n\n")
        },

        # ── Cloze ─────────────────────────────────────────────────────────────

        render_cloze = function(inode, opts) {
            num <- opts$num

            inst_parts <- NULL
            index      <- 1
            for (sq in inode$subquestions) {
                sq_opts        <- opts
                sq_opts$numbered <- FALSE
                part <- private$subq_text_and_number(sq, index, sq_opts)
                inst_parts <- c(inst_parts, part)
                index <- index + 1
            }

            if (!is.null(inode$header))
                inst_parts <- c(list(inode$header), inst_parts)

            text <- paste(inst_parts, collapse = "\n\n")
            if (!is.null(num) && isTRUE(opts$numbered))
                text <- paste0("**Question ", num, " :** ", text)

            fb <- private$cloze_feedback(inode, opts)
            parts <- c(list(text), if (!is.null(fb)) list(fb))
            paste(parts, collapse = "\n\n")
        }
    ),

    private = list(

        quiz_title_block = function(inode, opts) {
            title_yaml <- sprintf("---\ntitle: \"%s\"\n", inode$title)
            if (!is.null(opts$header))
                title_yaml <- paste0(title_yaml, opts$header, "\n")
            if (!is.null(opts$preamble)) {
                preamble <- paste0("  - ", strsplit(opts$preamble, "\n")[[1]],
                                   collapse = "\n")
                title_yaml <- paste0(title_yaml, "header-includes:\n", preamble, "\n")
            }
            paste0(title_yaml, "---")
        },

        data_chunk = function(inode) {
            expr <- inode$data_expr
            if (is_empty_language(expr)) return(NULL)
            sprintf("```{r include = FALSE}\n%s\n```", answerstr(expr))
        },

        mc_answers = function(inode, opts) {
            items <- inode$items
            lines <- lapply(items, function(item) {
                icon <- sprintf("![](%s){height=16px}",
                                system.file("etc", "checkbox_unchecked.png", package = "quizR"))
                paste(icon, item)
            })
            paste(lines, collapse = "\n\n")
        },

        render_feedback = function(inode, opts) {
            if (isFALSE(opts$feedback)) return(NULL)
            fb <- inode$feedback
            if (is.null(fb)) return(NULL)

            text <- if (isTRUE(fb$eval))
                fb$text
            else if (!is.null(fb$noeval_text))
                fb$noeval_text
            else
                fb$text

            if (is.null(text) || identical(text, "")) return(NULL)

            answer_str <- if (is.character(text)) {
                text
            } else if (is.language(text) || is.numeric(text)) {
                sprintf("```{r}\n%s\n```\n", answerstr(text))
            } else return(NULL)

            paste0("**Réponse :** ", answer_str)
        },

        subq_text_and_number = function(sq, index, opts) {
            text <- sq$text
            if (inherits_class(sq, "MultipleChoice")) {
                answers_md <- private$mc_answers(sq, opts)
                sprintf("%s (%d)\n\n%s", text, index, answers_md)
            } else {
                sprintf("%s (%d)", text, index)
            }
        },

        cloze_feedback = function(inode, opts) {
            if (isFALSE(opts$feedback)) return(NULL)

            feedbacks <- lapply(seq_along(inode$subquestions), function(i) {
                sq  <- inode$subquestions[[i]]
                fb  <- private$render_feedback(sq, opts)
                if (is.null(fb)) return(NULL)
                paste0(i, ". ", fb)
            })
            feedbacks <- Filter(Negate(is.null), feedbacks)
            if (length(feedbacks) == 0) return(NULL)

            header <- inode$cloze_feedback$header
            parts  <- c(if (!is.null(header)) list(header),
                        list("**Réponses :**"),
                        feedbacks)
            paste(parts, collapse = "\n\n")
        }
    )
)

# Helper: check inode class_name without R6 inheritance info
inherits_class <- function(inode, cls) inode$class_name == cls
