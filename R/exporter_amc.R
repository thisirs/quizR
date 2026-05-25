# AMC (Auto Multiple Choice) LaTeX exporter.

#' @export
AMCExporter <- R6::R6Class(
    "AMCExporter",
    public = list(

        render = function(inode, opts = list()) {
            switch(inode$class_name,
                Quiz          = self$render_quiz(inode, opts),
                Group         = self$render_group(inode, opts),
                ClozeQuestion = self$render_cloze(inode, opts),
                NumericalQuestion = self$render_numerical(inode, opts),
                MultipleChoice    = self$render_mc(inode, opts),
                stop("No AMC renderer for class: ", sQuote(inode$class_name))
            )
        },

        # ── Quiz / Group ─────────────────────────────────────────────────────

        render_quiz = function(inode, opts) {
            parts <- lapply(inode$children, function(child) {
                self$render(child, opts)
            })
            paste(Filter(Negate(is.null), parts), collapse = "\n\n")
        },

        render_group = function(inode, opts) {
            title    <- inode$title
            elements <- lapply(seq_along(inode$children), function(i) {
                child      <- inode$children[[i]]
                child_opts <- opts
                child_opts$num <- i

                if (child$class_name == "Group") {
                    self$render_group(child, child_opts)
                } else {
                    content <- self$render(child, child_opts)
                    if (is.null(content)) return(NULL)
                    sprintf("\\element{%s}{\n%s\n}", title, content)
                }
            })
            elements <- Filter(Negate(is.null), elements)
            if (length(elements) == 0) return(NULL)
            paste(elements, collapse = "\n\n")
        },

        # ── Cloze ────────────────────────────────────────────────────────────

        render_cloze = function(inode, opts) {
            parts <- list()

            if (!is.null(inode$header)) {
                parts <- c(parts,
                           list(render_latex_fragment(inode$header, opts,
                                                      list(env = inode$env))))
            }

            cloze_num <- opts$num
            for (i in seq_along(inode$subquestions)) {
                subq_opts      <- opts
                subq_opts$num  <- if (!is.null(cloze_num))
                                      paste0(cloze_num, "-", i)
                                  else i
                subq_latex <- self$render(inode$subquestions[[i]], subq_opts)
                if (!is.null(subq_latex))
                    parts <- c(parts, list(subq_latex))
            }

            if (length(parts) == 0) return(NULL)
            paste(parts, collapse = "\n\n")
        },

        # ── NumericalQuestion ────────────────────────────────────────────────

        render_numerical = function(inode, opts) {
            if (is.null(inode$distractors))
                stop("AMC export requires 'distractors' for numerical question: ",
                     sQuote(inode$title))

            answer_val     <- inode$answer
            distractor_vals <- unlist(inode$distractors)

            if (any(distractor_vals == answer_val))
                stop("Distractor(s) equal to correct answer (",
                     format(answer_val), ") in: ", sQuote(inode$title))
            if (anyDuplicated(distractor_vals))
                stop("Duplicate distractors in: ", sQuote(inode$title))

            qid   <- private$make_id(inode, opts)
            qtext <- render_latex_fragment(inode$text, opts, list(env = inode$env))

            choices_env   <- if (isTRUE(inode$options[["horizontal"]])) "horizchoices" else "choices"
            correct_line  <- sprintf("    \\correctchoice{$%s$}", format(answer_val))
            wrong_lines   <- sapply(format(distractor_vals), function(d)
                                 sprintf("    \\wrongchoice{$%s$}", d))
            choices_block <- paste(c(correct_line, wrong_lines), collapse = "\n")

            sprintf(
                "\\begin{question}{%s}\n  %s\n  \\begin{%s}\n%s\n  \\end{%s}\n\\end{question}",
                qid, qtext, choices_env, choices_block, choices_env
            )
        },

        # ── MultipleChoice ───────────────────────────────────────────────────

        render_mc = function(inode, opts) {
            qtype <- if (inode$single) "question" else "questionmult"
            qid   <- private$make_id(inode, opts)
            qtext <- render_latex_fragment(inode$text, opts, list(env = inode$env))

            inst_items <- lapply(inode$items, function(item)
                render_latex_fragment(item, opts, list(env = inode$env)))

            choices_env  <- if (isTRUE(inode$options[["horizontal"]])) "horizchoices" else "choices"
            choice_lines <- sapply(seq_along(inst_items), function(i) {
                cmd <- if (inode$answer[[i]]) "\\correctchoice" else "\\wrongchoice"
                sprintf("    %s{%s}", cmd, inst_items[[i]])
            })
            choices_block <- paste(choice_lines, collapse = "\n")

            sprintf(
                "\\begin{%s}{%s}\n  %s\n  \\begin{%s}\n%s\n  \\end{%s}\n\\end{%s}",
                qtype, qid, qtext, choices_env, choices_block, choices_env, qtype
            )
        }
    ),

    private = list(
        make_id = function(inode, opts) {
            title <- evaluate_inline_r(inode$title, inode$env)
            id    <- gsub("[^a-zA-Z0-9]", "-", title)
            id    <- gsub("-+", "-", id)
            id    <- gsub("^-|-$", "", id)
            if (nchar(id) > 30) id <- substr(id, 1, 30)
            if (!is.null(opts$num)) id <- paste0("q", opts$num, "-", id)
            if (nchar(id) == 0)   id <- "question"
            id
        }
    )
)
