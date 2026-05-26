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

                if (!is.null(inode$header)) {
                    if (child$class_name == "ClozeQuestion")
                        child$header <- paste(c(inode$header, child$header), collapse = "\n\n")
                    else
                        child$text <- paste(c(inode$header, child$text), collapse = "\n\n")
                }

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
            cloze_num <- opts$num
            n_subqs   <- length(inode$subquestions)
            info      <- list(env = inode$env)
            is_subq   <- isTRUE(opts$as_subquestion)

            subq_opts_list <- lapply(seq_len(n_subqs), function(i) {
                so      <- opts
                so$num  <- if (!is.null(cloze_num)) paste0(cloze_num, "-", i) else i
                so
            })

            parts <- list()

            if (!is_subq) {
                first_id  <- private$first_leaf_id(inode$subquestions[[1]],      subq_opts_list[[1]])
                last_id   <- private$last_leaf_id(inode$subquestions[[n_subqs]], subq_opts_list[[n_subqs]])
                range_hdr <- if (n_subqs > 1)
                    sprintf("\\noindent\\textbf{Question \\AMCref{%s} à \\AMCref{%s}}\\hspace*{1em}",
                            first_id, last_id)
                else
                    sprintf("\\noindent\\textbf{Question \\AMCref{%s}}\\hspace*{1em}", first_id)
                parts <- list(range_hdr)
            }

            if (!is.null(inode$header))
                parts <- c(parts,
                           list(paste0(render_latex_fragment(inode$header, opts, info), "\\par")))

            if (isTRUE(inode$text_based)) {
                if (!is_subq)
                    parts <- c(parts, list("\\AMCnobloc\\def\\AMCbeginQuestion#1#2{}"))

                # Embed a unique plain-ASCII marker at each cookie position, then
                # render the whole text once so the markdown list structure (e.g. a
                # numbered list) is preserved as a single \begin{enumerate} block.
                markers   <- sprintf("ZZQMARK%d", seq_len(n_subqs))
                fragments <- sapply(inode$subquestions, `[[`, "text")
                rendered  <- render_latex_fragment(
                    paste(paste0(fragments, " ", markers), collapse = "\n"),
                    opts, info)

                # Answer-only question blocks (no text).
                answer_blocks <- lapply(seq_len(n_subqs), function(i) {
                    sq_copy      <- inode$subquestions[[i]]
                    sq_copy$text <- ""
                    self$render(sq_copy, subq_opts_list[[i]])
                })
                if (all(sapply(answer_blocks, is.null))) return(NULL)

                if (all(sapply(markers, grepl, rendered, fixed = TRUE))) {
                    # Split the rendered LaTeX at the markers and interleave the
                    # answer blocks so each question env sits right after its text.
                    pieces <- strsplit(rendered,
                                       paste(markers, collapse = "|"),
                                       perl = TRUE)[[1]]
                    assembled <- pieces[[1L]]
                    for (i in seq_len(n_subqs)) {
                        if (!is.null(answer_blocks[[i]]))
                            assembled <- paste0(assembled, "\n", answer_blocks[[i]])
                        if (i + 1L <= length(pieces))
                            assembled <- paste0(assembled, pieces[[i + 1L]])
                    }
                    parts <- c(parts, list(trimws(assembled)))
                } else {
                    # Fallback: full text first, then answer blocks sequentially.
                    parts <- c(parts,
                               list(render_latex_fragment(
                                   paste(fragments, collapse = "\n"), opts, info)),
                               Filter(Negate(is.null), answer_blocks))
                }
            } else {
                if (!is_subq)
                    parts <- c(parts, list("\\AMCnobloc\\def\\AMCbeginQuestion#1#2{}"))

                # ClozeQuestion children rendered inside an \item must not emit
                # their own range header or \AMCnobloc — the parent owns those.
                child_opts <- lapply(seq_len(n_subqs), function(i) {
                    so <- subq_opts_list[[i]]
                    if (inode$subquestions[[i]]$class_name == "ClozeQuestion")
                        so$as_subquestion <- TRUE
                    so
                })

                subq_latex_list <- lapply(seq_len(n_subqs), function(i)
                    self$render(inode$subquestions[[i]], child_opts[[i]]))
                subq_latex_list <- Filter(Negate(is.null), subq_latex_list)
                if (length(subq_latex_list) == 0) return(NULL)

                items <- lapply(subq_latex_list, function(s) paste0("\\item\n", s))
                parts <- c(parts, list(paste0("\\begin{enumerate}\n",
                                              paste(items, collapse = "\n"),
                                              "\n\\end{enumerate}")))
            }
            paste(parts, collapse = "\n")
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
            qtext <- if (nzchar(trimws(inode$text)))
                render_latex_fragment(inode$text, opts, list(env = inode$env)) else ""

            choices_env   <- if (isTRUE(inode$options[["horizontal"]])) "horizchoices" else "choices"
            correct_line  <- sprintf("    \\correctchoice{$%s$}", format(answer_val))
            wrong_lines   <- sapply(format(distractor_vals), function(d)
                                 sprintf("    \\wrongchoice{$%s$}", d))
            choices_block <- paste(c(correct_line, wrong_lines), collapse = "\n")

            sprintf(
                "\\begin{question}{%s}\n  \\AMCqlabel{%s}\n  %s\n  \\begin{%s}\n%s\n  \\end{%s}\n\\end{question}",
                qid, qid, qtext, choices_env, choices_block, choices_env
            )
        },

        # ── MultipleChoice ───────────────────────────────────────────────────

        render_mc = function(inode, opts) {
            qtype <- if (inode$single) "question" else "questionmult"
            qid   <- private$make_id(inode, opts)
            qtext <- if (nzchar(trimws(inode$text)))
                render_latex_fragment(inode$text, opts, list(env = inode$env)) else ""

            inst_items <- lapply(inode$items, function(item)
                render_latex_fragment(item, opts, list(env = inode$env)))

            choices_env  <- if (isTRUE(inode$options[["horizontal"]])) "horizchoices" else "choices"
            choice_lines <- sapply(seq_along(inst_items), function(i) {
                cmd <- if (inode$answer[[i]]) "\\correctchoice" else "\\wrongchoice"
                sprintf("    %s{%s}", cmd, inst_items[[i]])
            })
            choices_block <- paste(choice_lines, collapse = "\n")

            sprintf(
                "\\begin{%s}{%s}\n  \\AMCqlabel{%s}\n  %s\n  \\begin{%s}\n%s\n  \\end{%s}\n\\end{%s}",
                qtype, qid, qid, qtext, choices_env, choices_block, choices_env, qtype
            )
        }
    ),

    private = list(
        make_id = function(inode, opts) {
            title <- evaluate_inline_r(inode$title, inode$env)
            if (length(title) == 0 || is.na(title)) title <- ""
            id    <- gsub("[^a-zA-Z0-9]", "-", title)
            id    <- gsub("-+", "-", id)
            id    <- gsub("^-|-$", "", id)
            if (nchar(id) > 30) id <- substr(id, 1, 30)
            if (!is.null(opts$num)) id <- paste0("q", opts$num, "-", id)
            if (nchar(id) == 0)   id <- "question"
            id
        },

        first_leaf_id = function(inode, opts) {
            if (inode$class_name != "ClozeQuestion")
                return(private$make_id(inode, opts))
            n <- length(inode$subquestions)
            if (n == 0) return("question")
            inner_opts     <- opts
            inner_opts$num <- if (!is.null(opts$num)) paste0(opts$num, "-1") else 1
            private$first_leaf_id(inode$subquestions[[1]], inner_opts)
        },

        last_leaf_id = function(inode, opts) {
            if (inode$class_name != "ClozeQuestion")
                return(private$make_id(inode, opts))
            n <- length(inode$subquestions)
            if (n == 0) return("question")
            inner_opts     <- opts
            inner_opts$num <- if (!is.null(opts$num)) paste0(opts$num, "-", n) else n
            private$last_leaf_id(inode$subquestions[[n]], inner_opts)
        }
    )
)
