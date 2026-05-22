#' @include simple_question.R
NULL

formats <- c(
    "multiplechoice",
    "multichoice",
    "mc",
    "multichoice_v",
    "mcv",
    "multichoice_h",
    "mch",
    "multiresponse",
    "mr",
    "multiresponse_h",
    "mrh",
    "multichoice_s",
    "mcs",
    "multichoice_vs",
    "mcvs",
    "multichoice_hs",
    "mchs",
    "multiresponse_s",
    "mrs",
    "multiresponse_hs",
    "mrhs"
)

single_formats <- c(
    "multiplechoice",
    "multichoice",
    "mc",
    "multichoice_v",
    "mcv",
    "multichoice_h",
    "mch",
    "multiresponse",
    "multichoice_s",
    "mcs",
    "multichoice_vs",
    "mcvs",
    "multichoice_hs",
    "mchs"
)

cookies_keyword <- list(
    "multiplechoice" = "MC",
    "multichoice" = "MC",
    "mc" = "MC",
    "multichoice_v" = "MCV",
    "mcv" = "MCV",
    "multichoice_h" = "MCH",
    "mch" = "MCH",
    "multiresponse" = "MR",
    "mr" = "MR",
    "multiresponse_h" = "MRH",
    "mrh" = "MRH",
    "multichoice_s" = "MCS",
    "mcs" = "MCS",
    "multichoice_vs" = "MCVS",
    "mcvs" = "MCVS",
    "multichoice_hs" = "MCHS",
    "mchs" = "MCHS",
    "multiresponse_s" = "MRS",
    "mrs" = "MRS",
    "multiresponse_hs" = "MRHS",
    "mrhs" = "MRHS"
)


#' @export
MultipleChoice <- R6::R6Class(
    "MultipleChoice",
    inherit = SimpleQuestion,
    public = list(
        shuffle_answers = FALSE,
        single = FALSE,
        items = NULL,
        answer_feedbacks = NULL,
        format = NULL,

        initialize = function(text,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              items = NULL,
                              answer_feedbacks = NULL,
                              shuffle_answers = NULL,
                              single = FALSE,
                              format = "MC",
                              tags = NULL) {
            super$initialize(text,
                             data = data,
                             hidden_data = hidden_data,
                             seed = seed,
                             hidden_seed = hidden_seed,
                             feedback = feedback,
                             answer = answer,
                             tags = tags,
                             )

            self$type <- "multichoice"

            if(is.null(format))
                format <- "mc"
            if(!tolower(format) %in% formats)
                stop("Unknown format")
            self$format <- tolower(format)

            # Set `single` and `shuffle_answers` based on format
            last_char <- substr(self$format, nchar(self$format), nchar(self$format))
            self$shuffle_answers <- (last_char == "s")
            self$single <- (self$format %in% single_formats)

            if (is.language(items)) {
                # Items is a language object to be evaluated from hidden_data
                self$items <- items
                self$answer_feedbacks <- answer_feedbacks  # NULL or provided
            } else {
                self$items <- as.list(items)

                if(is.null(self$answer)) {
                    n <- length(self$items)
                    answers <- lapply(seq_len(n), function(i) {
                        self$items[[i]][[1]]
                    })
                    items <- lapply(seq_len(n), function(i) {
                        self$items[[i]][[2]]
                    })
                    self$answer <- answers
                    self$items <- items
                }

                n <- length(self$items)
                if(is.null(answer_feedbacks)) {
                    self$answer_feedbacks <- as.list(rep("", n))
                } else {
                    if(length(answer_feedbacks) == n)
                        self$answer_feedbacks <- answer_feedbacks
                    else
                        stop("Wrong number of answer feedbacks")
                }
            }

            private$kind_to_placeholder[["answers.xml"]] <- "XML_ANSWERS"
            private$xml_ctx_field_map[["XML_ANSWERS"]] <- "answers"

            private$amc_placeholders <- list()
        },

        get_inst_cookie = function(opts, info) {
            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            prefix <- ifelse(evaluated_answers, "=", "")

            cookie <- paste0(prefix, self$instantiated_items, collapse = "~")

            cookie_kw <- cookies_keyword[[self$format]]
            if(is.null(cookie_kw))
                stop("Unknown format")

            sprintf("`{1:%s:%s}`", cookie_kw, cookie)
        },

        get_inst_text_and_number = function(opts, info) {
            stopifnot(is.numeric(info$index))

            # Indent answers if INST_TEXT appears to be an item
            inst_text <- super$get_inst_text(opts, info)
            indent <- ifelse(grepl("^ *[0-9]+\\.", inst_text), "    ", "")
            opts$indent <- indent

            if(substr(inst_text, nchar(inst_text) - 2, nchar(inst_text)) == '```')
                sep <- "\n\n"
            else
                sep <- " "

            sprintf("%s%s(%d)\n\n%s",
                    inst_text,
                    sep,
                    info$index,
                    self$get_answers(opts, info))
        },

        get_answers = function(opts, info) {
            ## indent <- ifelse(is.null(opts$indent), "", "    ")
            indent <- ""
            instantiated_items <- self$instantiated_items
            n <- length(instantiated_items)

            answers <- lapply(seq_len(n), function(i) {
                inst_item <- instantiated_items[[i]]

                fn <- "checkbox_unchecked.png"
                icon <- sprintf("![](%s){height=16px}", system.file("etc", fn, package="quizR"))
                sprintf("%s%s %s", indent, icon, inst_item)
            })

            paste(answers, collapse = "\n\n")
        },

        get_inst_text = function(opts, info) {
            if (is.null(opts$export))
                stop("Need an export type for multichoice question")

            inst_text <- super$get_inst_text(opts, info)

            if (opts$export == "xml") {
                inst_text
            } else if (opts$export == "markdown") {
                answers <- self$get_answers(opts, info)
                paste(c(inst_text, "\n\n", answers), collapse = "\n\n")
            } else stop("Unknown export type: ", sQuote(opts$export))
        },

        build_xml_context = function(opts, info) {
            if (is.null(info$env)) {
                info0 <- self$get_data_and_environment()
                info <- update_list(info, info0)
            }

            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            n <- length(evaluated_answers)
            n.true  <- max(sum(evaluated_answers), 1)
            n.false <- max(sum(!evaluated_answers), 1)
            fractions <- sprintf("%.4f",
                100 * ifelse(evaluated_answers, 1, -1) /
                ifelse(evaluated_answers, n.true, n.false))

            instantiated_items <- self$instantiated_items
            feedbacks <- self$instantiated_answer_feedbacks

            answers_ctx <- lapply(seq_len(n), function(i) {
                list(
                    fraction        = fractions[[i]],
                    text            = trimws(render_HTML(instantiated_items[[i]], opts, info)),
                    answer_feedback = feedbacks[[i]]
                )
            })

            list(
                type          = self$type,
                title         = self$title,
                question_text = self$get_XML_question_text(opts, info),
                feedback      = self$get_XML_generalfeedback(opts, info),
                answers       = answers_ctx,
                single        = if (self$single) "true" else "false",
                shuffle       = as.character(as.integer(self$shuffle_answers))
            )
        },

        get_answer_info = function(opts, info) {
            NULL
        },

        get_evaluated_answer = function(opts, info) {
            NULL
        },

        get_evaluated_answer2 = function(opts, info) {
            if (is.null(info$env))
                stop("Unable to evaluate answers ; no environment provided")

            if (is.language(self$answer)) {
                inst_answer <- instantiate_data_list(self$answer, self$hidden_data_list)
                evaluated_answers <- eval(inst_answer, info$env)
                stopifnot(is.logical(evaluated_answers))
                evaluated_answers
            } else {
                evaluated_answers <- sapply(self$instantiated_answer, function(answer) {
                    eval(answer, info$env)
                })
                stopifnot(is.logical(evaluated_answers))
                evaluated_answers
            }
        },

        get_feedback_answer = function(opts, info) {
            indent <- ifelse(is.null(opts$indent), "", "    ")
            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            instantiated_items <- self$instantiated_items

            feedback <- lapply(seq_along(evaluated_answers), function(i) {
                evaluated_answer <- evaluated_answers[[i]]
                instantiated_item <- instantiated_items[[i]]

                filename <- ifelse(evaluated_answer, "checkbox_checked.png", "checkbox_unchecked.png")
                icon <- sprintf("![](%s){height=16px}", system.file("etc", filename, package="quizR"))
                sprintf("%s%s %s", indent, icon, instantiated_item)
            })

            paste(feedback, collapse = "\n\n")
        },

        get_amc_id = function(opts, info) {
            title <- evaluate_inline_r(self$title, info$env)  # cheap eval for ID only
            id <- gsub("[^a-zA-Z0-9]", "-", title)
            id <- gsub("-+", "-", id)
            id <- gsub("^-|-$", "", id)
            if (nchar(id) > 30) id <- substr(id, 1, 30)
            if (!is.null(info$num)) id <- paste0("q", info$num, "-", id)
            if (nchar(id) == 0) id <- "question"
            id
        },

        to_amc = function(opts, info) {
            opts$export <- "amc"

            if (is.null(info$env)) {
                info0 <- self$get_data_and_environment()
                info <- update_list(info, info0)
            }

            qtype <- if (self$single) "question" else "questionmult"
            qid   <- self$get_amc_id(opts, info)
            qtext <- render_latex_fragment(self$instantiated_text, opts, info)

            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            inst_items <- lapply(self$instantiated_items, function(item) {
                render_latex_fragment(item, opts, info)
            })

            choices <- lapply(seq_along(inst_items), function(i) {
                cmd <- if (evaluated_answers[[i]]) "\\correctchoice" else "\\wrongchoice"
                sprintf("    %s{%s}", cmd, inst_items[[i]])
            })
            choices_str <- paste(choices, collapse = "\n")

            sprintf(
                "\\begin{%s}{%s}\n  %s\n  \\begin{choices}\n%s\n  \\end{choices}\n\\end{%s}",
                qtype, qid, qtext, choices_str, qtype)
        },

        rename_text = function(prefix, names = self$hidden_data_names()) {
            self$text <- prefix_object(prefix, names, private$.text)

            if (is.language(self$items)) {
                self$items <- prefix_object(prefix, names, self$items)
            } else {
                self$items <- lapply(self$items, function(item) {
                    prefix_object(prefix, names, item)
                })
            }

            self$invalidate_text()

            self
        },

        rename_answer = function(prefix, names0 = names(self$hidden_data_list)) {
            if (is.language(self$answer)) {
                self$answer <- prefix_object(prefix, names0, self$answer)
            } else {
                self$answer <- lapply(self$answer, function(answer) {
                    prefix_object(prefix, names0, answer)
                })
            }
        },

        copy = function() {
            MultipleChoice$new(self$text,
                            seed = self$seed,
                            hidden_seed = self$hidden_seed,
                            hidden_data = self$hidden_data,
                            items = self$items,
                            answer_feedbacks = self$answer_feedbacks,
                            data = self$data,
                            answer = self$answer,
                            feedback = self$feedback,
                            shuffle_answers = self$shuffle_answers,
                            single = self$single,
                            format = self$format)
        }
    ),

    active = list(
        instantiated_answer = function() {
            lapply(self$answer, function(ans) {
                instantiate_data_list(ans, self$hidden_data_list)
            })
        },

        instantiated_answer_feedbacks = function() {
            if (is.null(self$answer_feedbacks)) {
                as.list(rep("", length(self$instantiated_items)))
            } else {
                lapply(self$answer_feedbacks, function(f) {
                    instantiate_text_list(f, self$hidden_data_list)
                })
            }
        },

        instantiated_items = function() {
            if (is.language(self$items)) {
                env <- list2env(self$hidden_data_list, envir = empty_env())
                as.list(eval(self$items, envir = env))
            } else {
                lapply(self$items, function(statement) {
                    instantiate_text_list(statement, self$hidden_data_list)
                })
            }
        }
    ),

    private = list(
        amc_placeholders = NULL,

        whisker_xml_template = trimws('
<question type="{{type}}">
  <name>
    <text><![CDATA[{{{title}}}]]></text>
  </name>
  <questiontext format="html">
    <text><![CDATA[{{{question_text}}}]]></text>
  </questiontext>
{{#answers}}
  <answer fraction="{{{fraction}}}">
    <text><![CDATA[{{{text}}}]]></text>
    <feedback format="html">
      <text><![CDATA[{{{answer_feedback}}}]]></text>
    </feedback>
  </answer>
{{/answers}}
  <shuffleanswers>{{{shuffle}}}</shuffleanswers>
  <single>{{{single}}}</single>
{{#feedback}}
{{{feedback}}}
{{/feedback}}
  <hidden>0</hidden>
</question>
')))
