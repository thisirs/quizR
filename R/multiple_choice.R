#' @include simple_question.R
NULL

#' @export
MultipleChoice <- R6::R6Class(
    "MultipleChoice",
    inherit = SimpleQuestion,
    public = list(
        shuffle_answers = FALSE,
        single = FALSE,
        statements = NULL,
        answer_feedbacks = NULL,

        initialize = function(text,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              items = NULL,
                              answer_feedbacks = NULL,
                              shuffle_answers = FALSE,
                              single = FALSE,
                              tags = NULL) {
            super$initialize(text,
                             data = data,
                             hidden_data = hidden_data,
                             seed = seed,
                             hidden_seed = hidden_seed,
                             feedback = feedback,
                             answer = answer,
                             tag = tags,
                             )

            self$type <- "multichoice"

            # FIXME: choose between items and statements
            self$statements <- as.list(items)

            if(is.null(self$answer)) {
                n <- length(self$statements)
                answers <- lapply(seq_len(n), function(i) {
                    self$statements[[i]][[1]]
                })
                statements <- lapply(seq_len(n), function(i) {
                    self$statements[[i]][[2]]
                })
                self$answer <- answers
                self$statements <- statements
            }

            n <- length(self$statements)
            if(is.null(answer_feedbacks)) {
                self$answer_feedbacks <- as.list(rep("", n))
            } else {
                if(length(answer_feedbacks) == n)
                    self$answer_feedbacks <- answer_feedbacks
                else
                    stop("Wrong number of answer feedbacks")
            }

            self$shuffle_answers <- shuffle_answers
            self$single <- single

            private$xml_placeholders$SHUFFLEANSWERS <-
                "get_xml_shuffle_answers"
            private$xml_placeholders$SINGLE <-
                "get_xml_single"
            private$xml_placeholders$ANSWERS <-
                "get_xml_answers"
        },

        get_inst_cookie = function(opts, info) {
            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            prefix <- ifelse(evaluated_answers, "=", "")
            cookie <- paste0(prefix, self$instantiated_statements, collapse = "~")
            sprintf("{1:MC:%s}", cookie)
        },

        get_inst_text_and_number = function(opts, info) {
            stopifnot(is.numeric(info$index))

            # Indent answers if INST_TEXT appears to be an item
            inst_text <- super$get_inst_text(opts, info)
            indent <- ifelse(grepl("^ *[0-9]+\\.", inst_text), "    ", "")
            opts$indent <- indent

            sprintf("%s (%d)\n\n%s",
                    inst_text,
                    info$index,
                    self$get_answers(opts, info))
        },

        get_answers = function(opts, info) {
            indent <- ifelse(is.null(opts$indent), "", "    ")
            n <- length(self$statements)
            statement_list <- as.list(self$statements)

            answers <- lapply(seq_len(n), function(i) {
                statement <- statement_list[[i]]
                inst_stat <- instantiate_text_list(statement, self$hidden_data_list)

                fn <- "checkbox_unchecked.png"
                icon <- sprintf("![](%s){height=16px}", system.file("etc", fn, package="quizR"))
                sprintf("%s%s %s", indent, icon, inst_stat)
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

        get_xml_shuffle_answers = function(opts, info) {
            flag <- ifelse(self$single, "true", "false")
            sprintf("<single>%s</single>", flag)
        },

        get_xml_single = function(opts, info) {
            flag <- as.numeric(self$single)
            sprintf("<shuffleanswers>%d</shuffleanswers>", flag)
        },

        get_xml_answers = function(opts, info) {
            n <- length(self$answer)
            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            instantiated_statements <- self$instantiated_statements

            answers <- lapply(1:n, function(i) {
                evaluated_answer <- evaluated_answers[[i]]
                fraction <- ifelse(evaluated_answer, "100", "0")

                inst_stat <- instantiated_statements[[i]]
                inst_feed <- self$instantiated_answer_feedbacks[[i]]

                tmpl <- add_spaces_left(private$xml_answer_template, opts$indent + 2)

                sprintf(tmpl, fraction, inst_stat, inst_feed)
            })

            paste(answers, collapse = "\n")
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

            evaluated_answers <- sapply(self$instantiated_answer, function(answer) {
                eval(answer, info$env)
            })
            stopifnot(is.logical(evaluated_answers))

            evaluated_answers
        },

        get_feedback_answer = function(opts, info) {
            indent <- ifelse(is.null(opts$indent), "", "    ")
            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            instantiated_statements <- self$instantiated_statements

            feedback <- lapply(seq_along(evaluated_answers), function(i) {
                evaluated_answer <- evaluated_answers[[i]]
                instantiated_statement <- instantiated_statements[[i]]

                filename <- ifelse(evaluated_answer, "checkbox_checked.png", "checkbox_unchecked.png")
                icon <- sprintf("![](%s){height=16px}", system.file("etc", filename, package="quizR"))
                sprintf("%s%s %s", indent, icon, instantiated_statement)
            })

            paste(feedback, collapse = "\n\n")
        },

        rename_answer = function(prefix, names0 = names(self$hidden_data_list)) {
            self$answer <- lapply(self$answer, function(answer) {
                prefix_object(prefix, names0, answer)
            })
        },

        copy = function() {
            MultipleChoice$new(self$text,
                            seed = self$seed,
                            hidden_seed = self$hidden_seed,
                            hidden_data = self$hidden_data,
                            statements = self$statements,
                            answer_feedbacks = self$answer_feedbacks,
                            data = self$data,
                            answer = self$answer,
                            feedback = self$feedback,
                            shuffle_answers = self$shuffle_answers,
                            single = self$single)
        }
    ),

    active = list(
        instantiated_answer = function() {
            lapply(self$answer, function(ans) {
                instantiate_data_list(ans, self$hidden_data_list)
            })
        },

        instantiated_answer_feedbacks = function() {
            lapply(self$answer_feedbacks, function(f) {
                instantiate_text_list(f, self$hidden_data_list)
            })
        },

        instantiated_statements = function() {
            lapply(self$statements, function(statement) {
                instantiate_text_list(statement, self$hidden_data_list)
            })
        }
    ),

    private = list(
        xml_answer_template = trimws("
<answer fraction=\"%s\">
  <text><![CDATA[%s]]></text>
      <feedback format=\"html\">
        <text><![CDATA[%s]]></text>
      </feedback>
</answer>"),
        xml_question_template = trimws("
<question type=\"@TYPE@\">
  <name>
    <text><![CDATA[@TITLE@]]></text>
  </name>
  <questiontext format=\"html\">
    <text><![CDATA[@XML_QUESTION_TEXT@]]></text>
  </questiontext>
@ANSWERS@
  @SHUFFLEANSWERS@
  @SINGLE@
@XML_GENERALFEEDBACK@
  <hidden>0</hidden>
</question>
")))
