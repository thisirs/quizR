#' @include simple_question.R
NULL

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
                self$format <- "MC"
            else
                self$format <- format

            # Set shuffle_answers from format; can be overridden by
            # shuffle_answers argument
            if(is.null(shuffle_answers)) {
                last_char <- substr(self$format, nchar(self$format), nchar(self$format))
                if(last_char == "S")
                    self$shuffle_answers <- TRUE
            } else
                self$shuffle_answers <- shuffle_answers

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
            cookie <- paste0(prefix, self$instantiated_items, collapse = "~")

            cookie_kw <- cookies_keyword[[self$format]]
            if(is.null(cookie_kw))
                stop("Unknown format")

            sprintf("{1:%s:%s}", cookie_kw, cookie)
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
            n <- length(self$items)

            answers <- lapply(seq_len(n), function(i) {
                inst_item <- self$instantiated_items[[i]]

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

        get_xml_single = function(opts, info) {
            flag <- ifelse(self$single, "true", "false")
            sprintf("<single>%s</single>", flag)
        },

        get_xml_shuffle_answers = function(opts, info) {
            flag <- as.numeric(self$shuffle_answers)
            sprintf("<shuffleanswers>%d</shuffleanswers>", flag)
        },

        get_xml_item = function(md_text, opts, info) {
            HTML_question <- render_HTML(md_text, opts, info)
            trimws(HTML_question) # pandoc seems to add some leading newlines
        },

        get_xml_answers = function(opts, info) {
            n <- length(self$answer)
            evaluated_answers <- self$get_evaluated_answer2(opts, info)
            instantiated_items <- self$instantiated_items

            answers <- lapply(1:n, function(i) {
                evaluated_answer <- evaluated_answers[[i]]
                fraction <- ifelse(evaluated_answer, "100", "0")

                inst_stat <- self$get_xml_item(instantiated_items[[i]], opts, info)
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

        rename_text = function(prefix, names = self$hidden_data_names()) {
            self$text <- prefix_object(prefix, names, private$.text)

            self$items <- lapply(self$items, function(item) {
                prefix_object(prefix, names, item)
            })

            self$invalidate_text()

            self
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
            lapply(self$answer_feedbacks, function(f) {
                instantiate_text_list(f, self$hidden_data_list)
            })
        },

        instantiated_items = function() {
            lapply(self$items, function(statement) {
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
