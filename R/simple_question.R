#' Base class
#'
#' @export
SimpleQuestion <- R6::R6Class(
    "SimpleQuestion",
    public = list(
        type = "abstract",
        quiz = NULL,

        initialize = function(text,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              tag = NULL,
                              header = NULL) {
            private$.text <- text
            private$.data <- data
            private$.hidden_data <- hidden_data
            private$.seed <- seed
            private$.hidden_seed <- hidden_seed
            private$.answer <- answer
            private$.feedback <- feedback
            private$.tag <- tag
            private$.header <- header

            # Default placeholders
            private$placeholders <- list(
                TITLE = "get_title",
                REC_DATA_CHUNK = "get_rec_data_chunk",
                DATA_CHUNK = "get_data_chunk",
                ANSWER_INFO = "get_answer_info",
                INST_TEXT = "get_inst_text",
                SEED_CHUNK = "get_seed_chunk",
                EVALUATED_ANSWER = "get_evaluated_answer",
                FEEDBACK = "get_feedback",
                FEEDBACK_ANSWER = "get_feedback_answer",
                ANSWER_STRING = "get_answer_string")

            # Specific placeholders for XML export
            private$xml_placeholders <- list(
                TYPE = "get_type",
                TITLE = "get_title",
                XML_QUESTION_TEXT = "get_XML_question_text",
                XML_GENERALFEEDBACK = "get_XML_generalfeedback",
                XML_ANSWER = "get_XML_answer"
            )
        },

        instantiate_placeholders = function(template, placeholders, opts, info) {
            placeholders_regex <- paste0("@", names(placeholders), "@", collapse = "|")

            repeat {
                match <- stringi::stri_locate_first_regex(template, placeholders_regex)

                if(any(is.na(match)))   # breaking if no match
                    break
                if(nrow(match) == 0)
                    break

                # Computing replacement string
                id <- substring(template, match[1] + 1, match[2] - 1)
                funcname <- placeholders[[id]]
                if (is.null(funcname))
                    stop("Unable to find corresponding function for ", sQuote(id))
                replacement <- self[[funcname]](opts, info)

                # If replacement is NULL, try to replace whole line
                if (is.null(replacement)) {
                    template <- stringi::stri_replace_first_regex(template, paste0("^ *@", id, "@ *\n"), "", opts_regex = list(multiline = TRUE))
                    template <- stringi::stri_replace_first_regex(template, paste0("@", id, "@"), "")
                } else {
                    # Escape backslash and dollar in replacement string
                    replacement <- gsub("\\\\", "\\\\\\\\", replacement)
                    replacement <- gsub("\\$", "\\\\$", replacement)
                    template <- stringi::stri_replace_first_regex(template, paste0("@", id, "@"), replacement)
                }
            }
            if(nchar(template) == 0)
                NULL
            else
                template
        },

        update_quiz = function(quiz) {
            self$quiz <- quiz
            self$invalidate_hidden_data_list()
        },

        get_data_and_environment = function() {
            if(is.null(self$quiz))
                data <- self$recursive_instantiated_data()
            else
                data <- self$quiz$recursive_instantiated_data()

            env <- empty_env()
            eval(data, env)

            list(data = data, env = env)
        },

        validate_data = function(parent_data = NULL) {},

        get_markdown_from_template = function(template, opts = list(), info = list()) {
            self$instantiate_placeholders(template, private$placeholders, opts, info)
        },

        to_markdown = function(opts = list(), info = list()) {
            opts <- update_list(private$default_options, opts)
            opts$export <- "markdown"
            private$validate_options(opts, info)

            template <- "@INST_TEXT@\n\n@FEEDBACK@\n"
            self$instantiate_placeholders(template, private$placeholders, opts, info)
        },

        #' Feedback options if none is provided
        get_default_feedback = function() {
            list(text = self$answer)
        },

        #' Sanitize provided feedback
        get_feedback_from_field = function(feedback) {
            feedback <-
                if(is.null(feedback)) {
                    self$get_default_feedback()
                } else if(is.character(feedback)) {
                    list(text = feedback)
                } else if(is.numeric(feedback)) {
                    list(text = feedback)
                } else if(is.language(feedback)) {
                    list(text = feedback)
                } else if(is.list(feedback)) {
                    feedback
                }
            update_list(private$default_feedback_options, feedback)
        },

        #' Template for full feedback
        get_feedback = function(opts, info) {
            # No feedback
            if (!is.null(opts$feedback) && !opts$feedback)
                return(NULL)

            "@ANSWER_INFO@\n\n@ANSWER_STRING@@EVALUATED_ANSWER@\n\n@FEEDBACK_ANSWER@\n"
        },

        get_answer_string = function(opts, info) {
            if(is.null(info$answer_string))
                "**R\u00E9ponse :** "
            else
                info$answer_string
        },

        #' Feedback itself giving the right answer
        get_feedback_answer = function(opts, info) {
            # Get feedback as a proper named list with default arguments
            feedback_opts <- self$instantiated_feedback

            # Check for spurious arguments in feedback_options
            unknown_opts <- setdiff(names(feedback_opts),
                                    names(private$default_feedback_options))
            if (length(unknown_opts) > 0) {
                stop("Unknown options: ", paste0(unknown_opts))
            }

            # Override feedback_options with upstream opts
            feedback_opts <- update_list(feedback_opts, opts)

            # Check that options are coherent
            private$validate_feedback_options(feedback_opts, info)

            feedback <-
                if (feedback_opts$eval) {
                    feedback_opts$text
                } else {
                    if (is.null(feedback_opts$noeval_text))
                        feedback_opts$text
                    else
                        feedback_opts$noeval_text
                }

            feedback <- if (is.character(feedback)) {
                feedback
            } else if (is.language(feedback))
                sprintf("```{r}\n%s\n```\n", answerstr(feedback))
            else if (is.numeric(feedback))
                sprintf("```{r}\n%s\n```\n", answerstr(feedback))
            else stop("Unsupported feedback type: ", sQuote(feedback))

            # Maybe indent the feedback
            if(is.null(opts$indent))
                feedback
            else
                add_spaces_left(feedback, 4)
        },

        get_XML_question_text = function(opts, info) {
            # md_question <- self$get_XML_question_markdown(opts, info)
            md_question <- self$get_inst_text(opts, info)
            HTML_question <- render_HTML(md_question, opts, info)
            trimws(HTML_question) # pandoc seems to add some leading newlines
        },

        # Return XML "generalfeedback" node with feedback in HTML as CDATA
        get_XML_generalfeedback = function(opts, info) {
            if(!opts$feedback) return(NULL)

            # Generate HTML for feedback
            placeholders <- update_list(private$placeholders, private$xml_placeholders)
            tmpl <- self$get_feedback(opts, info)
            md_feedback <- self$instantiate_placeholders(tmpl, placeholders, opts, info)

            HTML_feedback <- render_HTML(md_feedback, opts, info)
            HTML_feedback0 <- trimws(HTML_feedback) # pandoc seems to add some leading newlines

            # Return XML with inner HTML
            tmpl <-"<generalfeedback format=\"html\">
  <text><![CDATA[%s]]></text>
</generalfeedback>"
            tmpl0 <- add_spaces_left(tmpl, opts$indent)
            sprintf(tmpl0, HTML_feedback0)
        },

        get_XML_answer = function(opts, info) {
            as.character(self$get_evaluated_answer2(opts, info))
        },

        #' Export Question as XML
        to_XML = function(opts = NULL, info = NULL) {
            # Set up environment for evaluating data if not already
            if (is.null(info$env)) {
                info0 <- self$get_data_and_environment()
                info <- update_list(info, info0)
            }

            # Setting options
            default_opts <- update_list(private$default_options, private$xml_default_options)
            opts <- update_list(default_opts, opts)
            opts$export <- "xml"

            private$validate_options(opts, info)

            template <- add_spaces_left(private$xml_question_template, opts$indent)
            placeholders <- update_list(private$placeholders, private$xml_placeholders)
            # Answers might modify datasets stored in info$env

            self$instantiate_placeholders(template, placeholders, opts, info)
        },

        invalidate_all = function() {
            self$invalidate_text()
            self$invalidate_answer()
            self$invalidate_feedback()
            self$invalidate_hidden_data()
            self$invalidate_data()
            self$invalidate_hidden_data_list()
        },

        invalidate_ancestor = function() {
            if (is.null(self$ancestor))
                self$invalidate_hidden_data()
            else
                self$ancestor$invalidate_hidden_data()
        },

        invalidate_data = function() {
            private$is_data_available <- FALSE
            self$invalidate_inst_data()
        },

        invalidate_hidden_data = function() {
            private$is_hidden_data_available <- FALSE
            self$invalidate_inst_text()
            self$invalidate_inst_answer()
            self$invalidate_inst_feedback()
            self$invalidate_inst_data()
            self$invalidate_hidden_data_list()
        },

        invalidate_hidden_data_list = function() {
            private$is_hidden_data_list_available <- FALSE
            self$invalidate_inst_text()
            self$invalidate_inst_answer()
            self$invalidate_inst_feedback()
            self$invalidate_inst_data()
        },

        invalidate_inst_data = function() {
            private$is_data_instantiated <- FALSE
        },

        invalidate_inst_text = function() {
            private$is_text_instantiated <- FALSE
        },

        invalidate_inst_answer = function() {
            private$is_answer_instantiated <- FALSE
        },

        invalidate_inst_feedback = function() {
            private$is_feedback_instantiated <- FALSE
        },

        invalidate_header = function() {
            self$invalidate_text()
        },

        invalidate_text = function() {
            private$is_text_available <- FALSE
            self$invalidate_inst_text()
        },

        invalidate_answer = function() {
            private$is_answer_available <- FALSE
            self$invalidate_inst_answer()
        },

        invalidate_feedback = function() {
            private$is_feedback_available <- FALSE
            self$invalidate_inst_feedback()
        },

        recursive_instantiated_data = function(seed_init = FALSE) {
            if (!seed_init & !is_empty_language(self$data) & is.null(self$seed))
                stop("Some data but no seed to initialize")

            if(is.null(self$seed))
                self$instantiated_data
            else
                merge_languages(
                    instantiate_data_list(
                        bquote(set.seed(.(self$seed))),
                        self$hidden_data_list),
                    self$instantiated_data)
        },

        get_type = function(opts, info) {
            self$type
        },

        get_title = function(opts, info) {
            self$title
        },

        get_data_chunk = function(opts, info) {
            l <- sanitize_language(self$local_instantiated_data)
            if (is.null(l))
                NULL
            else
                sprintf("```{r include = FALSE}\n%s\n```", answerstr(l))
        },

        get_rec_data_chunk = function(opts, info) {
            if (is.null(self$quiz))
                stop("No defined Quiz in question ", sQuote(self$title))

            l <- sanitize_language(self$quiz$recursive_instantiated_data())
            if (is.null(l))
                NULL
            else
                sprintf("```{r include = FALSE}\n%s\n```", answerstr(l))
        },

        # Data chunk for formatting answer
        get_answer_info = function(opts, info) {
            sprintf("```{r, include = FALSE}\n%s\nanswer <- {\n%s}\n```",
                    answerstr(quote(cquote <- function(s) {
                        if (is.character(s))
                            "`"
                        else if (is.numeric(s))
                            "$"
                        else stop("Argument is not character or numeric ", sQuote(s))
                    })),
                    answerstr(self$instantiated_answer))
        },

        get_evaluated_answer = function(opts, info) {
            "`r cquote(answer)``r answer``r cquote(answer)`"
        },

        get_evaluated_answer2 = function(opts, info) {
            # Set up environment for evaluating data if not already
            if (is.null(info$env)) {
                info0 <- self$get_data_and_environment()
                info <- update_list(info, info0)
            }

            eval(self$instantiated_answer, info$env)
        },

        get_seed_chunk = function(opts, info) {
            if (is.null(self$seed))
                NULL
            else
                sprintf("```{r}\nset.seed(%d)\n```", self$seed)
        },

        get_text = function(opts, info) {
            self$text
        },

        get_inst_text = function(opts, info) {
            if (!is.null(opts$numbered) && opts$numbered) {
                paste0("**Question ", info$num, " :** ", self$instantiated_text)
            } else
                self$instantiated_text
        },

        get_inst_cookie = function(opts, info) {
            stop("Abstract method")
        },

        get_inst_text_and_cookie = function(opts, info) {
            sprintf("%s\n%s", self$get_inst_text(opts, info), self$get_inst_cookie(opts, info))
        },

        get_inst_text_and_number = function(opts, info) {
            stopifnot(is.numeric(info$index))
            sprintf("%s (%d)", self$get_inst_text(opts, info), info$index)
        },

        get_guess = function() {

        },

        get_is_correct_icon = function() {

        },

        instantiate_hidden_data_list = function(var_list = NULL, seed_init = FALSE) {
            if (!seed_init & !is_empty_language(self$hidden_data) & is.null(self$hidden_seed))
                stop("Some hidden data but no seed to instantiate them")

            # VAR_LIST must be a named list for list2env to work
            if (is.null(var_list)) var_list <- list()
            env <- list2env(var_list, envir = empty_env())

            # Set seed if any and eval hidden data
            if (!is.null(self$hidden_seed))
                eval(bquote(set.seed(.(self$hidden_seed))), envir = env)
            eval(self$hidden_data, envir = env)

            # Update variables in VAR_LIST with ENV
            new_var_list <- update_list(var_list, as.list(env, all.names = TRUE))

            self$hidden_data_list <- new_var_list
            ## private$.hidden_data_list <- new_var_list
            ## private$is_hidden_data_list_available <- TRUE
        },

        instantiate_feedback_list = function(feedback, var_list) {
            text <- instantiate_object(feedback$text, self$hidden_data_list)
            noeval_text <- instantiate_object(feedback$noeval_text, self$hidden_data_list)

            feedback$text <- text
            feedback$noeval_text <- noeval_text

            feedback
        },

        hidden_data_names = function() {
            env <- empty_env()
            if (!is.null(self$hidden_seed))
                eval(bquote(set.seed(.(self$hidden_seed))), envir = env)
            eval(self$hidden_data, envir = env)
            ls(envir = env, all.names = TRUE)
        },

        rename = function(prefix, names = self$hidden_data_names()) {
            self$rename_text(prefix, names)
            self$rename_header(prefix, names)
            self$rename_answer(prefix, names)
            self$rename_feedback(prefix, names)
            self$rename_data(prefix, names)
            self$rename_hidden_data(prefix, names)
            self$invalidate_text()
            self$invalidate_answer()
            self$invalidate_hidden_data()
            self$invalidate_hidden_data_list()
            self$invalidate_feedback()
            self$invalidate_data()

            self
        },

        rename_header = function(prefix, names = self$hidden_data_names()) {
            self$header <- prefix_object(prefix, names, self$header)

            self
        },

        rename_text = function(prefix, names = self$hidden_data_names()) {
            self$text <- prefix_object(prefix, names, private$.text)
            self$invalidate_text()

            self
        },

        rename_answer = function(prefix, names = self$hidden_data_names()) {
            self$answer <- prefix_object(prefix, names, self$answer)
            self$invalidate_answer()

            self
        },

        rename_feedback = function(prefix, names = self$hidden_data_names()) {
            feedback <- self$feedback
            text <- prefix_object(prefix, names, feedback$text)
            noeval_text <- prefix_object(prefix, names, feedback$noeval_text)

            if (!is.null(text))
                feedback$text <- text
            if (!is.null(noeval_text))
                feedback$noeval_text <- noeval_text

            self$feedback <- feedback
            self$invalidate_feedback()

            self
        },

        rename_data = function(prefix, names = self$hidden_data_names()) {
            self$data <- prefix_object(prefix, names, self$data)
            self$invalidate_data()

            self
        },

        rename_hidden_data = function(prefix, names = self$hidden_data_names()) {
            self$hidden_data <- prefix_object(prefix, names, self$hidden_data)
            self$invalidate_hidden_data()

            self
        },

        copy = function() {
            Question(self$text,
                     type = self$type,
                     seed = self$seed,
                     hidden_seed = self$hidden_seed,
                     hidden_data = self$hidden_data,
                     data = self$data,
                     answer = self$answer,
                     feedback = self$feedback)
        }
    ),
    active = list(
        title = function(title) {
            if (missing(title)) {
                if (is.null(private$.title)) {
                    if (nchar(self$instantiated_text) > 60)
                        paste0(substr(self$instantiated_text, 0, 57), "...")
                    else
                        self$instantiated_text
                } else private$.title
            } else {
                private$.title <- title
            }
        },

        header = function(header) {
            if (missing(header)) {
                private$.header
            } else {
                private$.header <- header
                self$invalidate_header()
                private$.header
            }
        },

        ancestor = function(ancestor) {
            if (missing(ancestor)) {
                private$.ancestor
            } else {
                private$.ancestor <- ancestor
                self$invalidate_ancestor()
                private$.ancestor
            }
        },

        text = function(text) {
            if (missing(text)) {
                paste(c(trimws(self$header), trimws(private$.text)), collapse = "\n\n")
            } else {
                private$.text <- text
                self$invalidate_inst_text()
            }
        },

        instantiated_text = function() {
            if (private$is_text_instantiated)
                private$.instantiated_text
            else {
                private$.instantiated_text <- instantiate_text_list(
                    self$text,
                    self$hidden_data_list)
                private$is_text_instantiated <- TRUE
                private$.instantiated_text
            }
        },

        answer = function(answer) {
            if (missing(answer)) {
                private$.answer
            } else {
                private$.answer <- answer
                self$invalidate_inst_answer()
            }
        },

        instantiated_answer = function() {
            if (private$is_answer_instantiated)
                private$.instantiated_answer
            else {
                private$.instantiated_answer <- instantiate_object(
                    self$answer,
                    self$hidden_data_list)
                private$is_answer_instantiated <- TRUE
                private$.instantiated_answer
            }
        },

        feedback = function(feedback) {
            if (missing(feedback)) {
                if (private$is_feedback_available)
                    private$.feedback
                else {
                    private$.feedback <- self$get_feedback_from_field(private$.feedback)
                    private$is_feedback_available <- TRUE
                    private$.feedback
                }
            } else {
                private$.feedback <- self$get_feedback_from_field(feedback)
                self$invalidate_inst_feedback()
                private$is_feedback_available <- TRUE
            }
        },

        instantiated_feedback = function() {
            if (private$is_feedback_instantiated)
                private$.instantiated_feedback
            else {
                inst_feedback <- self$instantiate_feedback_list(self$feedback, self$hidden_data_list)
                private$is_feedback_instantiated <- TRUE
                private$.instantiated_feedback <- inst_feedback
            }
        },

        hidden_seed = function(seed) {
            if (missing(seed))
                private$.hidden_seed
            else {
                private$.hidden_seed <- seed
                self$invalidate_ancestor()
            }
        },

        hidden_data = function(hidden_data) {
            if (missing(hidden_data)) {
                private$.hidden_data
            } else {
                private$.hidden_data <- hidden_data
                self$invalidate_ancestor()
            }
        },

        hidden_data_list = function(hidden_data_list) {
            if (missing(hidden_data_list)) {
                if (private$is_hidden_data_list_available)
                    private$.hidden_data_list
                else {
                    root <- self
                    while (!is.null(root$ancestor))
                        root <- root$ancestor
                    root$instantiate_hidden_data_list()
                    private$is_hidden_data_list_available <- TRUE
                    private$.hidden_data_list
                }
            } else {
                private$is_hidden_data_list_available <- TRUE
                private$.hidden_data_list <- hidden_data_list

                ## Need to be recomputed
                self$invalidate_inst_text()
                self$invalidate_inst_answer()
                self$invalidate_inst_feedback()
                self$invalidate_inst_data()
            }
        },

        seed = function(seed) {
            if (missing(seed))
                private$.seed
            else {
                private$.seed <- seed
            }
        },

        data = function(data) {
            if (missing(data)) {
                private$.data
            } else {
                private$.data <- data
                self$invalidate_inst_data()
            }
        },

        instantiated_data = function() {
            if (private$is_data_instantiated)
                private$.instantiated_data
            else {
                private$.instantiated_data <- instantiate_data_list(
                    private$.data,
                    self$hidden_data_list)
                private$is_data_instantiated <- TRUE
                private$.instantiated_data
            }
        },

        local_instantiated_data = function() {
            if(is.null(self$seed))
                self$instantiated_data
            else
                merge_languages(
                    instantiate_data_list(
                        bquote(set.seed(.(self$seed))),
                        self$hidden_data_list),
                    self$instantiated_data)
        },

        tag = function(tag) {
            if (missing(tag)) {
                private$.tag
            } else {
                private$.tag <- tag
            }
        }
    ),
    private = list(
        .title = NULL,

        .header = NULL,

        .ancestor = NULL,

        .text = NULL,
        is_text_available = FALSE,

        .answer = NULL,
        is_answer_available = FALSE,

        .feedback = NULL,
        is_feedback_available = FALSE,

        .data = NULL,
        is_data_available = FALSE,

        .seed = NULL,

        .hidden_seed = NULL,

        .hidden_data = NULL,
        is_hidden_data_available = FALSE,

        .hidden_data_list = NULL,
        is_hidden_data_list_available = FALSE,

        .instantiated_text = NULL,
        is_text_instantiated = FALSE,

        .instantiated_answer = NULL,
        is_answer_instantiated = FALSE,

        .instantiated_feedback = NULL,
        is_feedback_instantiated = FALSE,

        .instantiated_data = NULL,
        is_data_instantiated = FALSE,

        .tag = NULL,

        default_feedback_options = list(text = "", noeval_text = NULL, eval = TRUE, indent = 2),
        default_options = list(numbered = TRUE,
                               export = "markdown",
                               feedback = TRUE,
                               quiet = FALSE),

        xml_default_options = list(
            numbered = FALSE,
            indent = 0
        ),

        feedback_options = NULL,
        placeholders = NULL,
        placeholders_regex = NULL,

        validate_options = function(opts, info) {
            if (!is.null(opts$numbered) && opts$numbered && is.null(info$num))
                stop("`numbered` option is enabled but no `num` available")
        },

        validate_feedback_options = function(opts, info) {
            ## super$validate_feedback_options(opts, info)
        },

        xml_question_template = trimws("
<question type=\"@TYPE@\">
  <name>
    <text><![CDATA[@TITLE@]]></text>
  </name>
  <questiontext format=\"html\">
    <text><![CDATA[@XML_QUESTION_TEXT@]]></text>
  </questiontext>
  <answer fraction=\"100\" format=\"plain_text\">
    <text>@XML_ANSWER@</text>
  </answer>
@XML_GENERALFEEDBACK@
  <hidden>0</hidden>
</question>
"),
xml_placeholders = NULL))
