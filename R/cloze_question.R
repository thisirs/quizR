#' @include simple_question.R
#' @include numerical_question.R
#' @include shortanswer_question.R
#' @include multiple_choice.R
NULL

#' @export
ClozeQuestion <- R6::R6Class(
    "Cloze",
    inherit = SimpleQuestion,
    public = list(
        type = "cloze",
        initialize = function(text = NULL,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              questions = NULL,
                              subquestions_opts = NULL,
                              tags = NULL) {

            if(is.null(text) & is.null(questions))
                stop("Either ", sQuote("text"), " or ", sQuote("questions"), " has to be specified")

            if(!is.null(text)) {
                super$initialize(text,
                                 data = data,
                                 hidden_data = hidden_data,
                                 seed = seed,
                                 hidden_seed = hidden_seed,
                                 feedback = feedback,
                                 answer = answer,
                                 tags = tags)

                self$subquestions <- private$get_subquestions(text, answer, feedback, subquestions_opts)

                if (!is.null(hidden_data))
                    private$.cloze_hidden_data <- hidden_data

                if (!is.null(data))
                    private$.cloze_data <- data

                # Invalidate all
                self$invalidate_all()
            } else if (!is.null(questions)) {

                ## Checking that answer, feedback and subquestions_opts are of the right size
                if (!is.null(answer))
                    stopifnot(length(answer) == length(questions))
                if (!is.null(feedback)) {
                    if (is.null(feedback$header))
                        stopifnot(length(feedback) == length(questions))
                    else if (length(feedback$feedbacks) != 0)
                        stopifnot(length(feedback$feedbacks) == length(questions))
                }
                if (!is.null(subquestions_opts))
                    stopifnot(length(subquestions_opts) == length(questions))


                super$initialize(text,
                                 data = data,
                                 hidden_data = hidden_data,
                                 seed = seed,
                                 hidden_seed = hidden_seed,
                                 feedback = feedback,
                                 answer = answer,
                                 tags = tags)

                ## hidden_data is used to store the merged hidden_data of subquestions
                if (!is.null(hidden_data))
                    private$.cloze_hidden_data <- hidden_data

                if (!is.null(data))
                    private$.cloze_data <- data

                ## Clone questions that will be modified
                questions <- lapply(questions, function(question) question$copy())

                for (question in questions)
                    question$invalidate_all()

                ## Rename subquestions to avoid hidden_data overlap
                ## self$subquestions <- questions # For self$hidden_data_list to succeed
                for (i in seq_along(questions)) {
                    prefix <- sprintf("data%s_", i)
                    questions[[i]]$rename(prefix)
                }

                ## Connect subquestions with cloze question
                for (i in seq_along(questions)) {
                    questions[[i]]$ancestor <- self
                    if (is.null(self$quiz))
                        questions[[i]]$update_quiz(self)
                    else
                        questions[[i]]$update_quiz(self$quiz)
                }

                ## Modify answer and feedback of subquestions
                if (!is.null(answer))
                    for (i in seq_len(length(questions)))
                        if (!is.null(answer[[i]]))
                            questions[[i]]$answer <- answer[[i]]

                if (!is.null(feedback)) {
                    feedback0 <- self$sanitize_feedback(feedback)

                    cloze_feedback <- feedback0
                    cloze_feedback$feedbacks <- NULL
                    self$cloze_feedback <- cloze_feedback

                    for (i in seq_len(length(questions)))
                        if (!is.null(feedback0$feedbacks[[i]]))
                            questions[[i]]$feedback <- feedback0$feedbacks[[i]]
                }

                self$subquestions <- questions # For self$hidden_data_list to succeed

                # Invalidate all
                self$invalidate_all()

                private$.subquestions_opts <- subquestions_opts
            } else stop("Logical flaw")

            private$cloze_cookie_list <- list(
                SHORTANSWER = private$get_shortanswer_answer_cookie,
                SA = private$get_shortanswer_answer_cookie,
                MW = NULL,
                SHORTANSWER_C = NULL,
                SAC = NULL,
                MWC = NULL,
                NUMERICAL = NULL,
                NM = NULL,
                MULTICHOICE = private$get_multiple_choice_cookie,
                MC = private$get_multiple_choice_cookie,
                MULTICHOICE_V = NULL,
                MCV = NULL,
                MULTICHOICE_H = NULL,
                MCH = NULL)

            # Add new placeholders for cloze questions
            private$placeholders$FEEDBACKS <- "get_feedbacks"

            # Add new feedback options for cloze questions
            private$default_feedback_options$header = NULL
        },

        update_quiz = function(quiz) {
            self$quiz <- quiz
            for (question in self$subquestions)
                question$update_quiz(quiz)
            self$invalidate_hidden_data_list()
        },

        sanitize_feedback = function(feedback) {
            if ("header" %in% names(feedback))
                feedback
            else if ("feedbacks" %in% names(feedback))
                feedback
            else
                list(header = NULL, feedbacks = feedback)
        },

        update_feedback = function(feedback) {
            feedback <- self$sanitize_feedback(feedback)

            if (length(feedback$feedbacks) != 0)
                stopifnot(length(feedback$feedbacks) == self$num)

            for (i in seq_along(self$subquestions))
                self$subquestions[[i]]$feedback <- feedback$feedbacks[[i]]

            feedback
        },

        instantiate_hidden_data_list = function(var_list = NULL, seed_init = FALSE) {
            if (!seed_init & !is_empty_language(private$.cloze_hidden_data) & is.null(self$hidden_seed))
                stop("Hidden data but no seed to instantiate it")

            ## if (!seed_init && is.null(self$hidden_seed)) {
            ##     seed_needed <- sapply(self$subquestions, function(question) {
            ##         is.null(question$hidden_seed) & !is_empty_language(question$hidden_data)
            ##     })
            ##     if(any(seed_needed))
            ##         stop("Hidden data in a subquestion but no seed to instantiate it")
            ## }

            if (is.null(var_list)) var_list <- list()
            env <- list2env(var_list, envir = empty_env())

            # Set seed if any and eval hidden data
            if (!is.null(private$.cloze_hidden_data))
                eval(bquote(set.seed(.(self$hidden_seed))), envir = env)
            eval(private$.cloze_hidden_data, envir = env)

            # Update variables in VAR_LIST with ENV
            new_var_list <- update_list(var_list, as.list(env, all.names = TRUE))

            self$hidden_data_list <- new_var_list
            ## private$.hidden_data_list <- new_var_list
            ## private$is_hidden_data_list_available <- TRUE

            for (question in self$subquestions)
                question$instantiate_hidden_data_list(new_var_list, seed_init | !is.null(self$hidden_seed))
        },

        get_inst_text = function(opts, info) {
            if (is.null(opts$export))
                stop("Need an export type for cloze question")

            if (opts$export == "xml") {
                inst_text <- self$get_inst_text_and_cookie(opts, info)
            } else if (opts$export == "markdown") {
                info$index <- 1
                inst_text <- self$get_inst_text_and_number(opts, info)
            } else stop("Unknown export type: ", sQuote(opts$export))

            if (!is.null(opts$numbered) &&opts$numbered) {
                paste0("**Question ", info$num, " :** ", inst_text)
            } else
                inst_text
        },

        get_inst_cookie = function(opts, info) {
            stop("Not needed")
        },

        get_inst_text_and_cookie = function(opts, info) {
            inst_texts <- lapply(self$subquestions, function(question) {
                # Use hidden data list from self
                ## question$hidden_data_list <- self$hidden_data_list
                question$get_inst_text_and_cookie(opts, info)
            })

            inst_header <- instantiate_text_list(self$header, self$hidden_data_list)
            paste(c(inst_header, inst_texts), collapse = "\n\n")
        },

        get_inst_text_and_number = function(opts, info) {
            stopifnot(is.numeric(info$index))

            inst_texts <- NULL
            for (question in self$subquestions) {
                # Use hidden data list from self
                ## question$hidden_data_list <- self$hidden_data_list

                opts$numbered <- FALSE

                text <- question$get_inst_text_and_number(opts, info)
                inst_texts <- c(inst_texts, text)
                info$index <- if (question$type == "cloze")
                                     info$index + question$total_num
                                 else
                                     info$index + 1
            }

            inst_header <- instantiate_text_list(self$header, self$hidden_data_list)
            paste(c(inst_header, inst_texts), collapse = "\n\n")
        },

        # Get feedback of each subquestion
        get_feedbacks = function(opts, info) {
            if (is.null(info$starting_index))
                current_index <- 1
            else
                current_index <- info$starting_index

            feedbacks <- list()
            for (question in self$subquestions) {
                # Use hidden data list from self
                ## question$hidden_data_list <- self$hidden_data_list

                # Set options for the feedback
                if (question$type == "cloze") {
                    info$starting_index <- current_index
                    opts$inner_cloze <- TRUE
                } else {
                    info$answer_string <- sprintf("%d. La r\u00E9ponse est : ", current_index)
                }

                # We are in an itemized env, add indentation
                opts$indent = 4

                tmpl <- question$get_feedback(opts, info)
                final_feedback <- question$get_markdown_from_template(tmpl, opts, info)
                # final_feedback <- add_spaces_left(final_feedback, 4)

                current_index <- if (question$type == "cloze")
                                     current_index + question$total_num
                                 else
                                     current_index + 1

                feedbacks <- c(feedbacks, final_feedback)
            }

            paste(c(self$instantiated_feedback$header, feedbacks), collapse = "\n\n")
        },

        get_default_feedback = function() {

        },

        get_feedback_from_field = function(feedback = self$feedback) {
            feedback <-
                if(is.null(feedback)) {
                    self$get_default_feedback()
                } else if(is.character(feedback)) {
                    lapply(feedback, function(f) {
                        list(text = f)
                    })
                } else if(is.language(feedback)) {
                    lapply(feedback, function(f) {
                        list(text = f)
                    })
                } else if(is.list(feedback)) {
                    feedback
                }
            update_list(private$default_feedback_options, feedback)
        },

        get_feedback = function(opts, info) {
            # No feedback
            if (!is.null(opts$feedback) && !opts$feedback)
                return(NULL)
            if (!is.null(opts$inner_cloze) && opts$inner_cloze)
                tmpl <- "@FEEDBACKS@"
            else
                tmpl <- "**R\u00E9ponses :**\n\n@FEEDBACKS@"
            self$instantiate_placeholders(tmpl, private$placeholders, opts, info)
        },

        get_XML_answer = function(opts, info) {
            "*"
        },

        get_feedback_answer = function(opts, info) {
            stop("Undefined for a cloze question")
        },

        get_answer_info = function(opts, info) {
            stop("Undefined for a cloze question")

        },

        instantiate_feedback_list = function(feedback, var_list) {
            header <- feedback$header
            inst_header <- instantiate_object(header, self$hidden_data_list)

            list(header = inst_header,
                 lapply(self$subquestions, function(question) {
                     ## question$hidden_data_list <- self$hidden_data_list
                     question$instantiated_feedback
                 }))
        },

        rename = function(prefix, names = self$hidden_data_names()) {
            self$rename_header(prefix, names)
            self$data <- prefix_object(prefix, names, private$.cloze_data)
            self$hidden_data <- prefix_object(prefix, names, private$.cloze_hidden_data)
            for (question in self$subquestions)
                question$rename(prefix, names)
            self$invalidate_text()
            self$invalidate_answer()
            self$invalidate_hidden_data()
            self$invalidate_hidden_data_list()
            self$invalidate_feedback()
            self$invalidate_data()
        },

        rename_text = function(prefix, names = self$hidden_data_names()) {
            for (question in self$subquestions)
                question$rename_text(prefix, names)
            self$invalidate_text()
            self$invalidate_header()
        },

        rename_answer = function(prefix, names = self$hidden_data_names()) {
            for (question in self$subquestions)
                question$rename_answer(prefix, names)
            self$invalidate_answer()
        },

        rename_feedback = function(prefix, names = self$hidden_data_names()) {
            for (question in self$subquestions)
                question$rename_feedback(prefix, names)
            self$invalidate_feedback()
        },

        rename_data = function(prefix, names = self$hidden_data_names()) {
            self$data <- prefix_object(prefix, names, private$.cloze_data)
            for (question in self$subquestions)
                question$rename_data(prefix, names)
            self$invalidate_data()
        },

        rename_hidden_data = function(prefix, names = self$hidden_data_names()) {
            self$hidden_data <- prefix_object(prefix, names, private$.cloze_hidden_data)
            for (question in self$subquestions)
                question$rename_hidden_data(prefix, names)
            self$invalidate_hidden_data()
        },

        copy = function() {
            subquestions <- lapply(self$subquestions, function(question) {
                question$copy()
            })

            ClozeQuestion$new(
                data = self$data,
                hidden_data = self$hidden_data,
                seed = self$seed,
                hidden_seed = self$hidden_seed,
                ## feedback = self$feedback,
                ## answer = self$answer,
                questions = subquestions,
                subquestions_opts = self$subquestions_opts)
        }
    ),

    private = list(
        .subquestions = NULL,

        .subquestions_opts = NULL,

        .cloze_data = NULL,

        is_cloze_hidden_data_available = FALSE,
        .cloze_hidden_data = NULL,

        is_cloze_feedback_available = FALSE,
        .cloze_feedback = NULL,

        .header = NULL,
        is_header_instantiated = FALSE,

        cloze_regex = "\\{(\\d+):(SHORTANSWER|SA|MW|SHORTANSWER_C|SAC|MWC|NUMERICAL|NM|MULTICHOICE|MC|MULTICHOICE_V|MCV|MULTICHOICE_H|MCH):=([^\\}]*)\\}",

        type_to_cloze_id = list(
            numerical = "NM",
            shortanswer = "SA",
            multichoice = "MC"
        ),

        questions_list = list(
            SHORTANSWER = ShortanswerQuestion,
            SA = ShortanswerQuestion,
            MW = NULL,
            SHORTANSWER_C = NULL,
            SAC = NULL,
            MWC = NULL,
            NUMERICAL = NumericalQuestion,
            NM = NumericalQuestion,
            MULTICHOICE = NULL,
            MC = MultipleChoice,
            MULTICHOICE_V = NULL,
            MCV = NULL,
            MULTICHOICE_H = NULL,
            MCH = NULL),

        cloze_cookie_list = NULL,

        get_multiple_choice_cookie = function(points, answer, opts) {
            if (is.null(opts$multichoice_type)) opts$multichoice_type <- "inline"
            type <- switch(opts$multichoice_type,
                           inline = "MULTICHOICE",
                           vectical = "MULTICHOICE_V",
                           horizontal = "MULTICHOICE_H")

            answers <- sapply(seq_along())

            sprintf("{%s:%s:%s}", points, type, answers)
        },

        get_shortanswer_answer_cookie = function(points, answer, opts) {
            sprintf("{%s:SA:=%s}", points, answer)
        },

        get_data_from_questions = function() {
            languages <- lapply(self$subquestions, function(question) {
                question$data
            })
            merge_languages(languages)
        },

        get_inst_data_from_questions = function() {
            languages <- lapply(self$subquestions, function(question) {
                question$instantiated_data
            })
            merge_languages(languages)
        },

        get_hidden_data_from_questions = function(questions = self$subquestions) {
            languages <- lapply(questions, function(question) {
                question$hidden_data
            })

            merge_languages(languages)
        },

        replace_cloze_cookies = function(text, opts, info) {
            loc <- stringi::stri_locate_all_regex(text, private$cloze_regex, omit_no_match = TRUE)[[1]]
            matches <- stringi::stri_match_all_regex(text, private$cloze_regex, omit_no_match = TRUE)[[1]]

            stopifnot(nrow(loc) == self$total_num)
            stopifnot(nrow(matches) == self$total_num)
            stopifnot(self$total_num == length(self$subquestions_opts))

            replacements <-
                lapply(seq_len(nrow(loc)), function(i) {
                    points <- matches[i, 2]
                    type <- matches[i, 3]
                    rest <- matches[i, 4]
                    method <- private$cloze_cookie_list[[type]]
                    if (is.null(method))
                        stop("Unsupported question type:", sQuote(type))
                    opts <- self$subquestions_opts[[i]]

                    answer <- self$instantiated_answer[[i]]
                    eval_answer <- eval(answer, envir = info$env)

                    method(points, eval_answer, opts)
                })

            for (i in rev(seq_len(nrow(loc)))) {
                stringi::stri_sub(text, loc[i, 1], loc[i, 2]) <- replacements[[i]]
            }

            text
        },

        get_text_from_questions = function() {
            texts <- sapply(self$subquestions, function(question) {
                if (question$type == "cloze") {
                    question$text
                } else {
                    id <- private$type_to_cloze_id[[question$type]]
                    if (is.null(id))
                        stop("No identifier ", sQuote(question$type), " in ", sQuote("type_to_cloze_id"))
                    sprintf("%s\n{1:%s:=}", question$text, id)
                }
            })
            paste(texts, collapse = "\n\n")
        },

        get_answer_from_questions = function() {
            answers <- lapply(self$subquestions, function(question) {
                if (question$type == "cloze") {
                    question$answer
                } else {
                    list(question$answer)
                }
            })

            unlist(answers, recursive = FALSE)
        },

        get_inst_answer_from_questions = function() {
            inst_answers <- lapply(self$subquestions, function(question) {
                if (question$type == "cloze") {
                    question$instantiated_answer
                } else {
                    list(question$instantiated_answer)
                }
            })

            unlist(inst_answers, recursive = FALSE)
        },

        get_inst_feedback_from_questions = function() {
            feedback <- self$cloze_feedback

            inst_feedbacks <- lapply(self$subquestions, function(question) {
                if (question$type == "cloze") {
                    question$instantiated_feedback$feedbacks
                } else {
                    list(question$instantiated_feedback)
                }
            })

            feedbacks <- unlist(inst_feedbacks, recursive = FALSE)
            feedback$feedbacks <- feedbacks
            feedback
        },


        get_inst_text_from_questions = function() {
            inst_texts <- sapply(self$subquestions, function(question) {
                if (question$type == "cloze") {
                    question$instantiated_text
                } else {
                    id <- private$type_to_cloze_id[[question$type]]
                    if (is.null(id))
                        stop("No identifier ", sQuote(question$type), " in ", sQuote("type_to_cloze_id"))
                    sprintf("%s\n{1:%s:=}", question$instantiated_text, id)
                }
            })
            paste(inst_texts, collapse = "\n\n")
        },

        get_feedback_from_questions = function() {
            feedback <- lapply(self$subquestions, function(question) {
                if (question$type == "cloze") {
                    question$feedback$feedbacks
                } else {
                    list(question$feedback)
                }
            })

            unlist(feedback, recursive = FALSE)
        },


        get_subquestions = function(text, answer = NULL, feedback = NULL, subquestions_opts = NULL) {
            loc <- stringi::stri_locate_all_regex(text, private$cloze_regex, omit_no_match = TRUE)[[1]]
            N <- nrow(loc)

            stopifnot(N > 0)
            if (length(answer) > 0)
                stopifnot(N == length(answer))
            if (length(feedback) > 0) {
                feedback <- self$sanitize_feedback(feedback)
                stopifnot(N == length(feedback$feedbacks))
            }
            if (length(subquestions_opts) > 0)
                stopifnot(N == length(subquestions_opts))

            cloze_feedback <- feedback
            cloze_feedback$feedbacks <- NULL
            self$cloze_feedback <- cloze_feedback

            texts <- trimws(substring(text, c(1, loc[seq_len(N-1), "end"] + 1), loc[, "start"] - 1))
            cookies <- trimws(substring(text, loc[, "start"], loc[, "end"]))
            stopifnot(length(texts) == length(cookies))

            lapply(1:N, function(i) {
                text <- texts[i]
                cookie <- cookies[i]
                match <- stringi::stri_match_first_regex(cookie, private$cloze_regex)[1,]

                qclass <- private$questions_list[[match[3]]]
                if (is.null(qclass)) stop("No corresponding class for ", sQuote(match[3]))
                question <- do.call(qclass$new, c(list(text,
                                                        answer = answer[[i]],
                                                        feedback = feedback$feedbacks[[i]]),
                                                   subquestions_opts[[i]]), quote = TRUE)

                question$ancestor  <- self

                if (is.null(self$quiz))
                    question$update_quiz(self)
                else
                    question$update_quiz(self$quiz)

                question
            })
        },

        # Return list of questions from text, answer and feedback fields
        # get_subquestions_from_text = function() {
        #     matches <- stringi::stri_match_all_regex(self$text, private$cloze_regex)[[1]]

        #     if (any(is.na(matches))) {
        #         self$answer <- list()
        #         self$feedback <- list()
        #         self$subquestions_opts <- list()

        #         list()
        #     } else {
        #         if (!private$is_answer_available || length(self$answer) != nrow(matches))
        #             self$answer <- vector(mode = "list", length = nrow(matches))

        #         if (!private$is_feedback_available || length(self$feedback) != nrow(matches))
        #             self$feedback <- vector(mode = "list", length = nrow(matches))

        #         if (is.null(self$subquestions_opts) || length(self$subquestions_opts) != nrow(matches))
        #             self$subquestions_opts <- vector(mode = "list", length = nrow(matches))

        #         lapply(seq_len(nrow(matches)), function(i) {
        #             qclass <- private$questions_list[[matches[i, 3]]]
        #             if (is.null(qclass)) stop("No corresponding class for ", sQuote(matches[i, 3]))
        #             do.call(qclass$new, c(list(sprintf("Subquestion number %d", i),
        #                                        answer = self$answer[[i]],
        #                                        feedback = self$feedback[[i]]),
        #                                   self$subquestions_opts[[i]]), quote = TRUE)
        #         })
        #     }
        # },

        replace_cloze_fields = function(text) {
            for (i in seq_len(self$total_num)) {
                text <- stringi::stri_replace_first_regex(text, private$cloze_regex, paste0("(", i, ")"))
            }
            return(text)
        }
    ),

    active = list(

        data = function(data) {
            if (missing(data)) {
                if (!private$is_data_available) {
                    private$.data <- merge_languages(
                        private$.cloze_data,
                        private$get_data_from_questions())
                    self$invalidate_inst_data()
                    private$is_data_available <- TRUE
                }
                private$.data
            } else {
                private$.cloze_data <- data
                private$is_data_available <- FALSE
                self$invalidate_data()
                private$.cloze_data
            }
        },

        instantiated_data = function() {
            if (private$is_data_instantiated)
                private$.instantiated_data
            else {
                private$.instantiated_data <- merge_languages(
                    instantiate_data_list(
                        private$.cloze_data,
                        self$hidden_data_list),
                    private$get_inst_data_from_questions())
                private$is_data_instantiated <- TRUE
                private$.instantiated_data
            }
        },

        hidden_data = function(hidden_data) {
            if (missing(hidden_data)) {
                if (!private$is_hidden_data_available) {
                    private$.hidden_data <- merge_languages(
                        private$.cloze_hidden_data,
                        private$get_hidden_data_from_questions())
                    private$is_hidden_data_available <- TRUE
                }
                private$.hidden_data
            } else {
                private$.cloze_hidden_data <- hidden_data
                for (question in self$subquestions)
                    question$invalidate_hidden_data()

                ## TODO Replace by a simple invalidate function?
                if (is.null(self$ancestor))
                    self$invalidate_hidden_data()
                else
                    self$ancestor$invalidate_hidden_data()
                private$is_hidden_data_available <- FALSE # Trigger reconstruction

                private$.cloze_hidden_data
            }
        },

        cloze_hidden_data = function(hidden_data) {
            if (missing(hidden_data)) {
                private$.cloze_hidden_data
            } else {
                private$.cloze_hidden_data <- hidden_data
                self$invalidate_hidden_data()
                private$.cloze_hidden_data
            }
        },

        text = function(text) {
            if (missing(text)) {
                if (private$is_text_available)
                    private$.text
                else {
                    header_text <- c(trimws(self$header), trimws(private$get_text_from_questions()))
                    private$.text <- paste(header_text, collapse = "\n\n")
                    private$is_text_available <- TRUE
                    private$.text
                }
            } else {
                self$invalidate_answer()
                self$invalidate_feedback()
                self$invalidate_text()
                self$subquestions <- private$get_subquestions(text)
                text
            }
        },

        instantiated_text = function() {
            if (private$is_text_instantiated)
                private$.instantiated_text
            else {
                private$.instantiated_text <- private$get_inst_text_from_questions()
                private$is_text_instantiated <- TRUE
                private$.instantiated_text
            }
        },

        answer = function(answer) {
            if (missing(answer)) {
                if (private$is_answer_available) {
                    private$.answer
                } else {
                    private$.answer <- private$get_answer_from_questions()
                    private$is_answer_available <- TRUE
                    private$.answer
                }
            } else {
                if (length(answer) > 0)
                    stopifnot(length(answer) == self$num)

                for (i in seq_along(self$subquestions))
                    self$subquestions[[i]]$answer <- answer[[i]]

                self$invalidate_answer()
            }
        },

        instantiated_answer = function() {
            if (private$is_answer_instantiated)
                private$.instantiated_answer
            else {
                private$.instantiated_answer <- private$get_inst_answer_from_questions()
                private$is_answer_instantiated <- TRUE
                private$.instantiated_answer
            }
        },

        cloze_feedback = function(cloze_feedback) {
            if (missing(cloze_feedback)) {
                if (private$is_cloze_feedback_available)
                    private$.cloze_feedback
                else {
                    private$is_cloze_feedback_available <- TRUE
                    private$.cloze_feedback <- list(header = NULL)
                }
            } else {
                private$.cloze_feedback <- cloze_feedback
                private$is_cloze_feedback_available <- TRUE
                self$invalidate_feedback()
            }
        },

        feedback = function(feedback) {
            if (missing(feedback)) {
                if (private$is_feedback_available) {
                    private$.feedback
                } else {
                    feedbacks <- private$get_feedback_from_questions()
                    feedback <- self$cloze_feedback
                    feedback$feedbacks <- feedbacks
                    private$is_feedback_available <- TRUE
                    private$.feedback <- feedback
                }
            } else {
                feedback <- self$update_feedback(feedback)
                cloze_feedback <- feedback
                cloze_feedback$feedbacks <- NULL
                private$.cloze_feedback <- cloze_feedback

                self$invalidate_feedback() # Trigger reconstruction from subquestions
                self$invalidate_inst_feedback()
                private$.cloze_feedback <- cloze_feedback
            }
        },

        instantiated_feedback = function() {
            if (private$is_feedback_instantiated)
                private$.instantiated_feedback
            else {
                private$.instantiated_feedback <- private$get_inst_feedback_from_questions()
                private$is_feedback_instantiated <- TRUE
                private$.instantiated_feedback
            }
        },

        subquestions = function(subquestions) {
            if (missing(subquestions)) {
                private$.subquestions
            } else {
                private$.subquestions <- subquestions
                self$invalidate_text()
                self$invalidate_answer()
                self$invalidate_feedback()
                self$invalidate_hidden_data()
                self$invalidate_data()
                self$invalidate_hidden_data_list()
                private$.subquestions
            }
        },

        subquestions_opts = function() {
            sqo <- lapply(self$subquestions, function(sq) {
                if (sq$type == "cloze") {
                    sq$subquestions_opts
                } else {
                    list(NULL)
                }
            })
            unlist(sqo, recursive = FALSE)
        },

        num = function() length(self$subquestions),

        total_num = function() {
            sum(sapply(self$subquestions, function(question) {
                if (question$type == "cloze")
                    question$num
                else 1
            }))
        }
    )
)
