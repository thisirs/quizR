#' @include simple_question.R
#' @include numerical_question.R
#' @include shortanswer_question.R
#' @include multiple_choice.R
NULL

#' @export
ClozeQuestion <- R6::R6Class(
    "ClozeQuestion",
    inherit = SimpleQuestion,
    public = list(
        type              = "cloze",
        text_based        = NULL,
        subquestions      = NULL,
        cloze_hidden_data = NULL,
        cloze_data        = NULL,
        cloze_feedback    = NULL,
        subquestions_opts = NULL,

        initialize = function(text             = NULL,
                              data             = quote({}),
                              hidden_data      = quote({}),
                              seed             = NULL,
                              hidden_seed      = NULL,
                              feedback         = NULL,
                              answer           = NULL,
                              questions        = NULL,
                              subquestions_opts = NULL,
                              header           = NULL,
                              tags             = NULL,
                              options          = list()) {
            if (is.null(text) && is.null(questions))
                stop("Either 'text' or 'questions' must be specified")

            # Initialize base fields (self$text / self$hidden_data / etc. go
            # through active bindings, but ClozeQuestion's bindings are safe
            # with NULL values during construction)
            super$initialize(NULL,
                             data        = data,
                             hidden_data = hidden_data,
                             seed        = seed,
                             hidden_seed = hidden_seed,
                             header      = header,
                             tags        = tags,
                             options     = options)
            self$cloze_hidden_data <- hidden_data
            self$cloze_data        <- data
            self$cloze_feedback    <- list(header = NULL)
            self$subquestions_opts <- subquestions_opts

            if (!is.null(text)) {
                # Text-based construction
                self$text_based     <- TRUE
                parsed <- private$parse_subquestions(text, answer, feedback, subquestions_opts)
                self$subquestions   <- parsed$subquestions
                self$cloze_feedback <- parsed$cloze_feedback
            } else {
                self$text_based <- FALSE
                # Questions-list construction
                if (!is.null(answer))
                    stopifnot(length(answer) == length(questions))
                if (!is.null(feedback)) {
                    fb0 <- private$sanitize_cloze_feedback(feedback)
                    if (length(fb0$feedbacks) != 0)
                        stopifnot(length(fb0$feedbacks) == length(questions))
                }
                if (!is.null(subquestions_opts))
                    stopifnot(length(subquestions_opts) == length(questions))

                questions <- lapply(questions, function(q) q$copy())
                for (i in seq_along(questions))
                    questions[[i]]$rename(sprintf("data%s_", i))

                if (!is.null(answer))
                    for (i in seq_along(questions))
                        if (!is.null(answer[[i]]))
                            questions[[i]]$answer <- answer[[i]]

                if (!is.null(feedback)) {
                    fb0 <- private$sanitize_cloze_feedback(feedback)
                    self$cloze_feedback <- list(header = fb0$header)
                    for (i in seq_along(questions))
                        if (!is.null(fb0$feedbacks[[i]]))
                            questions[[i]]$feedback <- fb0$feedbacks[[i]]
                }

                self$subquestions <- questions
            }

            # Give each subquestion access to the cloze's own vars as parent context
            cloze_vars <- eval_hidden_data(self$cloze_hidden_data, self$hidden_seed, list())
            for (q in self$subquestions)
                q$parent_var_list <- cloze_vars
        },

        hidden_data_names = function() {
            lhs_assigned_names(self$cloze_hidden_data)
        },

        rename = function(prefix, names = self$hidden_data_names()) {
            self$rename_header(prefix, names)
            self$cloze_data        <- prefix_object(prefix, names, self$cloze_data)
            self$cloze_hidden_data <- prefix_object(prefix, names, self$cloze_hidden_data)
            for (q in self$subquestions) {
                all_names <- union(names, q$hidden_data_names())
                q$rename(prefix, all_names)
            }
            invisible(self)
        },

        rename_text = function(prefix, names = self$hidden_data_names()) {
            for (q in self$subquestions)
                q$rename_text(prefix, names)
            self$rename_header(prefix, names)
            invisible(self)
        },

        rename_answer = function(prefix, names = self$hidden_data_names()) {
            for (q in self$subquestions)
                q$rename_answer(prefix, names)
            invisible(self)
        },

        rename_feedback = function(prefix, names = self$hidden_data_names()) {
            for (q in self$subquestions)
                q$rename_feedback(prefix, names)
            invisible(self)
        },

        rename_data = function(prefix, names = self$hidden_data_names()) {
            self$cloze_data <- prefix_object(prefix, names, self$cloze_data)
            for (q in self$subquestions)
                q$rename_data(prefix, names)
            invisible(self)
        },

        rename_hidden_data = function(prefix, names = self$hidden_data_names()) {
            self$cloze_hidden_data <- prefix_object(prefix, names, self$cloze_hidden_data)
            for (q in self$subquestions)
                q$rename_hidden_data(prefix, names)
            invisible(self)
        },

        copy = function() {
            q <- ClozeQuestion$new(
                data        = self$cloze_data,
                hidden_data = self$cloze_hidden_data,
                seed        = self$seed,
                hidden_seed = self$hidden_seed,
                questions   = lapply(self$subquestions, function(q) q$copy()),
                options     = self$options)
            q$text_based <- self$text_based
            q
        }
    ),

    active = list(
        # text is always computed from subquestions
        text = function(value) {
            if (!missing(value)) {
                if (is.null(value)) return(invisible(self))
                self$text_based     <- TRUE
                parsed <- private$parse_subquestions(value, NULL, NULL, self$subquestions_opts)
                self$subquestions   <- parsed$subquestions
                self$cloze_feedback <- parsed$cloze_feedback
                return(invisible(self))
            }
            private$gen_text_from_subqs()
        },

        hidden_data = function(value) {
            if (!missing(value)) {
                self$cloze_hidden_data <- value
                cloze_vars <- eval_hidden_data(self$cloze_hidden_data, self$hidden_seed, list())
                for (q in self$subquestions)
                    q$parent_var_list <- cloze_vars
                return(invisible(self))
            }
            parts <- list()
            if (!is_empty_language(self$cloze_hidden_data))
                parts <- c(parts, list(self$cloze_hidden_data))
            for (q in self$subquestions) {
                hd <- if (inherits(q, "ClozeQuestion")) q$cloze_hidden_data else q$hidden_data
                if (!is_empty_language(hd))
                    parts <- c(parts, list(hd))
            }
            private$to_block(parts)
        },

        data = function(value) {
            if (!missing(value)) {
                self$cloze_data <- value
                return(invisible(self))
            }
            parts <- list()
            if (!is_empty_language(self$cloze_data))
                parts <- c(parts, list(self$cloze_data))
            for (q in self$subquestions) {
                d <- if (inherits(q, "ClozeQuestion")) q$cloze_data else q$data
                if (!is_empty_language(d))
                    parts <- c(parts, list(d))
            }
            private$to_block(parts)
        },

        # feedback: aggregated from subquestions
        feedback = function(value) {
            if (!missing(value)) {
                if (is.null(value)) {
                    self$cloze_feedback <- list(header = NULL)
                    return(invisible(self))
                }
                fb0 <- private$sanitize_cloze_feedback(value)
                self$cloze_feedback <- list(header = fb0$header)
                if (length(fb0$feedbacks) > 0) {
                    stopifnot(length(fb0$feedbacks) == length(self$subquestions))
                    for (i in seq_along(self$subquestions))
                        self$subquestions[[i]]$feedback <- fb0$feedbacks[[i]]
                }
                return(invisible(self))
            }
            header <- if (!is.null(self$cloze_feedback)) self$cloze_feedback$header else NULL
            feedbacks <- unlist(lapply(self$subquestions, function(sq) {
                if (inherits(sq, "ClozeQuestion"))
                    sq$feedback$feedbacks
                else {
                    fb <- sq$feedback
                    if (identical(fb$text, "") && !is.null(sq$answer))
                        fb$text <- sq$answer
                    list(fb)
                }
            }), recursive = FALSE)
            list(header = header, feedbacks = feedbacks)
        },

        # answer: list of answers from all subquestions
        answer = function(value) {
            if (!missing(value)) {
                if (is.null(value)) return(invisible(self))
                stopifnot(length(value) == length(self$subquestions))
                for (i in seq_along(self$subquestions))
                    self$subquestions[[i]]$answer <- value[[i]]
                return(invisible(self))
            }
            unlist(lapply(self$subquestions, function(sq) {
                if (inherits(sq, "ClozeQuestion")) sq$answer else list(sq$answer)
            }), recursive = FALSE)
        },

        num = function(value) {
            if (!missing(value)) stop("num is read-only")
            sum(sapply(self$subquestions, function(sq) {
                if (inherits(sq, "ClozeQuestion")) sq$num else 1L
            }))
        },

        total_num = function(value) {
            if (!missing(value)) stop("total_num is read-only")
            sum(sapply(self$subquestions, function(sq) {
                if (inherits(sq, "ClozeQuestion")) sq$total_num else 1L
            }))
        },

        hidden_data_list = function(value) {
            if (!missing(value)) stop("hidden_data_list is read-only")
            eval_hidden_data(self$cloze_hidden_data, self$hidden_seed, list())
        },

        instantiated_text = function(value) {
            if (!missing(value)) stop("instantiated_text is read-only")
            instantiate_text_list(self$text, private$combined_var_list())
        },

        instantiated_answer = function(value) {
            if (!missing(value)) stop("instantiated_answer is read-only")
            var_list <- private$combined_var_list()
            lapply(self$answer, function(a) instantiate_object(a, var_list))
        }
    ),

    private = list(
        combined_var_list = function() {
            var_list <- self$hidden_data_list
            for (sq in self$subquestions)
                var_list <- c(var_list, sq$hidden_data_list)
            var_list
        },

        cloze_regex = paste0(
            "\\{(\\d+):(SHORTANSWER|SA|MW|SHORTANSWER_C|SAC|MWC|",
            "NUMERICAL|NM|MULTICHOICE|MC|MULTICHOICE_V|MCV|MULTICHOICE_H|MCH)",
            ":=([^\\}]*)\\}"
        ),

        questions_list = list(
            SHORTANSWER = ShortanswerQuestion, SA = ShortanswerQuestion,
            NUMERICAL   = NumericalQuestion,   NM = NumericalQuestion,
            MC          = MultipleChoice
        ),

        sanitize_cloze_feedback = function(feedback) {
            if (is.null(feedback))
                return(list(header = NULL, feedbacks = list()))
            if ("header" %in% names(feedback) || "feedbacks" %in% names(feedback))
                feedback
            else
                list(header = NULL, feedbacks = feedback)
        },

        cookie_type_id = function(sq) {
            list(numerical = "NM", shortanswer = "SA", multichoice = "MC")[[sq$type]]
        },

        make_cookie = function(sq) {
            type_id <- private$cookie_type_id(sq)
            if (is.null(type_id)) stop("No cookie type for: ", sq$type)
            sprintf("{1:%s:=}", type_id)
        },

        gen_text_from_subqs = function() {
            parts <- lapply(self$subquestions, function(sq) {
                if (inherits(sq, "ClozeQuestion"))
                    sq$text
                else
                    paste(sq$text, private$make_cookie(sq), sep = "\n")
            })
            if (!is.null(self$header))
                parts <- c(list(self$header), parts)
            paste(parts, collapse = "\n\n")
        },

        to_block = function(parts) {
            if (length(parts) == 0) return(quote({}))
            if (length(parts) == 1) {
                p <- parts[[1]]
                if (is.call(p) && identical(p[[1]], as.name("{"))) return(p)
                return(as.call(c(list(as.name("{")), list(p))))
            }
            merge_languages(parts)
        },

        parse_subquestions = function(text, answer = NULL, feedback = NULL,
                                      subquestions_opts = NULL) {
            loc <- stringi::stri_locate_all_regex(text, private$cloze_regex,
                                                   omit_no_match = TRUE)[[1]]
            N <- nrow(loc)
            stopifnot(N > 0)

            if (!is.null(answer))  stopifnot(length(answer) == N)
            if (!is.null(subquestions_opts)) stopifnot(length(subquestions_opts) == N)

            cloze_feedback <- list(header = NULL)
            if (!is.null(feedback)) {
                feedback <- private$sanitize_cloze_feedback(feedback)
                cloze_feedback <- list(header = feedback$header)
                stopifnot(length(feedback$feedbacks) == N)
            }

            texts   <- trimws(substring(text,
                                        c(1, loc[seq_len(N - 1), "end"] + 1),
                                        loc[, "start"] - 1))
            cookies <- trimws(substring(text, loc[, "start"], loc[, "end"]))

            subqs <- lapply(seq_len(N), function(i) {
                match  <- stringi::stri_match_first_regex(cookies[i], private$cloze_regex)[1, ]
                qclass <- private$questions_list[[match[3]]]
                if (is.null(qclass))
                    stop("No class for cloze type: ", sQuote(match[3]))
                do.call(qclass$new,
                        c(list(texts[i],
                               answer   = answer[[i]],
                               feedback = if (!is.null(feedback)) feedback$feedbacks[[i]] else NULL),
                          subquestions_opts[[i]]),
                        quote = TRUE)
            })

            list(subquestions = subqs, cloze_feedback = cloze_feedback)
        }
    )
)
