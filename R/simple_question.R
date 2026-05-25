#' @title SimpleQuestion
#' @name SimpleQuestion
NULL

#' @export
SimpleQuestion <- R6::R6Class(
    "SimpleQuestion",
    public = list(
        type            = "abstract",
        hidden_seed     = NULL,
        seed            = NULL,
        tags            = NULL,
        header          = NULL,
        options         = list(),
        quiz            = NULL,
        ancestor        = NULL,
        parent_var_list = NULL,

        initialize = function(text,
                              data        = quote({}),
                              hidden_data = quote({}),
                              seed        = NULL,
                              hidden_seed = NULL,
                              feedback    = NULL,
                              answer      = NULL,
                              tags        = NULL,
                              header      = NULL,
                              options     = list()) {
            self$text        <- text
            self$data        <- data
            self$hidden_data <- hidden_data
            self$seed        <- seed
            self$hidden_seed <- hidden_seed
            self$feedback    <- feedback   # active binding normalizes
            self$answer      <- answer
            self$tags        <- tags
            self$header      <- header
            self$options     <- options
        },

        # Returns names of variables produced by hidden_data
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
            invisible(self)
        },

        rename_header = function(prefix, names = self$hidden_data_names()) {
            self$header <- prefix_object(prefix, names, self$header)
            invisible(self)
        },

        rename_text = function(prefix, names = self$hidden_data_names()) {
            self$text <- prefix_object(prefix, names, self$text)
            invisible(self)
        },

        rename_answer = function(prefix, names = self$hidden_data_names()) {
            self$answer <- prefix_object(prefix, names, self$answer)
            invisible(self)
        },

        rename_feedback = function(prefix, names = self$hidden_data_names()) {
            fb <- self$feedback   # already normalized list
            if (is.list(fb)) {
                if (!is.null(fb$text))
                    fb$text <- prefix_object(prefix, names, fb$text)
                if (!is.null(fb$noeval_text))
                    fb$noeval_text <- prefix_object(prefix, names, fb$noeval_text)
            }
            self$feedback <- fb
            invisible(self)
        },

        rename_data = function(prefix, names = self$hidden_data_names()) {
            self$data <- prefix_object(prefix, names, self$data)
            invisible(self)
        },

        rename_hidden_data = function(prefix, names = self$hidden_data_names()) {
            self$hidden_data <- prefix_object(prefix, names, self$hidden_data)
            invisible(self)
        },

        copy = function() {
            Question(self$text,
                     type        = self$type,
                     seed        = self$seed,
                     hidden_seed = self$hidden_seed,
                     hidden_data = self$hidden_data,
                     data        = self$data,
                     answer      = self$answer,
                     feedback    = self$feedback,
                     options     = self$options)
        }
    ),

    active = list(
        text = function(value) {
            if (missing(value)) return(private$.text)
            private$.text <- value
        },

        data = function(value) {
            if (missing(value)) return(private$.data)
            private$.data <- value
        },

        hidden_data = function(value) {
            if (missing(value)) return(private$.hidden_data)
            private$.hidden_data <- value
        },

        answer = function(value) {
            if (missing(value)) return(private$.answer)
            private$.answer <- value
        },

        feedback = function(value) {
            if (missing(value)) return(private$.feedback)
            private$.feedback <- normalize_feedback(value)
        },

        title = function(value) {
            if (!missing(value)) stop("title is read-only")
            txt <- if (!is.null(private$.text)) private$.text else ""
            first <- strsplit(txt, "\n")[[1]][1]
            first <- trimws(first)
            if (nchar(first) > 60) paste0(substr(first, 1, 57), "...") else first
        },

        hidden_data_list = function(value) {
            if (!missing(value)) stop("hidden_data_list is read-only")
            parent_vars <- if (!is.null(self$parent_var_list)) self$parent_var_list else list()
            eval_hidden_data(self$hidden_data, self$hidden_seed, parent_vars)
        },

        instantiated_text = function(value) {
            if (!missing(value)) stop("instantiated_text is read-only")
            instantiate_text_list(self$text, self$hidden_data_list)
        },

        instantiated_data = function(value) {
            if (!missing(value)) stop("instantiated_data is read-only")
            instantiate_data_list(self$data, self$hidden_data_list)
        },

        instantiated_answer = function(value) {
            if (!missing(value)) stop("instantiated_answer is read-only")
            instantiate_object(self$answer, self$hidden_data_list)
        },

        instantiated_feedback = function(value) {
            if (!missing(value)) stop("instantiated_feedback is read-only")
            fb <- instantiate_feedback(self$feedback, self$hidden_data_list)
            fb[names(fb) != "noeval_text"]
        }
    ),

    private = list(
        .text        = NULL,
        .data        = NULL,
        .hidden_data = NULL,
        .answer      = NULL,
        .feedback    = NULL
    )
)

#' Normalize raw feedback to canonical list form.
#' @export
normalize_feedback <- function(raw) {
    defaults <- list(text = "", noeval_text = NULL, eval = TRUE, indent = 2)
    if (is.null(raw))
        defaults
    else if (is.character(raw) || is.numeric(raw) || is.language(raw))
        update_list(defaults, list(text = raw))
    else if (is.list(raw))
        update_list(defaults, raw)
    else
        stop("Unsupported feedback type: ", class(raw))
}
