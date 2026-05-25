#' @export
Group <- R6::R6Class(
    "Group",
    inherit = SimpleQuestion,
    public = list(
        children = NULL,
        num      = NULL,

        initialize = function(title,
                              type        = c("sequential", "random"),
                              header      = NULL,
                              data        = quote({}),
                              hidden_data = quote({}),
                              seed        = NULL,
                              hidden_seed = NULL,
                              children    = list(),
                              num         = NULL,
                              options     = list()) {
            super$initialize(title,
                             data        = data,
                             hidden_data = hidden_data,
                             seed        = seed,
                             hidden_seed = hidden_seed,
                             header      = header,
                             options     = options)
            self$type     <- match.arg(type)
            if (self$type == "random" && is.null(num))
                stop("Group is random but no num specified")
            self$num      <- num
            self$children <- list()
            for (child in children) self$add_child(child)
        },

        add_child = function(child) {
            self$children[[length(self$children) + 1]] <- child
            child$ancestor <- self
            child$quiz     <- self$quiz
            # Propagate quiz down into nested groups
            if (inherits(child, "Group"))
                private$propagate_quiz(child, self$quiz)
            invisible(self)
        },

        rename = function(prefix, names = self$hidden_data_names()) {
            self$rename_header(prefix, names)
            self$rename_data(prefix, names)
            self$rename_hidden_data(prefix, names)
            for (child in self$children)
                child$rename(prefix, names)
            invisible(self)
        },

        include_header = function() {
            if (is.null(self$header)) return()
            for (child in self$children)
                child$text <- paste(c(self$header, child$text), collapse = "\n\n")
            self$header <- NULL
        },

        validate_data = function() {
            private$check_siblings(self$children)
            invisible(self)
        }
    ),

    active = list(
        title = function(value) {
            if (!missing(value)) stop("title is read-only; set $text instead")
            txt <- if (!is.null(self$text)) self$text else ""
            first <- strsplit(txt, "\n")[[1]][1]
            trimws(first)
        }
    ),

    private = list(
        propagate_quiz = function(group, quiz_ref) {
            for (child in group$children) {
                child$quiz <- quiz_ref
                if (inherits(child, "Group"))
                    private$propagate_quiz(child, quiz_ref)
            }
        },

        # Check that no two siblings (or cousins) define the same variable
        # with different values.  Parent-child overrides are allowed.
        check_siblings = function(children) {
            seen <- list()
            for (child in children) {
                child_vars <- private$collect_subtree_vars(child)
                for (nm in names(child_vars)) {
                    if (!is.null(seen[[nm]]) &&
                        !identical(seen[[nm]], child_vars[[nm]]))
                        stop("Conflicting values for '", nm,
                             "' between sibling nodes")
                }
                seen <- update_list(seen, child_vars)
                if (inherits(child, "Group"))
                    private$check_siblings(child$children)
            }
            invisible(NULL)
        },

        # Return a named list of all variables defined in node's subtree.
        # Child values override parent values (deepest definition wins).
        collect_subtree_vars = function(node) {
            own <- private$eval_data_expr(
                if (inherits(node, "ClozeQuestion")) node$cloze_data else node$data)
            if (inherits(node, "Group")) {
                for (child in node$children) {
                    child_vars <- private$collect_subtree_vars(child)
                    own <- update_list(own, child_vars)
                }
            }
            own
        },

        eval_data_expr = function(expr) {
            if (is_empty_language(expr)) return(list())
            env <- empty_env()
            eval(expr, envir = env)
            as.list(env)
        }
    )
)
