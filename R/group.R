group.xml <- "<question type=\"category\">
  <category>
    <text>$course$/%s</text>
  </category>
</question>"

#' @export
Group <- R6::R6Class(
    "Group",
    inherit = SimpleQuestion,
    public = list(
        initialize = function(title,
                              type = c("sequential", "random"),
                              header = NULL,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              children = list(),
                              num = NULL) {
            super$initialize(title,
                             data = data,
                             hidden_data = hidden_data,
                             seed = seed,
                             hidden_seed = hidden_seed)

            self$type <- match.arg(type)

            if (self$type == "random" & is.null(num))
                stop("Group is random but no num specified")

            private$.num = num

            self$header = header

            for (child in children)
                self$add_child(child)
        },

        add_child = function(child) {
            private$.children[[length(private$.children) + 1]] <- child
            child$ancestor <- self
            child$update_quiz(self$quiz)
        },

        update_quiz = function(quiz) {
            super$update_quiz(quiz)

            for (c in self$children)
                c$update_quiz(quiz)
        },

        validate_data = function(parent_data = NULL) {
            if (length(self$children) <= 1)
                return(TRUE)

            children_lang <- sapply(self$children, function(child) {
                child$recursive_instantiated_data(!is.null(self$seed))
            })

            utils::combn(children_lang, 2, function(args) {
                lang1 <- args[[1]]
                lang2 <- args[[2]]

                lang1 <- merge_languages(parent_data, self$local_instantiated_data, lang1)
                lang2 <- merge_languages(parent_data, self$local_instantiated_data, lang2)

                if (!distinct_language(lang1, lang2))
                    stop("Language ", sQuote(lang1), " and ", sQuote(lang2), " are overlapping")

                TRUE                    # combn must return something other than NULL
            })

            for (child in self$children)
                child$validate_data(merge_languages(parent_data, self$local_instantiated_data))
        },

        instantiate_hidden_data_list = function(var_list = NULL, seed_init = FALSE) {
            super$instantiate_hidden_data_list(var_list, seed_init)

            for (c in self$children) {
                c$instantiate_hidden_data_list(self$hidden_data_list, seed_init | !is.null(self$hidden_seed))
            }
        },

        recursive_instantiated_data = function(seed_init = FALSE) {
            if (!seed_init & !is_empty_language(self$data) & is.null(self$seed))
                stop("Some data but no seed to initialize")

            if(is.null(self$seed))
                inst_data <- self$instantiated_data
            else
                inst_data <- merge_languages(
                    instantiate_data_list(
                        bquote(set.seed(.(self$seed))),
                        self$hidden_data_list),
                    self$instantiated_data)

            langs <- lapply(self$children, function(c) {
                c$recursive_instantiated_data(seed_init | !is.null(self$seed))
            })
            ls <- unlist(c(inst_data, langs))

            if (is.null(ls)) NULL else merge_languages(ls)
        },

        to_XML = function(opts, info) {
            title_list <- lapply(self$get_ancestor_list(), function(obj) {
                obj$title
            })
            title <- paste(title_list, collapse = "/")

            ## Call to_XML on each child
            children_XML <- lapply(seq_along(self$children), function(i) {
                info$num <- i
                child <- self$children[[i]]
                child$to_XML(opts, info)
            })

            xml <- paste(children_XML, collapse = "\n")

            paste(list(sprintf(group.xml, title), xml), collapse = "\n\n")
        },

        get_ancestor_list = function() {
            ancestor_list <- list(self)
            obj <- self
            while (!is.null(obj$ancestor)) {
                ancestor_list <- c(list(obj$ancestor), ancestor_list)
                obj <- obj$ancestor
            }
            ancestor_list
        },

        get_title = function(opts, info) {
            if (self$type == "random")
                sprintf("## %s (al\u00E9atoire %d parmi %d)", self$title, self$num, length(self$children))
            else if (self$type == "sequential")
                sprintf("## %s", self$title)
            else
                stop("Unhandled group type")
        },

        to_markdown = function(opts = list(), info = list()) {
            template <- "@TITLE@\n\n"
            markdown_group <- self$instantiate_placeholders(template, private$placeholders, opts, info)

            markdown_list <- lapply(seq_along(self$children), function(i) {
                child <- self$children[[i]]
                info$num <- i
                child$to_markdown(opts, info)
            })

            markdown_list_all <- c(list(markdown_group), markdown_list)
            markdown_list_all[sapply(markdown_list_all, is.null)] <- NULL
            paste(markdown_list_all, collapse = "\n")
        },

        invalidate_hidden_data = function() {
            super$invalidate_hidden_data()
            for (child in private$.children)
                child$invalidate_hidden_data()
        },

        invalidate_data = function() {
            super$invalidate_data()
            for (child in private$.children)
                child$invalidate_data()
        },

        rename = function(prefix, names = self$hidden_data_names()) {
            self$rename_header(prefix, names)
            self$rename_data(prefix, names)
            self$rename_hidden_data(prefix, names)

            for (child in self$children)
                child$rename(prefix, names)

            self$invalidate_hidden_data()
            self$invalidate_hidden_data_list()
            self$invalidate_data()

            self
        },

        include_header = function() {
            if (is.null(self$header))
                return()
            stopifnot(all(sapply(self$children, function(child) {
                inherits(child, "SimpleQuestion")
            })))

            for (question in self$children) {
                question$header <- self$header
            }
            self$header <- NULL
        }
    ),
    active = list(
        children = function() {
            private$.children
        },

        num = function() {
            private$.num
        }
    ),
    private = list(
        .children = list(),
        .num = NULL,

        .header = NULL,
        is_header_instantiated = FALSE
    )
)
