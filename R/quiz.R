quiz.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<quiz>
%s
</quiz>"


#' @export
Quiz <- R6::R6Class(
    "Quiz",
    inherit = Group,
    public = list(
        initialize = function(title,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              children = list()) {
            self$quiz <- self
            super$initialize(
                      title = title,
                      data = data,
                      hidden_data = hidden_data,
                      seed = seed,
                      hidden_seed = hidden_seed,
                      children = children)
            self$title <- title
            self$ancestor <- NULL
        },

        validate = function(lang){
            stopifnot(length(self$children) > 0)
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

        to_markdown = function(opts = list(), info = list()) {
            template <- "@TITLE@\n\n@REC_DATA_CHUNK@\n"
            markdown_group <- self$instantiate_placeholders(template, private$placeholders, opts, info)

            markdown_list <- lapply(seq_len(length(self$children)), function(i) {
                child <- self$children[[i]]
                info$num <- i
                child$to_markdown(opts, info)
            })

            markdown_list_all <- c(list(markdown_group), markdown_list)
            markdown_list_all[sapply(markdown_list_all, is.null)] <- NULL
            paste(markdown_list_all, collapse = "\n")
        },

        recursive_hidden_data = function() {
            data <- self$full_instantiated_data()

            self$quiz$recursive_instantiated_hidden_data()
        },

        get_recursive_instantiated_data = function(opts, info) {
            data <- self$recursive_instantiated_data()
            sprintf("```{r include = FALSE}\n%s\n```", answerstr(data))
        },

        get_title = function(opts, info) {
            title = sprintf("---
title: \"%s\"
", self$title)

            if(!is.null(opts$preamble)) {
                title <- paste0(title, "header-includes:\n")
                preamble <- paste0("  - ", strsplit(opts$preamble, "\n")[[1]], collapse="\n")
                title <- paste0(title, preamble, "\n")
            }

            paste0(title, "---")
        },

        generate_correction = function(...) {
            opts <- list(...)
            if (is.null(opts$output)) opts$output <- paste0(self$title, ".pdf")
            if (is.null(opts$quiet)) opts$quiet <- FALSE

            # Pass whole data and associated environment
            data <- self$recursive_instantiated_data()
            env <- empty_env()
            eval(data, env)
            info <- list(data = data, env = env)

            markdown <- self$to_markdown(opts = opts, info = info)

            tmpfile <- tempfile("quiz", fileext = ".Rmd")
            write(markdown, tmpfile)

            rmarkdown::render(input = tmpfile,
                              output_dir = dirname(opts$output),
                              output_file = basename(opts$output),
                              quiet = opts$quiet,
                              envir = empty_env(),
                              "pdf_document")
        },

        generate_data_file = function(filename = paste0(self$title, "-data.R")) {
            data <- self$recursive_instantiated_data()
            if (!is_empty_language(data))
                write(answerstr(data), filename)
        },

        generate_XML = function(...) {
            opts <- list(...)
            if (is.null(opts$output)) opts$output <- paste0(self$title, ".xml")

            # Pass whole data and associated environment
            data <- self$recursive_instantiated_data()
            env <- empty_env()
            eval(data, env)
            info <- list(data = data, env = env)

            xml <- self$to_XML(opts = opts, info = info)
            write(xml, opts$output)
        },

        to_XML = function(opts, info) {
            opts$indent <- 2

            ## Call to_XML on each child with args
            children_XML <- lapply(self$children, function(child) {
                child$to_XML(opts, info)
            })
            xml <- paste(children_XML, collapse = "\n")
            sprintf(quiz.xml, xml)
        }
    )
)
