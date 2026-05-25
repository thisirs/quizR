#' @export
Quiz <- R6::R6Class(
    "Quiz",
    inherit = Group,
    public = list(
        initialize = function(title,
                              data        = quote({}),
                              hidden_data = quote({}),
                              seed        = NULL,
                              hidden_seed = NULL,
                              children    = list(),
                              options     = list()) {
            super$initialize(title,
                             data        = data,
                             hidden_data = hidden_data,
                             seed        = seed,
                             hidden_seed = hidden_seed,
                             children    = children,
                             options     = options)
            self$quiz     <- self
            self$ancestor <- NULL
            # Propagate quiz reference to any children added via super
            for (child in self$children) {
                child$quiz <- self
                if (inherits(child, "Group"))
                    private$propagate_quiz(child, self)
            }
        },

        generate_XML = function(...) {
            opts <- list(...)
            if (is.null(opts$output)) opts$output <- paste0(self$text, ".xml")

            inode    <- instantiate(self)
            exporter <- XMLExporter$new()
            xml      <- exporter$render(inode, opts)
            write(xml, opts$output)
        },

        generate_AMC = function(...) {
            opts <- list(...)
            if (is.null(opts$output)) opts$output <- paste0(self$text, ".tex")

            inode    <- instantiate(self)
            exporter <- AMCExporter$new()
            tex      <- exporter$render(inode, opts)
            write(tex, opts$output)
        },

        generate_correction = function(...) {
            opts <- list(...)
            if (is.null(opts$output)) opts$output <- paste0(self$text, ".pdf")
            if (is.null(opts$quiet))  opts$quiet  <- FALSE

            inode    <- instantiate(self)
            exporter <- MarkdownExporter$new()
            md       <- exporter$render(inode, opts)

            tmpfile <- tempfile("quiz", fileext = ".Rmd")
            on.exit(unlink(tmpfile), add = TRUE)
            write(md, tmpfile)

            rmarkdown::render(input      = tmpfile,
                              output_dir  = dirname(opts$output),
                              output_file = basename(opts$output),
                              quiet       = opts$quiet,
                              envir       = inode$env,
                              "pdf_document")
        },

        generate_data_file = function(filename = paste0(self$text, "-data.R")) {
            inode <- instantiate(self)
            data  <- inode$data_expr
            if (!is_empty_language(data))
                write(answerstr(data), filename)
        }
    )
)
