#' @include simple_question.R
NULL

#' @export
NumericalQuestion <- R6::R6Class(
    "Numerical",
    inherit = SimpleQuestion,
    public = list(
        tolerance = NULL,
        tolerance_type = NULL,
        distractors = NULL,

        initialize = function(text,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              tolerance_type = "relative",
                              tolerance = 1e-3,
                              distractors = NULL,
                              tags = NULL) {
            super$initialize(text,
                             data = data,
                             hidden_data = hidden_data,
                             seed = seed,
                             hidden_seed = hidden_seed,
                             feedback = feedback,
                             answer = answer,
                             tags = tags)

            self$type <- "numerical"

            private$xml_placeholders$TOLERANCE = "get_xml_tolerance"
            private$xml_placeholders$TOLERANCE_TYPE = "get_xml_tolerance_type"

            self$tolerance_type <- tolerance_type
            self$tolerance <- tolerance
            self$distractors <- if (is.null(distractors)) NULL
                               else if (is.language(distractors)) distractors
                               else as.list(distractors)
        },

        get_inst_cookie = function(opts, info) {
            answer <- self$get_evaluated_answer2(opts, info)
            if (is.null(answer))
                stop("Cannot return cookie: ", sQuote("answer"), " is NULL")
            if (self$tolerance_type == "relative")
                tolerance <- abs(answer) * self$tolerance
            else
                tolerance <- self$tolerance
            sprintf("{%s:NM:=%s:%s}", 1, answer, tolerance)
        },

        get_xml_tolerance = function(opts, info) {
            if (is.null(self$tolerance))
                NULL
            else
                sprintf("<tolerance>%f</tolerance>", self$tolerance)
        },

        get_xml_tolerance_type = function(opts, info) {
            if (is.null(self$tolerance_type))
                NULL
            else
                sprintf("<tolerancetype>%s</tolerancetype>", self$tolerance_type)
        },

        build_xml_context = function(opts, info) {
            ctx <- super$build_xml_context(opts, info)
            ctx$tolerance      <- if (!is.null(self$tolerance)) sprintf("%f", self$tolerance) else NULL
            ctx$tolerance_type <- self$tolerance_type
            ctx
        },

        rename_answer = function(prefix, names0 = names(self$hidden_data_list)) {
            super$rename_answer(prefix, names0)
            if (!is.null(self$distractors)) {
                if (is.language(self$distractors)) {
                    self$distractors <- prefix_object(prefix, names0, self$distractors)
                } else {
                    self$distractors <- lapply(self$distractors, function(d) {
                        prefix_object(prefix, names0, d)
                    })
                }
            }
        },

        to_amc = function(opts, info) {
            if (is.null(self$distractors))
                stop("AMC export requires 'distractors' for numerical question: ",
                     sQuote(self$title))

            opts$export <- "amc"

            if (is.null(info$env)) {
                info0 <- self$get_data_and_environment()
                info <- update_list(info, info0)
            }

            qid <- {
                title <- evaluate_inline_r(self$title, info$env)
                id <- gsub("[^a-zA-Z0-9]", "-", title)
                id <- gsub("-+", "-", id)
                id <- gsub("^-|-$", "", id)
                if (nchar(id) > 30) id <- substr(id, 1, 30)
                if (!is.null(info$num)) id <- paste0("q", info$num, "-", id)
                if (nchar(id) == 0) id <- "question"
                id
            }

            qtext  <- render_latex_fragment(self$instantiated_text, opts, info)
            answer <- eval(self$instantiated_answer, info$env)
            distractor_values <- sapply(self$instantiated_distractors, function(d) {
                eval(d, info$env)
            })

            if (any(distractor_values == answer))
                stop("Distractor(s) equal to the correct answer (",
                     format(answer), ") in question: ", sQuote(self$title))
            if (anyDuplicated(distractor_values))
                stop("Duplicate distractors in question: ", sQuote(self$title))

            choices <- c(
                sprintf("    \\correctchoice{$%s$}", format(answer)),
                sprintf("    \\wrongchoice{$%s$}", format(distractor_values))
            )
            choices_str <- paste(choices, collapse = "\n")

            sprintf(
                "\\begin{question}{%s}\n  %s\n  \\begin{choices}\n%s\n  \\end{choices}\n\\end{question}",
                qid, qtext, choices_str)
        }
    ),

    active = list(
        instantiated_distractors = function() {
            if (is.null(self$distractors)) return(NULL)
            if (is.language(self$distractors)) {
                env <- list2env(self$hidden_data_list, envir = empty_env())
                as.list(eval(self$distractors, envir = env))
            } else {
                lapply(self$distractors, function(d) {
                    instantiate_data_list(d, self$hidden_data_list)
                })
            }
        }
    ),

    private = list(
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
    @TOLERANCE@
    @TOLERANCE_TYPE@
  </answer>
@XML_GENERALFEEDBACK@
  <hidden>0</hidden>
</question>
"),
        whisker_xml_template = trimws('
<question type="{{type}}">
  <name>
    <text><![CDATA[{{{title}}}]]></text>
  </name>
  <questiontext format="html">
    <text><![CDATA[{{{question_text}}}]]></text>
  </questiontext>
  <answer fraction="100" format="plain_text">
    <text>{{{answer}}}</text>
{{#tolerance}}
    <tolerance>{{{tolerance}}}</tolerance>
{{/tolerance}}
{{#tolerance_type}}
    <tolerancetype>{{{tolerance_type}}}</tolerancetype>
{{/tolerance_type}}
  </answer>
{{#feedback}}
{{{feedback}}}
{{/feedback}}
  <hidden>0</hidden>
</question>
')))
