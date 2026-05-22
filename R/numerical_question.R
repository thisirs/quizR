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

        build_xml_context = function(opts, info) {
            ctx <- super$build_xml_context(opts, info)
            ctx$tolerance      <- if (!is.null(self$tolerance)) sprintf("%f", self$tolerance) else NULL
            ctx$tolerance_type <- self$tolerance_type
            ctx
        },

        copy = function() {
            Question(self$text,
                     type = self$type,
                     seed = self$seed,
                     hidden_seed = self$hidden_seed,
                     hidden_data = self$hidden_data,
                     data = self$data,
                     answer = self$answer,
                     feedback = self$feedback,
                     distractors = self$distractors)
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

        build_amc_context = function(opts, info) {
            if (is.null(self$distractors))
                stop("AMC export requires 'distractors' for numerical question: ",
                     sQuote(self$title))

            qid   <- self$get_amc_id(opts, info)
            qtext <- render_latex_fragment(self$instantiated_text, opts, info)

            answer <- eval(self$instantiated_answer, info$env)
            distractor_values <- sapply(self$instantiated_distractors, function(d) {
                eval(d, info$env)
            })

            if (any(distractor_values == answer))
                stop("Distractor(s) equal to the correct answer (",
                     format(answer), ") in question: ", sQuote(self$title))
            if (anyDuplicated(distractor_values))
                stop("Duplicate distractors in question: ", sQuote(self$title))

            choices_ctx <- c(
                list(list(choice = sprintf("    \\correctchoice{$%s$}", format(answer)))),
                lapply(format(distractor_values), function(d) {
                    list(choice = sprintf("    \\wrongchoice{$%s$}", d))
                })
            )

            list(id = sprintf("{%s}", qid), text = qtext, choices = choices_ctx)
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
        whisker_amc_template = trimws('
\\begin{question}{{{id}}}
  {{{text}}}
  \\begin{choices}
{{#choices}}
{{{choice}}}
{{/choices}}
  \\end{choices}
\\end{question}
'),

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
