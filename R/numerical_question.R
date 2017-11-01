#' @include simple_question.R
NULL

#' @export
NumericalQuestion <- R6::R6Class(
    "Numerical",
    inherit = SimpleQuestion,
    public = list(
        tolerance = NULL,
        tolerance_type = NULL,

        initialize = function(text,
                              data = quote({}),
                              hidden_data = quote({}),
                              seed = NULL,
                              hidden_seed = NULL,
                              feedback = NULL,
                              answer = NULL,
                              tolerance_type = "relative",
                              tolerance = 1e-3,
                              tag = NULL) {
            super$initialize(text,
                             data = data,
                             hidden_data = hidden_data,
                             seed = seed,
                             hidden_seed = hidden_seed,
                             feedback = feedback,
                             answer = answer,
                             tag = tag)

            self$type <- "numerical"

            private$xml_placeholders$TOLERANCE = "get_xml_tolerance"
            private$xml_placeholders$TOLERANCE_TYPE = "get_xml_tolerance_type"

            self$tolerance_type <- tolerance_type
            self$tolerance <- tolerance
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
")))
