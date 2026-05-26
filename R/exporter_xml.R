# Moodle XML exporter.

#' @export
XMLExporter <- R6::R6Class(
    "XMLExporter",
    public = list(

        render = function(inode, opts = list()) {
            switch(inode$class_name,
                Quiz              = self$render_quiz(inode, opts),
                Group             = self$render_group(inode, opts),
                ClozeQuestion     = self$render_cloze(inode, opts),
                NumericalQuestion = self$render_numerical(inode, opts),
                MultipleChoice    = self$render_mc(inode, opts),
                ShortanswerQuestion = self$render_shortanswer(inode, opts),
                stop("No XML renderer for class: ", sQuote(inode$class_name))
            )
        },

        # ── Quiz / Group ─────────────────────────────────────────────────────

        render_quiz = function(inode, opts) {
            opts$indent <- 2
            children_xml <- lapply(inode$children, function(child) {
                self$render(child, opts)
            })
            xml <- paste(Filter(Negate(is.null), children_xml), collapse = "\n")
            sprintf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<quiz>\n%s\n</quiz>", xml)
        },

        render_group = function(inode, opts) {
            title_path <- if (!is.null(opts$.path))
                paste0(opts$.path, "/", inode$title)
            else
                inode$title

            cat_xml <- sprintf(
                "<question type=\"category\">\n  <category>\n    <text>$course$/%s</text>\n  </category>\n</question>",
                title_path)

            child_opts          <- opts
            child_opts$.path    <- title_path
            children_xml <- lapply(inode$children, function(child) {
                if (!is.null(inode$header)) {
                    if (child$class_name == "ClozeQuestion")
                        child$header <- paste(c(inode$header, child$header), collapse = "\n\n")
                    else
                        child$text <- paste(c(inode$header, child$text), collapse = "\n\n")
                }
                self$render(child, child_opts)
            })

            parts <- c(list(cat_xml), Filter(Negate(is.null), children_xml))
            xml   <- paste(parts, collapse = "\n\n")
            if (!is.null(opts$indent) && opts$indent > 0)
                xml <- add_spaces_left(xml, opts$indent)
            xml
        },

        # ── Cloze ────────────────────────────────────────────────────────────

        render_cloze = function(inode, opts) {
            info <- list(env = inode$env)

            # Build combined text with inline cookies
            parts <- lapply(inode$subquestions, function(sq) {
                cookie <- private$make_cookie(sq, opts, info)
                paste0(sq$text, "\n\n", cookie)
            })
            if (!is.null(inode$header))
                parts <- c(list(inode$header), parts)
            combined_text <- paste(parts, collapse = "\n\n")
            html_text     <- trimws(render_HTML(combined_text, opts, info))

            ctx <- list(
                type          = "cloze",
                title         = private$html_escape(inode$subquestions[[1]]$title),
                question_text = html_text,
                answer        = "*",
                feedback      = private$render_feedback(inode, opts, info)
            )
            whisker::whisker.render(private$cloze_xml_template, ctx)
        },

        # ── NumericalQuestion ────────────────────────────────────────────────

        render_numerical = function(inode, opts) {
            info  <- list(env = inode$env)
            html  <- trimws(render_HTML(inode$text, opts, info))
            ctx <- list(
                type          = "numerical",
                title         = private$html_escape(inode$title),
                question_text = html,
                answer        = as.character(inode$answer),
                tolerance     = if (!is.null(inode$tolerance))
                                    sprintf("%f", inode$tolerance)
                                else NULL,
                tolerance_type = inode$tolerance_type,
                feedback      = private$render_feedback(inode, opts, info)
            )
            whisker::whisker.render(private$numerical_xml_template, ctx)
        },

        # ── MultipleChoice ───────────────────────────────────────────────────

        render_mc = function(inode, opts) {
            info <- list(env = inode$env)
            html <- trimws(render_HTML(inode$text, opts, info))

            n       <- length(inode$answer)
            n_true  <- max(sum(inode$answer), 1)
            n_false <- max(sum(!inode$answer), 1)
            fracs   <- sprintf("%.4f",
                100 * ifelse(inode$answer, 1, -1) /
                ifelse(inode$answer, n_true, n_false))

            feedbacks <- if (!is.null(inode$answer_feedbacks))
                inode$answer_feedbacks
            else
                as.list(rep("", n))

            answers_ctx <- lapply(seq_len(n), function(i) {
                list(
                    fraction        = fracs[[i]],
                    text            = trimws(render_HTML(inode$items[[i]], opts, info)),
                    answer_feedback = feedbacks[[i]]
                )
            })

            ctx <- list(
                type          = "multichoice",
                title         = private$html_escape(inode$title),
                question_text = html,
                feedback      = private$render_feedback(inode, opts, info),
                answers       = answers_ctx,
                single        = if (inode$single) "true" else "false",
                shuffle       = as.character(as.integer(inode$shuffle_answers))
            )
            whisker::whisker.render(private$mc_xml_template, ctx)
        },

        # ── ShortanswerQuestion ──────────────────────────────────────────────

        render_shortanswer = function(inode, opts) {
            info  <- list(env = inode$env)
            html  <- trimws(render_HTML(inode$text, opts, info))
            ans   <- if (is.language(inode$answer)) eval(inode$answer, inode$env) else inode$answer

            ctx <- list(
                type          = "shortanswer",
                title         = private$html_escape(inode$title),
                question_text = html,
                answer        = as.character(ans),
                feedback      = private$render_feedback(inode, opts, info)
            )
            whisker::whisker.render(private$simple_xml_template, ctx)
        }
    ),

    private = list(

        html_escape = function(s) {
            s <- gsub("&",  "&amp;",  s, fixed = TRUE)
            s <- gsub("<",  "&lt;",   s, fixed = TRUE)
            s <- gsub(">",  "&gt;",   s, fixed = TRUE)
            s
        },

        render_feedback = function(inode, opts, info) {
            if (isFALSE(opts$feedback)) return(NULL)
            fb   <- inode$feedback
            if (is.null(fb) || is.null(fb$text) || fb$text == "") return(NULL)

            text <- if (isTRUE(fb$eval) || is.null(fb$eval))
                fb$text
            else if (!is.null(fb$noeval_text))
                fb$noeval_text
            else
                fb$text

            text_str <- if (is.character(text))
                text
            else
                answerstr(text)

            html <- trimws(render_HTML(text_str, opts, info))
            sprintf("<generalfeedback format=\"html\">\n  <text><![CDATA[%s]]></text>\n</generalfeedback>",
                    html)
        },

        make_cookie = function(sq, opts, info) {
            type_id <- list(
                numerical   = "NM",
                shortanswer = "SA",
                multichoice = "MC"
            )[[sq$type]]
            if (is.null(type_id)) stop("No cookie type for: ", sq$type)

            ans <- if (is.language(sq$answer))
                eval(sq$answer, sq$env)
            else if (is.list(sq$answer))
                sq$answer   # logical vector for MC
            else
                sq$answer

            if (sq$type %in% c("numerical", "shortanswer")) {
                sprintf("{1:%s:=%s}", type_id, ans)
            } else {
                prefix <- ifelse(as.logical(ans), "=", "")
                items  <- paste0(prefix, sq$items, collapse = "~")
                sprintf("`{1:%s:%s}`", type_id, items)
            }
        },

        simple_xml_template = trimws('
<question type="{{type}}">
  <name>
    <text><![CDATA[{{{title}}}]]></text>
  </name>
  <questiontext format="html">
    <text><![CDATA[{{{question_text}}}]]></text>
  </questiontext>
  <answer fraction="100" format="plain_text">
    <text>{{{answer}}}</text>
  </answer>
{{#feedback}}
{{{feedback}}}
{{/feedback}}
  <hidden>0</hidden>
</question>
'),

        numerical_xml_template = trimws('
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
'),

        mc_xml_template = trimws('
<question type="{{type}}">
  <name>
    <text><![CDATA[{{{title}}}]]></text>
  </name>
  <questiontext format="html">
    <text><![CDATA[{{{question_text}}}]]></text>
  </questiontext>
{{#answers}}
  <answer fraction="{{{fraction}}}">
    <text><![CDATA[{{{text}}}]]></text>
    <feedback format="html">
      <text><![CDATA[{{{answer_feedback}}}]]></text>
    </feedback>
  </answer>
{{/answers}}
  <shuffleanswers>{{{shuffle}}}</shuffleanswers>
  <single>{{{single}}}</single>
{{#feedback}}
{{{feedback}}}
{{/feedback}}
  <hidden>0</hidden>
</question>
'),

        cloze_xml_template = trimws('
<question type="cloze">
  <name>
    <text><![CDATA[{{{title}}}]]></text>
  </name>
  <questiontext format="html">
    <text><![CDATA[{{{question_text}}}]]></text>
  </questiontext>
{{#feedback}}
{{{feedback}}}
{{/feedback}}
  <hidden>0</hidden>
</question>
')
    )
)
