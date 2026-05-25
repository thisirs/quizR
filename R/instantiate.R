# Instantiator: evaluates hidden_data/data, substitutes variables in all
# question fields, and returns a plain-list inode ready for exporters.

# ── Entry point ───────────────────────────────────────────────────────────────

#' Instantiate a question, group, or quiz node.
#'
#' @param node A SimpleQuestion, Group, or Quiz R6 object.
#' @param parent_var_list Named list of variables inherited from parent scope.
#' @return A plain list (inode) with all fields evaluated.
#' @export
instantiate <- function(node, parent_var_list = list()) {
    if (inherits(node, "Quiz"))
        instantiate_group(node, parent_var_list)
    else if (inherits(node, "Group"))
        instantiate_group(node, parent_var_list)
    else if (inherits(node, "ClozeQuestion"))
        instantiate_cloze(node, parent_var_list)
    else
        instantiate_leaf(node, parent_var_list)
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Evaluate hidden_data with hidden_seed in context of parent_var_list.
# Returns the merged named list of all available variables.
eval_hidden_data <- function(hidden_data, hidden_seed, parent_var_list) {
    env <- list2env(parent_var_list, envir = empty_env())
    if (!is.null(hidden_seed))
        eval(bquote(set.seed(.(hidden_seed))), envir = env)
    if (!is_empty_language(hidden_data))
        eval(hidden_data, envir = env)
    update_list(parent_var_list, as.list(env, all.names = TRUE))
}

# Build an R evaluation environment from var_list, then optionally evaluate
# data (with seed) into it.  Returns the environment.
build_env <- function(var_list, data = quote({}), seed = NULL) {
    env <- list2env(var_list, envir = empty_env())
    if (!is.null(seed))
        eval(bquote(set.seed(.(seed))), envir = env)
    if (!is_empty_language(data))
        eval(instantiate_data_list(data, var_list), envir = env)
    env
}

# Build the data expression (set.seed + data block) that would reproduce env.
build_data_expr <- function(var_list, data = quote({}), seed = NULL) {
    parts <- list()
    if (!is.null(seed))
        parts <- c(parts, list(bquote(set.seed(.(seed)))))
    if (!is_empty_language(data)) {
        inst <- instantiate_data_list(data, var_list)
        parts <- c(parts, list(inst))
    }
    if (length(parts) == 0) quote({}) else merge_languages(parts)
}

# Instantiate variable references inside a normalized feedback list.
instantiate_feedback <- function(raw_feedback, var_list) {
    fb <- normalize_feedback(raw_feedback)
    fb$text <- instantiate_object(fb$text, var_list)
    if (!is.null(fb$noeval_text))
        fb$noeval_text <- instantiate_object(fb$noeval_text, var_list)
    fb
}

# Derive a display title: use first line of instantiated text if text is long.
derive_title <- function(inst_text) {
    first <- strsplit(inst_text, "\n")[[1]][1]
    first <- trimws(first)
    if (nchar(first) > 60) paste0(substr(first, 1, 57), "...") else first
}

# ── Leaf questions ────────────────────────────────────────────────────────────

instantiate_leaf <- function(q, parent_var_list = list()) {
    var_list  <- eval_hidden_data(q$hidden_data, q$hidden_seed, parent_var_list)
    env       <- build_env(var_list, q$data, q$seed)
    data_expr <- build_data_expr(var_list, q$data, q$seed)

    full_text <- trimws(paste(
        c(if (!is.null(q$header)) trimws(q$header),
          if (!is.null(q$text))   trimws(q$text)),
        collapse = "\n\n"))
    text     <- instantiate_text_list(full_text, var_list)
    answer   <- instantiate_object(q$answer, var_list)
    feedback <- instantiate_feedback(q$feedback, var_list)

    inode <- list(
        class_name = class(q)[1],
        type       = q$type,
        title      = derive_title(text),
        text       = text,
        answer     = answer,
        feedback   = feedback,
        env        = env,
        data_expr  = data_expr,
        options    = q$options,
        var_list   = var_list
    )

    # Type-specific extra fields
    if (inherits(q, "NumericalQuestion")) {
        inode$tolerance      <- q$tolerance
        inode$tolerance_type <- q$tolerance_type
        inode$distractors    <- instantiate_distractors(q$distractors, var_list, env)
        if (is.language(inode$answer))
            inode$answer <- eval(instantiate_data_list(inode$answer, var_list), envir = env)
    } else if (inherits(q, "MultipleChoice")) {
        inode$items            <- instantiate_mc_items(q$items, var_list, env)
        inode$answer_feedbacks <- instantiate_mc_answer_feedbacks(q$answer_feedbacks, var_list)
        inode$answer           <- instantiate_mc_answer(q$answer, var_list, env)
        inode$single           <- q$single
        inode$shuffle_answers  <- q$shuffle_answers
        inode$format           <- q$format
    }

    inode
}

instantiate_distractors <- function(distractors, var_list, env) {
    if (is.null(distractors)) return(NULL)
    if (is.language(distractors)) {
        inst <- instantiate_data_list(distractors, var_list)
        as.list(eval(inst, envir = env))
    } else {
        lapply(distractors, function(d) {
            inst <- instantiate_data_list(d, var_list)
            eval(inst, envir = env)
        })
    }
}

instantiate_mc_items <- function(items, var_list, env) {
    if (is.language(items)) {
        inst <- instantiate_data_list(items, var_list)
        as.list(eval(inst, envir = env))
    } else {
        lapply(items, function(item) instantiate_text_list(item, var_list))
    }
}

instantiate_mc_answer_feedbacks <- function(feedbacks, var_list) {
    if (is.null(feedbacks)) return(NULL)
    lapply(feedbacks, function(f) instantiate_text_list(f, var_list))
}

# Returns a logical vector.
instantiate_mc_answer <- function(answer, var_list, env) {
    if (is.language(answer)) {
        inst <- instantiate_data_list(answer, var_list)
        result <- eval(inst, envir = env)
        stopifnot(is.logical(result))
        result
    } else {
        result <- sapply(answer, function(a) {
            inst <- instantiate_data_list(a, var_list)
            eval(inst, envir = env)
        })
        stopifnot(is.logical(result))
        result
    }
}

# ── Cloze questions ───────────────────────────────────────────────────────────

instantiate_cloze <- function(q, parent_var_list = list()) {
    var_list  <- eval_hidden_data(q$cloze_hidden_data, q$hidden_seed, parent_var_list)
    env       <- build_env(var_list, q$cloze_data, q$seed)
    data_expr <- build_data_expr(var_list, q$cloze_data, q$seed)

    full_var_list <- as.list(env, all.names = TRUE)

    header <- if (!is.null(q$header))
        instantiate_text_list(q$header, full_var_list)
    else NULL

    inst_subqs <- lapply(q$subquestions, function(sq) instantiate_leaf(sq, full_var_list))

    cloze_fb <- if (!is.null(q$cloze_feedback))
        q$cloze_feedback
    else
        list(header = NULL)

    list(
        class_name   = "ClozeQuestion",
        type         = "cloze",
        header       = header,
        subquestions = inst_subqs,
        cloze_feedback = cloze_fb,
        env          = env,
        data_expr    = data_expr,
        options      = q$options,
        var_list     = var_list
    )
}

# ── Groups and Quiz ───────────────────────────────────────────────────────────

instantiate_group <- function(g, parent_var_list = list()) {
    var_list  <- eval_hidden_data(g$hidden_data, g$hidden_seed, parent_var_list)
    env       <- build_env(var_list, g$data, g$seed)
    data_expr <- build_data_expr(var_list, g$data, g$seed)

    children <- lapply(g$children, function(child) instantiate(child, var_list))

    cls <- class(g)[1]
    list(
        class_name = cls,
        type       = g$type,
        title      = if (!is.null(g$text)) g$text else "",
        num        = g$num,
        children   = children,
        env        = env,
        data_expr  = data_expr,
        options    = g$options,
        var_list   = var_list
    )
}

# ── Data-file helper ──────────────────────────────────────────────────────────

# Recursively collect non-empty data_expr from an inode tree (depth-first).
collect_data_exprs <- function(inode) {
    exprs <- list()
    if (!is_empty_language(inode$data_expr))
        exprs <- c(exprs, list(inode$data_expr))
    if (!is.null(inode$children))
        exprs <- c(exprs, unlist(lapply(inode$children, collect_data_exprs), recursive = FALSE))
    if (!is.null(inode$subquestions))
        exprs <- c(exprs, unlist(lapply(inode$subquestions, collect_data_exprs), recursive = FALSE))
    exprs
}
