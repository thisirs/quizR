add_spaces_left <- function(s, spaces) {
    indent <- paste(rep(" ", spaces), collapse = "")
    gsub("(?m)^", indent, s, perl = TRUE)
}

merge_languages <- function(...) {
    ls <- unlist(list(...))
    if (length(ls) == 1) return(ls[[1]])
    if (is.null(ls))
        quote({})
    else {
        langs <- sapply(ls, function(l) {
            if (is.symbol(l))
                return(list(l))
            else if (l[[1]] == as.name("{"))
                if (length(l) == 1)     # Empty {}
                    return(NULL)
                else                    # Remove {}
                    return(as.list(l)[-1])
            else return(list(l))
        })
        do.call(call, c(list("{"), unlist(langs)), quote = TRUE)
    }
}

empty_env <- function() {
    new.env(parent = parent.env(.GlobalEnv))
}

fails <- function(language) {
    tryCatch({
        eval(language, cleanenv())
    },
    error = function(e) stop("Some errors in data code chunks:\n", e))
}

cleanenv <- function() {
    new.env(parent = parent.env(.GlobalEnv))
}

#' Helper function to specify list of languages
#'
#' @param ...
#'
#' @return A quoted expression or a list of quoted expression
#' @export
expr <- function(...) {
    l <- as.list(match.call(expand.dots = TRUE)[-1])
    if (length(l) == 1) l[[1]] else l
}

to_string <- function(filename) {
    paste(readLines(filename), collapse = "\n")
}

translate_hidden_data <- function(question) {
    hdata <- question$get_hdata()
    env <- cleanenv()
    eval(hdata, env)
    if (length(ls(env)) >= 1) {
        prefix <- sprintf("data%s", question$id)
        varnames <- paste0(prefix, ls(env))
        aliases <- lapply(as.list(varnames), as.name)
        names(aliases) <- ls(env)
        as.environment(aliases)
    } else cleanenv()
}

instantiate_data_list <- function(data, var_list) {
    call <- substitute(substitute(data, var_list), list(data = data))
    eval(call)
}

instantiate_text_list <- function(text, var_list) {
    if (is.null(text))
        return(NULL)

    text <- instantiate_chunk_list(text, var_list)
    instantiate_inline_list(text, var_list)
}

instantiate_inline_list <- function(text, var_list) {
    loc <- stringi::stri_locate_all_regex(text, "`r[ #]([^`]+)\\s*`",
                                          omit_no_match = TRUE)[[1]]

    if (nrow(loc) >= 1) {
        for (i in rev(1:nrow(loc))) {
            chunk <- stringi::stri_sub(text, loc[i, 1] + 2, loc[i, 2] - 1)
            expr <- parse(text = chunk)
            chunks <- lapply(as.list(expr), function(lang) {
                lang_rep <- instantiate_data_list(lang, var_list)
                paste(deparse(lang_rep), collapse = " ")
            })
            new_chunk <- sprintf("`r %s`", paste(chunks, collapse = "; "))
            stringi::stri_sub(text, loc[i, 1], loc[i, 2]) <- new_chunk
        }
    }
    return(text)
}

instantiate_chunk_list <- function(text, var_list) {
    begin <- stringi::stri_locate_all_regex(text, "^[\t >]*```+ *\\{([a-zA-Z0-9]+.*)\\}\\s*$",
                                            omit_no_match = TRUE,
                                            multiline = TRUE)[[1]]

    if (nrow(begin) >= 1) {
        end <- stringi::stri_locate_all_regex(text, "^[\t >]*```+\\s*$",
                                              omit_no_match = TRUE,
                                              multiline = TRUE)[[1]]

        stopifnot(nrow(begin) == nrow(end))
        N <- nrow(begin)
        stopifnot(all(diff(c(begin[, 1], end[, 2])[rep(1:N, each = 2) + c(0, N)]) > 0))

        for (i in rev(1:nrow(begin))) {
            chunk <- stringi::stri_sub(text, begin[i, 2] + 2, end[i, 1] - 1)
            expr <- parse(text = chunk)
            chunks <- lapply(as.list(expr), function(lang) {
                lang_rep <- instantiate_data_list(lang, var_list)
                paste(deparse(lang_rep), collapse = "\n")
            })
            new_chunk <- paste(chunks, collapse = "\n")
            stringi::stri_sub(text, begin[i, 2] + 2, end[i, 1] - 2) <- new_chunk
        }
    }
    return(text)
}

render_HTML <- function(text, opts, info) {
    if (is.null(info$env))
        env <- parent.frame()
    else
        env <- info$env

    tmpfile <- tempfile("question", tmpdir = ".", fileext = ".Rmd")
    on.exit(unlink(tmpfile), add = TRUE)

    if(!is.null(opts$header) | !is.null(opts$preamble)) {
        write("---", tmpfile, append = TRUE)
        if(!is.null(opts$header)) {
            write(opts$header, tmpfile, append = TRUE)
        }

        # Add a Markdown header with LaTeX preamble
        if(!is.null(opts$preamble)) {
            preamble <- trimws(opts$preamble)
            preamble <- paste0("  - ", strsplit(opts$preamble, "\n")[[1]], collapse="\n")
            write("header-includes:", tmpfile, append = TRUE)
            write(preamble, tmpfile, append = TRUE)
        }
        write("---", tmpfile, append = TRUE)
    }

    write(text, tmpfile, append = TRUE)

    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment(), envir = env,
                                quiet = isTRUE(opts$quiet))
    on.exit(unlink(output), add = TRUE)

    result <- to_string(output)
    return(result)
}

render_latex_fragment <- function(text, opts, info) {
    if (is.null(info$env))
        env <- parent.frame()
    else
        env <- info$env

    rmd_file <- tempfile("question", tmpdir = ".", fileext = ".Rmd")
    on.exit(unlink(rmd_file), add = TRUE)
    write(text, rmd_file)

    # Knit evaluates inline R and code chunks, producing plain markdown
    md_file <- tempfile("question", tmpdir = ".", fileext = ".md")
    on.exit(unlink(md_file), add = TRUE)
    knitr::knit(rmd_file, output = md_file, quiet = TRUE, envir = env)

    # Pandoc converts markdown to LaTeX; without --standalone it emits a fragment
    tex_file <- tempfile("question", tmpdir = ".", fileext = ".tex")
    on.exit(unlink(tex_file), add = TRUE)
    rmarkdown::pandoc_convert(md_file, from = "markdown+tex_math_single_backslash",
                               to = "latex", output = tex_file, options = "--wrap=none")

    trimws(paste(readLines(tex_file), collapse = "\n"))
}

## Taken from RCurl
update_list <- function (y, x)
{
    if (length(x) == 0)
        return(y)
    if (length(y) == 0)
        return(x)
    i = match(names(y), names(x))
    i = is.na(i)
    if (any(i))
        x[names(y)[which(i)]] = y[which(i)]
    x
}

is_empty_language <- function(lang) {
    if (is.null(lang))
        return(TRUE)
    else if (is.symbol(lang))
        return(FALSE)
    else if (lang[[1]] == as.name("{"))
        if (length(lang) == 1)     # Empty {}
            return(TRUE)
    return(FALSE)
}

instantiate_object <- function(object, lang) {
    rename_object(lang, object)
}

rename_object <- function(aliases, object) {
    if (is.null(object))
        return(NULL)

    # Convert integer to numeric ; no "L" in expressions
    aliases <- lapply(aliases, function(alias) {
        if (is.integer(alias))
            as.numeric(alias)
        else
            alias
    })

    if (is.list(object))
        lapply(object, instantiate_object, aliases)
    else if (is.language(object))
        instantiate_data_list(object, aliases)
    else if (is.numeric(object))
        object
    else if (is.logical(object))
        object
    else if (is.character(object))
        instantiate_text_list(object, aliases)
    else stop("Failed to rename object ", sQuote(object))
}

# Return names of all variables assigned in a language expression (AST walk).
# Does not evaluate the expression, so parent-scope references are not needed.
# Does not recurse into local() or function() — they create their own scopes.
lhs_assigned_names <- function(expr) {
    if (!is.call(expr)) return(character(0))
    head <- expr[[1]]
    if (identical(head, quote(local)) || identical(head, quote(`function`)))
        return(character(0))
    if (identical(head, quote(`<-`)) || identical(head, quote(`=`)) || identical(head, quote(`<<-`)))
        return(c(if (is.symbol(expr[[2]])) as.character(expr[[2]]) else character(0),
                 lhs_assigned_names(expr[[length(expr)]])))
    unique(unlist(lapply(as.list(expr)[-1], lhs_assigned_names)))
}

prefix_object <- function(prefix, names, object) {
    if (is.null(object))
        return(NULL)

    prefix_names <- paste0(prefix, names)
    aliases <- lapply(as.list(prefix_names), as.name)
    names(aliases) <- names

    rename_object(aliases, object)
}

sanitize_language <- function(lang) {
    if (is.null(lang))
        return(NULL)
    else if (is.symbol(lang))
        return(lang)
    else if (lang[[1]] == as.name("{"))
        if (length(lang) == 1)     # Empty {}
            return(NULL)
        else                    # Remove {}
            return(lang)
    else return(lang)

}

answerstr <- function(answer) {
    if (is.null(answer)) return("")
    type <- typeof(answer)
    switch(type,
           closure = {
               d <- deparse(body(answer), width.cutoff = 120)
               lines <- d[-c(1, length(d))]
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse = "\n")
           },
           language = {
               d <- deparse(answer, width.cutoff = 120)
               lines <-
                   if (class(answer) == "{" & length(d) >=3)
                       d[-c(1, length(d))]
                   else
                       d
               indent <- min(attr(regexpr("^ *", lines), "match.length"))
               paste(substring(lines, indent + 1), collapse = "\n")
           },
           symbol = {deparse(answer, width.cutoff = 120)},
           double = {deparse(answer, width.cutoff = 120)},
           character = paste0("\"", answer, "\""),
           stop("Unhandled type in ", sQuote("answerstr"), ": ", type))
}


#' Merge several group quiz objects
#'
#' @param groups A list of groups
#' @param title The title of the merged group
#' @return The merged group
merge_groups <- function(groups, title) {
    for (group in groups)
        group$include_header()

    renamed_groups <- lapply(seq_along(groups), function(i) {
        group <- groups[[i]]
        prefix <- sprintf("g%d_", i)
        group$rename(prefix)
    })

    children <- unlist(lapply(renamed_groups, function(group) {
        group$children
    }), recursive = FALSE)
    hidden_datas <- lapply(renamed_groups, function(g) {
        g$hidden_data
    })
    hidden_data <- merge_languages(hidden_datas)

    datas <- lapply(renamed_groups, function(g) {
        g$data
    })
    data <- merge_languages(datas)

    Group$new(title,
              hidden_data = hidden_data,
              data = data,
              children = children)
}


#' @export
expand_with_gaussian <- function(
                                    answers,
                                    target_n,
                                    constraint = function(x) TRUE,
                                    min_sep = NULL,
                                    integer_mode = NULL,
                                    max_iter = 10000,
                                    use_spline = FALSE
                                    ) {
    # Keep original answers untouched
    answers <- unique(answers)

    # Work on sorted copy for the mapping
    a_sorted <- sort(unique(answers))
    n <- length(a_sorted)

    if (n >= target_n) {
        return(sort(a_sorted)[1:target_n])
    }

    # Detect integer mode
    if (is.null(integer_mode)) {
        integer_mode <- all(abs(a_sorted - round(a_sorted)) < .Machine$double.eps^0.5)
    }

    # Gaussian quantiles
    probs <- (seq_len(n) - 0.5) / n
    z_vals <- stats::qnorm(probs)

    # Interpolation function
    if (use_spline && n >= 3) {
        f <- stats::splinefun(z_vals, a_sorted, method = "natural")
    } else {
        f <- stats::approxfun(z_vals, a_sorted, rule = 2)
    }

    result <- answers
    iter <- 0

    while (length(result) < target_n && iter < max_iter) {
        iter <- iter + 1

        z <- stats::rnorm(1)
        candidate <- f(z)

        if (integer_mode) {
            candidate <- round(candidate)
        }

        # Constraint check
        if (!constraint(candidate)) next

        # Distance / uniqueness
        if (!is.null(min_sep)) {
            if (any(abs(candidate - result) < min_sep)) next
        } else {
            if (any(abs(candidate - result) < .Machine$double.eps^0.5)) next
        }

        result <- c(result, candidate)
    }

    if (length(result) < target_n) {
        warning("Could not generate enough answers")
    }

    return(result)
}

evaluate_inline_r <- function(text, env) {
    if (is.null(env) || is.null(text)) return(text)

    loc <- stringi::stri_locate_all_regex(
        text, "`r[ #]([^`]+)\\s*`", omit_no_match = TRUE)[[1]]

    if (nrow(loc) == 0) return(text)

    for (i in rev(seq_len(nrow(loc)))) {
        chunk <- stringi::stri_sub(text, loc[i, 1] + 2, loc[i, 2] - 1)
        result <- tryCatch(
            paste(eval(parse(text = chunk), envir = env), collapse = " "),
            error = function(e) paste0("`r", chunk, "`")
        )
        stringi::stri_sub(text, loc[i, 1], loc[i, 2]) <- result
    }
    return(text)
}

distinct_language <- function (lang1, lang2) {
    env1 <- cleanenv()
    env2 <- cleanenv()
    eval(lang1, env1)
    eval(lang2, env2)
    all(sapply(intersect(ls(env1), ls(env2)), function(e) identical(get(e, envir = env1), get(e, envir = env2))))
}


# Create a new version of question
Sampler <- R6::R6Class(
                   "Sampler",
                   public = list(
                       initialize = function(question,
                                             batch_size = Inf,
                                             seed = NULL) {
                           private$.question = question
                           private$.batch_size = batch_size
                           private$.seed = seed
                           private$.counter = 0
                       },
                       sample = function() {
                           stopifnot(private$.counter < private$.batch_size)

                           private$.counter = private$.counter + 1
                           ident <- paste0(private$.seed, sprintf("%d", private$.counter))

                           if (private$.question$type == "cloze")
                               original_hidden_data <- private$.question$cloze_hidden_data
                           else
                               original_hidden_data <- private$.question$hidden_data

                           ## Prepend custom ds_name and ds_sym to be used in question
                           new_hdata <- merge_languages(bquote({
                               ds_name <- .(ident)
                               ds_sym <- as.symbol(ds_name)
                           }), original_hidden_data)

                           if (inherits(private$.question, "Group"))
                               qc <- private$.question$clone()
                           else
                               qc <- private$.question$copy()

                           if (private$.question$type == "cloze")
                               qc$cloze_hidden_data <- new_hdata
                           else
                               qc$hidden_data <- new_hdata

                           qc
                       },
                       count = function() {
                           private$.batch_size - private$.counter
                       }),
                   active = list(
                       seed = function() private$.seed
                   ),
                   private = list(
                       .question = NULL,
                       .seed = NULL,
                       .questions = NULL,
                       .batch_size = NULL,
                       .counter = NULL
                   ))


# Build the group factor and sampler closure from a question list.
# Returns list(group, n_groups, sampler).
#' @importFrom stats runif
make_group_sampler <- function(questions, group_by, group_sizes) {
    n_questions <- length(questions)

    if (is.null(group_by)) {
        group <- factor(1:n_questions)
    } else if (identical(group_by, "tags")) {
        group <- sapply(questions, function(q)
            if (is.character(q$tags) & length(q$tags) == 1) q$tags
            else digest::digest(runif(1), "md5"))
        group <- factor(group, levels = unique(group))
    } else if (is.vector(group_by)) {
        stopifnot(length(group_by) == n_questions)
        group <- factor(group_by, levels = unique(group_by))
    } else stop("Unsupported group_by")

    n_groups <- nlevels(group)

    if (is.list(group_sizes)) {
        stopifnot(setequal(names(group_sizes), levels(group)))
        stopifnot(all(group_sizes <= table(group)[names(group_sizes)]))
        group_sampler <- function(counts) {
            unlist(lapply(levels(group), function(level) {
                idxs <- which(group == level & counts > 0)
                stopifnot(length(idxs) >= group_sizes[[level]])
                idxs[sample.int(length(idxs), group_sizes[[level]])]
            }))
        }
    } else if (is.vector(group_sizes) & length(group_sizes) > 1) {
        stopifnot(n_groups == length(group_sizes))
        stopifnot(all(group_sizes <= table(group)))
        group_sampler <- function(counts) {
            unlist(lapply(1:nlevels(group), function(i) {
                level <- levels(group)[i]
                idxs <- which(group == level & counts > 0)
                stopifnot(length(idxs) >= group_sizes[i])
                idxs[sample.int(length(idxs), group_sizes[i])]
            }))
        }
    } else if (is.vector(group_sizes) & length(group_sizes) == 1) {
        stopifnot(n_groups >= group_sizes)
        group_sampler <- function(counts) {
            avail_levels <- levels(group)[sapply(levels(group), function(lv)
                any(group == lv & counts > 0))]
            stopifnot(length(avail_levels) >= group_sizes)
            # Weight by remaining capacity so no group exhausts prematurely;
            # fall back to uniform when counts are Inf
            weights <- sapply(avail_levels, function(lv) sum(counts[group == lv]))
            prob <- if (any(is.infinite(weights))) NULL else weights
            groups <- sample(avail_levels, group_sizes, prob = prob)
            sapply(groups, function(level) {
                idxs <- which(group == level & counts > 0)
                idxs[sample.int(length(idxs), 1)]
            })
        }
    }

    list(group = group, n_groups = n_groups, sampler = group_sampler)
}

#' Return a list of questions or a list of clozified questions.
#'
#' This function samples repeatedly (\code{sample_size} times without
#' replacement) from \code{questions} a number of questions
#' (controlled by \code{group_sizes} and \code{group_by}) from a set
#' of provided questions. If \code{clozified} is false, it returns a
#' list of lists of questions. If \code{clozified} is true each list
#' consisting of questions turned into a cloze question.
#'
#' The argument \code{sample_size} is the desired number of returned
#' list of questions or clozified questions.
#'
#' The argument \code{group_by} is used to first make groups of
#' questions so that sampling without replacement is done in each
#' subgroups according to \code{group_sizes}. If \code{group_by} is
#' set to \code{NULL} each question is in its own group. If
#' \code{group_by} is set to \code{"tags"}, the field tags each
#' questions is used to make groups. If \code{group_by} is a vector of
#' the same length as \code{questions} it is used as a grouping key.
#'
#' The argument \code{batch_size} is a vector of same length as
#' \code{questions} and specify the number of different versions to
#' create for each question. If \code{batch_size} is just a number,
#' it is used for each question.
#'
#' The argument \code{group_sizes} is a vector whose length is the
#' number of groups. It specified the number of questions to sample in
#' each subgroups.
#'
#' The argument \code{seed} is a base identifier that is used to
#' generate unique identifiers for each question when creating
#' multiple versions of each questions.
#'
#' @param questions A list of base questions to sample from
#' @param sample_size Number of questions to create
#' @param group_by Group question before sampling
#' @param batch_size Number of duplicates for each question
#' @param seed Seed for each question
#' @param group_sizes Size a sample in each subgroup
#' @param clozify Specify if questions should be clozified
#' @return A list of questions or a list of list of questions
#' @export
#' @importFrom stats runif
sample_questions <- function(questions,
                             sample_size = 10,
                             group_by = "tags",
                             batch_size = Inf,
                             seed = "DATA",
                             group_sizes = 1,
                             clozify = TRUE) {

    # Group mode: children of the group are sampled; each output cloze carries
    # a versioned copy of the group's hidden_data/data so sub-questions from
    # the same cloze share one consistent data context.
    if (inherits(questions, "Group")) {
        children   <- questions$children
        n_children <- length(children)

        gs            <- make_group_sampler(children, group_by, group_sizes)
        group_sampler <- gs$sampler

        counts <- if (length(batch_size) == 1)
            rep(as.numeric(batch_size), n_children)
        else {
            stopifnot(length(batch_size) == n_children)
            as.numeric(batch_size)
        }

        width      <- floor(log10(sample_size)) + 1
        ident_seed <- sprintf("%s%%0%dd", seed, width)
        original_hidden_data <- questions$hidden_data

        result <- vector("list", sample_size)
        for (i in seq_len(sample_size)) {
            idxs         <- group_sampler(counts)
            counts[idxs] <- counts[idxs] - 1

            ident <- sprintf(ident_seed, i)
            versioned_hdata <- merge_languages(bquote({
                ds_name <- .(ident)
                ds_sym  <- as.symbol(ds_name)
            }), original_hidden_data)

            selected   <- children[idxs]
            result[[i]] <- Question(type        = "cloze",
                                    questions   = selected,
                                    hidden_data = versioned_hdata,
                                    data        = questions$data,
                                    hidden_seed = questions$hidden_seed,
                                    seed        = questions$seed,
                                    header      = questions$header)
        }
        return(result)
    }

    if (inherits(questions, "SimpleQuestion"))
        questions <- list(questions)

    n_questions  <- length(questions)

    gs            <- make_group_sampler(questions, group_by, group_sizes)
    group         <- gs$group
    n_groups      <- gs$n_groups
    group_sampler <- gs$sampler

    # Make SEEDS contain a unique seed for each question
    if (length(seed) == 1) {
        width = floor(log10(n_questions)) + 1
        ident <- sprintf("%s%%0%dd", seed, width)
        seeds <- sprintf(ident, 1:n_questions)
    } else if (length(seed) == n_groups) {
        seeds <- seed[group]
        for (i in 1:n_groups) {
            level = levels(group)[i]
            n_level = length(which(group == level))
            width = floor(log10(n_level)) + 1
            idents <- sprintf(sprintf("%%0%dd", width), 1:n_level)
            seeds[group == level] <- paste0(seeds[group == level], idents)
        }
    } else {
        stopifnot(length(seed) == length(questions))
        seeds = seed
    }

    # Size of batch for each question
    if (length(batch_size) == 1)
        batch_sizes = as.list(rep(batch_size, length(questions)))
    else {
        stopifnot(length(batch_size) == length(questions))
        batch_sizes = as.list(batch_size)
    }

    # Build a sampler for each question using `Sampler`
    make_sampler <- function(question, batch_size, seed) {
        Sampler$new(question, batch_size = batch_size, seed = seed)
    }

    # Create a sampler for each question with corresponding,
    # batch_size and seed.
    samplers <- mapply(make_sampler, questions, batch_sizes, seeds)

    # Sample `sample_size` of list of questions or clozified list of
    # questions
    lapply(seq_len(sample_size), function(i) {
        # Number of remaining versions for each question
        counts <- sapply(samplers, function(s) s$count())
        stopifnot(length(group) == length(counts))

        # Indexes of chosen questions
        group_indexes <- group_sampler(counts)

        # Sample from those questions
        questions <- lapply(group_indexes, function(index) {
            sampler <- samplers[[index]]
            sampler$sample()
        })

        # Clozify these questions to have only one or return a list of
        # questions.
        if (clozify) {
            if (length(questions) == 1)
                questions[[1]]
            else {
                Question(type = "cloze", questions = questions)
            }
        } else
            questions
    })
}


