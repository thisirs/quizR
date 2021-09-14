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
                paste(deparse(lang_rep, control = c("keepInteger")), collapse = "\n")
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

    write(text, tmpfile, append = TRUE)

    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment(), envir = env, quiet = opts$quiet)
    on.exit(unlink(output), add = TRUE)

    result <- to_string(output)
    return(result)
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

# Génération de N questions résultants de la clozification de n
# questions prises au hasard dans questions.
clozify_questions <- function(questions, N, n, make_sampler = NULL, ...) {
    nq <- length(questions)

    if (is.null(make_sampler))
        sampler <- function() sample(1:nq, n)
    else
        sampler <- make_sampler(questions, ...)

    max_iter <- n*N^2
    choices <- matrix(ncol = n, nrow = 0)
    iter <- 0
    while (nrow(choices) < N & iter < max_iter) {
        spl <- sampler()
        choices <- rbind(choices, spl)
        choices <- choices[!duplicated(choices), , drop = FALSE]
        iter <- iter + 1
    }

    # Make sure we have N rows
    stopifnot(nrow(choices) == N)

    lapply(seq_len(N), function(i) {
        subquestions <- questions[choices[i, ]]
        Question(type = "cloze", hidden_seed = i, questions = subquestions)
    })
}


## Clozify questions taken from a group quiz object

## QUIZ_GROUP is a group quiz object that contains questions.

## SAMPLE_SIZE is the number of questions consisting in clozification
## of questions from QUIZ_GROUP.

## GROUP_BY is used to group questions in the group quiz object.

## GROUP_SIZES encodes the number of questions to sample from group
## quiz object before clozifying them.


clozify_group <- function(quiz_group,
                          sample_size = NULL,
                          group_by = "tags",
                          group_sizes = NULL) {

    questions <- quiz_group$children
    n <- length(questions)

    if (is.null(group_by)) {
        group <- factor(1:n)
    } else if (identical(group_by, "tags")) {
        # Extract tag or random tag if none
        group <- sapply(questions, function(q) {
            # Keep strings as is
            if(is.character(q$tags) & length(q$tags) == 1)
                q$tags
            else
                digest::digest(runif(1), "md5")
        })

        group <- factor(group, levels = unique(group))
    } else if (is.vector(group_by)) {
        stopifnot(length(group_by) == length(questions))
        # Respect order c("B", "A", "A") gives c(1, 2, 2)
        group <- factor(group, levels = unique(group_by))
    } else stop('Unsupported group_by')

    # Number of groups. From previous example: 3
    n_groups <- nlevels(group)

    if (is.list(group_sizes)) {
        stopifnot(setequal(names(group_sizes), levels(group)))
        stopifnot(all(group_sizes <= table(group)[names(group_sizes)]))

        group_sampler <- function() {
            unlist(sapply(levels(group), function(level) {
                idxs <- which(group == level)
                idxs[sample.int(length(idxs), group_sizes[[level]])]
            }))
        }
    } else if (is.vector(group_sizes) & length(group_sizes) > 1) {
        stopifnot(n_groups == length(group_sizes))
        stopifnot(all(group_sizes <= table(group)))
        group_sampler <- function() {
            unlist(sapply(1:levels(group), function(i) {
                level = levels(group)[i]
                idxs <- which(group == level)
                idxs[sample.int(length(idxs), group_sizes[i])]
            }))
        }
    } else if (is.vector(group_sizes) & length(group_sizes) == 1) {
        stopifnot(n_groups >= group_sizes)
        group_sampler <- function() {
            # Which groups to sample from
            groups <- sample(group, group_sizes)

            # Sample indexes for questions in a given group
            sapply(groups, function(level) {
                idxs <- which(group == level)
                idxs[sample.int(length(idxs), 1)]
            })
        }
    }

    cl_list <- lapply(seq_len(sample_size), function(i) {
        # Indexes of chosen questions
        group_indexes <- group_sampler()

        # Sample from those questions
        ql <- questions[group_indexes]

        ClozeQuestion$new(questions = ql)
    })

    Group$new(quiz_group$title,
              header = quiz_group$header,
              seed = quiz_group$seed,
              data = quiz_group$data,
              hidden_seed = quiz_group$hidden_seed,
              hidden_data = quiz_group$hidden_data,
              children = cl_list)
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

##' Create copies of a given group
##'
##' @param group An quiz object group
##' @param seed Identifier used to create the title
##' @param N Number of groups to create
##' @return The list of newly created groups
versionize_group <- function(group, seed, N) {
    original_hidden_data <- group$hidden_data
    title <- group$title

    width = floor(log10(N)) + 1
    ident_seed <- sprintf("%s%%0%dd", seed, width)

    lapply(seq_len(N), function(i) {

        ident <- sprintf(ident_seed, i)
        group_name <- sprintf("%s v%03d", title, i)

        ## Prepend custom ds_name and ds_sym to be used in question
        new_hdata <- merge_languages(bquote({
            ds_name <- .(ident)
            ds_sym <- as.symbol(ds_name)
        }), original_hidden_data)

        new_group <- group$clone()
        new_group$hidden_data <- new_hdata
        new_group$title <- group_name

        new_group
    })
}

#' @export
versionize_questions <- function(questions, seed, nver) {
    n <- length(questions)

    if (length(nver) == 1) nver <- rep(nver, n)
    stopifnot(length(nver) == n, all(nver > 0))

    lapply(seq_len(n), function(i) {
        question <- questions[[i]]
        if (question$type == "cloze")
            original_hidden_data <- question$cloze_hidden_data
        else
            original_hidden_data <- question$hidden_data

        lapply(seq_len(nver[i]), function(j) {
            ident <- paste0(seed, as.character(100*i + j))

            ## Prepend custom ds_name and ds_sym to be used in question
            new_hdata <- merge_languages(bquote({
                ds_name <- .(ident)
                ds_sym <- as.symbol(ds_name)
            }), original_hidden_data)

            qc <- question$copy()

            if (question$type == "cloze")
                qc$cloze_hidden_data <- new_hdata
            else
                qc$hidden_data <- new_hdata

            qc
        })
    })
}

# Build NVER versions of QUESTIONS with SEED identifier. Then
# construct N cloze questions from K different questions with random
# version.
#' @export
clozify_independant_questions <- function(questions, seed, N, k, nver) {
    n <- length(questions)
    if (length(nver) == 1) nver <- rep(nver, n)

    stopifnot(floor(sum(nver)/k) >= N)
    stopifnot(length(questions) >= k)
    # stopifnot(rev(sort(nver))[k] >= N)

    versions_list <- versionize_questions(questions, seed, nver)

    ## Sample from VERSIONS_LIST
    questions_list <- list()
    for (i in seq_len(N)) {
        # Current number of versions
        nver <- sapply(versions_list, length)

        # Choose k questions that are present the most
        qchoice <- sample(which(nver >= rev(sort(nver))[k]), k)

        # Choose versions
        ver_choice <- ceiling(runif(k) * nver[qchoice])

        # Extracting these questions
        questions_set <- list()
        for (j in seq_len(k)) {
            question <- versions_list[[qchoice[j]]][[ver_choice[j]]]
            versions_list[[qchoice[j]]][[ver_choice[j]]] <- NULL
            questions_set <- c(questions_set, list(question))
        }

        qq <- Question(type = "cloze", questions = questions_set)

        questions_list <- c(questions_list, list(qq))
    }

    questions_list
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
#' The argument \code{seed} is used to set the seed for each question.
#'
#' @param questions A list of base questions to sample from
#' @param sample_size Number of questions to create
#' @param group_by Group question before sampling
#' @param batch_size Number of duplicates for each question
#' @param seed Seed for each question
#' @param group_sizes Size a sample in each subgroup
#' @param clozify Specify if questions should be clozified
#' @return A list of questions or a list of list of questions
#' @importFrom stats runif
sample_questions <- function(questions,
                             sample_size = 10,
                             group_by = "tags",
                             batch_size = Inf,
                             seed = "DATA",
                             group_sizes = 1,
                             clozify = TRUE) {

    if (inherits(questions, "SimpleQuestion"))
        questions <- list(questions)

    n_questions  <- length(questions)

    # Assign each question a group from GROUP_BY
    if (is.null(group_by)) {
        group <- factor(1:n_questions)
    } else if (identical(group_by, "tags")) {
        ## Extract tag or random tag if none
        group <- sapply(questions, function(q)
            ## Keep strings as is
            if(is.character(q$tags) & length(q$tags) == 1)
                q$tags
            else
                digest::digest(runif(1), "md5"))

        group <- factor(group, levels = unique(group))
    } else if (is.vector(group_by)) {
        stopifnot(length(group_by) == length(questions))
        # Respect order c("B", "A", "A") gives c(1, 2, 2)
        group <- factor(group_by, levels = unique(group_by))
    } else stop('Unsupported group_by')

    n_groups <- length(unique(group))

    # Set GROUP_SAMPLER; how to sample from the list of questions with
    # groups and GROUP_SIZES
    if (is.list(group_sizes)) {
        stopifnot(setequal(names(group_sizes), levels(group)))
        stopifnot(all(group_sizes <= table(group)[names(group_sizes)]))
        group_sampler <- function(counts) {
            unlist(lapply(levels(group), function(level) {
                idxs <- which(group == level & counts > 0)
                stopifnot(length(idxs) >= group_sizes[level])
                idxs[sample.int(length(idxs), group_sizes[level])]
            }))
        }
    } else if (is.vector(group_sizes) & length(group_sizes) > 1) {
        stopifnot(n_groups == length(group_sizes))
        stopifnot(all(group_sizes <= table(group)))
        group_sampler <- function(counts) {
            unlist(lapply(1:nlevels(group), function(i) {
                level = levels(group)[i]
                idxs <- which(group == level & counts > 0)
                stopifnot(length(idxs) >= group_sizes[i])
                idxs[sample.int(length(idxs), group_sizes[i])]
            }))
        }
    } else if (is.vector(group_sizes) & length(group_sizes) == 1) {
        stopifnot(n_groups >= group_sizes)
        group_sampler <- function(counts) {
            ## Which groups to sample from
            groups <- sample(levels(group), group_sizes)

            ## Sample indexes for questions in a given group
            sapply(groups, function(level) {
                idxs <- which(group == level & counts > 0)
                idxs[sample.int(length(idxs), 1)]
            })
        }
    }

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
            idents <- sprintf(sprintf("%%0%dd%d", width), 1:n_level)
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

    # Build a sampler for one question
    make_sampler <- function(question, batch_size, seed) {
        Sampler$new(question, batch_size = batch_size, seed = seed)
    }

    # Create a sampler for each question with corresponding,
    # batch_size and seed.
    samplers <- mapply(make_sampler, questions, batch_sizes, seeds)

    # Sample SAMPLE_SIZE of list of questions or clozified list of
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
            else
                Question(type = "cloze", questions = questions)
        } else
            questions
    })
}


