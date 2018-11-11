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
#' Helper function
#' @param Expressions
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

render_HTML <- function(text, args, info) {
    if (is.null(info$env))
        env <- parent.frame()
    else
        env <- info$env

    tmpfile <- tempfile("question", fileext = ".Rmd")
    write(text, tmpfile, append = TRUE)
    output <- rmarkdown::render(tmpfile, rmarkdown::html_fragment(), envir = env, quiet = args$quiet)
    to_string(output)
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

clozify_group <- function(group, N, k, sampler = NULL) {
    questions <- group$children
    n <- length(questions)
    stopifnot(k <= n)
    if (is.null(sampler))
        sample_questions <- function() questions[sample(1:n, k)]
    else
        sample_questions <- sampler(questions)

    cl_list <- lapply(seq_len(N), function(i) {
        ql <- sample_questions()
        ClozeQuestion$new(questions = ql)
    })

    Group$new(group$title,
              header = group$header,
              seed = group$seed,
              data = group$data,
              hidden_seed = group$hidden_seed,
              hidden_data = group$hidden_data,
              children = cl_list)
}

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

versionize_group <- function(group, seed, N) {
    original_hidden_data <- group$hidden_data
    title <- group$title

    lapply(seq_len(N), function(i) {
        ident <- sprintf("%s%03d", seed, i)
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

Sampler <- R6::R6Class(
                   "Sampler",
                   public = list(
                       initialize = function(question,
                                             policy = "copy",
                                             batch_size = NULL,
                                             seed = NULL) {
                           private$.question = question
                           private$.policy = policy
                           private$.batch_size = batch_size
                           private$.seed = seed
                           private$.counter = 0
                       },
                       sample = function() {
                           if(!is.null(private$.batch_size))
                               stopifnot(private$.counter < private$.batch_size)

                           private$.counter = private$.counter + 1
                           ident <- sprintf(private$.seed, private$.counter)

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
                           if(private$.policy != "batch")
                               -1
                           else
                               private$.batch_size - private$.counter
                       }),
                   active = list(
                       seed = function() private$.seed
                   ),
                   private = list(
                       .policy = NULL,
                       .question = NULL,
                       .seed = NULL,
                       .questions = NULL,
                       .batch_size = NULL,
                       .counter = NULL
                   ))


sample_questions <- function(questions,
                             group = "tag",
                             policy = "batch",
                             batch_size = 5,
                             seed = "DATA",
                             sample_size = 10,
                             # group_sampler = NULL,
                             group_sizes = 2,
                             clozify = TRUE
                             ) {

    # Set group as c(1, 1, 2, 3, 3)
    if (group == "tag") {
        tags <- sapply(questions, function(q) if(is.null(q$tag)) runif(1) else q$tag)
        hashes <- sapply(tags, function(tag) digest::digest(tag, "md5"))
        group <- as.numeric(factor(hashes))
    }

    # Number of groups
    n_groups <- length(unique(group))

    if (length(group_sizes) > 1)
        stopifnot(n_groups == length(group_sizes))
    else
        stopifnot(n_groups >= group_sizes)

    # Set a unique seed for each question
    if (length(seed) == 1) {
        ident <- sprintf("%s%%0%dd", seed, floor(log10(group)) + 1)
        seeds <- paste0(sprintf(ident, 1:length(group)), "%02d")
        seeds = as.list(seeds)
    } else if (length(seed) == n_groups) {
        seeds = seed[group]
        for (gp in 1:n_groups) {
            ng <- sum(group == gp)
            seeds[group == gp] <- paste0(seeds[group == gp], sprintf("%d", 1:ng))
        }
        seeds = as.list(seeds)
    } else {
        stopifnot(length(seed) == length(questions))
        seeds = as.list(paste0(seed, "%02d"))
    }

    if (length(policy) == 1)
        policies = as.list(rep(policy, length(questions)))
    else {
        stopifnot(length(policy) == length(questions))
        policies = policy
    }

    if (length(batch_size) == 1)
        batch_sizes = as.list(rep(batch_size, length(questions)))
    else {
        stopifnot(length(batch_size) == length(questions))
        batch_sizes = as.list(batch_size)
    }

    make_sampler <- function(question, policy, batch_size, seed) {
        if (policy == "batch")
            Sampler$new(question, policy = "batch", batch_size = batch_size, seed = seed)
        else
            Sampler$new(question, policy = policy, seed = seed)
    }

    samplers <- mapply(make_sampler, questions, policies, batch_sizes, seeds)

    # Set how we sample from group information
    if (length(group_sizes) == 1) {
        group_sampler <- function(counts) {
            groups <- sample(1:n_groups, group_sizes)
            sapply(groups, function(g) sample(which(group == g), 1))
        }
    } else {
        group_sampler <- function(counts) {
            sapply(seq_len(n_groups), function(i) {
                idxs <- which(group == i)
                sample(idxs, group_sizes[i])
            })
        }
    }

    lapply(seq_len(sample_size), function(i) {
        counts <- sapply(samplers, function(s) s$count())
        stopifnot(length(group) == length(counts))
        group_indexes <- group_sampler(counts)
        questions <- lapply(group_indexes, function(index) {
            sampler <- samplers[[index]]
            sampler$sample()
        })

        if (clozify)
            Question(type = "cloze", questions = questions)
        else
            questions
    })
}
