#' Directed Acyclic Graph
#'
#' Define a directed acyclic graph (DAG).
#'
#' @param x A character string containing a sequence of definitions
#'     of edges in the form `X -> Y`, `X <- Y` or `X <-> Y`.
#'     See details for more advanced constructs.
#'
#' @details
#' The syntax for `x` follows closely that of [dagitty] for compatibility.
#' The resulting adjacency matrix of the definition is checked for cycles.
#'
#' Directed edges are defined as `X -> Y` meaning that there is an edge from
#' `X` to `Y` in the graph. Edges can be combined in sequence to create paths
#' for concise descriptions, for example `X -> Y <- Z -> W`.
#'
#' Unobserved latent confounders are defined using bidirected edges as
#' `X <-> Y` which means that there is an additional variable `U[X,Y]` in
#' the graph, and the edges `X <- U[X,Y] -> Y`, respectively.
#'
#' Groups of vertices can be defined by enclosing the vertices within
#' curly braces. For example `X -> {Y Z}` defines that the dag has an edge
#' from `X` to both `Y` and `Z`.
#
#' Different statements in `x` are automatically distinguished from one
#' another without any additional delimiters, but semicolons, commas
#' and line breaks can be used if desired.
#'
#' Note that in the context of this package, vertex labels will always be
#' converted into upper case, meaning that typing `Z` or `z` within `x` will
#' always represent the same variable. This is done to enforce the notation
#' of counterfactual variables, where capital letter denote variables,
#' and small letters denote their value assignments.
#'
#' @return An object of class `dag`, which is a square adjacency matrix
#'     with the following attributes:
#' * `labels` A character vector (or a list) of vertex labels.
#' * `latent` A logical vector indicating latent variables.
#' * `order` An integer vector giving a topological order for the vertices.
#' .
#' @examples
#' dag("x -> {y z} <- w <-> g")
#'
#' # Groups can appear on both sides of an edge
#' dag("{x z} -> {y w}")
#'
#' # Semicolons can be used to distinguish individual statements
#' dag("x -> z -> y; x <-> y")
#'
#' # Commas can be used to distinguish variables for example
#' dag("{x, y, z} -> w")
#'
#' # Line breaks are also allowed
#' dag("z -> w
#'      x -> y")
#'
#' @export
dag <- function(x) {
    x <- try_type(x = x, type = "character")
    x <- toupper(x)
    x <- gsub("[;\\,\r\n\t]", " ", x)
    if (!nchar(trimws(x))) {
        stop_("Argument `x` contains only whitespace or special characters")
    }
    e_within <- grepl("\\{[^\\}]+[<>\\-]+[^\\{]+\\}", x)
    if (e_within) {
        stop_("Edges are not allowed within groups defined by {...}")
    }
    g_str <- reg_match(x, "\\{([^<>\\-]+)\\}|([^<> \\-]+)|(<->|<-|->)")
    g_str[2,] <- trimws(g_str[2,])
    g_type <- apply(g_str[-1,,drop = FALSE], 2, function(x) nchar(x) > 0)
    g_str <- g_str[rbind(FALSE, g_type)]
    g_lst <- strsplit(g_str, "[ ]+", perl = TRUE)
    e_lst <- which(sapply(g_lst, function(e) e[1] %in% c("<->", "<-", "->")))
    v_lst <- Filter(function(v) all(!v %in% c("<->", "<-", "->")), g_lst)
    v_names <- unique(unlist(v_lst))
    n_v <- length(v_names)
    n_e <- length(e_lst)
    A_obs <- matrix(0L, n_v, n_v)
    A_bi <- matrix(0L, n_v, n_v)
    rownames(A_obs) <- colnames(A_obs) <- v_names
    rownames(A_bi) <- colnames(A_bi) <- v_names
    if (n_e) {
        for (i in e_lst) {
            if (i == 1 || i == length(g_lst)) {
                stop_("Invalid edge construct in argument `x`")
            }
            pairs <- expand.grid(g_lst[[i - 1]], g_lst[[i + 1]],
                                 stringsAsFactors = FALSE)
            if (identical(g_lst[[i]], "->")) {
                for (j in 1:nrow(pairs)) {
                    A_obs[pairs[j,1], pairs[j,2]] <- 1L
                }
            } else if (identical(g_lst[[i]], "<-")) {
                for (j in 1:nrow(pairs)) {
                    A_obs[pairs[j,2], pairs[j,1]] <- 1L
                }
            } else if (identical(g_lst[[i]], "<->")) {
                for (j in 1:nrow(pairs)) {
                    if (identical(pairs[j,1], pairs[j,2])) {
                        stop_("Invalid bidirected edge: ",
                              pairs[j,1], " <-> ", pairs[j,1])
                    }
                    A_bi[pairs[j,2], pairs[j,1]] <- 1L
                    A_bi[pairs[j,1], pairs[j,2]] <- 1L
                }
            }
        }
    }
    if (detect_cycles(A_obs)) {
        stop_("The graph specified by argument `x` is not acyclic")
    }
    n_u <- sum(A_bi[lower.tri(A_bi)])
    n_vu <- n_v + n_u
    A <- matrix(0L, n_vu, n_vu)
    labels <- character(n_vu)
    latent <- logical(n_vu)
    A[1:n_v,1:n_v] <- A_obs
    labels[1:n_v] <- v_names
    if (n_u > 0L) {
        k <- n_v
        for (i in 1:(n_v - 1)) {
            for (j in (i + 1):n_v) {
                if (A_bi[i,j]) {
                    k <- k + 1L
                    A[k,i] <- 1L
                    A[k,j] <- 1L
                    labels[k] <- paste0("U[", v_names[i], ",", v_names[j], "]")
                }
            }
        }
        latent[(n_v + 1):n_vu] <- TRUE
    }
    structure(A,
              labels = labels,
              latent = latent,
              order = topological_order(A, latent),
              class = "DAG")
}

# Parallel Worlds Graph
#
# Construct a parallel worlds graph from a DAG or an ADMG.
#
# @param g A `dag` object.
# @param gamma An object of class \code{CounterfactualConjunction}.
# @return A list representing the parallel worlds graph.
pwg <- function(g, gamma) {
    lab <- attr(g, "labels")
    lat <- attr(g, "latent")
    ord <- attr(g, "order")
    intv_lst <- unique(ints(gamma))
    intv_var <- lapply(intv_lst, function(i) which(lab %in% names(i)))
    n_worlds <- length(intv_lst)
    n_unobs <- sum(lat)
    n_obs <- length(lab) - n_unobs
    no_err <- integer(0)
    if (n_unobs) {
        no_err <- children(which(lat), g)
    }
    n_err <- n_obs - length(no_err)
    n_obs_pw <- n_worlds * n_obs
    n_unobs_pw <- n_err + n_unobs
    n_total <- n_obs_pw + n_unobs_pw
    A_pw <- matrix(0L, n_total, n_total)
    ix_obs <- 1:n_obs
    ix_err <- (n_obs_pw + 1):(n_obs_pw + n_err)
    ix_unobs <- (n_obs_pw + n_err + 1):n_total
    labels_pw <- character(n_total)
    labels_pw[1:n_obs] <- lab[1:n_obs]
    order_pw <- integer(n_total)
    if (n_err) {
        err_vars <- paste0("U[", lab[setdiff(ix_obs, no_err)], "]")
        labels_pw[ix_err] <- lapply(err_vars, cf)
    }
    if (n_unobs) {
        labels_pw[ix_unobs] <- lapply(lab[lat], cf)
    }
    for (w in 1:n_worlds) {
        offset <- (w - 1L) * n_obs
        from <- offset + 1L
        to <- offset + n_obs
        ix <- from:to
        A_pw[ix,ix] <- g[ix_obs,ix_obs]
        order_pw[n_unobs_pw + ix] <- offset + ord[-(1:n_unobs)]
        if (n_err) {
            ix_err_mat <- cbind(ix_err, setdiff(ix, offset + no_err))
            A_pw[ix_err_mat] <- 1L
        }
        if (n_unobs) {
            A_pw[ix_unobs,ix] <- g[-ix_obs,ix_obs]
        }
        intv_ix <- offset + intv_var[[w]]
        A_pw[,intv_ix] <- 0L
        labels_pw[from:to] <- lapply(ix_obs, function(v) {
            if (v %in% intv_var[[w]]) {
                cf(var = lab[v], obs = intv_lst[[w]])
            } else {
                cf(var = lab[v], int = intv_lst[[w]])
            }
        })
    }
    latent_pw <- logical(n_obs_pw)
    if (n_err) {
        order_pw[1:n_err] <- ix_err
        latent_pw[ix_err] <- TRUE
    }
    if (n_unobs_pw) {
        order_pw[(n_err + 1):(n_unobs_pw)] <- ix_unobs
        latent_pw[ix_unobs] <- TRUE
    }
    list(adjacency = A_pw,
         labels = labels_pw,
         latent = latent_pw,
         order = order_pw,
         n_obs = n_obs_pw,
         n_unobs = n_unobs_pw,
         n_obs_orig = n_obs,
         n_worlds = n_worlds)
}

# Counterfactual Graph
#
# Construct a counterfactual graph.
#
# @param p A list representing a parallel worlds graph from `pwd()`.
# @param gamma A `Counterfactualconjunction` object.
# @return A `dag` object representing the causal graph.
cg <- function(p, gamma) {
    A <- p$adjacency
    A_cg <- p$adjacency
    n_total <- p$n_obs + p$n_unobs
    merged <- list()
    keep <- rep(TRUE, n_total)
    eq_val <- rep(-(p$n_worlds + 1L), n_total)
    eq_val[1:p$n_obs] <- -1L * rep(1:p$n_worlds, each = p$n_obs_orig)
    if (p$n_worlds > 1) {
        for (v in 1:p$n_obs_orig) {
            world_skip <- logical(p$n_worlds)
            for (a in 1:(p$n_worlds - 1)) {
                if (world_skip[a]) {
                    next
                }
                val_pa_alpha <- NULL
                val_alpha <- NULL
                a_offset <- (a - 1L) * p$n_obs_orig
                alpha_ix <- p$order[p$n_unobs + a_offset + v]
                alpha <- p$labels[[alpha_ix]]
                pa_alpha <- parents(alpha_ix, A)
                n_pa_alpha <- length(pa_alpha)
                if (!n_pa_alpha) {
                    val_alpha <- alpha$obs
                    eq_val[alpha_ix] <- val_alpha
                } else {
                    val_alpha <- val(alpha, gamma)
                    if (!is.null(val_alpha)) {
                        eq_val[alpha_ix] <- val_alpha
                    }
                    val_pa_alpha <- eq_val[pa_alpha]
                }
                for (b in (a + 1):p$n_worlds) {
                    if (world_skip[b]) {
                        next
                    }
                    same_var <- FALSE
                    val_pa_beta <- NULL
                    val_beta <- NULL
                    b_offset <- (b - 1L) * p$n_obs_orig
                    beta_ix <- p$order[p$n_unobs + b_offset + v]
                    beta <- p$labels[[beta_ix]]
                    pa_beta <- parents(beta_ix, A)
                    n_pa_beta <- length(pa_beta)
                    if (!n_pa_beta) {
                        val_beta <- beta$obs
                        eq_val[beta_ix] <- val_beta
                    } else {
                        val_beta <- val(beta, gamma)
                        if (!is.null(val_beta)) {
                            eq_val[beta_ix] <- val_beta
                        }
                        val_pa_beta <- eq_val[pa_beta]
                    }
                    if (!n_pa_alpha || !n_pa_beta) {
                        if (identical(val_alpha, val_beta)) {
                            same_var <- TRUE
                        }
                    } else if (identical(val_pa_alpha, val_pa_beta)) {
                        same_var <- TRUE
                    }
                    if (same_var) {
                        if (!val_consistent(val_alpha, val_beta)) {
                            return(list(consistent = FALSE))
                        }
                        if (is.null(val_alpha) && !is.null(val_beta)) {
                            eq_val[alpha_ix] <- val_beta
                        } else if (!is.null(val_alpha) && is.null(val_beta)) {
                            eq_val[beta_ix] <- val_alpha
                        } else {
                            eq_val[beta_ix] <- eq_val[alpha_ix]
                        }
                        world_skip[b] <- TRUE
                        ch_beta <- children(beta_ix, A)
                        A_cg[pa_beta,alpha_ix] <- 1L
                        A_cg[alpha_ix,ch_beta] <- 1L
                        keep[beta_ix] <- FALSE
                        merged <- c(merged, list(list(original = beta,
                                                      replacement = alpha)))
                    }
                }
            }
        }
    }
    cg_dag <- structure(A_cg,
                        labels = p$labels,
                        latent = p$latent,
                        order = p$order,
                        class = "DAG")
    cg_dag <- subgraph(keep, cg_dag)
    n_cf <- length(gamma)
    gamma_ix <- 1:n_cf
    labels_cg <- attr(cg_dag, "labels")
    label_ix <- seq_along(labels_cg)
    for (i in seq_along(merged)) {
        for (g in gamma_ix) {
            g_temp <- gamma[[g]]
            g_obs <- g_temp$obs
            if (!length(merged[[i]]$original$obs)) {
                g_temp$obs <- integer(0)
            }
            if (identical(g_temp, merged[[i]]$original)) {
                gamma[[g]] <- merged[[i]]$replacement
                gamma[[g]]$obs <- g_obs
                gamma_ix <- setdiff(gamma_ix, g)
                break
            }
        }
        for (l in label_ix) {
            if (identical(labels_cg[[l]], merged[[i]]$original)) {
                labels_cg[[l]] <- merged[[i]]$replacement
                label_ix <- setdiff(label_ix, l)
                break
            }
        }
    }
    attr(cg_dag, "labels") <- labels_cg
    query_vars <- integer(n_cf)
    for (g in 1:n_cf) {
        for (l in 1:length(labels_cg)) {
            var_match <- FALSE
            int_match <- FALSE
            if (identical(gamma[[g]]$var, labels_cg[[l]]$var)) {
                if (length(labels_cg[[l]]$obs)) {
                    if (gamma[[g]]$obs == labels_cg[[l]]$obs) {
                        var_match <- TRUE
                    }
                } else {
                    var_match <- TRUE
                }
            }
            if (identical(gamma[[g]]$int, labels_cg[[l]]$int)) {
                int_match <- TRUE
            }
            if (var_match && int_match) {
                query_vars[g] <- l
            }
        }
    }
    an <- sort(union(query_vars, ancestors(query_vars, cg_dag)))
    cg_dag <- subgraph(an, cg_dag)
    latent_cg <- attr(cg_dag, "latent")
    keep <- rep(TRUE, length(an))
    if (any(latent_cg)) {
        for (l in which(latent_cg)) {
            ch_l <- children(l, cg_dag)
            if (length(ch_l) < 2L) {
                keep[l] <- FALSE
            }
        }
    }
    list(graph = subgraph(keep, cg_dag),
         conjunction = gamma,
         merged = merged,
         consistent = TRUE)
}

#' Import Graph
#'
#' Import and construct a valid DAG from an external format.
#'
#' @param x A graph object in a valid external format, see details.
#'
#' @details
#' Argument `x` accepts [dagitty] graphs, [igraph] graphs
#' in the [causaleffect] package syntax and character strings
#' in the [dosearch] package syntax.
#'
#' @return A `DAG` object if successful.
#' @export
import_graph <- function(x) {
    out <- NULL
    if (inherits(x, "dagitty")) {
        names(x) <- NULL
        class(x) <- NULL
        x <- trimws(gsub("[;\\,\r\n\t]", " ", x))
        x_def <- reg_match(x, "dag \\{(.+)\\}")
        if (length(x_def)) {
            out <- dag(x_def[2,1])
        } else {
            stop_("Unable to parse argument `x` into an object of class `DAG`")
        }
    } else if (inherits(x, "igraph")) {
        if (requireNamespace("igraph", quietly = TRUE)) {
            e <- igraph::E(x)
            v <- igraph::vertex_attr(x, "name")
            g_obs <- ""
            g_unobs <- ""
            description <- NULL
            obs_edges <- e[(is.na(description) | description != "U")]
            unobs_edges <- e[description == "U" & !is.na(description)]
            if (length(obs_edges)) {
                obs_ind <- igraph::get.edges(x, obs_edges)
                g_obs <- paste(v[obs_ind[,1]], "->", v[obs_ind[,2]], collapse = " ")
            }
            if (length(unobs_edges)) {
                unobs_ind <- igraph::get.edges(x, unobs_edges)
                unobs_ind <- unobs_ind[unobs_ind[,1] < unobs_ind[,2],,drop=FALSE]
                g_unobs <- paste(v[unobs_ind[,1]], "<->",
                                 v[unobs_ind[,2]], collapse = " ")
            }
            out <- dag(paste0(c(g_obs, g_unobs), collapse = " "))
        } else {
            stop_("Attempting to use `igraph` input, but the package is not available")
        }
    } else if (is.character(x)) {
        out <- dag(x)
    } else if (is.null(out)) {
        stop_("Argument `x` has an unrecognized format")
    }
    out
}

#' Export Graph
#'
#' Convert a valid graph object to a supported external format.
#'
#' @param g An object of class `DAG`.
#' @param type A character string matching one of the following:
#'     "dagitty", "causaleffect" or "dosearch".
#' @param ... Additional arguments passed to `format`
#'     for formatting vertex labels.
#' @export
export_graph <- function(
    g,
    type = c("dagitty", "causaleffect", "dosearch"),
    use_bidirected = TRUE,
    ...
) {
    if (!is_dag(g)) {
        stop_("Argument `x` must be an object of class `DAG`")
    }
    type <- match.arg(type)
    if (identical(type, "dagitty")) {
        if (requireNamespace("dagitty", quietly = TRUE)) {
            lab <- attr(g, "labels")
            lab_form <- sapply(lab, format)
            lat <- attr(g, "latent")
            n_v <- ncol(g)
            n_e <- sum(g)
            e_bi_str <- character(n_e)
            e_di_str <- character(n_e)
            e_bi_ix <- 0L
            e_di_ix <- 0L
            for (i in 1:n_v) {
                if (use_bidirected && lat[i] && sum(g[i,]) == 2L) {
                    e_bi_ix <- e_bi_ix + 1L
                    bi <- which(g[i,] > 0)
                    e_bi_str[e_bi_ix] <- paste0(lab_form[bi[1]], " <-> ", lab_form[bi[2]])
                } else {
                    for (j in 1:n_v) {
                        if (g[i,j]) {
                            e_di_ix <- e_di_ix + 1L
                            e_di_str[e_di_ix] <- paste0(lab_form[i], " -> ", lab_form[j])
                        }
                    }
                }
            }
            e_bi_str <- e_bi_str[nchar(e_bi_str) > 0L]
            e_di_str <- e_di_str[nchar(e_di_str) > 0L]
            e_str <- collapse(paste0(e_di_str, collapse = " "),
                              " ",
                              paste0(e_bi_str, collapse = " "))
            out <- dagitty::dagitty(collapse("dag {", e_str, "}"))
        } else {
            stop_("Package `dagitty` is not available for export")
        }
    }
    out
}

# Verify that argument is a valid DAG
is_dag <- function(x) {
    inherits(x, "DAG")
}

# Create a counterfactual graph directly from a DAG
make_cg <- function(g, gamma) {
    cg(pwg(g, gamma), gamma)
}
