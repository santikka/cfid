#' Directed Acyclic Graph
#'
#' Define a directed acyclic graph (DAG) describing the causal model.
#'
#' The syntax for `x` follows the `dagitty` package closely for compatibility.
#' However, not all features of `dagitty` graphs are supported.
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
#' curly braces. For example `X -> {Y Z}` defines that the graph has an edge
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
#' @param x A `character` string containing a sequence of paths consisting of
#' of edges in the form `X -> Y`, `X <- Y` or `X <-> Y`.
#' See details for more advanced constructs.
#' @return An object of class `dag`, which is a square adjacency matrix
#' with the following attributes:
#'
#' * `labels`\cr A `character` vector (or a list) of vertex labels.
#' * `latent`\cr A `logical` vector indicating latent variables.
#' * `order`\cr An `integer` vector giving a topological order for the vertices.
#' * `text`\cr A `character` string giving representing the DAG.
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
#' # Commas can be used to distinguish variables
#' dag("{x, y, z} -> w")
#'
#' # Line breaks are also allowed
#' dag("z -> w
#'      x -> y")
#'
#' @export
dag <- function(x) {
  stopifnot_(
    length(x) == 1L,
    "Argument `x` must be a single character string."
  )
  x <- try_type(x = x, type = "character")
  x <- toupper(x)
  x <- gsub("[;\\,\r\n\t]", " ", x)
  stopifnot_(
    nzchar(trimws(x)),
    "Argument `x` contains only whitespace or special characters."
  )
  e_within <- grepl("\\{[^\\}]+[<>\\-]+[^\\{]+\\}", x)
  stopifnot_(
    !e_within,
    "Edges are not allowed within groups defined by {...}."
  )
  g_str <- reg_match(x, "\\{([^<>\\-]+)\\}|([^<> \\-]+)|(<->|<-|->)")
  g_str[2L, ] <- trimws(g_str[2L, ])
  g_type <- apply(g_str[-1L, , drop = FALSE], 2L, nzchar)
  g_str <- g_str[rbind(FALSE, g_type)]
  g_lst <- strsplit(g_str, "[ ]+", perl = TRUE)
  e_lst <- which(
    vapply(g_lst, function(e) e[1L] %in% c("<-", "->", "<->"), logical(1L))
  )
  v_lst <- Filter(function(v) all(!v %in% c("<->", "<-", "->")), g_lst)
  v_names <- unique(unlist(v_lst))
  n_v <- length(v_names)
  n_e <- length(e_lst)
  n_g <- length(g_lst)
  A_obs <- matrix(0L, n_v, n_v)
  A_bi <- matrix(0L, n_v, n_v)
  rownames(A_obs) <- colnames(A_obs) <- v_names
  rownames(A_bi) <- colnames(A_bi) <- v_names
  for (i in e_lst) {
    stopifnot_(
      i > 1L && i < n_g,
      "Invalid edge construct in argument `x`."
    )
    pairs <- expand.grid(
      g_lst[[i - 1L]],
      g_lst[[i + 1L]],
      stringsAsFactors = FALSE
    )
    p <- seq_len(nrow(pairs))
    if (g_lst[[i]] %in% c("<-", "->")) {
      right <- identical(g_lst[[i]], "->")
      lhs <- ifelse_(right, 1L, 2L)
      rhs <- ifelse_(right, 2L, 1L)
      ix <- cbind(pairs[p, lhs], pairs[p, rhs])
      A_obs[ix] <- 1L
    } else {
      ix <- rbind(
        cbind(pairs[p, 1L], pairs[p, 2L]),
        cbind(pairs[p, 2L], pairs[p, 1L])
      )
      invalid <- which(ix[, 1L] == ix[, 2L])
      stopifnot_(
        length(invalid) == 0L,
        paste0(
          "Invalid bidirected edge in `g`: ",
          pairs[invalid[1], 1L], " <-> ", pairs[invalid[1], 1L], "."
        )
      )
      A_bi[ix] <- 1L
    }
  }
  stopifnot_(
    !detect_cycles(A_obs),
    "The graph specified by argument `x` is not acyclic."
  )
  A_bi[lower.tri(A_bi)] <- 0L
  n_u <- sum(A_bi)
  n_vu <- n_v + n_u
  A <- matrix(0L, n_vu, n_vu)
  labels <- character(n_vu)
  latent <- logical(n_vu)
  A[seq_len(n_v), seq_len(n_v)] <- A_obs
  labels[seq_len(n_v)] <- v_names
  u_ix <- seq_asc(n_v + 1L, n_vu)
  lhs_ix <- rep(u_ix, 2L)
  rhs_ix <- which(A_bi == 1L, arr.ind = TRUE)
  A[cbind(lhs_ix, c(rhs_ix))] <- 1L
  labels[u_ix] <- paste0(
    "U[", v_names[rhs_ix[, 1L]], ",", v_names[rhs_ix[, 2L]], "]"
  )
  latent[u_ix] <- TRUE
  ord <- topological_order(A, latent)
  dag_str <- dag_string(A, labels, latent, ord)
  structure(
    A,
    labels = labels,
    latent = latent,
    order = ord,
    text = dag_str,
    class = "dag"
  )
}

#' Convert a DAG into a character string
#'
#' @param A An adjacency matrix.
#' @param lab A `character` vector of vertex labels.
#' @param lat A `logical` vector indicating latent variables by `TRUE`.
#' @param ord An `integer` vector giving a topological order of `A`.
#' @return A `character` representation of the DAG.
#' @noRd
dag_string <- function(A, lab, lat, ord) {
  ord <- ord[!lat[ord]]
  e <- character(ncol(A))
  for (i in ord) {
    ch <- lab[children(i, A)]
    n_ch <- length(ch)
    if (n_ch > 0L) {
      lhs <- ifelse_(n_ch > 1L, "{", "")
      rhs <- ifelse_(n_ch > 1L, "}", "")
      e[i] <- paste0(
        format(lab[i]), " -> ", lhs, paste0(ch, collapse = ", "), rhs
      )
    }
  }
  for (i in which(lat)) {
    e[i] <- paste0(format(lab[children(i, A)]), collapse = " <-> ")
  }
  paste0(e[nzchar(e)], collapse = "; ")
}

#' Is the argument a `dag` object?
#'
#' @param x An R object.
#' @return A `logical` value that is `TRUE` if the object is a `dag`.
#' @noRd
is.dag <- function(x) {
  inherits(x, "dag")
}

#' @method print dag
#' @rdname dag
#' @param x A `dag` object
#' @param ... Not used
#' @export
print.dag <- function(x, ...) {
  stopifnot_(
    is.dag(x),
    "Argument `x` must be a `dag` object."
  )
  cat(attr(x, "text"))
  invisible(x)
}

#' Parallel Worlds Graph
#'
#' @param g A `dag` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @return A `list` representing the parallel worlds graph.
#' @noRd
pwg <- function(g, gamma) {
  lab <- attr(g, "labels")
  lat <- attr(g, "latent")
  ord <- attr(g, "order")
  sub_lst <- unique(subs(gamma))
  #sub_lst <- sub_lst[order(lengths(sub_lst))]
  sub_var <- lapply(sub_lst, function(i) which(lab %in% names(i)))
  n_worlds <- length(sub_lst)
  n <- length(lab)
  n_unobs <- sum(lat)
  n_obs <- n - n_unobs
  no_err <- children(which(lat), g)
  n_err <- n_obs - length(no_err)
  n_obs_pw <- n_worlds * n_obs
  n_unobs_pw <- n_err + n_unobs
  n_total <- n_obs_pw + n_unobs_pw
  A_pw <- matrix(0L, n_total, n_total)
  ix_obs <- seq_len(n_obs)
  ix_err <- seq_asc(n_obs_pw + 1L, n_obs_pw + n_err)
  ix_unobs <- seq_asc(n_obs_pw + n_err + 1L, n_total)
  labels_pw <- character(n_total)
  labels_pw[seq_len(n_obs)] <- lab[seq_len(n_obs)]
  order_pw <- integer(n_total)
  err_vars <- paste0("U[", lab[setdiff(ix_obs, no_err)], "]")
  labels_pw[ix_err] <- lapply(err_vars, cf)
  labels_pw[ix_unobs] <- lapply(lab[lat], cf)
  for (w in seq_len(n_worlds)) {
    offset <- (w - 1L) * n_obs
    from <- offset + 1L
    to <- offset + n_obs
    ix <- seq.int(from, to)
    A_pw[ix, ix] <- g[ix_obs, ix_obs]
    ix_err_mat <- ifelse_(
      n_err > 0L,
      cbind(ix_err, setdiff(ix, offset + no_err)),
      integer(0L)
    )
    A_pw[ix_err_mat] <- 1L
    A_pw[ix_unobs, ix] <- g[-ix_obs, ix_obs]
    order_pw[n_unobs_pw + ix] <- offset + ord[seq_asc(n_unobs + 1L, n)]
    sub_ix <- offset + sub_var[[w]]
    A_pw[, sub_ix] <- 0L
    labels_pw[seq_int(from, to)] <- lapply(ix_obs, function(v) {
      if (v %in% sub_var[[w]]) {
        v_ix <- which(sub_var[[w]] %in% v)
        cf(var = lab[v], obs = sub_lst[[w]][v_ix])
      } else {
        cf(var = lab[v], sub = sub_lst[[w]])
      }
    })
  }
  latent_pw <- logical(n_obs_pw)
  order_pw[seq_len(n_err)] <- ix_err
  latent_pw[ix_err] <- TRUE
  order_pw[seq_asc(n_err + 1L, n_unobs_pw)] <- ix_unobs
  latent_pw[ix_unobs] <- TRUE
  list(
    adjacency = A_pw,
    labels = labels_pw,
    latent = latent_pw,
    order = order_pw,
    n_obs = n_obs_pw,
    n_unobs = n_unobs_pw,
    n_obs_orig = n_obs,
    n_worlds = n_worlds
  )
}

#' Counterfactual Graph
#'
#' @param p A list representing a parallel worlds graph from `pwg`.
#' @param gamma A `counterfactual_conjunction` object.
#' @return A `dag` object representing the counterfactual graph.
#' @noRd
cg <- function(p, gamma) {
  A <- p$adjacency
  A_cg <- p$adjacency
  latent <- p$latent
  obs <- which(!latent)
  n_total <- p$n_obs + p$n_unobs
  ord <- order(match(seq_len(p$n_obs_orig), p$order))
  merged <- list()
  keep <- rep(TRUE, n_total)
  eq_val <- replicate(
    p$n_obs_orig,
    as.list(seq_len(p$n_worlds)),
    simplify = FALSE
  )
  if (p$n_worlds > 1L) {
    for (v in ord) {
      for (a in seq_len(p$n_worlds - 1L)) {
        val_pa_alpha <- NULL
        val_alpha <- NULL
        alpha_offset <- (a - 1L) * p$n_obs_orig
        alpha_ix <- v + alpha_offset
        alpha <- p$labels[[alpha_ix]]
        pa_alpha <- parents(alpha_ix, A)
        n_pa_alpha <- length(pa_alpha)
        if (n_pa_alpha == 0L) {
          val_alpha <- alpha$obs
        } else {
          val_alpha <- val(alpha, gamma)
        }
        for (b in seq_int(a + 1L, p$n_worlds)) {
          same_var <- FALSE
          val_pa_beta <- NULL
          val_beta <- NULL
          beta_offset <- (b - 1L) * p$n_obs_orig
          beta_ix <- v + beta_offset
          beta <- p$labels[[beta_ix]]
          pa_beta <- parents(beta_ix, A)
          n_pa_beta <- length(pa_beta)
          if (n_pa_beta == 0L) {
            val_beta <- beta$obs
          } else {
            val_beta <- val(beta, gamma)
          }
          if (n_pa_alpha == 0L || n_pa_beta == 0L) {
            if (identical(val_alpha, val_beta)) {
              same_var <- TRUE
            }
          } else {
            obs_pa <- intersect(pa_alpha, obs) - alpha_offset
            if (length(obs_pa) > 0) {
              same_var <- all_equivalent(eq_val, obs_pa, a, b)
            } else {
              # Latent variables are shared across worlds
              same_var <- TRUE
            }
          }
          if (same_var) {
            if (!val_consistent(val_alpha, val_beta)) {
              return(list(consistent = FALSE))
            }
            eq_val[[v]] <- update_equivalence(eq_val[[v]], a, b)
            ch_beta <- children(beta_ix, A)
            A_cg[pa_beta, alpha_ix] <- 1L
            A_cg[alpha_ix, ch_beta] <- 1L
            keep[beta_ix] <- FALSE
            merged <- c(
              merged,
              list(
                list(original = beta, replacement = alpha)
              )
            )
          }
        }
      }
    }
  }
  cg_dag <- structure(
    A_cg,
    labels = p$labels,
    latent = p$latent,
    order = p$order,
    class = "dag"
  )
  cg_dag <- subgraph(keep, cg_dag)
  n_cf <- length(gamma)
  gamma_ix <- seq_len(n_cf)
  labels_cg <- attr(cg_dag, "labels")
  label_ix <- seq_along(labels_cg)
  for (i in seq_along(merged)) {
    for (g in gamma_ix) {
      g_temp <- gamma[[g]]
      g_obs <- g_temp$obs
      if (length(merged[[i]]$original$obs) == 0L) {
        g_temp$obs <- integer(0L)
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
  for (g in seq_len(n_cf)) {
    for (l in seq_along(labels_cg)) {
      var_match <- FALSE
      sub_match <- FALSE
      if (identical(gamma[[g]]$var, labels_cg[[l]]$var)) {
        if (length(labels_cg[[l]]$obs) > 0L) {
          if (gamma[[g]]$obs == labels_cg[[l]]$obs) {
            var_match <- TRUE
          }
        } else {
          var_match <- TRUE
        }
      }
      if (identical(gamma[[g]]$sub, labels_cg[[l]]$sub)) {
        sub_match <- TRUE
      }
      if (var_match && sub_match) {
        query_vars[g] <- l
      }
    }
  }
  #gamma <- as.counterfactual_conjunction(unique(gamma))
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
  list(
    graph = subgraph(keep, cg_dag),
    conjunction = gamma,
    merged = merged,
    consistent = TRUE
  )
}

update_equivalence <- function(eq, a, b) {
  if (length(eq) == 1L) {
    return(eq)
  }
  a_cl <- Position(function(x) a %in% x, eq)
  b_cl <- Position(function(x) b %in% x, eq)
  merged <- list(union(eq[[a_cl]], eq[[b_cl]]))
  c(eq[-c(a_cl, b_cl)], merged)
}

all_equivalent <- function(eq, vars, a, b) {
  for (v in vars) {
    eq_v <- eq[[v]]
    if (length(eq_v) == 1L) {
      next
    }
    a_cl <- Position(function(x) a %in% x, eq_v)
    b_cl <- Position(function(x) b %in% x, eq_v)
    if (a_cl != b_cl) {
      return(FALSE)
    }
  }
  TRUE
}

#' Import Graph
#'
#' Import and construct a valid DAG from an external format. Accepts
#' `dagitty` graphs, `igraph` graphs in the `causaleffect` package syntax,
#' and character strings in the `dosearch` package syntax.
#'
#' @param x A graph object in a valid external format.
#' @return A `dag` object.
#' @export
import_graph <- function(x) {
  stopifnot_(
    !missing(x),
    "Argument `x` is missing."
  )
  out <- NULL
  if (inherits(x, "dagitty")) {
    names(x) <- NULL
    class(x) <- NULL
    x <- trimws(gsub("[;\\,\r\n\t]", " ", x))
    x_def <- reg_match(x, "dag \\{(.+)\\}")
    stopifnot_(
      length(x_def) > 0L,
      "Unable to parse argument `x` into an object of class `dag`."
    )
    out <- dag(x_def[2L, 1L])
  } else if (inherits(x, "igraph")) {
    stopifnot_(
      requireNamespace("igraph", quietly = TRUE),
      "Attempting to use `igraph` input, but the package is not available."
    )
    e <- igraph::E(x)
    v <- igraph::vertex_attr(x, "name")
    g_obs <- ""
    g_unobs <- ""
    description <- NULL
    obs_edges <- e[(is.na(description) | description != "U")]
    unobs_edges <- e[description == "U" & !is.na(description)]
    if (length(obs_edges) > 0L) {
      obs_ind <- igraph::get.edges(x, obs_edges)
      g_obs <- paste(v[obs_ind[,1]], "->", v[obs_ind[, 2L]], collapse = " ")
    }
    if (length(unobs_edges) > 0L) {
      unobs_ind <- igraph::get.edges(x, unobs_edges)
      unobs_ind <- unobs_ind[unobs_ind[,1] < unobs_ind[, 2L],,drop=FALSE]
      g_unobs <- paste(
        v[unobs_ind[, 1L]], "<->", v[unobs_ind[, 2L]], collapse = " "
      )
    }
    out <- dag(paste0(c(g_obs, g_unobs), collapse = " "))
  } else if (is.character(x)) {
    out <- dag(x)
  } else if (is.null(out)) {
    stop_("Argument `x` has an unrecognized format.")
  }
  out
}

#' Export Graph
#'
#' Convert a valid graph object into a supported external format.
#'
#' @param g An object of class `dag`.
#' @param type A character string matching one of the following:
#' `"dagitty"`, `"causaleffect"` or `"dosearch"`. For `"dagitty"` and
#' `"causaleffect"`, the packages `dagitty` and `igraph` must be available,
#' respectively.
#' @param use_bidirected A logical value indicating if bidirected edges
#' should be used in the resulting object.
#' If `TRUE`, the result will have explicit `X <-> Y`
#' edges. If `FALSE`, an explicit latent variable `X <- U[X,Y] -> Y` will
#' be used instead (only applicable if `type` is `"dosearch"`).
#' @param ... Additional arguments passed to `format` for formatting
#' vertex labels.
#' @return If `type` is `"dagitty"`, returns a `dagitty` object.
#' If `type` is `"causaleffect"`, returns an `igraph` graph, with its edge
#' attributes set according to the `causaleffect` package syntax. If `type`
#' is `"dosearch"`, returns a character vector of length one that describes
#' `g` in the `dosearch` package syntax.
#' @export
export_graph <- function(g, type = c("dagitty", "causaleffect", "dosearch"),
                         use_bidirected = TRUE, ...) {
  stopifnot_(
    !missing(g),
    "Argument `g` is missing."
  )
  stopifnot_(
    is.dag(g),
    "Argument `g` must be a `dag` object."
  )
  out <- NULL
  type <- match.arg(type)
  lab <- attr(g, "labels")
  lab_form <- sapply(lab, format)
  lat <- attr(g, "latent")
  lat_ix <- which(lat)
  e_ix <-  which(g > 0L, arr.ind = TRUE)
  obs_e <- e_ix[!e_ix[, 1L] %in% lat_ix,,drop = FALSE]
  unobs_e <- e_ix[e_ix[, 1L] %in% lat_ix,,drop = FALSE]
  n_o <- nrow(obs_e)
  n_u <- nrow(unobs_e)
  e_str <- character(0L)
  e_di_str <- character(0L)
  e_bi_str <- character(0L)
  if (type %in% c("dagitty", "dosearch")) {
    if (n_o > 0L) {
      e_di_str <- paste0(
        lab_form[obs_e[, 1L]], " -> ", lab_form[obs_e[, 2L]]
      )
    }
    if (n_u > 0L) {
      if (use_bidirected || identical(type, "dagitty")) {
        bi_start <- seq.int(1L, n_u - 1L, by = 2L)[seq_len(n_u %/% 2L)]
        bi_end <- seq.int(2L, n_u, by = 2L)[seq_len(n_u %/% 2L)]
        e_bi_str <- paste0(
          lab_form[unobs_e[bi_start, 2L]],
          " <-> ",
          lab_form[unobs_e[bi_end, 2L]]
        )
      } else {
        e_bi_str <- paste0(
          lab_form[unobs_e[, 1L]], " -> ", lab_form[unobs_e[, 2L]])
      }
    }
  }
  if (identical(type, "dagitty")) {
    stopifnot_(
      requireNamespace("dagitty", quietly = TRUE),
      "Package `dagitty` is not available."
    )
    e_str <- collapse(
      paste0(e_di_str, collapse = " "),
      " ",
      paste0(e_bi_str, collapse = " ")
    )
    out <- dagitty::dagitty(collapse("dag {", e_str, "}"))
  } else if (identical(type, "causaleffect")) {
    stopifnot_(
      requireNamespace("igraph", quietly = TRUE),
      "Package `igraph` is not available."
    )
    ig <- igraph::make_empty_graph(n = sum(!lat))
    obs_e_ix <- c(t(obs_e))
    unobs_e_ix <- e_ix[e_ix[, 1L] %in% lat_ix, 2L]
    if (n_o > 0L) {
      ig <- ig + igraph::edges(obs_e_ix)
    }
    if (n_u > 0L) {
      ig <- ig + igraph::edges(c(unobs_e_ix, rev(unobs_e_ix)))
      ig <- igraph::set_edge_attr(
        ig, "description",
        index = seq.int(n_o + 1, n_u + n_o),
        value = "U"
      )
    }
    ig <- igraph::set_vertex_attr(ig, "name", igraph::V(ig), lab_form[!lat])
    out <- ig
  } else if (identical(type, "dosearch")) {
    e_str <- collapse(
      paste0(e_di_str, collapse = "\n"),
      "\n",
      paste0(e_bi_str, collapse = "\n")
    )
    out <- trimws(e_str)
  }
  out
}

#' Create a counterfactual graph directly from a DAG
#'
#' @param g A `dag` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @return A `dag` representing the counterfactual graph.
#' @noRd
make_cg <- function(g, gamma) {
  cg(pwg(g, gamma), gamma)
}
