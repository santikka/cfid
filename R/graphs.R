#' Directed Acyclic Graph
#'
#' Define a directed acyclic graph (DAG).
#'
#' @param x A character string containing a sequence of definitions
#'     of edges in the form `X -> Y`, `X <- Y` or `X <-> Y`.
#'     See details for more advanced constructs.
#'
#' @details
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
#' @return An object of class `dag`, which is a square adjacency matrix
#'   with the following attributes:
#'
#' * `labels` A `character` vector (or a list) of vertex labels.
#' * `latent` A `logical` vector indicating latent variables.
#' * `order` An `integer` vector giving a topological order for the vertices.
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
  if (!nzchar(trimws(x))) {
    stop_("Argument `x` contains only whitespace or special characters.")
  }
  e_within <- grepl("\\{[^\\}]+[<>\\-]+[^\\{]+\\}", x)
  if (e_within) {
    stop_("Edges are not allowed within groups defined by {...}.")
  }
  g_str <- reg_match(x, "\\{([^<>\\-]+)\\}|([^<> \\-]+)|(<->|<-|->)")
  g_str[2L,] <- trimws(g_str[2L, ])
  g_type <- apply(g_str[-1L, , drop = FALSE], 2L, nzchar)
  g_str <- g_str[rbind(FALSE, g_type)]
  g_lst <- strsplit(g_str, "[ ]+", perl = TRUE)
  e_lst <- which(sapply(g_lst, function(e) e[1L] %in% c("<->", "<-", "->")))
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
      if (i == 1L || i == length(g_lst)) {
        stop_("Invalid edge construct in argument `x`.")
      }
      pairs <- expand.grid(
        g_lst[[i - 1L]],
        g_lst[[i + 1L]],
        stringsAsFactors = FALSE
      )
      if (identical(g_lst[[i]], "->")) {
        for (j in seq_len(nrow(pairs))) {
          A_obs[pairs[j, 1L], pairs[j, 2L]] <- 1L
        }
      } else if (identical(g_lst[[i]], "<-")) {
        for (j in seq_len(nrow(pairs))) {
          A_obs[pairs[j, 2L], pairs[j, 1L]] <- 1L
        }
      } else if (identical(g_lst[[i]], "<->")) {
        for (j in seq_len(nrow(pairs))) {
          if (identical(pairs[j, 1L], pairs[j, 2L])) {
            stop_(
              "Invalid bidirected edge: ", pairs[j, 1L], " <-> ", pairs[j, 1L]
            )
          }
          A_bi[pairs[j, 2L], pairs[j, 1L]] <- 1L
          A_bi[pairs[j, 1L], pairs[j, 2L]] <- 1L
        }
      }
    }
  }
  if (detect_cycles(A_obs)) {
    stop_("The graph specified by argument `x` is not acyclic.")
  }
  n_u <- sum(A_bi[lower.tri(A_bi)])
  n_vu <- n_v + n_u
  A <- matrix(0L, n_vu, n_vu)
  labels <- character(n_vu)
  latent <- logical(n_vu)
  A[seq_len(n_v), seq_len(n_v)] <- A_obs
  labels[seq_len(n_v)] <- v_names
  if (n_u > 0L) {
    k <- n_v
    for (i in seq_len(n_v - 1L)) {
      for (j in seq.int(i + 1L, n_v)) {
        if (A_bi[i, j]) {
          k <- k + 1L
          A[k, i] <- 1L
          A[k, j] <- 1L
          labels[k] <- paste0("U[", v_names[i], ",", v_names[j], "]")
        }
      }
    }
    latent[seq.int(n_v + 1L, n_vu)] <- TRUE
  }
  structure(
    A,
    labels = labels,
    latent = latent,
    order = topological_order(A, latent),
    class = "dag"
  )
}

#' Parallel Worlds Graph
#'
#' Construct a parallel worlds graph from a DAG or an ADMG.
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
  sub_var <- lapply(sub_lst, function(i) which(lab %in% names(i)))
  n_worlds <- length(sub_lst)
  n_unobs <- sum(lat)
  n_obs <- length(lab) - n_unobs
  no_err <- integer(0L)
  if (n_unobs > 0L) {
    no_err <- children(which(lat), g)
  }
  n_err <- n_obs - length(no_err)
  n_obs_pw <- n_worlds * n_obs
  n_unobs_pw <- n_err + n_unobs
  n_total <- n_obs_pw + n_unobs_pw
  A_pw <- matrix(0L, n_total, n_total)
  ix_obs <- seq_len(n_obs)
  ix_err <- seq.int(n_obs_pw + 1L, n_obs_pw + n_err)
  ix_unobs <- seq.int(n_obs_pw + n_err + 1L, n_total)
  labels_pw <- character(n_total)
  labels_pw[seq_len(n_obs)] <- lab[seq_len(n_obs)]
  order_pw <- integer(n_total)
  if (n_err > 0L) {
    err_vars <- paste0("U[", lab[setdiff(ix_obs, no_err)], "]")
    labels_pw[ix_err] <- lapply(err_vars, cf)
  }
  if (n_unobs > 0L) {
    labels_pw[ix_unobs] <- lapply(lab[lat], cf)
  }
  for (w in seq_len(n_worlds)) {
    offset <- (w - 1L) * n_obs
    from <- offset + 1L
    to <- offset + n_obs
    ix <- from:to
    A_pw[ix, ix] <- g[ix_obs,ix_obs]
    if (n_err > 0L) {
      ix_err_mat <- cbind(ix_err, setdiff(ix, offset + no_err))
      A_pw[ix_err_mat] <- 1L
    }
    if (n_unobs > 0L) {
      A_pw[ix_unobs, ix] <- g[-ix_obs, ix_obs]
      order_pw[n_unobs_pw + ix] <- offset + ord[-seq_len(n_unobs)]
    } else {
      order_pw[n_unobs_pw + ix] <- offset + ord
    }
    sub_ix <- offset + sub_var[[w]]
    A_pw[, sub_ix] <- 0L
    labels_pw[seq.int(from, to)] <- lapply(ix_obs, function(v) {
      if (v %in% sub_var[[w]]) {
        cf(var = lab[v], obs = sub_lst[[w]])
      } else {
        cf(var = lab[v], sub = sub_lst[[w]])
      }
    })
  }
  latent_pw <- logical(n_obs_pw)
  if (n_err > 0L) {
    order_pw[seq_len(n_err)] <- ix_err
    latent_pw[ix_err] <- TRUE
  }
  if (n_unobs > 0L) {
    order_pw[seq.int(n_err + 1L, n_unobs_pw)] <- ix_unobs
    latent_pw[ix_unobs] <- TRUE
  }
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
#' Construct a counterfactual graph.
#'
#' @param p A list representing a parallel worlds graph from `pwg`.
#' @param gamma A `counterfactual_conjunction` object.
#' @return A `dag` object representing the causal graph.
#' @noRd
cg <- function(p, gamma) {
  A <- p$adjacency
  A_cg <- p$adjacency
  n_total <- p$n_obs + p$n_unobs
  merged <- list()
  keep <- rep(TRUE, n_total)
  eq_val <- rep(-(p$n_worlds + 1L), n_total)
  eq_val[seq_len(p$n_obs)] <-
    -1L * rep(seq_len(p$n_worlds), each = p$n_obs_orig)
  if (p$n_worlds > 1L) {
    for (v in seq_len(p$n_obs_orig)) {
      world_skip <- logical(p$n_worlds)
      for (a in seq_len(p$n_worlds - 1L)) {
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
        if (n_pa_alpha == 0L) {
          val_alpha <- alpha$obs
          eq_val[alpha_ix] <- val_alpha
        } else {
          val_alpha <- val(alpha, gamma)
          if (!is.null(val_alpha)) {
            eq_val[alpha_ix] <- val_alpha
          }
          val_pa_alpha <- eq_val[pa_alpha]
        }
        for (b in seq.int(a + 1L, p$n_worlds)) {
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
          if (n_pa_beta == 0L) {
            val_beta <- beta$obs
            eq_val[beta_ix] <- val_beta
          } else {
            val_beta <- val(beta, gamma)
            if (!is.null(val_beta)) {
              eq_val[beta_ix] <- val_beta
            }
            val_pa_beta <- eq_val[pa_beta]
          }
          if (n_pa_alpha == 0L || n_pa_beta == 0L) {
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
            A_cg[pa_beta, alpha_ix] <- 1L
            A_cg[alpha_ix, ch_beta] <- 1L
            keep[beta_ix] <- FALSE
            # sub_alpha <- length(alpha$sub)
            # sub_beta <- length(beta$sub)
            # replacement <- alpha
            # original <- beta
            # if (sub_alpha > sub_beta) {
            #   replacement <- beta
            #   original <- alpha
            # }
            merged <- c(
              merged,
              list(
                #list(original = original, replacement = replacement)
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

#' Import Graph
#'
#' Import and construct a valid DAG from an external format.
#'
#' @param x A graph object in a valid external format, see details.
#'
#' @details
#' Argument `x` accepts `dagitty` graphs, `igraph` graphs
#' in the `causaleffect` package syntax and character strings
#' in the `dosearch` package syntax.
#'
#' @return A `dag` object.
#' @export
import_graph <- function(x) {
  out <- NULL
  if (inherits(x, "dagitty")) {
    names(x) <- NULL
    class(x) <- NULL
    x <- trimws(gsub("[;\\,\r\n\t]", " ", x))
    x_def <- reg_match(x, "dag \\{(.+)\\}")
    if (length(x_def) > 0L) {
      out <- dag(x_def[2L, 1L])
    } else {
      stop_("Unable to parse argument `x` into an object of class `dag`.")
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
    } else {
      stop_(
        "Attempting to use `igraph` input, but the package is not available."
      )
    }
  } else if (is.character(x)) {
    out <- dag(x)
  } else if (is.null(out)) {
    stop_("Argument `x` has an unrecognized format.")
  }
  out
}

#' Export Graph
#'
#' Convert a valid graph object to a supported external format.
#'
#' @param g An object of class `dag`.
#' @param type A character string matching one of the following:
#'     `"dagitty"`, `"causaleffect"` or `"dosearch"`.
#' @param use_bidirected A logical value indicating if bidirected edges
#'     should be used in the resulting object.
#'     If `TRUE`, the result will have explicit `X <-> Y`
#'     edges. If `FALSE`, an explicit latent variable `X <- U[X,Y] -> Y` will
#'     be used instead (only applicable if `type` is `"dosearch"`).
#' @param ... Additional arguments passed to `format` for formatting
#'     vertex labels.
#' @return If `type` is `"dagitty"`, returns a `dagitty` object.
#'     If `type` is `"causaleffect"`, returns an `igraph` graph, with its edge
#'     attributes set according to the `causaleffect` package syntax. If `type`
#'     is `"dosearch"`, returns a character vector of length one that describes
#'     `g` in the `dosearch` package syntax.
#' @export
export_graph <- function(g, type = c("dagitty", "causaleffect", "dosearch"),
                         use_bidirected = TRUE, ...) {
  if (!is.dag(g)) {
    stop_("Argument `x` must be a `dag` object.")
  }
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
    if (requireNamespace("dagitty", quietly = TRUE)) {
      e_str <- collapse(
        paste0(e_di_str, collapse = " "),
        " ",
        paste0(e_bi_str, collapse = " ")
      )
      out <- dagitty::dagitty(collapse("dag {", e_str, "}"))
    } else {
      stop_("Package `dagitty` is not available for exporting the graph.")
    }
  } else if (identical(type, "causaleffect")) {
    if (requireNamespace("igraph", quietly = TRUE)) {
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
    } else {
      stop_("Package `igraph` is not available for exporting the graph.")
    }
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

#' Verify that argument is a valid DAG
#'
#' @param x An R object.
#' @noRd
is.dag <- function(x) {
  inherits(x, "dag")
}

#' Create a counterfactual graph directly from a DAG
#'
#' @param g A `dag` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @noRd
make_cg <- function(g, gamma) {
  cg(pwg(g, gamma), gamma)
}
