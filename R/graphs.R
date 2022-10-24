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
#' Different statements in `x` can be distinguished from one
#' another using either semicolons, line breaks, or spaces.
#'
#' Subgraphs can be defined by enclosing the definition within
#' curly braces. For example `X -> {Y Z}` defines an edge
#' from `X` to both `Y` and `Z`. Individual statements within a subgraph can be
#' separated by a comma or semicolon, but this is optional.
#' Edges can also be defined within subgraphs, and subgraphs can be nested.
#' For example, `X -> {Z -> Y}` is the same definition as
#' `X -> Z; X -> Y; Z -> Y`. Similarly `X <-> {Z -> {A B}} -> Y` is the same as
#' `X <-> {Z A B} -> Y; Z -> {A B}`.
#'
#' Note that in the context of this package, vertex labels will always be
#' converted into upper case, meaning that typing `Z` or `z` will
#' always represent the same variable. This is done to enforce the notation
#' of counterfactual variables, where capital letters denote variables
#' and small letters denote their value assignments.
#'
#' @param x A `character` string containing a sequence of paths consisting of
#' nodes or subgraphs `X` or edges in the form `X -> Y`, `X <- Y` or `X <-> Y`
#' where `X` and `Y` are subgraphs or nodes See details for more advanced
#' constructs.
#' @param u A `character` vector of variable names which should be considered
#' unobserved (besides those defined by bidirected edges). These variables
#' are subsequently removed via latent projection. Variable names not found
#' in the graph are ignored.
#' @return An object of class `dag`, which is a square adjacency matrix
#' with the following attributes:
#'
#' * `labels`\cr A `character` vector (or a list) of vertex labels.
#' * `latent`\cr A `logical` vector indicating latent variables.
#' * `order`\cr An `integer` vector giving a topological order for the vertices.
#' * `text`\cr A `character` string giving representing the DAG.
#' .
#' @examples
#' dag("X -> {Y Z} <- W <-> G")
#'
#' # Subgraphs can appear on both sides of an edge
#' dag("{X Z} -> {Y W}")
#'
#' # Semicolons can be used to distinguish individual statements
#' dag("X -> Z -> Y; X <-> Y")
#'
#' # Commas can be used to distinguish variables within groups if there
#' # are no edges within the group
#' dag("{x, y, z} -> w")
#'
#' # Edges within subgraphs is supported
#' dag("{X -> Z} -> {Y <-> W}")
#'
#' # Nested subgraphs are supported
#' dag("{X -> {Z -> {Y <-> W}}}")
#'
#' # Line breaks are also supported for statement separation
#' dag("
#'   Z -> W
#'   X -> Y
#' ")
#'
#' @export
dag <- function(x, u = character(0L)) {
  stopifnot_(
    length(x) == 1L,
    "Argument `x` must be a single character string."
  )
  x <- try_type(x = x, type = "character")
  x <- toupper(x)
  # Normalize whitespace to a single space globally
  x <- gsub(",|;", " ", x)
  x <- gsub("<->", " -- ", x)
  x <- gsub("->", " -> ", x)
  x <- gsub("<-", " <- ", x)
  x <- trimws(gsub("\\s+", " ", x))
  stopifnot_(
    nzchar(x),
    "Argument `x` contains only whitespace or special characters."
  )
  stopifnot_(
    validate_dag(strsplit(x, "", fixed = TRUE)[[1L]]),
    "Argument `x` contains invalid syntax."
  )
  u <- try_type(u = u, type = "character")
  u <- toupper(u)
  x <- parse_dag(x)
  v_names <- unique(x$vars)
  u <- intersect(u, v_names)
  n_v <- length(v_names)
  A_di <- matrix(FALSE, n_v, n_v)
  A_bi <- matrix(FALSE, n_v, n_v)
  rownames(A_di) <- colnames(A_di) <- v_names
  rownames(A_bi) <- colnames(A_bi) <- v_names
  edgemat <- unique(x$edgemat)
  n_e <- nrow(edgemat)
  for (i in seq_len(n_e)) {
    lhs <- edgemat[i, 1L]
    rhs <- edgemat[i, 2L]
    arrow <- edgemat[i, 3L]
    edge_str <- paste0(lhs, " ", arrow, " ", rhs)
    # eliminate self-loops
    stopifnot_(
      lhs != rhs,
      paste0("Invalid edge construct in argument `x`: ", edge_str)
    )
    A_di[lhs, rhs] <- A_di[lhs, rhs] || arrow == "->"
    A_di[rhs, lhs] <- A_di[rhs, lhs] || arrow == "<-"
    A_bi[lhs, rhs] <- A_bi[lhs, rhs] || arrow == "--"
    A_bi[rhs, lhs] <- A_bi[rhs, lhs] || arrow == "--"
  }
  stopifnot_(
    !detect_cycles(A_di),
    "The graph specified by argument `x` is not acyclic."
  )
  lp <- latent_projection(A_di, A_bi, u)
  A_di <- 1L * lp$A_di
  A_bi <- 1L * lp$A_bi
  v_names <- colnames(A_di)
  n_v <- length(v_names)
  n_u <- sum(A_bi)
  n_vu <- n_v + n_u
  A <- matrix(0L, n_vu, n_vu)
  labels <- character(n_vu)
  latent <- logical(n_vu)
  A[seq_len(n_v), seq_len(n_v)] <- A_di
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

validate_dag <- function(x) {
  edge_tokens <- c("<", ">", "-")
  x_len <- length(x)
  open <- 0L
  prev <- 1L
  sub_start <- 0L
  sub_end <- 0L
  sub <- FALSE
  prev_edge <- TRUE
  edge_rhs <- TRUE
  lhs <- FALSE
  i <- 0L
  while (i < x_len) {
    i <- i + 1L
    open <- open + (x[i] == "{") - (x[i] == "}")
    sub_start <- sub_start + i * (open == 1L && !sub)
    sub_end <- sub_end + i * (open == 0L && sub)
    sub <- (open > 0L)
    if (open < 0L) {
      return(FALSE)
    }
    if (open == 0L && (x[i] == " " || i == x_len)) {
      if (sub_start > 0L) {
        tmp <- validate_dag(x[seq_int(sub_start + 1L, sub_end - 1L)])
        if (!tmp) {
          return(FALSE)
        }
        sub <- FALSE
        sub_start <- 0L
        sub_end <- 0L
      } else {
        if (any(x[seq.int(prev, i - 1L)] %in% edge_tokens)) {
          return(FALSE)
        }
      }
      edge_rhs <- prev_edge
      edge <- isTRUE(
        (x[i + 1L] == "<" && x[i + 2L] == "-" ) ||
        (x[i + 1L] == "-" && x[i + 2L] == ">" ) ||
        (x[i + 1L] == "-" && x[i + 2L] == "-" )
      )
      if (edge) {
        edge_rhs <- !edge
        prev_edge <- edge
      }
      i <- i + 3L * edge
      prev <- i + 1L
      edge <- FALSE
    }
  }
  open == 0L && edge_rhs
}


#' Parse a DAG statement
#'
#' @param x A `character` vector of length 1 corresponding to a single
#' statement in the input to `dag`.
#' @return A `list` with two components: `vars` containing unique variable
#' names as a `character` vector and `edgemat` which is a matrix of edge
#' definitions.
#' @noRd
parse_dag <- function(x) {
  out <- list()
  x <- trimws(x)
  vars <- character(0L)
  edgemat <- matrix(0L, 0L, 3L)
  edges <- find_edges(x)
  no_edges <- length(edges$arrows) == 0L || !any(nzchar(edges$arrows))
  if (no_edges) {
    for (i in seq_along(edges$endpoints)) {
      if (grepl("-|\\{|\\}", edges$endpoints[i])) {
        endpoint_parsed <- parse_dag(edges$endpoints[i])
        vars <- c(vars, endpoint_parsed$vars)
        edgemat <- rbind(edgemat, endpoint_parsed$edgemat)
      } else {
        vars <- c(vars, strsplit(edges$endpoints[i], "\\s")[[1L]])
      }
    }
  }
  for (i in seq_along(edges$arrows)) {
    lhs_parsed <- parse_dag(edges$endpoints[i])
    rhs_parsed <- parse_dag(edges$endpoints[i + 1L])
    vars <- c(
      vars,
      lhs_parsed$vars,
      rhs_parsed$vars
    )
    if (nzchar(edges$arrows[i])) {
      pairs <- expand.grid(
        lhs_parsed$vars,
        rhs_parsed$vars,
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      )
      new_edges <- cbind(pairs, edges$arrows[i])
      edgemat <- rbind(
        edgemat,
        lhs_parsed$edgemat,
        rhs_parsed$edgemat,
        new_edges
      )
    }
  }
  list(vars = vars, edgemat = edgemat)
}

#' Find edges and group definitions in a DAG statement
#'
#' @inheritParams parse_dag
#' @return A `list` with two elements: `arrows` which is a `character` vector
#' of the edges and `endpoints` which is a `character` vector of the edge
#' endpoint variable names or group constructs.
#' @noRd
find_edges <- function(x) {
  arrows <- character(0L)
  endpoints <- character(0L)
  open <- 0L
  group <- FALSE
  g_ix <- 1L
  ix <- 0L
  ix_offset <- 0L
  x_len <- nchar(x)
  # Elements are added for lookahead
  x <- c(strsplit(x, "", fixed = TRUE)[[1L]], "", "")
  while (ix <= x_len) {
    ix <- ix + 1L
    open <- open + (x[ix] == "{") - (x[ix] == "}")
    next_edge <- x[ix + 1L] %in% c("-", "<")
    group <- group || (open > 0L)
    if (open == 0L && x[ix] == " ") {
      if (next_edge) {
        e <- collapse(x[seq.int(ix + 1L, ix + 2L)])
        ix_offset <- 3L
      } else {
        e <- ""
        ix_offset <- 0L
      }
      arrows <- c(arrows, e)
      v <- ifelse_(
        group,
        collapse(x[seq_int(g_ix + 1L, ix - 2L)]),
        collapse(x[seq_int(g_ix, ix - 1L)])
      )
      endpoints <- c(endpoints, v)
      group <- FALSE
      has_endpoint <- FALSE
      ix <- ix + ix_offset
      g_ix <- ix + 1L
    }
  }
  v <- ifelse_(
    group,
    collapse(x[seq_int(g_ix + 1L, x_len - 1L)]),
    collapse(x[seq_int(g_ix, x_len)])
  )
  endpoints <- c(endpoints, v)
  list(arrows = arrows, endpoints = endpoints)
}

#' Latent projection of a latent variable DAG
#'
#' @param A_di An adjacency matrix of directed edges.
#' @param A_bi An adjacency matrix of bidirected edges.
#' @param u A `character` vector of variable names to be projected out.
#' @return A `list` with modified versions of `A_di` and `A_bi` such that
#' the model no longer contains `u` explicitly.
#' @noRd
latent_projection <- function(A_di, A_bi, u) {
  v <- colnames(A_di)
  ui <- match(u, v)
  for (i in ui) {
    ubi <- parents(i, A_bi)
    upa <- parents(i, A_di)
    uch <- children(i, A_di)
    n_upa <- length(upa)
    n_uch <- length(uch)
    n_ubi <- length(ubi)
    if (n_upa > 0L && n_uch > 0L) {
      ix <- as.matrix(expand.grid(upa, uch))
      A_di[ix] <- TRUE
    }
    if (n_ubi > 0L && n_uch > 0L) {
      ix <- as.matrix(expand.grid(ubi, uch))
      A_bi[ix] <- TRUE
      A_bi[ix[, 2L], ix[, 1L]] <- TRUE
    }
    if (n_uch > 1L) {
      ix <- as.matrix(expand.grid(uch, uch))
      A_bi[ix] <- TRUE
    }
  }
  if (length(ui) > 0L) {
    A_di <- A_di[-ui, -ui]
    A_bi <- A_bi[-ui, -ui]
  }
  A_bi[lower.tri(A_bi)] <- FALSE
  diag(A_bi) <- FALSE
  list(A_di = A_di, A_bi = A_bi)
}

#' Convert a DAG into a character string
#'
#' @param A An adjacency matrix.
#' @param labels A `character` vector of vertex labels.
#' @param latent A `logical` vector indicating latent variables by `TRUE`.
#' @param ord An `integer` vector giving a topological order of `A`.
#' @return A `character` representation of the DAG.
#' @noRd
dag_string <- function(A, labels, latent, ord) {
 ord <- ord[!latent[ord]]
 e <- character(ncol(A))
 for (i in ord) {
   ch <- labels[children(i, A)]
   n_ch <- length(ch)
   if (n_ch > 0L) {
     lhs <- ifelse_(n_ch > 1L, "{", "")
     rhs <- ifelse_(n_ch > 1L, "}", "")
     e[i] <- paste0(
       format(labels[i]), " -> ", lhs, paste0(ch, collapse = ", "), rhs
     )
   }
 }
 for (i in which(latent)) {
   e[i] <- paste0(format(labels[children(i, A)]), collapse = " <-> ")
 }
 paste0(c(labels[!latent], e[nzchar(e)]), collapse = "; ")
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
          same_val <- FALSE
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
              same_var <- (n_pa_alpha == 0L && n_pa_beta == 0L)
              same_val <- TRUE
            }
          } else {
            obs_pa <- intersect(pa_alpha, obs) - alpha_offset
            if (length(obs_pa) > 0) {
              same_val <- all_equivalent(eq_val, obs_pa, a, b)
            } else {
              # Latent variables are shared across worlds
              same_val <- TRUE
            }
            same_var <- same_val
          }
          if (same_val) {
            if (!val_consistent(val_alpha, val_beta)) {
              return(list(consistent = FALSE))
            }
            eq_val[[v]] <- update_equivalence(eq_val[[v]], a, b)
            if (same_var) {
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
    stopifnot_(
      grepl("dag", x),
      "Argument `x` is not a DAG."
    )
    x <- gsub("dag", "", x)
    stopifnot_(
      !grepl("[\\[\\]]", x),
      "Attribute definitions via [...] are not supported in argument `x`."
    )
    out <- try(dag(x), silent = TRUE)
    stopifnot_(
      !inherits(out, "try-error"),
      "Unable to parse argument `x` into an object of class `dag`."
    )
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
      g_obs <- paste(v[obs_ind[,1]], "->", v[obs_ind[, 2L]], collapse = "; ")
    }
    if (length(unobs_edges) > 0L) {
      unobs_ind <- igraph::get.edges(x, unobs_edges)
      unobs_ind <- unobs_ind[unobs_ind[,1] < unobs_ind[, 2L],,drop=FALSE]
      g_unobs <- paste(
        v[unobs_ind[, 1L]], "<->", v[unobs_ind[, 2L]], collapse = "; "
      )
    }
    out <- dag(paste0(c(g_obs, g_unobs), collapse = "; "))
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
