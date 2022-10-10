#' Identify a Counterfactual Query
#'
#' Determine the identifiability of a (conditional) counterfactual conjunction.
#'
#' To identify a non-conditional conjunction \eqn{p(\gamma)}, the argument
#' `delta` should be `NULL`.
#'
#' To identify a conditional conjunction \eqn{p(\gamma|\delta)}, both `gamma`
#' and `delta` should be specified.
#'
#' First, a parallel worlds graph is constructed based on the query. In a
#' parallel worlds graph, for each \eqn{do}-action that appears in \eqn{gamma}
#' (and \eqn{delta}) a copy of the original graph is created with the new
#' observational variables attaining their post-interventional values
#' but sharing the latent variables. This graph is known as a parallel worlds
#' graph. From the parallel worlds graph, a counterfactual graph
#' is derived such that each variable is unique, which might not be the case
#' in a parallel worlds graph.
#'
#' Finally, the ID* (or IDC*) algorithm is applied to determine identifiability
#' of the query. Similar to the ID and IDC algorithms for causal effects,
#' these algorithms exploit the so called c-component factorization to split
#' the query into smaller subproblems, which are then solved recursively.
#' If argument `data` is `"observations"` or `"both"`, identification of
#' interventional probabilities in the resulting functional is further
#' attempted in terms of the joint probability distribution.
#'
#' @param g A `DAG` object describing the causal graph
#'     (to obtain a `DAG` from another format, see [cfid::import_graph].
#' @param gamma A `counterfactual_conjunction` object
#'     representing the counterfactual causal query.
#' @param delta A `counterfactual_conjunction` object
#'     representing the conditioning conjunction (optional).
#' @param data A `character` string that accepts one of the following:
#' `"interventions"` (the default), `"observations"`or `"both"`. This argument
#' defines the available data for identification. If `"interventions"` is used,
#' the identification is attempted down to the intervention level. If
#' `"observations"` is used, identification is attempted down to the
#' observational level. If `"both"` is used, identification is carried out
#' term wise, where identification of each interventional distribution that
#' appears in the identifying functional is further attempted in terms of the
#' joint observed probability distribution.
#'
#' @seealso [cfid::dag], [cfid::counterfactual_variable],
#'     [cfid::counterfactual_conjunction], [cfid::probability]
#'
#' @return A list containing one or more of the following:
#'
#' * `id` A logical value that is `TRUE` if the query is identifiable and
#'     `FALSE` otherwise from the available `data` in `g`.
#'     Note that in cases where `gamma` itself is
#'     inconsistent, the query will be identifiable, but with probability 0.
#' * `formula` An object of class `functional` giving the identifying
#'     functional of the query in LaTeX syntax via `format` or `print`,
#'     if identifiable. This expression is given in terms of the
#'     available `data`. For tautological statements, the resulting
#'     probability is 1, and for inconsistent statements, the resulting
#'     probability is 0. For formatting options, see
#'     [cfid::format.functional()] and [cfid::format.probability()].
#' * `undefined` A logical value that is `TRUE` if
#'     a conditional conjunction \eqn{p(\gamma|\delta)} is undefined,
#'     for example when \eqn{p(\delta) = 0}, and `FALSE` otherwise.
#'
#' @examples
#' # Examples that appears in Shpitser and Pearl (2008)
#' g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
#' g2 <- dag("X -> W -> Y <- Z <- D X <-> Y X -> Y")
#' v1 <- cf("Y", 0, c(X = 0))
#' v2 <- cf("X", 1)
#' v3 <- cf("Z", 0, c(D = 0))
#' v4 <- cf("D", 0)
#' c1 <- conj(v1)
#' c2 <- conj(v2, v3, v4)
#' c3 <- conj(v1, v2, v3, v4)
#'
#' # Identifiable conditional conjunction
#' identifiable(g1, c1, c2)
#'
#' # Identifiable conjunction
#' identifiable(g1, c3)
#'
#' # Non-identifiable conjunction
#' identifiable(g2, c3)
#' @export
identifiable <- function(g, gamma, delta = NULL, data = "interventions") {
  if (missing(g)) {
    stop_("Argument `g` is missing.")
  } else if (!is.dag(g)) {
    stop_("Argument `g` must be a `dag` object.")
  }
  if (missing(gamma)) {
    stop_("Argument `gamma` is missing.")
  } else if (!is.counterfactual_conjunction(gamma)) {
    if (is.counterfactual_variable(gamma)) {
      if (length(gamma$obs) == 0L) {
        stop_(
          "Argument `gamma` contains counterfactual variables ",
          "without a value assignment"
        )
      }
      gamma <- counterfactual_conjunction(gamma)
    } else {
      stop_("Argument `gamma` must be a `counterfactual_conjunction` object.")
    }
  }
  if (any(!assigned(gamma))) {
    stop_(
      "Argument `gamma` contains counterfactual variables ",
      "without a value assignment."
    )
  }
  if (is.null(delta)) {
    out <- id_star(g, gamma)
  } else {
    if (!is.counterfactual_conjunction(delta)) {
      if (is.counterfactual_variable(delta)) {
        if (length(delta$obs) == 0L) {
          stop_(
            "Argument `delta` contains counterfactual variables ",
            "without a value assignment."
          )
        }
        delta <- counterfactual_conjunction(delta)
      } else {
        stop_("Argument `delta` must be a `counterfactual_conjunction` object.")
      }
    }
    if (any(!assigned(delta))) {
      stop_(
        "Argument `delta` contains counterfactual variables ",
        "without a value assignment"
      )
    }
    out <- idc_star(g, gamma, delta)
  }
  if (is.null(out$undefined)) {
    out$undefined <- FALSE
  }
  if (is.probability(out$formula)) {
    out$formula <- functional(terms = list(out$formula))
  }
  if (out$id && data != "interventions") {
    if (!is.null(out$formula$numerator)) {
      n <- out$formula$numerator
      sumset <- out$formula$denominator$sumset
      terms <- identify_terms(n)
      if (terms$id) {
        d <- terms
        d$sumset <- sumset
        out <- list(
          id = TRUE,
          formula = functional(
            numerator = terms$formula,
            denominator = d
          )
        )
      } else {
        out$identifiable <- FALSE
        out$formula <- NULL
      }
    }
    out <- identify_terms(out$formula, data, g)
    out$undefined <- FALSE
  }
  out
}

#' Causal Effect Identification
#'
#' Identify a causal effect of the form \eqn{p(y|do(x),z)}.
#'
#' @param y A `character` vector of outcome variables.
#' @param x A `character` vector of intervention variables.
#' @param z An optional `character` vector of conditioning variables
#' @param v An optional named `integer` vector giving the value assignments
#' for observed variables in the model.
#' @param g A `dag` object depicting the causal model.
#' @export
causal_effect <- function(y, x = character(0), z = character(0), v = integer(0), g) {
  stopifnot_(
    !missing(y),
    "Argument `y` is missing."
  )
  stopifnot_(
    is.character(y) && is.character(x) && is.character(z),
    "Arguments `x`, `y` and `z` must be character vectors."
  )
  stopifnot_(
    is.dag(g),
    "Argument `g` must be a `dag` object."
  )
  n_v <- length(v)
  v_names <- names(v)
  if (n_v > 0) {
    stopifnot_(
      is.integer(v),
      "Argument `v` must be an integer vector."
    )
    stopifnot_(
      !is.null(v_names),
      "Argument `v` must have names."
    )
    stopifnot_(
      setequal(v_names, attr(g, "labels")[!attr(g, "latent")]),
      "Argument `v` must assign a value to every observed variable of `g`."
    )
  } else {
    v <- integer(sum(!attr(g, "latent")))
    v_names <- attr(g, "labels")[!attr(g, "latent")]
    names(v) <- v_names
    n_v <- length(v)
  }
  out <- idc(y, x, z, probability(val = 1), g)
  if (out$id) {
    bound <- integer(n_v) - 1L
    names(bound) <- v_names
    out$formula <- assign_values(out$formula, v, v_names, bound)
  }
  out
}

assign_values <- function(x, v, v_names, bound) {
  sumset_vars <- vars(x$sumset)
  now_bound <- v_names %in% sumset_vars
  sumset_bound <- match(sumset_vars, v_names)
  bound[now_bound] <- bound[now_bound] + 1L
  for (i in seq_along(x$sumset)) {
    x$sumset[[i]]$obs <- bound[sumset_bound[i]]
  }
  if (length(x$terms) > 0) {
    for (i in seq_along(x$terms)) {
      x$terms[[i]] <- assign_values(x$terms[[i]], v, v_names, bound)
    }
  } else if (length(x$numerator) > 0) {
    x$numerator <- assign_values(x$numerator, v, v_names, bound)
    x$denominator <- assign_values(x$denominator, v, v_names, bound)
  } else {
    v[bound > 0] <- -bound[bound > 0]
    var <- vars(x$var)
    cond <- vars(x$cond)
    x <- probability(
      var = .mapply(function(a, b) cf(a, b), list(var, v[var]), list()),
      cond = .mapply(function(a, b) cf(a, b), list(cond, v[cond]), list())
    )
  }
  x
}

identify_terms <- function(x, data, g) {
  n_terms <- length(x$terms)
  out <- NULL
  if (n_terms > 0) {
    terms <- vector(mode = "list", length = n_terms)
    for (i in seq_len(n_terms)) {
      terms[[i]] <- identify_terms(x$terms[[i]], data, g)
      if (data == "observations" && !terms[[i]]$id) {
        return(list(id = FALSE, formula = NULL))
      }
    }
    formulas <- lapply(terms, "[[", "formula")
    out <- list(
      id = TRUE,
      formula = functional(sumset = x$subset, terms = formulas)
    )
  } else {
    var <- vars(x$var)
    do <- vars(x$do)
    cond <- vars(x$cond)
    prob_vals <- unlist(evs(c(x$var, x$do, x$cond)))
    prob_names <- names(prob_vals)
    lab <- attr(g, "labels")[!attr(g, "latent")]
    lab <- lab[!lab %in% prob_names]
    v_mis <- integer(length(lab))
    names(v_mis) <- lab
    v <- c(prob_vals, v_mis)
    out <- causal_effect(y = var, x = do, z = cond, v = v, g = g)
  }
  out
}