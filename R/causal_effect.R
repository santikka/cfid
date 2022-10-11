#' Causal Effect Identification
#'
#' Identify a causal effect of the form \eqn{p(y|do(x),z)} from \eqn{p(v)} in
#' \eqn{G}.
#'
#' @param y A `character` vector of response variables.
#' @param x A `character` vector of intervention variables.
#' @param z An optional `character` vector of conditioning variables
#' @param v An optional named `integer` vector giving the value assignments
#' for observed variables in the model.
#' @param g A `dag` object depicting the causal diagram \eqn{G}.
#' @return A `list` with two components:
#'
#' * `id`\cr A `logical` value that is `TRUE` if the query is identifiable and
#' `FALSE` otherwise.
#' * `formula`\cr A  `functional` object expressing the causal effect in terms of
#'  the joint probability distribution \eqn{p(v)} for identifiable queries or
#'  `NULL` if the queri is not identifiable.
#'
#' @export
causal_effect <- function(y, x = character(0), z = character(0),
                          v = integer(0), g) {
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

#' Set Value Assignment Levels for a Probability Distribution
#'
#' @param x A `functional` object.
#' @param v A named `integer` vector of values to assign
#' @param v_names A `character` vector of the names of `v`
#' @param bound An `integer` vector counting the number of times specific
#' variables have been bound by summation.
#' @noRd
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
