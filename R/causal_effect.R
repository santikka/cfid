#' Causal Effect Identification
#'
#' Identify a causal effect of the form \eqn{P(y|do(x),z)} from \eqn{P(v)} in
#' \eqn{G}.
#'
#' @param g A `dag` object depicting the causal diagram \eqn{G}.
#' @param y A `character` vector of response variables \eqn{Y}.
#' @param x A `character` vector of intervention variables \eqn{X}.
#' @param z An optional `character` vector of conditioning variables \eqn{Z}.
#' @param v An optional named `integer` vector giving the value assignments
#' for observed variables in the model, i.e. \eqn{V = v} or
#' for a subset of \eqn{V}.
#' @return An object of class `query` which is a `list` with the following
#' components:
#'
#' * `id`\cr A `logical` value that is `TRUE` if the query is identifiable and
#'   `FALSE` otherwise.
#' * `formula`\cr A  `functional` object expressing the causal effect in terms
#'   of the joint probability distribution \eqn{P(v)} for identifiable queries
#'   or `NULL` if the query is not identifiable.
#' * `data`\cr The available data, for `causal_effect` this is always
#'   `"observations"`
#' * `causaleffect`\cr The original query \eqn{P(y|do(x),z)} as a `probability`
#'   object.
#' * `undefined`\cr A `logical` value, this is always `FALSE` for
#'   `causaleffect`
#'
#' @export
causal_effect <- function(g, y, x = character(0),
                          z = character(0), v = integer(0)) {
  stopifnot_(
    !missing(g),
    "Argument `g` is missing."
  )
  stopifnot_(
    is.dag(g),
    "Argument `g` must be a `dag` object."
  )
  stopifnot_(
    !missing(y),
    "Argument `y` is missing."
  )
  stopifnot_(
    is.character(y) && is.character(x) && is.character(z),
    "Arguments `x`, `y` and `z` must be character vectors."
  )
  n_v <- length(v)
  v_names <- names(v)
  if (n_v > 0) {
    v <- try_type(v = v, type = "integer")
    stopifnot_(
      !is.null(v_names),
      "Argument `v` must have names."
    )
  }
  v_g <- attr(g, "labels")[!attr(g, "latent")]
  stopifnot_(
    all(y %in% v_g),
    paste0(
      "Argument `y` contains variables that are not present in `g`: ",
      comma_sep(setdiff(y, v_g))
    )
  )
  stopifnot_(
    all(x %in% v_g),
    paste0(
      "Argument `x` contains variables that are not present in `g`: ",
      comma_sep(setdiff(x, v_g))
    )
  )
  stopifnot_(
    length(z) == 0L || all(z %in% v_g),
    paste0(
      "Argument `z` contains variables that are not present in `g`: ",
      comma_sep(setdiff(z, v_g))
    )
  )
  stopifnot_(
    n_v == 0 || all(v_names %in% v_g),
    paste0(
      "Argument `v` has names that are not present in `g`: ",
      comma_sep(setdiff(v_names, v_g))
    )
  )
  n_obs <- sum(!attr(g, "latent"))
  if (n_v != n_obs) {
    v_temp <- v
    v <- set_names(integer(n_obs), v_g)
    v[v_names] <- v_temp
    v_names <- names(v)
  }
  out <- idc(y, x, z, probability(val = 1), g)
  if (out$id) {
    bound <- integer(n_obs) - 1L
    names(bound) <- v_names
    xyz <- c(x, y, z)
    bound[xyz] <- bound[xyz] + 1L
    out$formula <- assign_values(out$formula, bound, v)
  }
  out$counterfactual <- FALSE
  out$causaleffect <- probability(
    var = cflistv(y, v = v[y]),
    do = cflistv(x, v = v[x]),
    cond = cflistv(z, v = v[z])
  )
  out$data <- "observations"
  out$undefined <- FALSE
  structure(
    out,
    class = "query"
  )
}
