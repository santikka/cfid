#' Symbolic Probability Distributions
#'
#' Defines an interventional or observational (conditional) probability
#' \eqn{P(y|do(x),z)}. For formatting options, see [cfid::format.probability()].
#'
#' @param val An `integer` value of either 0 or 1 for almost sure events.
#' @param var A `list` of objects of class `counterfactual_variable`
#' (without interventions and with value assignments).
#' `var` defines the observations \eqn{y} in \eqn{P(y|do(x),z)}.
#' @param cond A `list` of `counterfactual_variable` variable objects
#' (without interventions and with value assignments).Defines the
#' conditioning set \eqn{z} in \eqn{P(y|do(x),z)}.
#' @param do A `list` of `counterfactual_variable` variable objects
#' (without interventions and with value assignments). Defines the
#' intervention set \eqn{x} in \eqn{P(y|do(x),z)}.
#'
#' @seealso [cfid::counterfactual_variable()], [cfid::functional()]
#' @return An object of class `probability`, which is a `list` containing
#' all of the arguments of the constructor.
#' @export
probability <- function(val = NULL, var = NULL, do = NULL, cond = NULL) {
  stopifnot_(
    !is.null(val) || !is.null(var),
    "Either `val` or `var` must be provided."
  )
  if (!is.null(val)) {
    val <- try_type(val = val, type = "integer")[1]
  }
  stopifnot_(
    is.null(var) || all(vapply(var, is.counterfactual_variable, logical(1L))),
    "All elements of `var` must be `counterfactual_variable` objects."
  )
  stopifnot_(
    is.null(do) || all(vapply(do, is.counterfactual_variable, logical(1L))),
    "All elements of `do` must be `counterfactual_variable` objects."
  )
  stopifnot_(
    is.null(cond) || all(vapply(cond, is.counterfactual_variable, logical(1L))),
    "All elements of `cond` must be `counterfactual_variable` objects."
  )
  structure(
    list(
      val = val,
      var = var,
      do = do,
      cond = cond
    ),
    class = "probability"
  )
}

#' Is the argument a `probability` object?
#'
#' @param x An \R object.
#' @return A `logical` value that is `TRUE` if `x` is a `probability` object.
#' @noRd
is.probability <- function(x) {
  inherits(x, "probability")
}

#' @method format probability
#' @rdname probability
#' @param x A `probability` object.
#' @param use_primes A `logical` value. If `TRUE` (the default), any value
#' assignment of a counterfactual variable with `obs` will be formatted with
#' as many primes in the superscript as the value of `obs`, e.g.,
#' `obs = 0` outputs `"y"`, `obs = 1` outputs `"y'"`,
#' `obs = 2` outputs `"y''"` and so forth. The alternative when `FALSE` is
#' to simply denote the `obs` value via superscript directly as
#' `"y^{(obs)}"`, where obs is evaluated.
#' @param use_do A `logical` value. If `TRUE`, the explicit do-operation is
#' used to denote interventional probabilities (e.g., \eqn{P(y|do(x))}).
#' If `FALSE` (the default), the subscript notation is used instead
#' (e.g., \eqn{P_x(y)}).
#' @param ... Additional arguments passed to `format`.
#' @return A `character` representation of the `probability` object
#' in LaTeX syntax.

#' @examples
#' # Example from Makhlouf, Zhioua and Palamidessi (2021)
#' g2 <- dag("C -> A -> Y; C -> Y")
#' v1 <- cf("Y", 0, c(A = 1))
#' v2 <- cf("A", 0)
#' c1 <- conj(v1)
#' c2 <- conj(v2)
#' f <- identifiable(g2, c1, c2)$formula
#'
#' # Default, using primes and subscript notation
#' format(f)
#'
#' # Without primes, no do-operator
#' format(f, use_primes = FALSE)
#'
#' # Primes, with do-operator
#' format(f, use_do = TRUE)
#'
#' # Without primes, with do-operator
#' format(f, use_primes = FALSE, use_do = TRUE)
#'
#' @export
format.probability <- function(x, use_primes = TRUE, use_do = FALSE, ...) {
  if (length(x$val) > 0L) {
    return(as.character(x$val))
  }
  sub <- ""
  do <- ""
  cond <- ""
  rhs <- ""
  any_do <- length(x$do) > 0L
  any_cond <- length(x$cond) > 0L
  if (any_do) {
    form_do <- comma_sep(vapply(x$do, format, character(1L), use_primes))
    if (!use_do) {
      sub <- paste0("_{", form_do, "}")
    } else {
      do <- paste0("do(", form_do, ")")
    }
  }
  if (any_cond) {
    cond <- paste0(
      comma_sep(vapply(x$cond, format, character(1L), use_primes))
    )
  }
  if ((any_do && use_do) || any_cond) {
    rhs <- paste0("|", do, cond)
  }
  var <- paste0(comma_sep(vapply(x$var, format, character(1L), use_primes)))
  paste0("P", sub, "(", var, rhs, ")")
}

#' @method print probability
#' @rdname probability
#' @export
print.probability <- function(x, ...) {
  cat(format(x, ...), "\n")
}
