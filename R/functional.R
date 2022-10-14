#' Identifying Functional of a Counterfactual Query
#'
#' Identifying functionals are more complicated probabilistic expressions
#' that cannot be expressed as simple observational or interventional
#* probability using [cfid::probability()].
#'
#' When formatted via `print` or `format`, the arguments are
#' prioritized in the following order if conflicting definitions are given:
#' (`sumset`, `terms`), (`numerator`, `denominator`).
#'
#' @param sumset A `list` of objects of class `counterfactual_variable`
#' (without interventions and with value assignments).
#' If the probability depicts marginalization, `sumset`
#' defines the set of variables to be marginalized over.
#' @param terms A `list` of `functional` objects if the object in question
#' is meant to represent a product of terms.
#' @param numerator A `functional` or a `probability` object.
#' If the functional represents a conditional probability that
#' cannot be expressed simply in terms of the set of inputs \eqn{P*},
#' this is the numerator of the quotient representation.
#' @param denominator A `functional` or a `probability` object.
#' The denominator of the quotient representation.
#'
#' @return An object of class `functional`, which is a `list` containing
#' all of the arguments of the constructor.
#'
#' @export
functional <- function(sumset = NULL, terms = NULL,
                       numerator = NULL, denominator = NULL) {
  stopifnot_(
    is.null(sumset) ||
      all(vapply(sumset, is.counterfactual_variable, logical(1L))),
    "All elements of `sumset` must be `counterfactual_variable` objects."
  )
  stopifnot_(
    is.null(terms) || is.list(terms),
    "Argument `terms` must be a `list`"
  )
  valid_terms <- vapply(
    terms,
    function(x) { is.functional(x) || is.probability(x) },
    logical(1L)
  )
  stopifnot_(
    all(valid_terms),
    "Argument `terms` must be a list of `functional` or `probability` objects."
  )
  stopifnot_(
    is.null(numerator) ||
      is.functional(numerator) ||
      is.probability(numerator),
    "Argument `numerator` must be a `functional` or a `probability` object."
  )
  stopifnot_(
    is.null(denominator) ||
      is.functional(denominator) ||
      is.probability(denominator),
    "Argument `denominator` must be a `functional` or a `probability` object."
  )
  stopifnot_(
    (is.null(numerator) && is.null(denominator)) ||
      (!is.null(numerator) && !is.null(denominator)),
    "Both `denominaor` and `numerator` must be provided when either is used."
  )
  structure(
    list(
      sumset = sumset,
      terms = terms,
      numerator = numerator,
      denominator = denominator
    ),
    class = "functional"
  )
}

is.functional <- function(x) {
  inherits(x, "functional")
}

#' @rdname functional
#' @param x A `functional` object.
#' @param use_primes A `logical` value. If `TRUE` (the default), any value
#' assignment of a counterfactual variable with `obs` will be formatted with
#' as many primes in the superscript as the value of `obs`, e.g.,
#' `obs = 0` outputs `"y"`, `obs = 1` outputs `"y'"`,
#' `obs = 2` outputs `"y''"` and so forth. The alternative when `FALSE` is
#' to simply denote the `obs` value via superscript directly as
#' `"y^{(obs)}"`, where obs is evaluated.
#' @param use_do A `logical` value. If `TRUE`, the explicit do-operation is
#' used to denote interventional probabilities (e.g., \eqn{p(y|do(x))}).
#' If `FALSE` (the default), the subscript notation is used instead
#' (e.g., \eqn{p_x(y)}).
#' @param ... Additional arguments passed to `format`.
#' @return A `character` representation of the `functional` object
#' in LaTeX syntax.
#'
#' @export
format.functional <- function(x, use_primes = TRUE, use_do = FALSE, ...) {
  terms <- ""
  sumset <- ""
  fraction <- ""
  brace_l <- ""
  brace_r <- ""
  if (length(x$sumset) > 0) {
    sumset <- paste0(
      "\\sum_{",
      comma_sep(vapply(x$sumset, format, character(1L), use_primes)),
      "} "
    )
  }
  if (!is.null(x$terms)) {
    terms <- vapply(x$terms, format, character(1L), use_primes, use_do)
    sums <- vapply(
      x$terms,
      function(y) { is.functional(y) && length(y$sumset) > 0 },
      logical(1L)
    )
    if (length(x$terms) > 1L && any(sums)) {
      terms[sums] <- paste0("\\left(", terms[sums], "\\right)")
    }
    terms <- collapse(terms)
  } else if (!is.null(x$numerator)) {
    if (length(x$denominator$val) > 0L && x$denominator$val == 1L) {
      fraction <- format(x$numerator, use_primes, use_do)
    } else {
      fraction <- paste0(
        "\\frac{",
        format(x$numerator, use_primes, use_do),
        "}{",
        format(x$denominator, use_primes, use_do),
        "}"
      )
    }
  }
  paste0(sumset, terms, fraction)
}

#' @rdname functional
#' @export
print.functional <- function(x, ...) {
  cat(format(x, ...), "\n")
}