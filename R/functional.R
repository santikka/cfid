#' Identifying Functional of a Counterfactual Query
#'
#' Identifying functionals are more complicated probabilistic expressions
#' that cannot be expressed as simple observational or interventional
#' probabilities using [cfid::probability()].
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
#' cannot be expressed simply in terms of the set of inputs,
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
#' @param ... Additional arguments passed to `format`.
#' @return A `character` representation of the `functional` object
#' in LaTeX syntax.
#'
#' @export
format.functional <- function(x, ...) {
  terms <- ""
  sumset <- ""
  fraction <- ""
  brace_l <- ""
  brace_r <- ""
  if (length(x$sumset) > 0) {
    sumset <- paste0(
      "\\sum_{",
      comma_sep(vapply(x$sumset, format, character(1L), ...)),
      "} "
    )
  }
  if (!is.null(x$terms)) {
    terms <- vapply(x$terms, format, character(1L), ...)
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
      fraction <- format(x$numerator, ...)
    } else {
      fraction <- paste0(
        "\\frac{",
        format(x$numerator, ...),
        "}{",
        format(x$denominator, ...),
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

#' Set Value Assignment Levels for a Functional
#'
#' @param x A `functional` or a `probability` object.
#' @param v A named `integer` vector of values to assign.
#' @param bound An `integer` vector counting the number of times specific
#' variables have been bound by summation.
#' @noRd
assign_values <- function(x, bound, v, termwise = FALSE) {
  sumset_vars <- vars(x$sumset)
  v_names <- names(bound)
  now_bound <- v_names %in% sumset_vars
  sumset_bound <- match(sumset_vars, v_names)
  bound[now_bound] <- bound[now_bound] + 1L
  for (i in seq_along(x$sumset)) {
    x$sumset[[i]]$obs <- -bound[sumset_bound[i]]
  }
  if (length(x$terms) > 0) {
    for (i in seq_along(x$terms)) {
      x$terms[[i]] <- assign_values(x$terms[[i]], bound, v, termwise)
    }
  } else if (length(x$numerator) > 0) {
    x$numerator <- assign_values(x$numerator, bound, v, termwise)
    x$denominator <- assign_values(x$denominator, bound, v, termwise)
  } else {
    if (!is.null(x$val)) {
      return(x)
    }
    if (termwise) {
      v_term <- unlist(c(evs(x$var), evs(x$cond), evs(x$do)))
      v[names(v_term)] <- v_term
      bind <- bound > 0 & v_names %in% attr(x, "free_vars")
      attr(x, "free_vars") <- NULL
    } else {
      bind <- bound > 0
    }
    v[bind] <- -bound[bind]
    var <- vars(x$var)
    cond <- vars(x$cond)
    do <- vars(x$do)
    x <- probability(
      var = .mapply(function(a, b) cf(a, b), list(var, v[var]), list()),
      cond = .mapply(function(a, b) cf(a, b), list(cond, v[cond]), list()),
      do = .mapply(function(a, b) cf(a, b), list(do, v[do]), list())
    )
  }
  x
}