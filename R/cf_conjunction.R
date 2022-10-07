#' Counterfactual Conjunction
#'
#' `conj` defines a conjunction of counterfactual statements (variables).
#'
#' @section Counterfactual Conjunctions:
#' A counterfactual conjunction is a conjunction (or a set in some contexts)
#' of counterfactual statements that are assumed to hold simultaneously.
#'
#' For example, the statement "The value of \eqn{Y} was observed to
#' be \eqn{y}, and the value of \eqn{Y} was observed to be \eqn{y'}
#' under the intervention \eqn{do(X = x)}" consists of two variables:
#' variable \eqn{Y} without intervention, and \eqn{Y} under the intervention
#' \eqn{do(X = x)} (which is \eqn{Y_x}). This conjunction can be succinctly
#' written as \eqn{y \wedge y'_x}.
#'
#' Conjunctions can also be constructed
#' via the alias `conj` or iteratively from `counterfactual_variable`
#' objects (see examples).
#'
#' @rdname counterfactuals
#' @param ... For `conj`, `counterfactual_variable` objects. For `print` and
#' `format` methods, additional arguments passed to [base::format()].
#' @return `conj` returns an object of class `counterfactual_conjunction`.
#'
#' @examples
#' # The conjunction described under 'details'
#' v1 <- cf("Y", 0)
#' v2 <- cf("Y", 1, c("X" = 0))
#' c1 <- conj(v1, v2)
#'
#' # Alternative construction
#' c1 <- v1 + v2
#'
#' # Adding further variables
#' v3 <- cf("X", 1)
#' c2 <- c1 + v3
#'
#' # A specific variable (a unique combination of `var` and `sub`)
#' # can only appear once in a given conjunction,
#' # otherwise the conjunction would be trivially inconsistent
#' v4 <- cf("Y", 0, c("X" = 0))
#' v5 <- cf("Y", 1, c("X" = 0))
#' c3 <- try(conj(v4, v5))
#' @export
counterfactual_conjunction <- function(...) {
  dots <- list(...)
  if (all(sapply(dots, is.counterfactual_variable))) {
    check_conflicts(dots)
    structure(unique(dots), class = "counterfactual_conjunction")
  } else {
    stop_("All arguments must be `counterfactual_variable` objects.")
  }
}

as.counterfactual_conjunction <- function(x) {
  UseMethod("as.counterfactual_conjunction")
}

as.counterfactual_conjunction.list <- function(x) {
  do.call(counterfactual_conjunction, x)
}

as.counterfactual_conjunction.default <- function(x) {
  if (is.counterfactual_conjunction(x)) {
    x
  } else {
    stop_("Cannot object to class `counterfactual_conjunction`", x)
  }
}

is.counterfactual_conjunction <- function(x) {
  inherits(x, "counterfactual_conjunction")
}

#' @method format counterfactual_conjunction
#' @rdname counterfactuals
#' @param varsep A `character` string to separate counterfactual variables.
#' @param ... Additional arguments passed to
#' [cfid::format.counterfactual_variable()].
#' @export
format.counterfactual_conjunction <- function(x, varsep = " \u2227 ", ...) {
  cf <- sapply(x, function(y) format.counterfactual_variable(y, ...))
  paste0(cf, collapse = varsep)
}

#' @method print counterfactual_conjunction
#' @rdname counterfactuals
#' @export
print.counterfactual_conjunction <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @method + counterfactual_conjunction
#' @rdname counterfactuals
#' @param e1 A `counterfactual_variable` or a `counterfactual_conjunction`
#' object.
#' @param e2 A `counterfactual_variable` or a `counterfactual_conjunction`
#' object.
#' @export
`+.counterfactual_conjunction` <- function(e1, e2) {
  if (is.counterfactual_conjunction(e1)) {
    if (is.counterfactual_conjunction(e2)) {
      y <- c(e1, e2)
      check_conflicts(y)
      out <- structure(unique(y), class = "counterfactual_conjunction")
    } else if (is.counterfactual_variable(e2)) {
      x <- list(e2)
      if (x %in% e1) {
        y <- e1
      } else {
        check_conflicts(e2, e1)
        y <- c(e1, x)
      }
      out <- structure(y, class = "counterfactual_conjunction")
    } else {
      stop_(
        "Unable to add object of class `", class(e2), "` ",
        "to a counterfactual conjunction"
      )
    }
  } else if (is.counterfactual_variable(e1)) {
    if (is.counterfactual_conjunction(e2)) {
      x <- list(e1)
      if (x %in% e2) {
        y <- e2
      } else {
        check_conflicts(e1, e2)
        y <- c(x, e2)
      }
      out <- structure(y, class = "counterfactual_conjunction")
    } else if (is.counterfactual_variable(e2)) {
      out <- counterfactual_conjunction(e1, e2)
    } else {
      stop_(
        "Unable to add object of class `", class(e2), "` ",
        "to a counterfactual variable"
      )
    }
  } else {
    stop_("Unsupported input for method `+.counterfactual_conjunction`")
  }
  out
}

#' @method [ counterfactual_conjunction
#' @rdname counterfactuals
#' @param i An `integer` index vector.
#' @export
`[.counterfactual_conjunction` <- function(x, i) {
  as.counterfactual_conjunction(NextMethod())
}

#' @method + counterfactual_variable
#' @rdname counterfactuals
#' @export
`+.counterfactual_variable` <- `+.counterfactual_conjunction`


#' @rdname counterfactuals
#' @export
conj <- counterfactual_conjunction
