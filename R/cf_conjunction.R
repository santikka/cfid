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
#' # A specific value of a variable (a unique combination of `var` and `sub`)
#' # can only appear once in a given conjunction,
#' # otherwise the conjunction would be trivially inconsistent
#' v4 <- cf("Y", 0, c("X" = 0))
#' v5 <- cf("Y", 1, c("X" = 0))
#' c3 <- try(conj(v4, v5))
#'
#' @export
counterfactual_conjunction <- function(...) {
  dots <- list(...)
  stopifnot_(
    all(vapply(dots, is.counterfactual_variable, logical(1L))),
    "All arguments must be `counterfactual_variable` objects."
  )
  check_conflicts(dots)
  structure(unique(dots), class = "counterfactual_conjunction")
}

#' Convert an \R object into a `counterfactual_conjunction`
#'
#' @param x An \R object.
#' @return A `counterfactual_conjunction` object, if a suitable method exists.
#' @noRd
as.counterfactual_conjunction <- function(x) {
  UseMethod("as.counterfactual_conjunction")
}

#' Convert a `list` into a counterfactual conjunction
#'
#' @param x A `list` object.
#' @return A `counterfactual_conjunction` object.
#' @noRd
as.counterfactual_conjunction.list <- function(x) {
  do.call(counterfactual_conjunction, x)
}

#' Convert a `counterfactual_variable` into a `counterfactual_conjunction`
#'
#' @param x A `counterfactual_variable` object.
#' @return A `counterfactual_conjunction` object.
#' @noRd
as.counterfactual_conjunction.counterfactual_variable <- function(x) {
  counterfactual_conjunction(x)
}

#' Convert an \R object into a `counterfactual_conjunction`
#'
#' @param x An \R object.
#' @return A `counterfactual_conjunction` object, if `x` is a counterfactual
#' @noRd
as.counterfactual_conjunction.default <- function(x) {
  if (is.counterfactual_conjunction(x)) {
    x
  } else {
    stop_("Cannot coerce object to class `counterfactual_conjunction`: ", x)
  }
}

#' Is the argument a `counterfactual_conjunction` object?
#'
#' @param x An \R object.
#' @return A `logical` value that is `TRUE` if `x` is a
#' `counterfactual_conjunction` object.
#' @noRd
is.counterfactual_conjunction <- function(x) {
  inherits(x, "counterfactual_conjunction")
}

#' @method format counterfactual_conjunction
#' @rdname counterfactuals
#' @param var_sep A `character` string to separate counterfactual variables.
#' @param ... Additional arguments passed to
#' [cfid::format.counterfactual_variable()].
#' @export
format.counterfactual_conjunction <- function(x, var_sep = " /\\ ", ...) {
  cf <- vapply(
    x,
    function(y) format.counterfactual_variable(y, ...),
    character(1L)
  )
  paste0(cf, collapse = var_sep)
}

#' @method print counterfactual_conjunction
#' @rdname counterfactuals
#' @export
print.counterfactual_conjunction <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
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
        "Cannot add an object of class `", class(e2), "` ",
        "to a counterfactual conjunction."
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
        "Cannot add an object of class `", class(e2), "` ",
        "to a counterfactual variable."
      )
    }
  } else {
    stop_("Unsupported input for method `+.counterfactual_conjunction`.")
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

#' Check for conflicting value assignments within one or between two
#' counterfactual conjunctions
#'
#' @param x A `counterfactual_conjunction` object.
#' @param y A `counterfactual_conjunction` object.
#' @noRd
check_conflicts <- function(x, y) {
  if (missing(y)) {
    conf <- trivial_conflicts(x)
  } else {
    conf <- trivial_conflict(x, y)
  }
  if (length(conf) > 0L) {
    conf_form <- comma_sep(vapply(conf, format, character(1L)))
    stop_("Inconsistent definitions given for variables: ", conf_form)
  }
}

#' Determines trivially conflicting variables in a counterfactual conjunction
#'
#' @param cf_list A list of `counterfactual_variable` objects.
#' @return A `list` of conflicting variables.
#' @noRd
trivial_conflicts <- function(cf_list) {
  x <- cfvars(cf_list)
  out <- list()
  for (y in cf_list) {
    y_cf <- list(cfvar(y))
    z <- which(x %in% y_cf)
    if (length(z) > 1L) {
      x_vals <- vapply(cf_list[z], "[[", integer(1L), "obs")
      if (length(unique(x_vals)) > 1L) {
        out <- c(out, y_cf)
      }
    }
  }
  unique(out)
}

#' Determines whether a counterfactual variable conflicts with another
#' counterfactual conjunction
#'
#' @param y A `counterfactual_variable` object.
#' @param gamma A `Counterfactual_conjunction` object.
#' @return A `list` of conflicting variables.
#' @noRd
trivial_conflict <- function(y, gamma) {
  y_cf <- list(cfvar(y))
  x <- cfvars(gamma)
  z <- which(x %in% y_cf)
  if (length(z) > 0L) {
    xy_vals <- c(vapply(gamma[z], "[[", integer(1L), "obs"), y$obs)
    if (length(unique(xy_vals)) > 1L) {
      return(y_cf)
    }
  }
  list()
}

#' Remove tautological statements from a counterfactual conjunction
#'
#' @param x A `counterfactual_conjunction` object.
#' @return A `logical` value that is `TRUE` if `x` is a tautology.
#' @noRd
remove_tautologies <- function(x) {
  x_len <- length(x)
  taut <- logical(x_len)
  for (i in seq_len(x_len)) {
    y <- x[[i]]
    if (length(y$sub) > 0L) {
      if (y$var %in% names(y$sub)) {
        z <- which(names(y$sub) %in% y$var)
        if (y$obs == y$sub[z]) {
          taut[i] <- TRUE
        }
      }
    }
  }
  x[!taut]
}

#' Check if a counterfactual statement is inconsistent
#'
#' @param x A `counterfactual_conjunction` object
#' @return A `logical` value that is `TRUE` if `x` is inconsistent.
#' @noRd
is_inconsistent <- function(x) {
  for (y in x) {
    if (length(y$sub) > 0L) {
      if (y$var %in% names(y$sub)) {
        z <- which(names(y$sub) %in% y$var)
        if (y$obs != y$sub[z]) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}