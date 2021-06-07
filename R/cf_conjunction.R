#' Counterfactual Conjunction
#'
#' Defines a conjunction of counterfactual statements (variables).
#'
#' @param ... Objects of class `CounterfactualVariable`.
#'
#' @details
#' A counterfactual conjunction is a conjunction (or a set in some contexts)
#' of counterfactual statements that are assumed to hold simultaneously.
#'
#' For example, "The value of \eqn{Y} was observed to
#' be \eqn{y}, and the value of \eqn{Y} was observed to be \eqn{y'}
#' under the intervention \eqn{do(X = x)}" consists of two variables:
#' variable \eqn{Y} without intervention, and \eqn{Y} under the intervention
#' \eqn{do(X = x)}. Conjunctions can also be constructed via the alias
#' `conj` or iteratively from `CounterfactualVariable` objects (see examples).
#'
#' @return An object of class `CounterfactualConjunction`.
#'
#' @seealso [cfid::CounterfactualVariable()]
#'
#' @examples
#' # The conjunction described in 'details'
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
#' # A specific variable (a unique combination of `var` and `int`)
#' # can only appear once in a given conjunction,
#' # otherwise the conjunction would be trivially inconsistent
#' v4 <- cf("Y", 0, c("X" = 0))
#' v5 <- cf("Y", 1, c("X" = 0))
#' c3 <- try(conj(v4, v5))
#' @export
CounterfactualConjunction <- function(...) {
    dots <- list(...)
    if (all(sapply(dots, is.CounterfactualVariable))) {
        check_conflicts(dots)
        structure(unique(dots), class = "CounterfactualConjunction")
    } else {
        stop_("All arguments must be of class 'CounterfactualVariable'")
    }
}

#' @export
as.CounterfactualConjunction <- function(x) {
    if (is.CounterfactualConjunction(x)) {
        out <- x
    } else {
        if (is.list(x)) {
            out <- do.call(CounterfactualConjunction, x)
        } else {
            stop("Cannot coerce type'", typeof(x),
                 "' to an object of class 'CounterfactualConjunction'")
        }
    }
    out
}

#' @export
is.CounterfactualConjunction <- function(x) {
    inherits(x, "CounterfactualConjunction")
}

#' @export
format.CounterfactualConjunction <- function(x, ...) {
    cf <- sapply(x, function(y) format.CounterfactualVariable(y, ...))
    paste0(cf, collapse = " \u2227 ")
}

#' @export
print.CounterfactualConjunction <- function(x, ...) {
    cat(format(x, ...), "\n")
}

#' @export
`+.CounterfactualConjunction` <- function(e1, e2) {
    if (is.CounterfactualConjunction(e1)) {
        if (is.CounterfactualConjunction(e2)) {
            y <- c(e1, e2)
            check_conflicts(y)
            out <- structure(unique(y),
                             class = "CounterfactualConjunction")
        } else if (is.CounterfactualVariable(e2)) {
            x <- list(e2)
            if (x %in% e1) {
                y <- e1
            } else {
                check_conflict(e2, e1)
                y <- c(e1, x)
            }
            out <- structure(y, class = "CounterfactualConjunction")
        } else {
            stop_("Unable to add object of class '", class(e2),
                  "' to a counterfactual conjunction")
        }
    } else if (is.CounterfactualVariable(e1)) {
        if (is.CounterfactualConjunction(e2)) {
            out <- e2 + e1
        } else if (is.CounterfactualVariable(e2)) {
            out <- CounterfactualConjunction(e1, e2)
        } else {
            stop_("Unable to add object of class '", class(e2),
                  "' to a counterfactual variable")
        }
    } else {
        stop_("Unsupported input for method '+.CounterfactualConjunction'")
    }
    out
}

#' @export
`[.CounterfactualConjunction` <- function(x, i) {
    as.CounterfactualConjunction(NextMethod())
}

#' @export
`+.CounterfactualVariable` <- `+.CounterfactualConjunction`


#' @rdname CounterfactualConjunction
#' @export
conj <- CounterfactualConjunction
