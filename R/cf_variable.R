#' Counterfactual Variable
#'
#' Defines a counterfactual variable \eqn{y_x}.
#'
#' @param var A character vector of length one naming
#'     the variable (i.e., \eqn{Y}).
#' @param obs An integer vector of length one or zero. If given, denotes
#'     the observed value of `var` (i.e., \eqn{Y = 0})
#' @param int A named integer vector where the names correspond to the variables
#'     intervened on (i.e., \eqn{X}) and values to the value assignments
#'     (their levels).
#'
#' @details
#' Assume that \eqn{Y} is a single variable and \eqn{X} is a vector
#' of variables. Here, The notation \eqn{y_x} means that the variable
#' \eqn{Y} (`var`) attains the value \eqn{y} (`obs`) under the
#' intervention \eqn{do(X = x)} (`int`).
#'
#' Note that different values of `obs` for a two variables with the same `var`
#' and the same `int` do not denote their actual values, but the levels
#' (i.e., `obs = 0` is different from `obs = 1`, but the variables do not
#' actually attain values 0 and 1). For more information about the
#' \eqn{do}-operator, see Pearl (2009). The shortcut alias `cf` can also
#' be used to construct counterfactual variables variables.
#'
#' @references
#' Pearl, J. (2009) *Causality: Models, Reasoning, and Inference*. Cambridge
#' University Press, 2nd edition.
#'
#' @return An object of class `CounterfactualVariable`.
#'
#' @seealso [cfid::CounterfactualConjunction()]
#'
#' @examples
#' # Y without an assigned value or any interventions
#' cf("Y")
#'
#' # Y with a value assignment y, but no interventions
#' cf("Y", 0)
#'
#' # Y with a different value y', but no interventions
#' cf("Y", 1)
#'
#' # Y with the same value as the previous under the intervention do(X = x)
#' cf("Y", 1, c("X" = 0))
#'
#' # Y with yet another value y'', under the intervention
#' # do(X = x', Z = z), i.e., the intervention on X has a different value
#' # than the previous (x != x') and Z is also assigned the value z
#' cf("Y", 2, c("X" = 1, "Z" = 0))
#' @export
CounterfactualVariable <- function(var, obs = integer(0), int = integer(0)) {
    var <- toupper(try_type(var = var, type = "character")[1])
    if (length(obs)) {
        obs <- try_type(obs = obs, type = "integer")[1]
    }
    names(var) <- names(obs) <- NULL
    if (length(int)) {
        if (is.null(names(int))) {
            stop_("Argument 'int' must be named")
        }
        int <- try_type(int = int, type = "integer")
        names(int) <- toupper(names(int))
    }
    structure(
        list(var = var, obs = obs, int = int),
        class = "CounterfactualVariable"
    )
}

is.CounterfactualVariable <- function(x) {
    inherits(x, "CounterfactualVariable")
}

#' @export
format.CounterfactualVariable <- function(x, use_primes = TRUE) {
    super_var <- character(0L)
    super_int <- character(0L)
    form <- list(var = x$var, int = character(0L))
    out <- ""
    if (length(x$obs)) {
        if (x$obs > 0L) {
            if (use_primes) {
                super_var <- rep_char("'", x$obs)
            } else {
                super_var <- paste0("^{(", x$obs, ")}")
            }
        }
        form$var <- collapse(tolower(x$var), super_var)
    } else {
        form$var <- x$var
    }
    if (length(x$int)) {
        if (use_primes) {
            super_int <- sapply(x$int, function(y) rep_char("'", y))
        } else {
            super_int <- sapply(x$int, function(y) paste0("^{(", y, ")}"))
            super_int[x$int == 0L] <- ""
        }
        form$int <- paste0(paste0(tolower(names(x$int)), super_int),
                           collapse = ",")
        out <- collapse(form$var, "_{", form$int, "}")
    } else {
        out <- form$var
    }
    out
}

#' @export
print.CounterfactualVariable <- function(x, ...) {
    cat(format(x, ...), "\n")
}

#' @rdname CounterfactualVariable
#' @export
cf <- CounterfactualVariable

# Check if x is a tautological statement
is_tautology <- function(x) {
    if (length(x$int)) {
        if (x$var %in% names(x$int)) {
            y <- which(names(x$int) %in% x$var)
            if (x$obs == x$int[y]) {
                return(TRUE)
            }
        }
    }
    FALSE
}

# Check if x is an inconsistent statement
is_inconsistent <- function(x) {
    if (length(x$int)) {
        if (x$var %in% names(x$int)) {
            y <- which(names(x$int) %in% x$var)
            if (x$obs != x$int[y]) {
                return(TRUE)
            }
        }
    }
    FALSE
}
