#' Counterfactual Variable
#'
#' `cf` defines a counterfactual variable \eqn{y_x}.
#'
#' @section Counterfactual Variables:
#' Assume that \eqn{Y} is a single variable and \eqn{X} is a vector
#' of variables. Here, The notation \eqn{y_x} means that the variable
#' \eqn{Y} (`var`) attains the value \eqn{y} (`obs`) under the
#' intervention \eqn{do(X = x)} (`sub`).
#'
#' Note that different values of `obs` for a two variables with the same `var`
#' and the same `sub` do not denote their actual values, but the levels
#' (i.e., `obs = 0` is different from `obs = 1`, but the variables do not
#' actually attain values 0 and 1). In other words, if `var` is different for
#' two counterfactual variables, but they have the same value `obs`, this
#' does not mean that these variables have the same value. They will only
#' actually have the same value if they share both `var` and `obs`.
#'
#' For more information about the
#' \eqn{do}-operator, see Pearl (2009). The shortcut alias `cf` can also
#' be used to construct counterfactual variables.
#'
#' @rdname counterfactuals
#' @param var A character vector of length one naming the variable
#' (i.e., \eqn{Y}).
#' @param obs An integer vector of length one or zero. If given, denotes
#' the observed value of `var` (i.e., \eqn{Y = y})
#' @param sub A named integer vector where the names correspond to the
#' variables intervened on (via \eqn{do(X = x)}) and values to the
#' value assignments (their levels, e.g., \eqn{x}).
#
#' @return `cf` returns an object of class `counterfactual_variable`.
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
counterfactual_variable <- function(var, obs = integer(0L), sub = integer(0L)) {
  stopifnot_(
    length(var) == 1L,
    "Argument `var` must be a signle character string."
  )
  var <- toupper(try_type(var = var, type = "character"))
  stopifnot_(
    length(obs) < 2L,
    "Argument `obs` must a single integer if provided."
  )
  obs <- try_type(obs = obs, type = "integer")
  names(var) <- NULL
  names(obs) <- NULL
  if (length(sub) > 0L) {
    stopifnot_(
      !is.null(names(sub)),
      "Argument `sub` must be a named integer vector."
    )
    sub <- try_type(sub = sub, type = "integer")
    names(sub) <- toupper(names(sub))
  }
  structure(
    list(var = var, obs = obs, sub = sub),
    class = "counterfactual_variable"
  )
}

#' Is the argument a `counterfactual_variable` object?
#'
#' @param x An \R object.
#' @return A `logical` value that is `TRUE` if `x` is a
#' `counterfactual_variable` object.
#' @noRd
is.counterfactual_variable <- function(x) {
  inherits(x, "counterfactual_variable")
}

#' @method format counterfactual_variable
#' @rdname counterfactuals
#' @param x A `counterfactual_variable` or a `counterfactual_conjunction`
#' object.
#' @param use_primes A `logical` value indicating whether primes should be
#' used to differentiate between value assignments
#' @export
format.counterfactual_variable <- function(x, use_primes = TRUE, ...) {
  super_var <- character(0L)
  super_sub <- character(0L)
  form <- list(var = x$var, sub = character(0L))
  out <- ""
  if (length(x$obs) > 0L) {
    if (x$obs > 0L) {
      if (use_primes) {
        super_var <- rep_char("'", x$obs)
      } else {
        super_var <- paste0("^{(", x$obs, ")}")
      }
    } else if (x$obs < 0L) {
      more <- x$obs < -1L
      lhs <- ifelse_(more, "{", "")
      rhs <- ifelse_(more, "}", "")
      super_var <- paste0("^", lhs, "*", rhs)
    }
    form$var <- collapse(tolower(x$var), super_var)
  } else {
    form$var <- x$var
  }
  if (length(x$sub)) {
    if (use_primes) {
      super_sub <- sapply(x$sub, function(y) rep_char("'", y))
    } else {
      super_sub <- sapply(x$sub, function(y) paste0("^{(", y, ")}"))
      super_sub[x$sub == 0L] <- ""
    }
    form$sub <- paste0(
      paste0(tolower(names(x$sub)), super_sub),
      collapse = ","
    )
    out <- collapse(form$var, "_{", form$sub, "}")
  } else {
    out <- form$var
  }
  out
}

#' @method print counterfactual_variable
#' @rdname counterfactuals
#' @export
print.counterfactual_variable <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @rdname counterfactuals
#' @export
cf <- counterfactual_variable

#' Check for consistent value assignments between counterfactual variables
#'
#' @param x A `counterfactual_variable` object.
#' @param x y `counterfactual_variable` object.
#' @noRd
val_consistent <- function(x, y) {
  if (is.null(x) || is.null(y) || identical(x, y)) {
    TRUE
  } else {
    FALSE
  }
}
