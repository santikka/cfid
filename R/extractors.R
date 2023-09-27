#' Determines the original variables present in a counterfactual conjunction.
#'
#' @param gamma A `counterfactual_conjunction` object..
#' @return A `character` vector of variable names.
#' @noRd
vars <- function(gamma) {
  vapply(gamma, function(x) x$var, character(1L))
}

#' Determines both variables and interventional variables in a
#' counterfactual conjunction
#'
#' @param gamma A `counterfactual_conjunction` object..
#' @return A `character` vector of variable names.
#' @noRd
all_vars <- function(gamma) {
  unique(
    c(
      vars(gamma),
      unlist(lapply(subs(gamma), names))
    )
  )
}

#' Get the counterfactual variables present in a counterfactual conjunction.
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return A modification of `gamma` without any value assignments.
#' @noRd
cfvars <- function(gamma) {
  lapply(gamma, cfvar)
}

#' Remove a value assignment from a counterfactual Variable
#'
#' @param x A `counterfactual_variable` object.
#' @return A modification of `x` without value assignment.
#' @noRd
cfvar <- function(x) {
  cf(x$var, sub = x$sub)
}

#' Create a list of counterfactual variables from a character vector
#'
#' @param x A `character` vector of variable names.
#' @return A list of corresponding `counterfactual_variable` objects.
#' @noRd
cflist <- function(x) {
  lapply(x, cf)
}

#' Create a list of counterfactual variables from a character vector
#' with value assignments
#'
#' @param x A `character` vector of variable names.
#' @param v A `integer` vector of values.
#' @return A list of corresponding `counterfactual_variable` objects.
#' @noRd
cflistv <- function(x, v) {
  n <- length(x)
  out <- vector(mode = "list", length = n)
  for (i in seq_len(n)) {
    out[[i]] <- cf(x[i], obs = v[i])
  }
  out
}

#' Determines the values present in a counterfactual conjunction.
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return A modification of `gamma` without any interventions.
#' @noRd
vals <- function(gamma) {
  lapply(gamma, function(x) {
    cf(x$var, x$obs)
  })
}

#' Determines which counterfactual variables within a
#' conjunction have a value assignment
#'
#' @param gamma A `counterfactual_conjunction` object..
#' @return A `character` vector of variable names.
#' @noRd
assigned <- function(gamma) {
  vapply(gamma, function(x) length(x$obs) > 0L, logical(1L))
}

#' Determines the interventions present in a counterfactual conjunction.
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return The `integer` vectors of interventions as a list.
#' @noRd
subs <- function(gamma) {
  lapply(gamma, "[[", "sub")
}

#' Determines the intervened and observed values in a counterfactual conjunction
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return Both observational and interventional assignments
#' as a `list` of `counterfactual_variable` objects.
#' @noRd
evs <- function(gamma) {
  lapply(gamma, function(x) {
    if (length(x$obs) > 0L) {
      y <- c(x$obs, x$sub)
      names(y)[1L] <- x$var
      y
    } else {
      x$sub
    }
  })
}

#' Determines the non-interventional variables in a counterfactual conjunction
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return A subset `gamma` with only observations
#' @noRd
obs <- function(gamma) {
  gamma[vapply(gamma, function(x) length(x$sub) == 0, logical(1L))]
}

#' Determines the value of a counterfactual variable within a conjunction.
#'
#' @param x A `counterfactual_variable` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @return An `integer` corresponding to the value assignment if present.
#' or `NULL`.
#' @noRd
val <- function(x, gamma) {
  for (y in gamma) {
    if (identical(x$var, y$var) && identical(x$sub, y$sub)) {
      return(y$obs)
    }
  }
  NULL
}
