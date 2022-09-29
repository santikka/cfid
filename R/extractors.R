#' Determines the original variables present in a counterfactual conjunction.
#'
#' @param gamma A `counterfactual_conjunction` object..
#' @return A `character` vector of variable names.
#' @noRd
vars <- function(gamma) {
  vapply(gamma, function(x) x$var, character(1L))
}

#' Get the counterfactual variables present in a counterfactual conjunction.
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return A modification of `gamma` without any value assignments.
#' @noRd
cfvars <- function(gamma) {
  lapply(gamma, cfvar)
}

cfvar <- function(x) {
  cf(x$var, int = x$int)
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
ints <- function(gamma) {
  lapply(gamma, "[[", "int")
}

#' Determines the intervened and observed values in a counterfactual conjunction
#'
#' @param gamma A `counterfactual_conjunction` object.
#' @return Both observational and interventional assignments
#'   as a `list` of `counterfactual_variable` objects.
#' @noRd
evs <- function(gamma) {
  lapply(gamma, function(x) {
    if (length(x$obs) > 0L) {
      y <- c(x$obs, x$int)
      names(y)[1L] <- x$var
      y
    } else {
      x$int
    }
  })
}

#' Determines trivially conflicting variables in a counterfactual conjunction
#'
#' @param cf_list A list of `counterfactual_variable` objects.
#' @return A `list` of conflicting variables
#' @noRd
trivial_conflicts <- function(cf_list) {
  x <- cfvars(cf_list)
  out <- list()
  for (y in cf_list) {
    y_cf <- list(cfvar(y))
    z <- which(x %in% y_cf)
    if (length(z) > 1L) {
      x_vals <- sapply(cf_list[z], "[[", "obs")
      if (length(unique(x_vals)) > 1L) {
        out <- c(out, y_cf)
      }
    }
  }
  unique(out)
}

#' Determines whether a counterfactual variable conflicts with a
#' counterfactual conjunction
#'
#' @param y A `counterfactual_variable` object.
#' @param gamma A `Counterfactual_conjunction` object.
#' @return A `list` of conflicting variables.
trivial_conflict <- function(y, gamma) {
  y_cf <- list(cfvar(y))
  x <- cfvars(gamma)
  z <- which(x %in% y_cf)
  if (length(z) > 0L) {
    xy_vals <- c(sapply(gamma[z], "[[", "obs"), y$obs)
    if (length(unique(xy_vals)) > 1L) {
      return(y_cf)
    }
  }
  list()
}

#' Determines the value of a counterfactual variable within a conjunction.
#'
#' @param x A `counterfactual_variable` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @return An integer correspodning to the value assignment if present
#'   or `NULL`.
#' @noRd
val <- function(x, gamma) {
  for (y in gamma) {
    if (identical(x$var, y$var) && identical(x$int, y$int)) {
      return(y$obs)
    }
  }
  NULL
}
