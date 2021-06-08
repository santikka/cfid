# Determines the original variables present in a counterfactual conjunction.
#
# @param gamma A counterfactual conjunction.
# @return A character vector of variable names.
vars <- function(gamma) {
    sapply(gamma, function(x) x$var)
}

# Determines the counterfactual variables present in a counterfactual conjunction.
#
# @Param gamma A counterfactual conjunction.
# @param A modification of gamma without any value assignments.
cfvars <- function(gamma) {
    lapply(gamma, cfvar)
}

cfvar <- function(x) {
    cf(x$var, int = x$int)
}

# Determines the values present in a counterfactual conjunction.
#
# @param gamma A counterfactual conjunction.
# @return A modification of gamma without any interventions.
vals <- function(gamma) {
    lapply(gamma, function(x) {
        cf(x$var, x$obs)
    })
}

# Determines the variables that have been fixed by interventions in
# a counterfactual conjunction
#
# @param gamma A counterfactual conjunction.
# @return A character vector of variable names.
fixed <- function(gamma) {
    sapply(gamma, function(x) length(x$obs) > 0)
}

# Determines the interventions present in a counterfactual conjunction.
#
# @param gamma A counterfactual conjunction.
# @return The integer vectors of interventions as a list
ints <- function(gamma) {
    lapply(gamma, "[[", "int")
}

# Determines the intervened and observed values in a counterfactual conjunction
#
# @param gamma A counterfactual conjunction.
# @return Both observational and interventional assignments
#     as a list of counterfactual variables
evs <- function(gamma) {
    lapply(gamma, function(x) {
        if (length(x$obs)) {
            y <- c(x$obs, x$int)
            names(y)[1] <- x$var
            y
        } else {
            x$int
        }
    })
}

# Determines trivially conflicting variables in a counterfactual conjunction
#
# @param cf_list A list of counterfactual variables
# @return A list of conflicting variables
trivial_conflicts <- function(cf_list) {
    x <- cfvars(cf_list)
    out <- list()
    for (y in cf_list) {
        y_cf <- list(cfvar(y))
        z <- which(x %in% y_cf)
        if (length(z) > 1) {
            x_vals <- sapply(cf_list[z], "[[", "obs")
            if (length(unique(x_vals)) > 1) {
                out <- c(out, y_cf)
            }
        }
    }
    unique(out)
}

# Determines whether a counterfactual variable conflicts with a
# counterfactual conjunction
#
# @param y A `CounterfactualVariable` object.
# @param gamma A `CounterfactualConjunction` object.
trivial_conflict <- function(y, gamma) {
    y_cf <- list(cfvar(y))
    x <- cfvars(gamma)
    z <- which(x %in% y_cf)
    if (length(z)) {
        xy_vals <- c(sapply(gamma[z], "[[", "obs"), y$obs)
        if (length(unique(xy_vals)) > 1) {
            return(y_cf)
        }
    }
    list()
}

# Determines the value of a counterfactual variable within a conjunction.
#
# @param x A counterfactual variable.
# @param gamma A counterfactual conjunction.
# @return An integer correspodning to the value assignment if present or `NULL`.
val <- function(x, gamma) {
    for (y in gamma) {
        if (identical(x$var, y$var) && identical(x$int, y$int)) {
            return(y$obs)
        }
    }
    NULL
}
