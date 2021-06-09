#' Probability
#'
#' Defines an interventional or observational probability \eqn{p(y|do(x))}.
#'
#' @param val An integer value of either 0 or 1 for almost sure events.
#' @param var A list of objects of class `CounterfactualVariable`
#'     (without interventions and with value assignments).
#'     `var` defines the observations \eqn{y} in \eqn{p(y|...)}.
#' @param do A list of objects of class `CounterfactualVariable`
#'     (without interventions and with value assignments).
#'     If an interventional probability is defined, these depict the \eqn{do(.)}
#'     variables.
#' @param sumset A list of objects of class `CounterfactualVariable`
#'     (without interventions and with value assignments).
#'     If the probability depicts marginalization, `sumset`
#'     defines the set of variable to be marginalized over.
#' @param summand An object of class `Probability`. If `sumset`
#'     is not `NULL`, this defines the probability being marginalized.
#' @param terms A list of `Probability` objects if the object in question
#'     is meant to represent a product of terms.
#' @param numerator An object of class `Probability`. If the probability
#'     depicts a conditional probability that cannot be expressed simply
#'     in terms of the set of inputs \eqn{P*}, this is the numerator
#'     of the quotient representation.
#' @param denominator An object of class `Probability`. The denominator
#'     of the quotient representation.
#'
#' @details
#' When formatted via `print` or `format`, the  arguments are
#' prioritized in the following order if conflicting definitions are given:
#' `val`, (`var`, `do`), (`sumset`, `summand`), `terms`,
#' (`numerator`, `denominator`)
#'
#' @return An object of class `Probability`.
#'
#' @export
Probability <- function(val = NULL, var = NULL, do = NULL,
    sumset = NULL, summand = NULL, terms = NULL,
    numerator = NULL, denominator = NULL)
{
    if (!is.null(val)) {
        val <- try_type(val = val, type = "integer")[1]
    }
    if (!is.null(var)) {
        if (!all(sapply(var, is.CounterfactualVariable))) {
            stop_("All elements of ?var` must be of class `CounterfactualVariable`")
        }
    }
    if (!is.null(do)) {
        if (!all(sapply(do, is.CounterfactualVariable))) {
            stop_("All elements of `do` must be of class `CounterfactualVariable`")
        }
    }
    if (!is.null(sumset)) {
        if (!all(sapply(sumset, is.CounterfactualVariable))) {
            stop_("All elements of `sumset` must be of class `CounterfactualVariable`")
        }
    }
    if (!is.null(summand)) {
        if (!is.Probability(summand)) {
            stop_("Argument `summand` must be of class `Probability`")
        }
    }
    if (!is.null(numerator)) {
        if (!is.Probability(numerator)) {
            stop_("Argument `numerator` must be of class `Probability`")
        }
    }
    if (!is.null(denominator)) {
        if (!is.Probability(denominator)) {
            stop_("Argument `denominator` must be of class `Probability`")
        }
    }
    structure(
        list(val = val,
             var = var,
             do = do,
             sumset = sumset,
             summand = summand,
             terms = terms,
             numerator = numerator,
             denominator = denominator),
        class = "Probability"
    )
}

is.Probability <- function(x) {
    inherits(x, "Probability")
}

#' @export
format.Probability <- function(x, use_primes = TRUE, use_do = FALSE, ...) {
    out <- ""
    if (length(x$val)) {
        out <- as.character(x$val)
    } else if (!is.null(x$sumset)) {
        form_sumset <- comma_sep(sapply(x$sumset, format, use_primes))
        out <- paste0("\\sum_{", form_sumset, "} ",
                      format(x$summand, use_primes, use_do))
    } else if (!is.null(x$terms)) {
        out <- collapse(sapply(x$terms, format, use_primes, use_do))
    } else if (!is.null(x$numerator)) {
        if (length(x$denominator$val) && x$denominator$val == 1L) {
            out <- format(x$numerator, use_primes, use_do)
        } else {
            out <- paste0("\\frac{", format(x$numerator, use_primes, use_do),
                          "}{", format(x$denominator, use_primes, use_do), "}")
        }
    } else {
        sub <- ""
        cond <- ""
        if (length(x$do)) {
            form_do <- comma_sep(sapply(x$do, format, use_primes))
            if (!use_do) {
                sub <- paste0("_{", form_do, "}")
            } else {
                cond <- paste0("|do(", form_do, ")")
            }
        }
        form_var <- comma_sep(sapply(x$var, format, use_primes))
        out <- paste0("p", sub, "(", form_var, cond, ")")
    }
    out
}

#' @export
print.Probability <- function(x, ...) {
    cat(format(x, ...), "\n")
}
