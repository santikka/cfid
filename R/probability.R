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
#' @seealso [cfid::CounterfactualVariable]
#' @details For formatting options, see [cfid::format.Probability].
#' When formatted via `print` or `format`, the  arguments are
#' prioritized in the following order if conflicting definitions are given:
#' `val`, (`var`, `do`), (`sumset`, `summand`), `terms`,
#' (`numerator`, `denominator`)
#'
#' @return An object of class `Probability`, which is a list containing
#' all of the arguments of the constructor.
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

#' Format a `Probability` object representing \eqn{p(y|do(x))}.
#'
#' @param x An object of class `Probability`.
#' @param use_primes A logical value. If `TRUE` (the default), any value
#'     assignment of a counterfactual variable with `obs` will be formatted with
#'     as many primes in the superscript as the value of `obs`, e.g.,
#'     `obs = 0` outputs `"y"`, `obs = 1` outputs `"y'"`,
#'     `obs = 2` outputs `"y''"` and so forth. The alternative when `FALSE` is
#'     to simply denote the `obs` value via superscript directly as
#'     `"y^{(obs)}"`, where obs is evaluated.
#' @param use_do A logical value. If `TRUE`, the explicit do-operation is used
#'     to denote interventional probabilities (e.g., \eqn{p(y|do(x))}).
#'     If `FALSE` (the default), the subscript notation is used instead
#'     (e.g., \eqn{p_x(y})).
#'
#' @return An character representation of the `Probability` object
#' in LaTeX syntax.
#'
#' @references Makhlouf, K., Zhioua, S. and Palamidessi, C. (2021).
#'     Survey on causal-based machine learning fairness notions.
#'     *arXiv:2010.09553*
#'
#' @examples
#' # Example from Makhlouf, Zhioua and Palamidessi (2021)
#' g <- dag("C -> A -> Y; C -> Y")
#' v1 <- cf("Y", 0, c(A = 1))
#' v2 <- cf("A", 0)
#' c1 <- conj(v1)
#' c2 <- conj(v2)
#' p <- identifiable(g2, c1, c2)$prob
#'
#' # Default, using primes and subscript notation
#' format(p)
#'
#' # Without primes, no do-operator
#' format(p, use_primes = FALSE)
#'
#' # Primes, with do-operator
#' format(p, use_do = TRUE)
#'
#' # Without primes, with do-operator
#' format(p, use_primes = FALSE, use_do = TRUE)
#'
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
