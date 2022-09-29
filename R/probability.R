#' Symbolic Probability Distributions
#'
#' Defines an interventional or observational probability \eqn{p(y|do(x))}.
#'
#' @param val An `integer` value of either 0 or 1 for almost sure events.
#' @param var A `list` of objects of class `counterfactual_variable`
#'     (without interventions and with value assignments).
#'     `var` defines the observations \eqn{y} in \eqn{p(y|...)}.
#' @param do A `list` of objects of class `counterfactual_variable`
#'     (without interventions and with value assignments).
#'     If an interventional probability is defined, these depict the \eqn{do(.)}
#'     variables.
#' @param sumset A `list` of objects of class `counterfactual_variable`
#'     (without interventions and with value assignments).
#'     If the probability depicts marginalization, `sumset`
#'     defines the set of variable to be marginalized over.
#' @param summand A `probability` object. If `sumset`
#'     is not `NULL`, this defines the probability being marginalized.
#' @param terms A `list` of `probability` objects if the object in question
#'     is meant to represent a product of terms.
#' @param numerator A`probability` object. If the probability
#'     depicts a conditional probability that cannot be expressed simply
#'     in terms of the set of inputs \eqn{P*}, this is the numerator
#'     of the quotient representation.
#' @param denominator A `probability` object. The denominator
#'     of the quotient representation.
#'
#' @seealso [cfid::counterfactual_variable]
#' @details For formatting options, see [cfid::format.probability].
#' When formatted via `print` or `format`, the  arguments are
#' prioritized in the following order if conflicting definitions are given:
#' `val`, (`var`, `do`), (`sumset`, `summand`), `terms`,
#' (`numerator`, `denominator`)
#'
#' @return An object of class `probability`, which is a `list` containing
#' all of the arguments of the constructor.
#'
#' @export
probability <- function(val = NULL, var = NULL, do = NULL,
                        sumset = NULL, summand = NULL, terms = NULL,
                        numerator = NULL, denominator = NULL) {
  if (!is.null(val)) {
    val <- try_type(val = val, type = "integer")[1]
  }
  if (!is.null(var)) {
    if (!all(vapply(var, is.counterfactual_variable, logical(1L)))) {
      stop_("All elements of `var` must be `counterfactual_variable` objects.")
    }
  }
  if (!is.null(do)) {
    if (!all(vapply(do, is.counterfactual_variable, logical(1L)))) {
      stop_("All elements of `do` must be `counterfactual_variable` objects.")
    }
  }
  if (!is.null(sumset)) {
    if (!all(vapply(sumset, is.counterfactual_variable, logical(1L)))) {
      stop_(
        "All elements of `sumset` must be `counterfactual_variable` objects."
      )
    }
  }
  if (!is.null(summand)) {
    if (!is.probability(summand)) {
      stop_("Argument `summand` must be a `probability` object.")
    }
  }
  if (!is.null(numerator)) {
    if (!is.probability(numerator)) {
      stop_("Argument `numerator` must be a `probability` object.")
    }
  }
  if (!is.null(denominator)) {
    if (!is.probability(denominator)) {
      stop_("Argument `denominator` must be a `probability` object.")
    }
  }
  structure(
    list(
      val = val,
      var = var,
      do = do,
      sumset = sumset,
      summand = summand,
      terms = terms,
      numerator = numerator,
      denominator = denominator
    ),
    class = "probability"
  )
}

is.probability <- function(x) {
  inherits(x, "probability")
}

#' Format a `probability` object representing \eqn{p(y|do(x))}.
#'
#' @param x A `probability` object.
#' @param use_primes A `logical` value. If `TRUE` (the default), any value
#'   assignment of a counterfactual variable with `obs` will be formatted with
#'   as many primes in the superscript as the value of `obs`, e.g.,
#'   `obs = 0` outputs `"y"`, `obs = 1` outputs `"y'"`,
#'   `obs = 2` outputs `"y''"` and so forth. The alternative when `FALSE` is
#'   to simply denote the `obs` value via superscript directly as
#'   `"y^{(obs)}"`, where obs is evaluated.
#' @param use_do A `logical` value. If `TRUE`, the explicit do-operation is used
#'   to denote interventional probabilities (e.g., \eqn{p(y|do(x))}).
#'   If `FALSE` (the default), the subscript notation is used instead
#'   (e.g., \eqn{p_x(y})).
#' @param ... Additional arguments passed to `format`.
#' @return An character representation of the `probability` object
#'   in LaTeX syntax.
#' @references Makhlouf, K., Zhioua, S. and Palamidessi, C. (2021).
#'     Survey on causal-based machine learning fairness notions.
#'     *arXiv:2010.09553*
#' @examples
#' # Example from Makhlouf, Zhioua and Palamidessi (2021)
#' g2 <- dag("C -> A -> Y; C -> Y")
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
format.probability <- function(x, use_primes = TRUE, use_do = FALSE, ...) {
  out <- ""
  if (length(x$val) > 0L) {
    out <- as.character(x$val)
  } else if (!is.null(x$sumset)) {
    form_sumset <- comma_sep(
      vapply(x$sumset, format, character(1L), use_primes)
    )
    out <- paste0(
      "\\sum_{",
      form_sumset,
      "} ",
      format(x$summand, use_primes, use_do)
    )
  } else if (!is.null(x$terms)) {
    out <- collapse(
      vapply(x$terms, format, character(1L), use_primes, use_do)
    )
  } else if (!is.null(x$numerator)) {
    if (length(x$denominator$val) > 0L && x$denominator$val == 1L) {
      out <- format(x$numerator, use_primes, use_do)
    } else {
      out <- paste0(
        "\\frac{",
        format(x$numerator, use_primes, use_do),
        "}{",
        format(x$denominator, use_primes, use_do),
        "}"
      )
    }
  } else {
    sub <- ""
    cond <- ""
    if (length(x$do) > 0L) {
      form_do <- comma_sep(vapply(x$do, format, character(1L), use_primes))
      if (!use_do) {
        sub <- paste0("_{", form_do, "}")
      } else {
        cond <- paste0("|do(", form_do, ")")
      }
    }
    form_var <- comma_sep(vapply(x$var, format, character(1L), use_primes))
    out <- paste0("p", sub, "(", form_var, cond, ")")
  }
  out
}

#' @export
print.probability <- function(x, ...) {
  cat(format(x, ...), "\n")
}
