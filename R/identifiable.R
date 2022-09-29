#' Identify a Counterfactual Query
#'
#' Determine the identifiability of a (conditional) counterfactual conjunction.
#'
#' @param g A `DAG` object describing the causal graph
#'     (to obtain a `DAG` from another format, see [cfid::import_graph].
#' @param gamma A `counterfactual_conjunction` object
#'     representing the counterfactual causal query.
#' @param delta A `counterfactual_conjunction` object
#'     representing the conditioning conjunction (optional).
#'
#' @seealso [cfid::dag], [cfid::counterfactual_variable],
#'     [cfid::counterfactual_conjunction], [cfid::probability]
#'
#' @details
#' To identify a non-conditional conjunction \eqn{p(\gamma)}, the argument
#' `delta` should be `NULL`.
#'
#' To identify a conditional conjunction \eqn{p(\gamma|\delta)}, both `gamma`
#' and `delta` should be specified.
#'
#' First, a parallel worlds graph is constructed based on the query. In a
#' parallel worlds graph, for each \eqn{do}-action that appears in \eqn{gamma}
#' (and \eqn{delta}) a copy of the original graph is created with the new
#' observational variables attaining their post-interventional values
#' but sharing the latent variables.
#' This graph is known as a parallel worlds graph.
#' From the parallel worlds graph, a counterfactual graph
#' is derived such that each variable is unique, which might not be the case
#' in a parallel worlds graph.
#'
#' Finally, the ID* (or IDC*) algorithm is applied to determine identifiability
#' of the query. Similar to the ID and IDC algorithms for causal effects,
#' these algorithms exploit the so called c-component factorization to split
#' the query into smaller subproblems, which are then solved recursively.
#'
#' @references
#' Shpitser, I. and Pearl, J. (2006). Identification of joint interventional
#' distributions in semi-Markovian causal models.
#' In *21st National Conference on Artificial Intelligence*.
#'
#' Shpitser, I. and Pearl, J. (2007). What counterfactuals can be tested.
#' In *Proceedings of the 23rd Conference on Uncertainty*
#' *in Artificial Intelligence*, 352--359.
#'
#' Shpitser, I. and Pearl, J. (2008). Complete identification
#' methods for the causal hierarchy. *Journal of Machine Learning Research*,
#' **9(64)**:1941--1979.
#'
#' @return A list containing one or more of the following:
#'
#' * `id` A logical value that is `TRUE` if the query is identifiable and
#'     `FALSE` otherwise. Note that in cases where `gamma` is itself
#'     inconsistent, the query will be identifiable, but with probability 0.
#' * `prob` An object of class `probability` giving the formula of the query in
#'     LaTeX syntax via format or print, if identifiable.
#'     This expression is given in terms of \eqn{P_*},
#'     the set of all interventional distributions over `g`. For tautological
#'     statements, the resulting probability is 1, and for inconsistent
#'     statements, the resulting probability is 0. For formatting options,
#'     see [cfid::format.probability].
#' * `undefined` A logical value that is `TRUE` if
#'     a conditional conjunction \eqn{p(\gamma|\delta)} is undefined,
#'     for example when \eqn{p(\delta) = 0}, and `FALSE` otherwise.
#'
#' @examples
#' # Examples that appears in Shpitser and Pearl (2008)
#' g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
#' g2 <- dag("X -> W -> Y <- Z <- D X <-> Y X -> Y")
#' v1 <- cf("Y", 0, c(X = 0))
#' v2 <- cf("X", 1)
#' v3 <- cf("Z", 0, c(D = 0))
#' v4 <- cf("D", 0)
#' c1 <- conj(v1)
#' c2 <- conj(v2, v3, v4)
#' c3 <- conj(v1, v2, v3, v4)
#'
#' # Identifiable conditional conjunction
#' identifiable(g1, c1, c2)
#'
#' # Identifiable conjunction
#' identifiable(g1, c3)
#'
#' # Non-identifiable conjunction
#' identifiable(g2, c3)
#' @export
identifiable <- function(g, gamma, delta = NULL) {
  if (missing(g)) {
    stop_("Argument `g` is missing.")
  } else if (!is_dag(g)) {
    stop_("Argument `g` must be a `dag` object.")
  }
  if (missing(gamma)) {
    stop_("Argument `gamma` is missing.")
  } else if (!is.counterfactual_conjunction(gamma)) {
    if (is.counterfactual_variable(gamma)) {
      if (length(gamma$obs) == 0L) {
        stop_(
          "Argument `gamma` contains counterfactual variables ",
          "without a value assignment"
        )
      }
      gamma <- counterfactual_conjunction(gamma)
    } else {
      stop_("Argument `gamma` must be a `counterfactual_conjunction` object.")
    }
  }
  if (any(!assigned(gamma))) {
    stop_(
      "Argument `gamma` contains counterfactual variables ",
      "without a value assignment."
    )
  }
  if (is.null(delta)) {
    out <- id_star(g, gamma)
  } else {
    if (!is.counterfactual_conjunction(delta)) {
      if (is.counterfactual_variable(delta)) {
        if (length(delta$obs) == 0L) {
          stop_(
            "Argument `delta` contains counterfactual variables ",
            "without a value assignment."
          )
        }
        delta <- counterfactual_conjunction(delta)
      } else {
        stop_("Argument `delta` must be a `counterfactual_conjunction` object.")
      }
    }
    if (any(!assigned(delta))) {
      stop_(
        "Argument `delta` contains counterfactual variables ",
        "without a value assignment"
      )
    }
    out <- idc_star(g, gamma, delta)
  }
  if (is.null(out$undefined)) {
    out$undefined <- FALSE
  }
  out
}
