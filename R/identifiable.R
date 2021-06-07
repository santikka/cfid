#' Identify a Counterfactual Query
#'
#' Determine the identifiability of a (conditional) counterfactual conjunction.
#'
#' @param g A `DAG` object describing the causal graph
#'     (or an object that can be coerced, see [cfid::import_graph()].
#' @param gamma An object of class `CounterfactualConjunction`
#'     representing the counterfactual causal query.
#' @param delta An object of class `CounterfactualConjunction`
#'     representing the conditioning conjunction (optional).
#'
#' @seealso [cfid::dag()], [cfid::CounterfactualVariable()],
#'     [cfid::CounterfactualConjunction()]
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
#' but sharing the latent variables. From this graph, a counterfactual graph
#' is derived, where each variable is unique, which might not be the case
#' in a parallel worlds graph.
#'
#' Finally, the ID* (or IDC*) algorithm is applied to determine identifiability
#' of the query. Similar to the ID and IDC algorithms for causal effects,
#' these algorithms exploit the so called c-component factorization to split
#' the query into smaller subproblems, which are then solved recursively.
#'
#' @references
#' Shpister, I. and Pearl, J. (2006). Identification of joint interventional
#' distributions in semi-Markovian causal models.
#' In *21st National Conference on Artificial Intelligence*
#'
#' Shpitser, I. and Pearl, J. (2008). Complete identification
#' methods for the causal hierarchy. *Journal of Machine Learning Research*,
#' **9(64)**:1941--1979.
#'
#' @return A list containing one or more of the following:
#' * `id` A logical value that is `TRUE` if the query is identifiable and
#'     `FALSE` otherwise. Note that in cases where `gamma` is itself
#'     inconsistent, the query will be identifiable, but with probability 0.
#' * `prob` An object of class `Probability` giving the
#'     formula of the query in latex syntax, if identifiable. This expression
#'     is given in terms of \eqn{P*},
#'     the set of all interventional distributions over `g`. For tautological
#'     statements, the resulting probability is 1, and for inconsistent
#'     statements, the resulting probability is 0.
#' * `undefined` A logical value that is `TRUE` if
#'     a conditional conjunction \eqn{p(\gamma|\delta)} is undefined,
#'     for example when \eqn{p(\delta) = 0}, and `FALSE` otherwise.
#'
#' @examples
#' # The example that appears in Shpitser and Pearl (2008)
#' g <- dag("X -> W -> Y <- Z <- D X <-> Y")
#' v1 <- cf("Y", 0, c(X = 0))
#' v2 <- cf("X", 1)
#' v3 <- cf("Z", 0, c(D = 0))
#' v4 <- cf("D", 0)
#' c1 <- conj(v1)
#' c2 <- conj(v2, v3, v4)
#' identifiable(g, c1, c2)
#' @export
identifiable <- function(g, gamma, delta = NULL) {
    if (missing(g)) {
        stop_("Argument 'g' is missing")
    } else if (!is_dag(g)) {
        stop_("Argument 'g' must be an object of class 'DAG'")
    }
    if (missing(gamma)) {
        stop_("Argument 'gamma' is missing")
    } else if (!is.CounterfactualConjunction(gamma)) {
        if (is.CounterfactualVariable(gamma)) {
            gamma <- CounterfactualConjunction(gamma)
        } else {
            stop_("Argument 'gamma' must be an object of class 'CounterfactualConjunction'")
        }
    }
    if (is.null(delta)) {
        out <- id_star(g, gamma)
    } else {
        if (!is.CounterfactualConjunction(delta)) {
            stop_("Argument 'delta' must be an object of class 'CounterfactualConjunction")
        }
        out <- idc_star(g, gamma, delta)
    }
    out
}
