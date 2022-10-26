#' The `cfid` package
#'
#' Identification of Counterfactual Queries in Causal Models
#'
#' @details
#' This package provides tools necessary for identifying counterfactual
#' queries in causal models. Causal graphs, counterfactual variables,
#' and counterfactual conjunctions are defined via simple interfaces.
#'
#' # Counterfactuals
#'
#' In simple terms, counterfactual are statements involving multiple
#' conceptual "worlds" where the observed state of the worlds is different.
#' As an example, consider two variables, Y = "headache", and X = "aspirin".
#' A counterfactual statement could be "If I have a headache and did not take
#' aspirin, would I not have a headache, had I taken aspirin instead".
#' This statement involves two worlds: the real or "actual" world, where
#' aspirin was not taken, and a hypothetical world, where it was taken.
#' In more formal terms, this statement involves a counterfactual variable
#' \eqn{Y_x} that attains two different values in two different worlds, forming
#' a counterfactual conjunction: \eqn{y_x \wedge y'_{x'}}, where \eqn{y} and
#' \eqn{y'} are two different values of \eqn{Y}, and \eqn{x} and \eqn{x'} are
#' two different values of \eqn{X}.
#'
#' # Identifiability
#'
#' Pearl's ladder of causation consists of the associational,
#' interventional and counterfactual levels, with counterfactual being
#' the highest level. The goal of identification is to find a transformation
#' of a higher level query into a lower level one.
#' For the interventional case, this transformation is known as causal effect
#' identifiability, where interventional distributions are expressed in terms
#' of observational quantities. Tools for this type of identification are
#' readily available,
#' such as in the `causaleffect`, `dagitty`, `pcalg`, and `dosearch`
#' packages. Transformation from the highest counterfactual level, is more
#' difficult, both conceptually and computationally, since to reach the
#' observational level, we must first find a transformation of our
#' counterfactual query into a interventional query, and then transform this
#' yet again to observational. Also, there transformations may not always
#' exist, for example in the presence of latent unobserved confounders, meaning
#' that the queries are non-identifiable. This package deals with the first
#' transformation, i.e., expressing the counterfactual queries in terms of
#' interventional queries (and observational, when possible), as well as the
#' second one, transforming interventional distributions to observational
#' quantities.
#'
#' # Algorithms
#'
#' Identification is carried out in terms of \eqn{G} and \eqn{P_*} and where
#' \eqn{G} is a directed acyclic graph (DAG) depicting the causal model
#' in question (a causal graph for short), and
#' \eqn{P_*} is the set of all interventional distributions
#' in causal models inducing \eqn{G}. Identification is carried
#' out by the ID* and IDC* algorithms by Shpitser and Pearl (2008) which aim to
#' convenrt the input counterfactual probability into an expression which can
#' be represented solely in terms of interventional distributions. These
#' algorithms are sound and complete, meaning that their output is always
#' correct, and in the case of a non-identifiable counterfactual, one can
#' always construct a counterexample, witnessing non-identifiability.
#'
#' # Graphs
#'
#' The causal graph associated with the causal model is given via a simple
#' text-based interface, similar to `dagitty` package syntax. Directed edges
#' are given as `X -> Y`, and bidirected edges as `X <-> Y`, which is a
#' shorthand notation for latent confounders. For more details on graph
#' construction, see [cfid::dag()].
#'
#' # Counterfactual variables and conjunctions
#'
#' Counterfactual variables are defined by their name, value and the conceptual
#' world that they belong to. A world is defined by a unique set of actions
#' (interventions) via the do-operator (Pearl, 2009). We can define the two
#' counterfactual variables of the headache/aspirin example as follows:
#' ```
#' cf(var = "Y", obs = 0, sub = c(X = 0))
#' cf(var = "Y", obs = 1, sub = c(X = 1))
#' ```
#' Here, `var` defines the name of the variable, `obs` gives level the variable
#' is assigned to (not the actual value), and `sub` defines the vector of
#' interventions that define the counterfactual world. For more details,
#' see [cfid::counterfactual_variable()]. Counterfactual conjunctions on the
#' other hand, are simply counterfactual statements (variables) that are
#' observed at the same time. For more details, see
#' [cfid::counterfactual_conjunction()].
#'
#' For complete examples of identifiable counterfactual queries, see
#' [cfid::identifiable()], which is the main function of the package.
#'
#' @docType package
#' @name cfid-package
#' @references
#' Pearl, J. (1995) Causal diagrams for empirical research. *Biometrika*,
#' **82(4)**:669--688.
#'
#' Pearl, J. (2009) *Causality: Models, Reasoning, and Inference*. Cambridge
#' University Press, 2nd edition.
#'
#' Shpitser, I. and Pearl, J. (2007). What counterfactuals can be tested.
#' In *Proceedings of the 23rd Conference on Uncertainty*
#' *in Artificial Intelligence*, 352--359.
#'
#' Shpitser, I. and Pearl, J. (2008). Complete identification
#' methods for the causal hierarchy. *Journal of Machine Learning Research*,
#' **9(64)**:1941--1979.
#'
#' Makhlouf, K., Zhioua, S. and Palamidessi, C. (2021).
#' Survey on causal-based machine learning fairness notions.
#' *arXiv:2010.09553*
NULL
