#' Identify a Counterfactual Query
#'
#' Determine the identifiability of a (conditional) counterfactual conjunction.
#'
#' To identify a non-conditional conjunction \eqn{p(\gamma)}, the argument
#' `delta` should be `NULL`.
#'
#' To identify a conditional conjunction \eqn{p(\gamma|\delta)}, both `gamma`
#' and `delta` should be specified.
#'
#' First, a parallel worlds graph is constructed based on the query. In a
#' parallel worlds graph, for each \eqn{do}-action that appears in \eqn{\gamma}
#' (and \eqn{\delta}) a copy of the original graph is created with the new
#' observational variables attaining their post-interventional values
#' but sharing the latent variables. This graph is known as a parallel worlds
#' graph. From the parallel worlds graph, a counterfactual graph
#' is derived such that each variable is unique, which might not be the case
#' in a parallel worlds graph.
#'
#' Finally, the ID* (or IDC*) algorithm is applied to determine identifiability
#' of the query. Similar to the ID and IDC algorithms for causal effects,
#' these algorithms exploit the so called c-component factorization to split
#' the query into smaller subproblems, which are then solved recursively.
#' If argument `data` is `"observations"` or `"both"`, identification of
#' interventional probabilities in the resulting functional is further
#' attempted in terms of the joint probability distribution by using the
#' ID and IDC algorithms (see [cfid::causal_effect]).
#'
#' @param g A `dag` object describing the causal graph
#' (to obtain a `dag` from another format, see [cfid::import_graph()].
#' @param gamma An R object that can be coerced into a
#' `counterfactual_conjunction` object that represents the
#' counterfactual causal query.
#' @param delta An R object that can be coerced into a
#' `counterfactual_conjunction` object that represents the conditioning
#' conjunction (optional).
#' @param data A `character` string that accepts one of the following:
#' `"interventions"` (the default), `"observations"`or `"both"`. This argument
#' defines the target level of identification. If `"interventions"` is used,
#' the identification is attempted down to the intervention level. If
#' `"observations"` is used, identification is attempted down to the
#' observational level. If `"both"` is used, identification is carried out
#' for each term to the lowest level where the term is still identifiable.
#'
#' @seealso [cfid::dag()], [cfid::counterfactual_variable()],
#' [cfid::probability()], [cfid::functional()]
#'
#' @return An object of class `query` which is a `list` containing
#' one or more of the following:
#'
#' * `id`\cr A `logical` value that is `TRUE` if the query is identifiable and
#' `FALSE` otherwise from the available `data` in `g`.
#' Note that in cases where `gamma` itself is
#' inconsistent, the query will be identifiable, but with probability 0.
#' * `formula`\cr An object of class `functional` giving the identifying
#' functional of the query in LaTeX syntax via `format` or `print`,
#' if identifiable. This expression is given in terms of the
#' available `data`. For tautological statements, the resulting
#' probability is 1, and for inconsistent statements, the resulting
#' probability is 0. For formatting options, see
#' [cfid::format.functional()] and [cfid::format.probability()].
#' * `undefined`\cr A logical value that is `TRUE` if
#' a conditional conjunction \eqn{p(\gamma|\delta)} is undefined,
#' for example when \eqn{p(\delta) = 0}, and `FALSE` otherwise.
#' * `gamma`\cr The original counterfactual conjunction..
#' * `delta`\cr The original conditioning counterfactual conjunction.
#' * `data`\cr The original data.
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
#'
#' @export
identifiable <- function(g, gamma, delta = NULL,
                         data = c("interventions", "observations", "both")) {
  stopifnot_(
    !missing(g),
    "Argument `g` is missing."
  )
  stopifnot_(
    is.dag(g),
    "Argument `g` must be a `dag` object."
  )
  stopifnot_(
    !missing(gamma),
    "Argument `gamma` is missing."
  )
  gamma <- try(as.counterfactual_conjunction(gamma), silent = TRUE)
  stopifnot_(
    !inherits(gamma, "try-error"),
    "Unable to coerce `gamma` into a `counterfactual_conjunction` object."
  )
  stopifnot_(
    all(assigned(gamma)),
    paste0(
      "Argument `gamma` contains counterfactual variables ",
      "without a value assignment."
    )
  )
  if (!is.null(delta)) {
    delta <- try(as.counterfactual_conjunction(delta), silent = TRUE)
    stopifnot_(
      !inherits(delta, "try-error"),
      "Unable to coerce `delta` into a `counterfactual_conjunction` object."
    )
    stopifnot_(
      all(assigned(delta)),
      paste0(
        "Argument `delta` contains counterfactual variables ",
        "without a value assignment."
      )
    )
  }
  data <- try(match.arg(data, c("interventions", "observations", "both")))
  stopifnot_(
    !inherits(data, "try-error"),
    'Argument `data` must be either "interventions", "observations" or "both".'
  )
  out <- idc_star(g, gamma, delta)
  out$formula <- ifelse_(
    is.probability(out$formula),
    functional(terms = list(out$formula)),
    out$formula
  )
  if (out$id && data != "interventions") {
    out <- identify_terms(out$formula, data, g)
  }
  out$undefined <- ifelse_(is.null(out$undefined), FALSE, out$undefined)
  out$counterfactual <- TRUE
  out$gamma <- gamma
  out$delta <- delta
  out$data <- data
  structure(
    out,
    class = "query"
  )
}

#' Query Objects
#'
#' Objects of class `query` describe the output of `identifiable` and
#' `causal_effect`. They are `list` objects with a custom `print` method and
#' contain data related to the identifiability results. See
#' [cfid::identifiable] and [cfid::causal_effect] for details.
#'
#' @name query
NULL

#' @rdname query
#' @method print query
#' @param x A `query` object
#' @param ... Not used.
#' @export
print.query <- function(x, ...) {
  if (x$counterfactual) {
    delta_str <- ifelse_(
      is.null(x$delta),
      "",
      paste0("|", format(x$delta))
    )
    query_str <- paste0("p(", format(x$gamma), delta_str, ")")
    cat("The query", query_str)
  } else {
    cat("The query", format(x$causaleffect))
  }
  id_str <- ifelse_(x$id, "identifiable", "not identifiable")
  data_str <- switch(
    x$data,
    both = "(P_*, p(v)).",
    observations = "p(v).",
    interventions = "P_*."
  )
  cat(" is", id_str, "from", data_str)
  if (x$undefined) {
    cat("\nThe query is undefined.")
  }
  if (x$id) {
    cat("\nFormula:", format(x$formula))
  }
}

#' Attempt to Identify Terms from the Output of IDC*
#'
#' @param x A `functional` or a `probability` object.
#' @param data Either `"observations"` or `"both"`
#' @param g A `dag` object
#' @noRd
identify_terms <- function(x, data, g) {
  n_terms <- length(x$terms)
  out <- NULL
  if (n_terms > 0) {
    terms <- vector(mode = "list", length = n_terms)
    for (i in seq_len(n_terms)) {
      terms[[i]] <- identify_terms(x$terms[[i]], data, g)
      if (!terms[[i]]$id) {
        if (data == "observations") {
          return(list(id = FALSE, formula = NULL))
        }
        terms[[i]] <- list(
          id = TRUE,
          formula = x$terms[[i]]
        )
      }
    }
    formulas <- lapply(terms, "[[", "formula")
    out <- list(
      id = TRUE,
      formula = functional(sumset = x$sumset, terms = formulas)
    )
  } else if (!is.null(x$numerator)) {
    n <- identify_terms(x$numerator, data, g)
    if (!n$id) {
      if (data == "observations") {
        return(list(id = FALSE, formula = NULL))
      }
    }
    # Denominator must be identifiable if the numerator is
    d <- identify_terms(x$denominator, data, g)
    out <- list(
      id = TRUE,
      formula = functional(
        sumset = x$sumset,
        numerator = n$formula,
        denominator = d$formula
      )
    )
  } else {
    do <- vars(x$do)
    if (length(do) > 0L) {
      var <- vars(x$var)
      cond <- vars(x$cond)
      v <- unlist(evs(c(x$var, x$do, x$cond)))
      out <- causal_effect(g = g, y = var, x = do, z = cond, v = v)
    } else {
      out <- list(id = TRUE, formula = x)
    }
  }
  out
}