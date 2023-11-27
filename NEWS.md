# cfid 0.1.7

  * Fixed some formulas having incorrect variables indicated as summation variables.

# cfid 0.1.6

  * Summation variables are now properly distinguished from query variables in the output formulas of `identifiable()`.
  * Inputs for `causal_effects()` are now validated.
  * Fixed a rare issue that could result in duplicate variables in the output distributions.
  * Fixed an issue where some identifiable but inconsistent queries were reported as nonidentifiable.

# cfid 0.1.5

  * Added missing value assignments to the query for `causal_effect()`.

# cfid 0.1.4

  * Fixed an issue related to fixed variables in the counterfactual graph.
  * Fixed an issue related to counterfactual graph generation and equivalence of random variables.
  * Now uses lower case and snake case for classes.
  * Added the ID and IDC algorithms for a full identification pipeline. These algorithms can be used directly via `causal_effect()`.
  * The syntax used by `dag()` is now more flexible, allowing edges within subgraph definitions and nested subgraphs.
  * Dropped dependency on R version 4.1.0.
  * Improved the package documentation.
  * Changed the default value of `var_sep` argument.

# cfid 0.1.3

  * An identifiable conditional counterfactual of the form P(gamma)/P(gamma) will have value 1 instead of the formula.
  * Fixed an error when identifying conditional counterfactuals that had common counterfactual variables.
  * Inputs for `identifiable()` are now checked more thoroughly.
  * Added additional package tests.
  * Further refined documentation in general.
  * Fixed an internal indexing issue when no bidirected edges were present in a DAG.
  * Added documentation for `format.probability()` with examples.
  * Added citation to Shpitser and Pearl (2007). "What counterfactuals can be tested".
  * The package now correctly lists dependency on R >= 4.1.0.
  * Added NEWS.

# cfid 0.1.2

  * Added package documentation.
  * Improved documentation of existing functions.
  * Added support for more graph export formats.

# cfid v0.1.0

  * Initial version.
