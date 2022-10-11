# cfid 0.1.4

  * Fixed an issue related to fixed variables in the counterfactual graph.
  * Now uses lower case and snake case for classes.
  * Added `id` and `idc` algorithms for full identification pipeline. These algorithms can also be used directly via `causal_effect`.
  * Improved the package documentation.

# cfid 0.1.3

  * An identifiable conditional counterfactual of the form P(gamma)/P(gamma) will have value 1 instead of the formula.
  * Fixed an error when identifying conditional counterfactuals that had common counterfactual variables.
  * Inputs for `identifiable` are now checked more thoroughly.
  * Added additional package tests.
  * Further refined documentation in general.
  * Fixed an internal indexing issue when no bidirected edges were present in a DAG.
  * Added documentation for `format.probability` with examples.
  * Added citation to Shpitser and Pearl (2007). "What counterfactuals can be tested".
  * The package now correctly lists dependency on R >= 4.1.0.
  * Added NEWS.

# cfid 0.1.2

  * Added package documentation.
  * Improved documentation of existing functions.
  * Added support for more graph export formats.

# cfid v0.1.0

  * Initial version.
