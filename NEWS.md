# cfid 0.1.4

  * Fixed an issue related to fixed variables in the counterfactual graph.
  * Now uses lower case and snake case for classes.

# cfid 0.1.3

  * An identifiable conditional counterfactual of the form P(gamma)/P(gamma) will have value 1 instead of the formula.
  * Fixed an error when identifying conditional counterfactuals that had common counterfactual variables.
  * Inputs for `identifiable` are now checked more thorougly.
  * Added additional package tests.
  * Further refined documentation in general.
  * Fixed an interal indexing issue when no bidirected edges were present in a DAG.
  * Added documentation for `format.Probability` with examples.
  * Added citation to Shpitser and Pearl (2007). "What counterfactual can be tested".
  * The package now correctly lists dependency on R >= 4.1.0.
  * Added NEWS.

# cfid 0.1.2

  * Added package documentation.
  * Improved documentation of existing functions.
  * Added support for more graph export formats.

# cfid v0.1.0

  * Initial version.