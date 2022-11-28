
# cfid: An R Package for Identification of Counterfactual Queries in Causal Models

<!-- Badges -->

[![codecov](https://codecov.io/gh/santikka/cfid/branch/main/graph/badge.svg?token=13KFY7ULZ4)](https://codecov.io/gh/santikka/cfid)
[![R-CMD-check](https://github.com/santikka/cfid/workflows/R-CMD-check/badge.svg)](https://github.com/santikka/cfid/actions)

## Overview

Facilitates the identification of counterfactual queries in structural
causal models via the ID\* and IDC\* algorithms by Shpitser, I. and
Pearl, J. (2007, 2008) <https://arxiv.org/abs/1206.5294>,
<https://jmlr.org/papers/v9/shpitser08a.html>. Provides a simple
interface for defining causal graphs and counterfactual conjunctions.
Construction of parallel worlds graphs and counterfactual graphs is done
automatically based on the counterfactual query and the causal graph.

## Installation

Install the latest development version by using the devtools package:

``` r
# install.packages("devtools")
devtools::install_github("santikka/cfid")
```

## Graphs

Directed acyclic graphs (DAG) can be defined using the function `dag` in
a syntax similar to the
[`dagitty`](https://cran.r-project.org/package=dagitty) package. This
function accepts edges of the form `X -> Y`, `X <- Y`, and `X <-> Y`,
where the last variant is a shorthand for a latent confounder affecting
both `X` and `Y` (a so-called bidirected edge). Subgraphs can be defined
using curly braces `{...}`. Edges to and from subgraphs connect to all
vertices present in the subgraph. Subgraphs can also be nested. Some
examples of allowed constructs include:

``` r
dag("X -> {Y Z} <- W <-> G")
dag("{X, Y, Z} -> W")
dag("{X -> {Z -> {Y <-> W}}}")
```

These would define the following DAGs:

``` mermaid
flowchart LR;
  X((X))-->Y((Y));
  X((X))-->Z((Z));
  W((W))-->Z;
  W-->Y;
  W<-.->G((G));
```

``` mermaid
flowchart LR;
  X((X))-->W((W));
  Y((Y))-->W;
  Z((Z))-->W;
```

``` mermaid
flowchart LR;
  X((X))--->Z((Z));
  X-->Y((Y));
  X-->W((W));
  Z-->Y;
  Z-->W;
  Y<-.->W;
```

## Counterfactual variables and conjunctions

A counterfactual variable is defined by its name, value, and the
submodel that it originated from (a set of interventions). For example,
$y_x$ is a counterfactual variable named $Y$ with the value assignment
$y$ that originated from a submodel where the intervention $do(X = x)$
took place.

The function `counterfactual_variable` and its shorthand alias `cf` can
be used to construct counterfactual variables. This function takes three
arguments: `var`, `obs`, and `sub` that correspond to the variable name,
observed value assignment and subscript (the submodel). For example,
$y_x$ is defined as follows:

``` r
cf(var = "Y", obs = 0, sub = c(X = 0))
#> y_{x}
```

by default, the value 0 is the “default” or baseline level, and integer
values different from 0 are denoted by primes. For example $y'_x$ is a
similar counterfactual variable to $y_x$, except that it was observed to
take the value $y'$ instead of $y$ This can be accomplished by changing
the `obs` argument:

``` r
cf(var = "Y", obs = 1, sub = c(X = 0))
#> y'_{x}
```

Purely observational counterfactual variables (of the original causal
model) can be defined by omitting the `sub` argument.

Conjunctions of multiple counterfactual variables can be constructed
using the function `counterfactual_conjunction` or its shorthand alias
`conj`. This function simply takes an arbitrary number of
`"counterfacual_variable"` objects as its argument. For example, the
counterfactual conjunction $y \wedge y'_x$ can be defined as follows:

``` r
v1 <- cf("Y", 0)
v2 <- cf("Y", 1, c("X" = 0))
conj(v1, v2)
#> y /\ y'_{x}
```

## Identification

Identifiability of (conditional) counterfactual conjunctions can be
determined via the function `identifiable`. This function takes the
conjunction `gamma` to be identified from the set of all interventional
distributions $P_*$ of the causal model represented by the `"dag"`
object `g`. An optional conditioning conjunction `delta` can also be
provided. The solution is provided in LaTeX syntax if the query is
identifiable. For instance, we can consider the identifiability of
$P(y_x|x' \wedge z_d \wedge d)$ in the DAG shown below as follows:

``` mermaid
flowchart TB;
  X(X)-->W(W);
  W-->Y(Y);
  D(D)-->Z(Z);
  Z-->Y;
  X<-.->Y;
```

``` r
g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
v1 <- cf("Y", 0, c(X = 0))
v2 <- cf("X", 1)
v3 <- cf("Z", 0, c(D = 0))
v4 <- cf("D", 0)
c1 <- conj(v1)
c2 <- conj(v2, v3, v4)
identifiable(g = g1, gamma = c1, delta = c2)
#> The query P(y_{x}|x' /\ z_{d} /\ d) is identifiable from P_*.
#> Formula: \frac{\sum_{w} P_{x}(w)P_{w,z}(y,x')}{P(x')}
```

For more information and examples, please see the package documentation.

## Related packages

-   The
    [`causaleffect`](https://cran.r-project.org/package=causaleffect)
    package provides the ID and IDC algorithms for the identification of
    causal effects (among other algorithms).
-   The [`dosearch`](https://cran.r-project.org/package=dosearch)
    package provides a heuristic search algorithm that uses do-calculus
    to identify causal effects from an arbitrary combination of input
    distributions.
-   The [`dagitty`](https://cran.r-project.org/package=dagitty) provides
    various tools for causal modeling, such as finding adjustment sets
    and instrumental variables.
