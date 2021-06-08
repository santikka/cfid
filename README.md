# cfid: An R Package for Identification of Counterfactual Queries in Causal Models

<!-- Badges -->
[![codecov](https://codecov.io/gh/santikka/cfid/branch/main/graph/badge.svg?token=13KFY7ULZ4)](https://codecov.io/gh/santikka/cfid)
  
## Overview

Facilitates the identification of counterfactual queries in structural causal 
models via the ID* and IDC* algorithms by Shpitser, I. and Pearl, J. (2008) 
<http://jmlr.org/papers/v9/shpitser08a.html>. Provides a simple interface for 
defining causal graphs and counterfactual conjunctions. Construction of parallel
worlds graphs and counterfactual graphs is done automatically based on the 
counterfactual query and the causal graph.

## Installation
Install the latest development version by using the devtools package:
```R
# install.packages("devtools")
devtools::install_github("santikka/cfid")
```
