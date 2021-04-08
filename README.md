
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the
hyperparameter response surface for several interesting machine learing
algorithms across several tasks.

| instance    | space   | dims | fidelity | n\_problems |
|:------------|:--------|-----:|:---------|------------:|
| NASBench301 | Cat+Dep |   35 | epochs   |           1 |
| RBV2-SVM    | Mix+Dep |    7 | NA       |         100 |

Toy test functions:

| instance | space | dims | fidelity | n\_problems |
|:---------|:------|-----:|:---------|------------:|
| Branin   | Num   |    2 | fidelity |           1 |

# Overview

This should perhaps have a table of implemented surrogates

## NASBench-301

We first load the config:

``` r
library(checkmate)
library(paradox)
library(mfsurrogates)
cfg = BenchmarkConfigNB301$new(
  workdir = "/tmp/"
)
cfg$setup()
#> setup sucessful.
```

this config contains our `objective` which we can use to optimize.

``` r
library("bbotk")
library("data.table")
ins = OptimInstanceMultiCrit$new(
  objective = cfg$objective,
  terminator = trm("evals", n_evals = 10L)
 )
# opt('random_search')$optimize(ins)
```

## Branin

We first load the config:

``` r
library(checkmate)
library(paradox)
library(mfsurrogates)
cfg = BenchmarkConfigBranin$new()
#plot(cfg, method = "rgl") # or ggplot2
```

this config contains our `objective` which we can use to optimize.

``` r
library("bbotk")
library("data.table")
ins = OptimInstanceSingleCrit$new(
  objective = cfg$objective,
  terminator = trm("evals", n_evals = 10L)
 )
# opt('random_search')$optimize(ins)
```

## RandomBotv2 - SVM

We first load the config:

``` r
library(mfsurrogates)
cfg = BenchmarkConfigRBv2SVM$new(
  workdir = paste0(path.expand("~"), "/LRZ Sync+Share/multifidelity_data")
)
```

this config contains our `objective` which we can use to optimize.

``` r
library("bbotk")
library("data.table")
ins = OptimInstanceMultiCrit$new(
  objective = cfg$objective,
  terminator = trm("evals", n_evals = 10L)
)
# opt('random_search')$optimize(ins)
```
