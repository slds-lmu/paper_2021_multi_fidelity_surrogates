---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the hyperparameter response surface for
several interesting machine learing algorithms across several tasks.



# Overview

This should perhaps have a table of implemented surrogates

## NASBench-301

We first load the config:


```r
library(checkmate)
library(paradox)
library(mfsurrogates)
cfg = BenchmarkConfigNB301$new(
  workdir = paste0(path.expand("~"), "/LRZ Sync+Share/multifidelity_data")
)
#> Error in eval(expr, envir, enclos): object 'BenchmarkConfigNB301' not found
#cfg$setup()
```

this config contains our `objective` which we can use to optimize.


```r
library("bbotk")
library("data.table")
#> data.table 1.14.0 using 2 threads (see ?getDTthreads).  Latest news: r-datatable.com
ins = OptimInstanceMultiCrit$new(
  objective = cfg$objective,
  terminator = trm("evals", n_evals = 10L)
 )
#> Error in checkR6(x, classes, ordered, cloneable, public, private, null.ok): object 'cfg' not found
# opt('random_search')$optimize(ins)
```
