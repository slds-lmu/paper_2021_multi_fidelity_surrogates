
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the
hyperparameter response surface for several interesting machine learing
algorithms across several tasks.

## Overview

| instance    | space   | n\_dims | n\_targets | fidelity | n\_problems |    R.2 | status |
| :---------- | :------ | ------: | ---------: | :------- | ----------: | -----: | :----- |
| NB301       | Cat+Dep |      34 |          2 | epochs   |           1 | 0.9866 | ready  |
| LCBench     | Mix     |       7 |          6 | epoch    |          35 | 0.9820 | ready  |
| RBv2SVM     | Mix+Dep |       7 |          4 | NA       |          98 |     NA | \-     |
| RBv2rpart   | Mix     |       5 |          4 | NA       |          22 |     NA | \-     |
| RBv2aknn    | Mix     |       6 |          4 | NA       |          33 |     NA | \-     |
| RBv2glmnet  | Mix     |       3 |          4 | NA       |          56 |     NA | \-     |
| RBv2ranger  | Mix+Dep |       9 |          4 | NA       |         114 |     NA | \-     |
| RBv2xgboost | Mix+Dep |      14 |          4 | NA       |         119 |     NA | \-     |

Toy test functions:

| instance | space | n\_dims | n\_targets | fidelity | n\_problems |
| :------- | :---- | ------: | ---------: | :------- | ----------: |
| Branin   | Num   |       2 |          1 | fidelity |           1 |
| Shekel   | Num   |       4 |          1 | fidelity |           1 |

## NASBench-301

We first load the config:

``` r
library(checkmate)
library(paradox)
library(mfsurrogates)
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("nb301", workdir = workdir)
cfg$setup()  # automatic download of files; necessary if you didn't download manually
cfg
```

this config contains our `objective` which we can use to optimize.

``` r
library("bbotk")
library("data.table")
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 2L)
)
opt("random_search")$optimize(ins)
```

Since NASBench is only trained on `CIFAR10`, we do not need to set a
specific `task_id` here.

## LCBench

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("lcbench", workdir = workdir)
cfg$setup()
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
opt("random_search")$optimize(ins)
```

### Subsetting to a specific task

In practice, we need to subset to a `task_id` if we want to tune on a
specific task.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(task = "126025"),
  terminator = trm("evals", n_evals = 10L)
)
opt("random_search")$optimize(ins)
```

A list of available `task_id`s can be obtained from the `param_set`:

``` r
cfg$param_set$params$OpenML_task_id$levels
```

## Branin

We first load the config:

``` r
cfg = cfgs("branin")
#cfg$plot()  # or method = "rgl"
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

## Shekel

We first load the config:

``` r
cfg = cfgs("shekel")
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceSingleCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

## RandomBotv2 - svm

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_svm", workdir = workdir)
cfg$setup()
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

Example to select target variables and a task:

``` r
objective = cfg$get_objective(task = "3", target_variables = c("perf.mmce", "traintime"))
objective$codomain
objective$constants
```

## RandomBotv2 - rpart

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_rpart", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

## RandomBotv2 - aknn

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_aknn", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

## RandomBotv2 - glmnet

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_glmnet", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

## RandomBotv2 - ranger

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_ranger", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```

## RandomBotv2 - xgboost

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_xgboost", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
#opt("random_search")$optimize(ins)
```
