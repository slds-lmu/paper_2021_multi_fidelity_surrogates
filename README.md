
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the
hyperparameter response surface for several interesting machine learing
algorithms across several tasks.

## Overview

| instance      | space   | n\_dims | n\_targets       | fidelity     | n\_problems | status | Rsq       | Rho        |
|:--------------|:--------|--------:|:-----------------|:-------------|------------:|:-------|:----------|:-----------|
| nb301         | Cat+Dep |      34 | 2:perf(1)+rt     | epochs       |           1 | ready  | 0.6-0.99  | 0.91-0.99  |
| lcbench       | Mix     |       7 | 6:perf(5)+rt     | epochs       |          35 | ready  | 0.95-1    | 0.94-1     |
| rbv2\_ranger  | Mix+Dep |       6 | 6:perf(4)+rt+pt  | frac+repls   |          96 | ready  | 0.4-0.98  | 0.64-0.99  |
| rbv2\_rpart   | Mix     |       5 | 6:perf(4)+rt+pt  | frac+repls   |         101 | ready  | 0-0.99    | 0.02-1     |
| rbv2\_aknn    | Mix     |       6 | 6:perf(4)+rt+pt  | frac+repls   |          99 | ready  | 0.4-0.97  | 0.63-0.98  |
| rbv2\_glmnet  | Mix     |       3 | 6:perf(4)+rt+pt  | frac+repls   |          98 | ready  | 0.02-0.98 | -0.15-0.99 |
| rbv2\_ranger  | Mix+Dep |       8 | 6:perf(4)+rt+pt  | frac+repls   |         114 | ready  | 0.4-0.98  | 0.64-0.99  |
| rbv2\_xgboost | Mix+Dep |      14 | 6:perf(4)+rt+pt  | frac+repls   |         109 | ready  | 0.53-0.98 | 0.72-0.99  |
| rbv2\_super   | Mix+Dep |      34 | 6:perf(4)+rt+pt  | frac+repls   |          89 | ready  | 0.65-0.97 | 0.81-0.99  |
| fcnet         | Mix     |      11 | 4:perf(2)+rt+ ms | epochs+repls |           4 | ready  | 0.77-1    | 0.97-1     |
| task\_set     | Num     |       9 | 4:perf(4)        | epochs+repls |          20 | ready  | 0.12-0.15 | 0.35-0.39  |

where for **n\_targets** (\#number):

-   perf = performance measure
-   ms = model\_size
-   rt = runtime
-   pt = predicttime

For more information either look directly into the
[code](https://github.com/compstat-lmu/paper_2021_multi_fidelity_surrogates/blob/main/R/BenchmarkConfigs.R)
or use the object’s printers via `cfgs("...")`.

Toy test functions:

| instance | space | n\_dims | n\_targets | fidelity | n\_problems | status           |
|:---------|:------|--------:|:-----------|:---------|------------:|:-----------------|
| Branin   | Num   |       2 | 1:y        | fidelity |           1 | ready            |
| Shekel   | Num   |       4 | 1:y        | fidelity |           1 | ready            |
| ZDT6     | Num   |      10 | 2:F1+F2    | fidelity |           1 | needs discussion |

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
# or using the active binding across all configs:
cfg$task_levels
```

If the configuration only has 1 task, `NULL` is returned.

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
opt("random_search")$optimize(ins)
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
opt("random_search")$optimize(ins)
```

## ZDT6

We first load the config:

``` r
cfg = cfgs("zdt6")
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
opt("random_search")$optimize(ins)
```

## RandomBotv2

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
opt("random_search")$optimize(ins)
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
opt("random_search")$optimize(ins)
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
opt("random_search")$optimize(ins)
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
opt("random_search")$optimize(ins)
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
opt("random_search")$optimize(ins)
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
opt("random_search")$optimize(ins)
```

## RandomBotv2 - super

The **super** learner is a learner that is parametrized as a choice over
all available randombot base learners. It has a highly hierarchical, 37
dimensional parameter space that includes the configurations of all
baselearners.

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("rbv2_super", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
opt("random_search")$optimize(ins)
```

## FCNet

We first load the config:

``` r
workdir = "/tmp/multifidelity_data/"
cfg = cfgs("fcnet", workdir = workdir)
```

this config contains our `objective` which we can use to optimize.

``` r
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
opt("random_search")$optimize(ins)
```

## Overview
