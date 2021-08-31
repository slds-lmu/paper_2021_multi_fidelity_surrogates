
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the
hyperparameter response surface for several interesting machine learing
algorithms across several tasks.

For a continuation of this project see [YAHPO
GYM](https://github.com/pfistfl/yahpo_gym).

## Overview

|     | instance     | space   | n_dims | n_targets        | fidelity       | n_problems | status | Rsq       | Rho       |
|:----|:-------------|:--------|-------:|:-----------------|:---------------|-----------:|:-------|:----------|:----------|
| 1   | nb301        | Cat+Dep |     34 | 2:perf(1)+rt     | epoch          |          1 | ready  | 0.83-0.98 | 0.91-0.99 |
| 2   | lcbench      | Numeric |      7 | 6:perf(5)+rt     | epoch          |         35 | ready  | 0.98-1    | 0.99-1    |
| 10  | fcnet        | Mix     |     11 | 4:perf(2)+rt+ ms | epoch+repl     |          4 | ready  | 0.96-1    | 0.98-1    |
| 9   | rbv2_super   | Mix+Dep |     38 | 6:perf(4)+rt+pt  | trainsize+repl |         89 | ready  | 0.86-0.99 | 0.93-0.99 |
| 3   | rbv2_svm     | Mix+Dep |      6 | 6:perf(4)+rt+pt  | trainsize+repl |         96 | ready  | 0.83-0.99 | 0.91-1    |
| 4   | rbv2_rpart   | Mix     |      5 | 6:perf(4)+rt+pt  | trainsize+repl |        101 | ready  | 0.79-0.99 | 0.89-1    |
| 5   | rbv2_aknn    | Mix     |      6 | 6:perf(4)+rt+pt  | trainsize+repl |         99 | ready  | 0.74-0.99 | 0.86-1    |
| 6   | rbv2_glmnet  | Mix     |      3 | 6:perf(4)+rt+pt  | trainsize+repl |         98 | ready  | 0.91-1    | 0.96-1    |
| 7   | rbv2_ranger  | Mix+Dep |      8 | 6:perf(4)+rt+pt  | trainsize+repl |        114 | ready  | 0.85-1    | 0.92-1    |
| 8   | rbv2_xgboost | Mix+Dep |     14 | 6:perf(4)+rt+pt  | trainsize+repl |        109 | ready  | 0.87-0.98 | 0.93-0.99 |

where for **n_targets** (#number):

-   perf = performance measure
-   ms = model_size
-   rt = runtime
-   pt = predicttime

For more information either look directly into the
[code](https://github.com/compstat-lmu/paper_2021_multi_fidelity_surrogates/blob/main/R/BenchmarkConfigs.R)
or use the object’s printers via `cfgs("...")`.

Toy test functions:

| instance | space | n_dims | n_targets | fidelity | n_problems | status |
|:---------|:------|-------:|:----------|:---------|-----------:|:-------|
| Branin   | Num   |      2 | 1:y       | fidelity |          1 | ready  |
| Shekel   | Num   |      4 | 1:y       | fidelity |          1 | ready  |

## NASBench-301

We first load the config:

``` r
library(checkmate)
library(paradox)
library(mfsurrogates)
workdir = "../multifidelity_data/"
cfg = cfgs("nb301", workdir = workdir)
cfg$setup()  # automatic download of files; necessary if you didn't download manually
cfg
```

this config contains our `objective` which we can use to optimize.

``` r
library(bbotk)
library(data.table)
ins = OptimInstanceMultiCrit$new(
  objective = cfg$get_objective(),
  terminator = trm("evals", n_evals = 10L)
)
opt("random_search")$optimize(ins)
```

Since NASBench is only trained on `CIFAR10`, we do not need to set a
specific `task_id` here.

## LCBench

We first load the config:

``` r
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
ins = OptimInstanceSingleCrit$new(
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

## RandomBotv2

## RandomBotv2 - svm

We first load the config:

``` r
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
objective = cfg$get_objective(task = "3", target_variables = c("mmce", "timetrain"))
objective$codomain
objective$constants
```

## RandomBotv2 - rpart

We first load the config:

``` r
cfg = cfgs("rbv2_rpart", workdir = workdir)
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

## RandomBotv2 - aknn

We first load the config:

``` r
cfg = cfgs("rbv2_aknn", workdir = workdir)
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

## RandomBotv2 - glmnet

We first load the config:

``` r
cfg = cfgs("rbv2_glmnet", workdir = workdir)
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

## RandomBotv2 - ranger

We first load the config:

``` r
cfg = cfgs("rbv2_ranger", workdir = workdir)
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

## RandomBotv2 - xgboost

We first load the config:

``` r
cfg = cfgs("rbv2_xgboost", workdir = workdir)
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

## RandomBotv2 - super

The **super** learner is a learner that is parametrized as a choice over
all available randombot base learners. It has a highly hierarchical, 38
dimensional parameter space that includes the configurations of all
baselearners.

We first load the config:

``` r
cfg = cfgs("rbv2_super", workdir = workdir)
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

%## Overview
