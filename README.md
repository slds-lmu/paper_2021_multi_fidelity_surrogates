
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the
hyperparameter response surface for several interesting machine learing
algorithms across several tasks.

## Overview

| instance    | space   | ndims | ntargets        | fidelity     | n\_problems | X.R.2. | status |
| :---------- | :------ | ----: | :-------------- | :----------- | ----------: | -----: | :----- |
| NB301       | Cat+Dep |    34 | 2:perf+rt       | epochs       |           1 | 0.9866 | ready  |
| LCBench     | Mix     |     7 | 6:perf(5)+rt    | epoch        |          35 | 0.9820 | ready  |
| RBv2SVM     | Mix+Dep |     7 | 6:perf(4)+rt+pt | frac+repls   |          96 |     NA | \-     |
| RBv2rpart   | Mix     |     5 | 6:perf(4)+rt+pt | frac+repls   |         101 |     NA | \-     |
| RBv2aknn    | Mix     |     6 | 6:perf(4)+rt+pt | frac+repls   |          99 |     NA | \-     |
| RBv2glmnet  | Mix     |     3 | 6:perf(4)+rt+pt | frac+repls   |          98 |     NA | \-     |
| RBv2ranger  | Mix+Dep |     9 | 6:perf(4)+rt+pt | fract+repls  |         114 |     NA | \-     |
| RBv2xgboost | Mix+Dep |    14 | 6:perf(4)+rt+pt | frac+repls   |         109 |     NA | \-     |
| RBv2super   | Mix+Dep |    36 | 6:perf(4)+rt+pt | frac+repls   |          89 |     NA | \-     |
| FCNet       | Mix     |    11 | perf(2)+rt+ ms  | epochs+repls |           4 | 0.7690 | ready  |
| TaskSet     | Num     |     9 | 4:perf(4)       | epochs+repls |          20 |     NA | \-     |

where for **ntargets** (\#number): - perf = performance measure - ms =
model\_size - rt = runtime - pt = predicttime

For more information either look directly into the
[code](https://github.com/compstat-lmu/paper_2021_multi_fidelity_surrogates/blob/main/R/BenchmarkConfigs.R)
or use the object’s printers via `cfgs("...")`.

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

## RandomBotv2 - super

The **super** learner is a learner that is parametrized as a choice over
all available randombot base learners. It has a highly hierarchical,
\(41\) dimensional parameter space that includes the configurations of
all baselearners.

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
#opt("random_search")$optimize(ins)
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

    #> ℹ Loading mfsurrogates
    #> BenchmarkConfig: <Branin>
    #> Target variables: y
    #> Budget parameter: "fidelity"
    #> <ParamSet>
    #>          id    class lower upper nlevels        default value
    #> 1:       x1 ParamDbl    -5    10     Inf <NoDefault[3]>      
    #> 2:       x2 ParamDbl     0    15     Inf <NoDefault[3]>      
    #> 3: fidelity ParamDbl     0     1     Inf <NoDefault[3]>      
    #> <ParamSet>
    #>    id    class lower upper nlevels        default value
    #> 1:  y ParamDbl  -Inf   Inf     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <FCNet>
    #> Target variables: valid_loss,valid_mse,runtime,n_params
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task" (4)
    #> <ParamSet>
    #>                  id    class lower upper nlevels        default value
    #>  1:           epoch ParamInt  1.00 100.0     100 <NoDefault[3]>      
    #>  2:     replication ParamInt  1.00   4.0       4 <NoDefault[3]>      
    #>  3: activation_fn_1 ParamFct    NA    NA       2 <NoDefault[3]>      
    #>  4: activation_fn_2 ParamFct    NA    NA       2 <NoDefault[3]>      
    #>  5:      batch_size ParamInt  8.00  64.0      57 <NoDefault[3]>      
    #>  6:       dropout_1 ParamDbl  0.00   0.6     Inf <NoDefault[3]>      
    #>  7:       dropout_2 ParamDbl  0.00   0.6     Inf <NoDefault[3]>      
    #>  8:         init_lr ParamDbl -3.31  -1.0     Inf <NoDefault[3]>      
    #>  9:     lr_schedule ParamFct    NA    NA       2 <NoDefault[3]>      
    #> 10:       n_units_1 ParamInt  4.00   9.0       6 <NoDefault[3]>      
    #> 11:       n_units_2 ParamInt  4.00   9.0       6 <NoDefault[3]>      
    #> 12:            task ParamFct    NA    NA       4 <NoDefault[3]>      
    #> Trafo is set.
    #> <ParamSet>
    #>            id    class lower upper nlevels        default value
    #> 1: valid_loss ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:  valid_mse ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:    runtime ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 4:   n_params ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <LCBench>
    #> Target variables: val_accuracy,val_cross_entropy,val_balanced_accuracy,test_cross_entropy,test_balanced_accuracy,time
    #> Budget parameter: "epoch"
    #> Task parameter (n): "OpenML_task_id" (35)
    #> <ParamSet>
    #>                id    class     lower     upper nlevels        default value
    #> 1: OpenML_task_id ParamFct        NA        NA      35 <NoDefault[3]>      
    #> 2:          epoch ParamInt  1.000000 52.000000      52 <NoDefault[3]>      
    #> 3:     batch_size ParamDbl  2.772589  6.238325     Inf <NoDefault[3]>      
    #> 4:  learning_rate ParamDbl -9.210340 -2.302585     Inf <NoDefault[3]>      
    #> 5:       momentum ParamDbl  0.100000  0.900000     Inf <NoDefault[3]>      
    #> 6:   weight_decay ParamDbl  0.000010  0.100000     Inf <NoDefault[3]>      
    #> 7:     num_layers ParamInt  1.000000  5.000000       5 <NoDefault[3]>      
    #> 8:      max_units ParamDbl  4.158883  6.931472     Inf <NoDefault[3]>      
    #> 9:    max_dropout ParamDbl  0.000000  1.000000     Inf <NoDefault[3]>      
    #> Trafo is set.
    #> <ParamSet>
    #>                        id    class lower upper nlevels        default value
    #> 1:           val_accuracy ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:      val_cross_entropy ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:  val_balanced_accuracy ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     test_cross_entropy ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 5: test_balanced_accuracy ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6:                   time ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <NASBench301>
    #> Target variables: val_accuracy,runtime
    #> Budget parameter: "epoch"
    #> <ParamSet>
    #>                                                                    id    class
    #>  1:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_0 ParamFct
    #>  2:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_1 ParamFct
    #>  3:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_10 ParamFct
    #>  4:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_11 ParamFct
    #>  5:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_12 ParamFct
    #>  6:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_13 ParamFct
    #>  7:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_2 ParamFct
    #>  8:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_3 ParamFct
    #>  9:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_4 ParamFct
    #> 10:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_5 ParamFct
    #> 11:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_6 ParamFct
    #> 12:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_7 ParamFct
    #> 13:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_8 ParamFct
    #> 14:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_normal_9 ParamFct
    #> 15:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_0 ParamFct
    #> 16:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_1 ParamFct
    #> 17:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_10 ParamFct
    #> 18:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_11 ParamFct
    #> 19:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_12 ParamFct
    #> 20:       NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_13 ParamFct
    #> 21:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_2 ParamFct
    #> 22:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_3 ParamFct
    #> 23:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_4 ParamFct
    #> 24:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_5 ParamFct
    #> 25:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_6 ParamFct
    #> 26:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_7 ParamFct
    #> 27:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_8 ParamFct
    #> 28:        NetworkSelectorDatasetInfo_COLON_darts_COLON_edge_reduce_9 ParamFct
    #> 29: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_3 ParamFct
    #> 30: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_4 ParamFct
    #> 31: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_5 ParamFct
    #> 32: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_3 ParamFct
    #> 33: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_4 ParamFct
    #> 34: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_5 ParamFct
    #> 35:                                                             epoch ParamInt
    #>                                                                    id    class
    #>     lower upper nlevels      default
    #>  1:    NA    NA       7 max_pool_3x3
    #>  2:    NA    NA       7 max_pool_3x3
    #>  3:    NA    NA       7 max_pool_3x3
    #>  4:    NA    NA       7 max_pool_3x3
    #>  5:    NA    NA       7 max_pool_3x3
    #>  6:    NA    NA       7 max_pool_3x3
    #>  7:    NA    NA       7 max_pool_3x3
    #>  8:    NA    NA       7 max_pool_3x3
    #>  9:    NA    NA       7 max_pool_3x3
    #> 10:    NA    NA       7 max_pool_3x3
    #> 11:    NA    NA       7 max_pool_3x3
    #> 12:    NA    NA       7 max_pool_3x3
    #> 13:    NA    NA       7 max_pool_3x3
    #> 14:    NA    NA       7 max_pool_3x3
    #> 15:    NA    NA       7 max_pool_3x3
    #> 16:    NA    NA       7 max_pool_3x3
    #> 17:    NA    NA       7 max_pool_3x3
    #> 18:    NA    NA       7 max_pool_3x3
    #> 19:    NA    NA       7 max_pool_3x3
    #> 20:    NA    NA       7 max_pool_3x3
    #> 21:    NA    NA       7 max_pool_3x3
    #> 22:    NA    NA       7 max_pool_3x3
    #> 23:    NA    NA       7 max_pool_3x3
    #> 24:    NA    NA       7 max_pool_3x3
    #> 25:    NA    NA       7 max_pool_3x3
    #> 26:    NA    NA       7 max_pool_3x3
    #> 27:    NA    NA       7 max_pool_3x3
    #> 28:    NA    NA       7 max_pool_3x3
    #> 29:    NA    NA       3          0_1
    #> 30:    NA    NA       6          0_1
    #> 31:    NA    NA      10          0_1
    #> 32:    NA    NA       3          0_1
    #> 33:    NA    NA       6          0_1
    #> 34:    NA    NA      10          0_1
    #> 35:     1    98      98            1
    #>     lower upper nlevels      default
    #>                                                               parents value
    #>  1:                                                                        
    #>  2:                                                                        
    #>  3: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_5      
    #>  4: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_5      
    #>  5: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_5      
    #>  6: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_5      
    #>  7: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_3      
    #>  8: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_3      
    #>  9: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_3      
    #> 10: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_4      
    #> 11: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_4      
    #> 12: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_4      
    #> 13: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_4      
    #> 14: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_normal_5      
    #> 15:                                                                        
    #> 16:                                                                        
    #> 17: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_5      
    #> 18: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_5      
    #> 19: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_5      
    #> 20: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_5      
    #> 21: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_3      
    #> 22: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_3      
    #> 23: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_3      
    #> 24: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_4      
    #> 25: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_4      
    #> 26: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_4      
    #> 27: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_4      
    #> 28: NetworkSelectorDatasetInfo_COLON_darts_COLON_inputs_node_reduce_5      
    #> 29:                                                                        
    #> 30:                                                                        
    #> 31:                                                                        
    #> 32:                                                                        
    #> 33:                                                                        
    #> 34:                                                                        
    #> 35:                                                                        
    #>                                                               parents value
    #> <ParamSet>
    #>              id    class lower upper nlevels        default value
    #> 1: val_accuracy ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:      runtime ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_aknn>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (33)
    #> <ParamSet>
    #>                         id    class lower upper nlevels        default value
    #> 1:                       k ParamInt     1    50      50 <NoDefault[3]>      
    #> 2:                distance ParamFct    NA    NA       3             l2      
    #> 3:                       M ParamInt    18    50      33 <NoDefault[3]>      
    #> 4:                      ef ParamDbl     3     8     Inf <NoDefault[3]>      
    #> 5:         ef_construction ParamDbl     4     9     Inf <NoDefault[3]>      
    #> 6:               trainsize ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 7:                    repl ParamInt     1    10      10 <NoDefault[3]>      
    #> 8: num.impute.selected.cpo ParamFct    NA    NA       3 <NoDefault[3]>      
    #> 9:                 task_id ParamFct    NA    NA      33 <NoDefault[3]>      
    #> Trafo is set.
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_glmnet>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (56)
    #> <ParamSet>
    #>                         id    class lower upper nlevels        default value
    #> 1:                   alpha ParamDbl     0     1     Inf              1      
    #> 2:                       s ParamDbl   -10    10     Inf              0      
    #> 3:               trainsize ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:                    repl ParamInt     1    10      10 <NoDefault[3]>      
    #> 5: num.impute.selected.cpo ParamFct    NA    NA       3 <NoDefault[3]>      
    #> 6:                 task_id ParamFct    NA    NA      56 <NoDefault[3]>      
    #> Trafo is set.
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_ranger>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (114)
    #> <ParamSet>
    #>                            id    class lower upper nlevels        default
    #>  1:             min.node.size ParamInt   1.0   100     100 <NoDefault[3]>
    #>  2:                mtry.power ParamInt   0.0     1       2 <NoDefault[3]>
    #>  3:   num.impute.selected.cpo ParamFct    NA    NA       3 <NoDefault[3]>
    #>  4:         num.random.splits ParamInt   1.0   100     100              1
    #>  5:                 num.trees ParamInt   1.0  2000    2000 <NoDefault[3]>
    #>  6:                      repl ParamInt   1.0    10      10 <NoDefault[3]>
    #>  7:                   replace ParamLgl    NA    NA       2 <NoDefault[3]>
    #>  8: respect.unordered.factors ParamFct    NA    NA       3 <NoDefault[3]>
    #>  9:           sample.fraction ParamDbl   0.1     1     Inf <NoDefault[3]>
    #> 10:                 splitrule ParamFct    NA    NA       2 <NoDefault[3]>
    #> 11:                   task_id ParamFct    NA    NA     114 <NoDefault[3]>
    #> 12:                 trainsize ParamDbl   0.0     1     Inf <NoDefault[3]>
    #>       parents value
    #>  1:                
    #>  2:                
    #>  3:                
    #>  4: splitrule      
    #>  5:                
    #>  6:                
    #>  7:                
    #>  8:                
    #>  9:                
    #> 10:                
    #> 11:                
    #> 12:                
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_glmnet>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (22)
    #> <ParamSet>
    #>                         id    class lower upper nlevels        default value
    #> 1:                      cp ParamDbl   -10     0     Inf      -6.643856      
    #> 2:                maxdepth ParamInt     1    30      30             30      
    #> 3:               minbucket ParamInt     1   100     100              1      
    #> 4:                minsplit ParamInt     1   100     100             20      
    #> 5:               trainsize ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6:                    repl ParamInt     1    10      10 <NoDefault[3]>      
    #> 7: num.impute.selected.cpo ParamFct    NA    NA       3 <NoDefault[3]>      
    #> 8:                 task_id ParamFct    NA    NA      22 <NoDefault[3]>      
    #> Trafo is set.
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_super>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (122)
    #> <ParamSet>
    #>                                   id    class  lower upper nlevels
    #>  1:                           aknn.M ParamInt  18.00    50      33
    #>  2:                    aknn.distance ParamFct     NA    NA       3
    #>  3:                          aknn.ef ParamDbl   3.00     8     Inf
    #>  4:             aknn.ef_construction ParamDbl   4.00     9     Inf
    #>  5:                           aknn.k ParamInt   1.00    50      50
    #>  6:                     glmnet.alpha ParamDbl   0.00     1     Inf
    #>  7:                         glmnet.s ParamDbl -10.00    10     Inf
    #>  8:                          learner ParamFct     NA    NA       6
    #>  9:          num.impute.selected.cpo ParamFct     NA    NA       3
    #> 10:             ranger.min.node.size ParamInt   1.00   100     100
    #> 11:                ranger.mtry.power ParamInt   0.00     1       2
    #> 12:         ranger.num.random.splits ParamInt   1.00   100     100
    #> 13:                 ranger.num.trees ParamInt   1.00  2000    2000
    #> 14:                   ranger.replace ParamLgl     NA    NA       2
    #> 15: ranger.respect.unordered.factors ParamFct     NA    NA       3
    #> 16:           ranger.sample.fraction ParamDbl   0.10     1     Inf
    #> 17:                 ranger.splitrule ParamFct     NA    NA       2
    #> 18:                             repl ParamInt   1.00    10      10
    #> 19:                         rpart.cp ParamDbl -10.00     0     Inf
    #> 20:                   rpart.maxdepth ParamInt   1.00    30      30
    #> 21:                  rpart.minbucket ParamInt   1.00   100     100
    #> 22:                   rpart.minsplit ParamInt   1.00   100     100
    #> 23:                         svm.cost ParamDbl -12.00    12     Inf
    #> 24:                       svm.degree ParamInt   2.00     5       4
    #> 25:                        svm.gamma ParamDbl -12.00    12     Inf
    #> 26:                       svm.kernel ParamFct     NA    NA       3
    #> 27:                    svm.shrinking ParamLgl     NA    NA       2
    #> 28:                    svm.tolerance ParamDbl -12.00    -3     Inf
    #> 29:                          task_id ParamFct     NA    NA     122
    #> 30:                        trainsize ParamDbl   0.00     1     Inf
    #> 31:                    xgboost.alpha ParamDbl -10.00    10     Inf
    #> 32:                  xgboost.booster ParamFct     NA    NA       3
    #> 33:        xgboost.colsample_bylevel ParamDbl   0.01     1     Inf
    #> 34:         xgboost.colsample_bytree ParamDbl   0.01     1     Inf
    #> 35:                      xgboost.eta ParamDbl -10.00     0     Inf
    #> 36:                    xgboost.gamma ParamDbl -15.00     3     Inf
    #> 37:                   xgboost.lambda ParamDbl -10.00    10     Inf
    #> 38:                xgboost.max_depth ParamInt   1.00    15      15
    #> 39:         xgboost.min_child_weight ParamDbl   0.00     7     Inf
    #> 40:                  xgboost.nrounds ParamInt   3.00    11       9
    #> 41:                xgboost.rate_drop ParamDbl   0.00     1     Inf
    #> 42:                xgboost.skip_drop ParamDbl   0.00     1     Inf
    #> 43:                xgboost.subsample ParamDbl   0.10     1     Inf
    #>                                   id    class  lower upper nlevels
    #>            default                  parents value
    #>  1: <NoDefault[3]>                  learner      
    #>  2:             l2                  learner      
    #>  3: <NoDefault[3]>                  learner      
    #>  4: <NoDefault[3]>                  learner      
    #>  5: <NoDefault[3]>                  learner      
    #>  6:              1                  learner      
    #>  7:              0                  learner      
    #>  8: <NoDefault[3]>                               
    #>  9: <NoDefault[3]>                               
    #> 10: <NoDefault[3]>                  learner      
    #> 11: <NoDefault[3]>                  learner      
    #> 12:              1 ranger.splitrule,learner      
    #> 13: <NoDefault[3]>                  learner      
    #> 14: <NoDefault[3]>                  learner      
    #> 15: <NoDefault[3]>                  learner      
    #> 16: <NoDefault[3]>                  learner      
    #> 17: <NoDefault[3]>                  learner      
    #> 18: <NoDefault[3]>                               
    #> 19:      -6.643856                  learner      
    #> 20:             30                  learner      
    #> 21:              1                  learner      
    #> 22:             20                  learner      
    #> 23: <NoDefault[3]>                  learner      
    #> 24: <NoDefault[3]>       svm.kernel,learner      
    #> 25: <NoDefault[3]>       svm.kernel,learner      
    #> 26: <NoDefault[3]>                  learner      
    #> 27: <NoDefault[3]>                  learner      
    #> 28: <NoDefault[3]>                  learner      
    #> 29: <NoDefault[3]>                               
    #> 30: <NoDefault[3]>                               
    #> 31: <NoDefault[3]>                  learner      
    #> 32: <NoDefault[3]>                  learner      
    #> 33: <NoDefault[3]>  xgboost.booster,learner      
    #> 34: <NoDefault[3]>  xgboost.booster,learner      
    #> 35: <NoDefault[3]>  xgboost.booster,learner      
    #> 36: <NoDefault[3]>  xgboost.booster,learner      
    #> 37: <NoDefault[3]>                  learner      
    #> 38: <NoDefault[3]>  xgboost.booster,learner      
    #> 39: <NoDefault[3]>  xgboost.booster,learner      
    #> 40: <NoDefault[3]>                  learner      
    #> 41: <NoDefault[3]>  xgboost.booster,learner      
    #> 42: <NoDefault[3]>  xgboost.booster,learner      
    #> 43: <NoDefault[3]>                  learner      
    #>            default                  parents value
    #> Trafo is set.
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_SVM>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (98)
    #> <ParamSet>
    #>                          id    class lower upper nlevels        default parents
    #>  1:                    cost ParamDbl   -12    12     Inf <NoDefault[3]>        
    #>  2:                  degree ParamInt     2     5       4 <NoDefault[3]>  kernel
    #>  3:                   gamma ParamDbl   -12    12     Inf <NoDefault[3]>  kernel
    #>  4:                  kernel ParamFct    NA    NA       3 <NoDefault[3]>        
    #>  5: num.impute.selected.cpo ParamFct    NA    NA       3 <NoDefault[3]>        
    #>  6:                    repl ParamInt     1    10      10 <NoDefault[3]>        
    #>  7:               shrinking ParamLgl    NA    NA       2 <NoDefault[3]>        
    #>  8:                 task_id ParamFct    NA    NA      98 <NoDefault[3]>        
    #>  9:               tolerance ParamDbl   -12    -3     Inf <NoDefault[3]>        
    #> 10:               trainsize ParamDbl     0     1     Inf <NoDefault[3]>        
    #>     value
    #>  1:      
    #>  2:      
    #>  3:      
    #>  4:      
    #>  5:      
    #>  6:      
    #>  7:      
    #>  8:      
    #>  9:      
    #> 10:      
    #> Trafo is set.
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <RBv2_xgboost>
    #> Target variables: mmce,f1,auc,logloss,timetrain,timepredict
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_id" (119)
    #> <ParamSet>
    #>                          id    class  lower upper nlevels        default
    #>  1:                   alpha ParamDbl -10.00    10     Inf <NoDefault[3]>
    #>  2:                 booster ParamFct     NA    NA       3 <NoDefault[3]>
    #>  3:       colsample_bylevel ParamDbl   0.01     1     Inf <NoDefault[3]>
    #>  4:        colsample_bytree ParamDbl   0.01     1     Inf <NoDefault[3]>
    #>  5:                     eta ParamDbl -10.00     0     Inf <NoDefault[3]>
    #>  6:                   gamma ParamDbl -15.00     3     Inf <NoDefault[3]>
    #>  7:                  lambda ParamDbl -10.00    10     Inf <NoDefault[3]>
    #>  8:               max_depth ParamInt   1.00    15      15 <NoDefault[3]>
    #>  9:        min_child_weight ParamDbl   0.00     7     Inf <NoDefault[3]>
    #> 10:                 nrounds ParamInt   3.00    11       9 <NoDefault[3]>
    #> 11: num.impute.selected.cpo ParamFct     NA    NA       3 <NoDefault[3]>
    #> 12:               rate_drop ParamDbl   0.00     1     Inf <NoDefault[3]>
    #> 13:                    repl ParamInt   1.00    10      10 <NoDefault[3]>
    #> 14:               skip_drop ParamDbl   0.00     1     Inf <NoDefault[3]>
    #> 15:               subsample ParamDbl   0.10     1     Inf <NoDefault[3]>
    #> 16:                 task_id ParamFct     NA    NA     119 <NoDefault[3]>
    #> 17:               trainsize ParamDbl   0.00     1     Inf <NoDefault[3]>
    #>     parents value
    #>  1:              
    #>  2:              
    #>  3: booster      
    #>  4: booster      
    #>  5: booster      
    #>  6: booster      
    #>  7:              
    #>  8: booster      
    #>  9: booster      
    #> 10:              
    #> 11:              
    #> 12: booster      
    #> 13:              
    #> 14: booster      
    #> 15:              
    #> 16:              
    #> 17:              
    #> Trafo is set.
    #> <ParamSet>
    #>             id    class lower upper nlevels        default value
    #> 1:        mmce ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 2:          f1 ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 3:         auc ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 4:     logloss ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 5:   timetrain ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 6: timepredict ParamDbl     0     1     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <Shekel>
    #> Target variables: y
    #> Budget parameter: "fidelity"
    #> <ParamSet>
    #>          id    class lower upper nlevels        default value
    #> 1:       x1 ParamDbl     0    10     Inf <NoDefault[3]>      
    #> 2:       x2 ParamDbl     0    10     Inf <NoDefault[3]>      
    #> 3:       x3 ParamDbl     0    10     Inf <NoDefault[3]>      
    #> 4: fidelity ParamDbl     0     1     Inf <NoDefault[3]>      
    #> <ParamSet>
    #>    id    class lower upper nlevels        default value
    #> 1:  y ParamDbl  -Inf   Inf     Inf <NoDefault[3]>      
    #> 
    #> 
    #> BenchmarkConfig: <TaskSet>
    #> Target variables: train,valid1,valid2,test
    #> Budget parameter: "epoch"
    #> Task parameter (n): "task_name" (20)
    #> <ParamSet>
    #>                    id    class lower upper nlevels        default value
    #>  1:             epoch ParamInt     1 10000   10000 <NoDefault[3]>      
    #>  2:       replication ParamInt     0     4       5 <NoDefault[3]>      
    #>  3:     learning_rate ParamDbl    -8     1     Inf <NoDefault[3]>      
    #>  4:             beta1 ParamDbl    -4     0     Inf <NoDefault[3]>      
    #>  5:             beta2 ParamDbl    -3     0     Inf <NoDefault[3]>      
    #>  6:           epsilon ParamDbl   -10     3     Inf <NoDefault[3]>      
    #>  7:                l1 ParamDbl    -8     1     Inf <NoDefault[3]>      
    #>  8:                l2 ParamDbl    -8     1     Inf <NoDefault[3]>      
    #>  9:      linear_decay ParamDbl    -7    -4     Inf <NoDefault[3]>      
    #> 10: exponential_decay ParamDbl    -6    -3     Inf <NoDefault[3]>      
    #> 11:         task_name ParamFct    NA    NA      20 <NoDefault[3]>      
    #> Trafo is set.
    #> <ParamSet>
    #>        id    class lower upper nlevels        default value
    #> 1:  train ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 2: valid1 ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 3: valid2 ParamDbl     0   Inf     Inf <NoDefault[3]>      
    #> 4:   test ParamDbl     0   Inf     Inf <NoDefault[3]>
