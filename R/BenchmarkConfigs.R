#' @export
BenchmarkConfigNB301 = R6Class("BenchmarkConfigNB301",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "NASBench301", workdir) {
      super$initialize(
        id,
        download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/nb301/",
        workdir = workdir,
        model_name = "nb301",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        budget_param = "epoch",
        target_variables = c("val_accuracy", "runtime"),
        codomain = ps(
          val_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          runtime = p_dbl(lower = 0, upper = 1, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_data_nb301(self)
      private$.data
    },
    param_set = function() readRDS(self$param_set_path)
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("nb301", BenchmarkConfigNB301)



#' @export
# with fidelity = 0 3 global optima that vanish until fidelity 1 only a single global optimum; idea from
# Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
# fidelity 0: x*1 = (-pi, 12.275), x*2 = (pi, 2.275), x*3 = c(9.42478, 2.475) f(x) = 0.39789
# fidelity 1: x*1 = (9.97247, 2.97574) f(x) = -48.05995
# increasing fidelity gradually shifts x*3 to the global x*1
BenchmarkConfigBranin = R6Class("BenchmarkConfigBranin",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Branin") {
      super$initialize(
        id,
        download_url = NULL,
        workdir = NULL,
        model_name = "branin",
        param_set_file = NULL,
        data_file = NULL,
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
        budget_param = "fidelity",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },

    setup = function() {
      message("no setup necessary.")
    },

    get_objective = function() {
      ObjectiveRFunDt$new(
        fun = function(xdt) {
          a = 1
          b = 5.1 / (4 * (pi ^ 2))
          c = 5 / pi
          r = 6
          s = 10
          t = 1 / (8 * pi)
          data.table(y = (a * ((xdt[["x2"]] - b * (xdt[["x1"]] ^ 2L) + c * xdt[["x1"]] - r) ^ 2) + ((s * (1 - t)) * cos(xdt[["x1"]])) + s -(5 * xdt[["fidelity"]] * xdt[["x1"]])))
        },
        domain = self$param_set,
        codomain = self$codomain
      )
    },

    plot = function(method = c("ggplot2", "rgl")) {
      method = match.arg(method, choices = c("ggplot2", "rgl"))
      objective = self$get_objective()
       design = generate_design_grid(objective$domain, param_resolutions = c(x1 = 100L, x2 = 100L, fidelity = 11L))$data
      for (f in unique(design$fidelity)) {
        tmp = design[fidelity == f]
        tmp = cbind(tmp, objective$eval_dt(tmp))
        if (method == "ggplot2") {
          requireNamespace("ggplot2")
          # ggplot2
          print(ggplot2::ggplot(tmp, ggplot2::aes(x = x1, y = x2, z = y)) +
            ggplot2::geom_contour_filled(bins = 50L, show.legend = FALSE) +
            ggplot2::ggtitle(gsub("x", f, "fidelity of x")))
        } else {
          requireNamespace("rgl")
          # rgl
          col = colorspace::diverging_hcl(50L)[cut(tmp$y, breaks = 50L)]
          rgl::plot3d(x = tmp$x1, y = tmp$x2, z = tmp$y, col = col, xlab = "x1", ylab = "x2", zlab = "y", main = gsub("x", f, "fidelity of x"))
        }
      }
    }
  ),
  active = list(
    param_set = function() {
      ps(
        x1 = p_dbl(lower = -5, upper = 10),
        x2 = p_dbl(lower = 0, upper = 15),
        fidelity = p_dbl(lower = 0L, upper = 1L)
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("branin", BenchmarkConfigBranin)



#' @export
# https://www.sfu.ca/~ssurjano/shekel.html
# 4d function with m (default = 10) local minima
# fidelity parameter replaces x4 (scaled from [0, 1] to [0, 4[)
# global minumum at m = 10: x* = (4, 4, 4, 4) f(x) = -10.536
BenchmarkConfigShekel = R6Class("BenchmarkConfigShekel",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Shekel") {
      super$initialize(
        id,
        download_url = NULL,
        workdir = NULL,
        model_name = "shekel",
        param_set_file = NULL,
        data_file = NULL,
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
        budget_param = "fidelity",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },

    setup = function() {
      message("no setup necessary.")
    },

    get_objective = function(m = 10L) {
      assert_int(m, lower = 1L, upper = 20L)
      ObjectiveRFunDt$new(
        fun = function(xdt) {
          xdt[["fidelity"]] =  4 * xdt[["fidelity"]]  # scale to from [0, 1] to [0, 4]
          xdt_mat = as.matrix(xdt)
          b = 0.1 * c(1, 2, 2, 4, 4, 6, 3, 7, 5, 5)
          C = c(4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
                4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.6,
                4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
                4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.6)
          C = matrix(C, nrow = 4L, ncol = 10L, byrow = TRUE)
          Ct = t(C)

          data.table(y = map_dbl(seq_len(NROW(xdt_mat)), function(i) {
            xxmat = matrix(rep(xdt_mat[i, ], times = m), nrow = m, ncol = 4L, byrow = TRUE)
            inner = rowSums((xxmat - Ct[, 1:4]) ^ 2)
             - sum(1 / (inner + b))
          }))

        },
        domain = self$param_set,
        codomain = self$codomain
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 10),
        x2 = p_dbl(lower = 0, upper = 10),
        x3 = p_dbl(lower = 0, upper = 10),
        fidelity = p_dbl(lower = 0L, upper = 1L)
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("shekel", BenchmarkConfigShekel)



#' @export
BenchmarkConfigLCBench = R6Class("BenchmarkConfigLCBench",
  inherit = BenchmarkConfig,
  public = list(
   initialize = function(id = "NASBench301", workdir) {
     super$initialize(
       id,
       download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/lcbench/",
       workdir = workdir,
       model_name = "lcbench",
       param_set_file = "param_set.rds",
       data_file = "data.rds",
       dicts_file = "dicts.rds",
       keras_model_file = "model.hdf5",
       onnx_model_file = "model.onnx",
       budget_param = "epoch",
       target_variables = c("val_accuracy", "val_cross_entropy","val_balanced_accuracy","test_cross_entropy","test_balanced_accuracy", "time"),
       codomain = ps(
         val_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
         val_cross_entropy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
         val_balanced_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
         test_cross_entropy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
         test_balanced_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
         time = p_dbl(lower = 0, upper = 1, tags = "minimize")
       ),
       packages = NULL
     )
   },
   get_task_ids = function() {
     self$param_set$params$OpenML_task_id$levels
   }
  ),
  active = list(
   data = function() {
     if (is.null(private$.data)) private$.data = preproc_data_lcbench(self)
     private$.data
   },
   param_set = function() {
     ps = ParamSet$new(list(
       ParamFct$new("OpenML_task_id", levels = c(
         "3945",   "7593",  "34539", "126025", "126026", "126029", "146212", "167083",
         "167104", "167149", "167152", "167161", "167168", "167181", "167184", "167185",
         "167190", "167200", "167201", "168329", "168330", "168331", "168335", "168868",
         "168908", "168910", "189354", "189862", "189865", "189866", "189873", "189905",
         "189906", "189908", "189909")),
       ParamInt$new("epoch", lower = 1L, upper = 52L, tags = "budget"),
       ParamDbl$new("batch_size", lower = log(16L), upper = log(512L)),
       ParamDbl$new("learning_rate", lower = log(1e-4), upper = log(1e-1)),
       ParamDbl$new("momentum", lower = 0.1, upper = 0.9),
       ParamDbl$new("weight_decay", lower = 1e-5, upper = 1e-1),
       ParamInt$new("num_layers", lower = 1L, upper = 5L),
       ParamDbl$new("max_units", lower = log(64L), upper = log(1024L)),
       ParamDbl$new("max_dropout", lower = 0, upper = 1))
     )
     ps$trafo = function(x, param_set) {
       x$batch_size = as.integer(round(exp(x$batch_size)))
       x$learning_rate = exp(x$learning_rate)
       x$max_units = as.integer(round(exp(x$max_units)))
       x
     }
     ps
   }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("lcbench", BenchmarkConfigLCBench)



BenchmarkConfigRBv2SVM = R6Class("BenchmarkConfigRBv2SVM",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_SVM", workdir) {
      super$initialize(
        id,
        download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_svm/",
        workdir = workdir,
        model_name = "rbv2_svm",
        param_set_file = NULL,
        data_file = "data.arff",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        budget_param = "epoch",
        target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
        codomain = ps(
          perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        kernel = p_fct(levels = c("linear", "polynomial", "radial")),
        cost =  p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x),
        gamma = p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x, depends = kernel == "radial"),
        tolerance = p_dbl(lower = -12, upper = -3, trafo = function(x) 2^x),
        degree = p_int(lower = 2, upper = 5, depends = kernel == "polynomial"),
        shrinking = p_lgl(),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = as.character(self$get_task_ids()))
      )
    },
    data = function(x) {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_svm(self)
      private$.data
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_svm", BenchmarkConfigRBv2SVM)



BenchmarkConfigRBv2ranger = R6Class("BenchmarkConfigRBv2ranger",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_ranger", workdir) {
      super$initialize(
        id,
        download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_ranger/",
        workdir = workdir,
        model_name = "rbv2_ranger",
        param_set_file = NULL,
        data_file = "data.arff",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        budget_param = "epoch",
        target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
        codomain = ps(
          perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
       ps(
        num.trees = p_int(lower = 1, upper = 2000),
        replace = p_lgl(),
        sample.fraction = p_dbl(lower = 0.1, upper = 1),
        mtry.power = p_int(lower = 0, upper = 1),
        respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
        min.node.size = p_int(lower = 1, upper = 100),
        splitrule = p_fct(levels = c("gini", "extratrees")),
        num.random.splits = p_int(lower = 1, upper = 100, default = 1L, depends = splitrule == "extratrees"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = as.character(self$get_task_ids()))
      )
    },
    data = function(x) {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_ranger(self)
      private$.data
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_ranger", BenchmarkConfigRBv2ranger)



BenchmarkConfigRBv2glmnet = R6Class("BenchmarkConfigRBv2glmnet",
                                    inherit = BenchmarkConfig,
                                    public = list(
                                      initialize = function(id = "RBv2_glmnet", workdir) {
                                        super$initialize(
                                          id,
                                          download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_glmnet/",
                                          workdir = workdir,
                                          model_name = "rbv2_glmnet",
                                          param_set_file = NULL,
                                          data_file = "data.arff",
                                          dicts_file = "dicts.rds",
                                          keras_model_file = "model.hdf5",
                                          onnx_model_file = "model.onnx",
                                          budget_param = "epoch",
                                          target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
                                          codomain = ps(
                                            perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
                                          ),
                                          packages = NULL
                                        )
                                      }
                                    ),
                                    active = list(
                                      param_set = function() {
                                        ps(
                                          alpha = p_dbl(lower = 0, upper = 1, default = 1, trafo = function(x) max(0, min(1, x))),
                                          s = p_dbl(lower = -10, upper = 10, default = 0, trafo = function(x) 2^x),
                                          num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
                                          task_id = p_fct(levels = as.character(self$get_task_ids()))
                                        )
                                      },
                                      data = function(x) {
                                        if(is.null(private$.data)) private$.data = preproc_data_rbv2_glmnet(self)
                                        private$.data
                                      }
                                    )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_glmnet", BenchmarkConfigRBv2glmnet)



BenchmarkConfigRBv2xgboost = R6Class("BenchmarkConfigRBv2xgboost",
                                    inherit = BenchmarkConfig,
                                    public = list(
                                      initialize = function(id = "RBv2_xgboost", workdir) {
                                        super$initialize(
                                          id,
                                          download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_xgboost/",
                                          workdir = workdir,
                                          model_name = "rbv2_xgboost",
                                          param_set_file = NULL,
                                          data_file = "data.arff",
                                          dicts_file = "dicts.rds",
                                          keras_model_file = "model.hdf5",
                                          onnx_model_file = "model.onnx",
                                          budget_param = "epoch",
                                          target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
                                          codomain = ps(
                                            perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
                                          ),
                                          packages = NULL
                                        )
                                      }
                                    ),
                                    active = list(
                                      param_set = function() {
                                        ps(
                                          booster = p_fct(levels = c("gblinear", "gbtree", "dart")),
                                          nrounds = p_int(lower = 3, upper = 11, trafo = function(x) round(2^x)),
                                          eta = p_dbl(lower = -10, upper = 0, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
                                          gamma = p_dbl(lower = -15, upper = 3, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
                                          lambda = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x),
                                          alpha = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x),
                                          subsample = p_dbl(lower = 0.1, upper = 1),
                                          max_depth = p_int(lower = 1, upper = 15), depends = booster %in% c("dart", "gbtree"),
                                          min_child_weight = p_dbl(lower = 0, upper = 7, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
                                          colsample_bytree = p_dbl(lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
                                          colsample_bylevel = p_dbl("", lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
                                          rate_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
                                          skip_drop = p_dbl(lower =  0, upper = 1, depends = booster == "dart"),
                                          num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
                                          task_id = p_fct(levels = as.character(self$get_task_ids()))
                                        )
                                      },
                                      data = function(x) {
                                        if(is.null(private$.data)) private$.data = preproc_data_rbv2_xgboost(self)
                                        private$.data
                                      }
                                    )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_xgboost", BenchmarkConfigRBv2xgboost)

BenchmarkConfigRBv2rpart = R6Class("BenchmarkConfigRBv2rpart",
                                    inherit = BenchmarkConfig,
                                    public = list(
                                      initialize = function(id = "RBv2_glmnet", workdir) {
                                        super$initialize(
                                          id,
                                          download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_rpart/",
                                          workdir = workdir,
                                          model_name = "rbv2_rpart",
                                          param_set_file = NULL,
                                          data_file = "data.arff",
                                          dicts_file = "dicts.rds",
                                          keras_model_file = "model.hdf5",
                                          onnx_model_file = "model.onnx",
                                          budget_param = "epoch",
                                          target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
                                          codomain = ps(
                                            perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
                                          ),
                                          packages = NULL
                                        )
                                      }
                                    ),
                                    active = list(
                                      param_set = function() {
                                        ps = ps(
                                          cp = p_dbl(lower = -10, upper = 0, default = log2(0.01), trafo = function(x) 2^x),
                                          maxdepth = p_int(lower = 1, upper = 30, default = 30),
                                          minbucket = p_int(lower = 1, upper = 100, default = 1),
                                          minsplit = p_int(lower = 1, upper = 100, default = 20),
                                          num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
                                          task_id = p_fct(levels = as.character(self$get_task_ids()))
                                        )
                                      },
                                      data = function(x) {
                                        if(is.null(private$.data)) private$.data = preproc_data_rbv2_rpart(self)
                                        private$.data
                                      }
                                    )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_rpart", BenchmarkConfigRBv2rpart)



BenchmarkConfigRBv2aknn = R6Class("BenchmarkConfigRBv2aknn",
                                    inherit = BenchmarkConfig,
                                    public = list(
                                      initialize = function(id = "RBv2_aknn", workdir) {
                                        super$initialize(
                                          id,
                                          download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_aknn/",
                                          workdir = workdir,
                                          model_name = "rbv2_aknn",
                                          param_set_file = NULL,
                                          data_file = "data.arff",
                                          dicts_file = "dicts.rds",
                                          keras_model_file = "model.hdf5",
                                          onnx_model_file = "model.onnx",
                                          budget_param = "epoch",
                                          target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
                                          codomain = ps(
                                            perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
                                            predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
                                          ),
                                          packages = NULL
                                        )
                                      }
                                    ),
                                    active = list(
                                      param_set = function() {
                                        ps(
                                          k = p_int(lower = 1L, upper = 50),
                                          distance = p_fct(levels = c("l2", "cosine", "ip"), default = "l2"),
                                          M = p_int(lower = 18, upper = 50),
                                          ef = p_dbl(lower = 3, upper = 8, trafo = function(x) round(2^x)),
                                          ef_construction = p_dbl(lower = 4, upper = 9, trafo = function(x) round(2^x)),
                                          num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
                                          task_id = p_fct(levels = as.character(self$get_task_ids()))
                                        )
                                      },
                                      data = function(x) {
                                        if(is.null(private$.data)) private$.data = preproc_data_rbv2_aknn(self)
                                        private$.data
                                      }
                                    )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_aknn", BenchmarkConfigRBv2aknn)


# Not sure whether to include kerasff
# classif.kerasff = ps(
#   p_dbl(id = "epochs", lower = 3, upper = 7, trafo = function(x) round(2^x)),
#   p_fct(id = "optimizer", values = c("sgd", "rmsprop", "adam")),
#   p_dbl(id = "lr", lower = -5, upper = 0, trafo = function(x) 5^x),
#   p_dbl(id = "decay", lower = -8, upper = 0, trafo = function(x) 5^x),
#   p_dbl(id = "momentum", lower = -8, upper = 0,trafo = function(x) 5^x,
#     requires = quote(optimizer == "sgd")),
#   p_int(id = "layers", lower = 1L, upper = 4L),
#   p_fct(id = "batchnorm_dropout", values = c("batchnorm", "dropout", "none")),
#   p_dbl(id = "input_dropout_rate", lower = -5, upper = 0, requires = quote(batchnorm_dropout == "dropout"), trafo =  function(x) 3^(x/2)),
#   p_dbl(id = "dropout_rate", lower = -5, upper = 0, requires = quote(batchnorm_dropout == "dropout"), trafo =  function(x) 3^(x/2)),
#   # Neurons / Layers
#   p_dbl(id = "units_layer1", lower = 3L, upper = 9, trafo = function(x) round(2^x)),
#   p_dbl(id = "units_layer2", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 2)),
#   p_dbl(id = "units_layer3", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 3)),
#   p_dbl(id = "units_layer4", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 4)),
#   # Activations
#   p_fct(id = "act_layer", values = c("relu", "tanh")),
#   # Initializers
#   p_fct(id = "init_layer", values = c("glorot_normal", "glorot_uniform", "he_normal", "he_uniform")),
#   # Regularizers
#   p_dbl(id = "l1_reg_layer",
#     lower = -10, upper = -2, trafo = function(x) 5^x),
#   p_dbl(id = "l2_reg_layer",
#     lower = -10, upper = -2, trafo = function(x) 5^x),
#   p_lgl(id = "learning_rate_scheduler", default = FALSE),
#   p_fct(id = "init_seed", values = c(1L, 11L, 101L, 131L, 499L)),
#   num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
# )
# # classif.kerasff.fixed_pars = list(early_stopping_patience = 0L, validation_split = 0, nthread = 1L)
# target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime")
