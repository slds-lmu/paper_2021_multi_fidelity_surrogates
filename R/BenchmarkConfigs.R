#' @export
BenchmarkConfigNB301 = R6Class("BenchmarkConfigNB301",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = 'NASBench301', workdir) {
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
    data = function() preproc_data_nb301(self$data_path),
    param_set = function() readRDS(self$param_set_path)
  )
)

BenchmarkConfigRBv2SVM = R6Class("BenchmarkConfigRBv2SVM",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = 'RBv2_SVM', workdir) {
      super$initialize(
        id,
        download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_svm/",
        workdir = workdir,
        param_set_file = NULL,
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
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
      )
    }
  )
)

# BenchmarkConfigRBv2RF = R6Class("BenchmarkConfigRBv2RF",
#   inherit = BenchmarkConfig,
#   public = list(
#     initialize = function(id = 'RBv2_SVM', workdir) {
#       super$initialize(
#         id,
#         download_url = "https://syncandshare.lrz.de/dl/fiSd4UWxmx9FRrQtdYeYrxEV/rbv2_svm/",
#         workdir = workdir,
#         param_set_file = NULL,
#         dicts_file = "dicts.rds",
#         keras_model_file = "model.hdf5",
#         onnx_model_file = "model.onnx",
#         budget_param = "epoch",
#         target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime"),
#         codomain = ps(
#           perf.mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
#           perf.logloss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
#           traintime = p_dbl(lower = 0, upper = 1, tags = "minimize"),
#           predicttime = p_dbl(lower = 0, upper = 1, tags = "minimize")
#         ),
#         packages = NULL
#       )
#     }
#   ),
#   active = list(
#     param_set = function() {
#       ps(
#         kernel = p_fct(levels = c("linear", "polynomial", "radial")),
#         cost =  p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x),
#         gamma = p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x, depends = kernel == "radial"),
#         tolerance = p_dbl(lower = -12, upper = -3, trafo = function(x) 2^x),
#         degree = p_int(lower = 2, upper = 5, depends = kernel == "polynomial"),
#         shrinking = p_lgl(),
#         num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
#       )
#     }
#   )
# )