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
    data = function() preproc_data_nb301(self$data_path),
    param_set = function() readRDS(self$param_set_path)
  )
)

#' @export
# with fidelity = 0 3 global optima that vanish until fidelity 1 only a single global optimum; idea from
# Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
BenchmarkConfigBranin = R6Class("BenchmarkConfigBranin",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Branin") {
      super$initialize(
        id,
        download_url = NULL,
        workdir = NULL,
        model_name = NULL,
        param_set_file = NULL,
        data_file = NULL,
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
        budget_param = "fidelity",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
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
          data.table(y = log(a * ((xdt[["x2"]] - b * (xdt[["x1"]] ^ 2L) + c * xdt[["x1"]] - r) ^ 2) + ((s * (1 - t)) * cos(xdt[["x1"]])) + 51 + s + (5 * xdt[["fidelity"]] * xdt[["x1"]])))
        },
        domain = self$param_set,
        codomain = self$codomain
      )
    },

    plot = function(method = c("ggplot2", "rgl")) {
      method = match.arg(method, choices = c("ggplot2", "rgl"))
      objective = self$get_objective()
       design = generate_design_grid(objective$domain, param_resolutions = c(x1 = 100L, x2 = 100L, fidelity = 11))$data
      for (f in unique(design$fidelity)) {
        tmp = design[fidelity == f]
        tmp = cbind(tmp, objective$eval_dt(tmp))
        if (method == "ggplot2") {
          # ggplot2
          print(ggplot(tmp, aes(x = x1, y = x2, z = y)) +
            geom_contour_filled() +
            ggtitle(gsub("x", f, "fidelity of x")))
        } else {
          # rgl
          col = colorspace::diverging_hcl(20)[cut(tmp$y, breaks = 20L)]
          plot3d(x = tmp$x1, y = tmp$x2, z = tmp$y, col = col, xlab = "x1", ylab = "x2", zlab = "y", main = gsub("x", f, "fidelity of x"))
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



BenchmarkConfigRBv2SVM = R6Class("BenchmarkConfigRBv2SVM",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_SVM", workdir) {
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
#     initialize = function(id = "RBv2_SVM", workdir) {
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
