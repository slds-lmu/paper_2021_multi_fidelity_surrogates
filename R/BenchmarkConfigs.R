#' @export
BenchmarkConfigNB301 = R6Class("BenchmarkConfigNB301",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "NASBench301", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "nb301",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("val_accuracy", "runtime"),
        codomain = ps(
          val_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          runtime = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_data_nb301(self)
      return(private$.data)
    },
    # FIXME: we should trigger this once and then store it private
    param_set = function() readRDS(self$param_set_path)
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("nb301", BenchmarkConfigNB301)



#' @export
BenchmarkConfigLCBench = R6Class("BenchmarkConfigLCBench",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "LCBench", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "lcbench",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("val_accuracy", "val_cross_entropy", "val_balanced_accuracy", "test_cross_entropy", "test_balanced_accuracy", "time"),
        codomain = ps(
          val_accuracy = p_dbl(lower = 0, upper = 100, tags = "maximize"),
          val_cross_entropy = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          val_balanced_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          test_cross_entropy = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          test_balanced_accuracy = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          time = p_dbl(lower = 0, upper = Inf, tags = "minimize")
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
    param_set = function() readRDS(self$param_set_path)
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("lcbench", BenchmarkConfigLCBench)



#' @export
# augmented Branin as described in Wu et al. 2019
# https://arxiv.org/pdf/1903.04703.pdf
# y_min = 0.3978874 at x1 = 3.141593, x2 = 2.275000, fidelity = 1
# y_max = 485.3732 at x1 = 10, x2 = 15, fidelity = 1e-3
BenchmarkConfigBranin = R6Class("BenchmarkConfigBranin",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Branin") {
      super$initialize(
        id,
        workdir = NULL,
        model_name = "branin",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
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

    get_objective = function(max_fidelity = FALSE) {
      if (max_fidelity) {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            xdt[, fidelity := fidelity]
            y = (xdt[["x2"]] - ((5.1 / (4 * pi^2)) - 0.1 * (1 - xdt[["fidelity"]])) * xdt[["x1"]]^2 + (5 / pi) * xdt[["x1"]] - 6) ^ 2 +
              10 * (1 - (1 / (8 * pi))) * cos(xdt[["x1"]]) + 10
            data.table(y = y)
          },
          domain = ps(x1 = p_dbl(lower = -5, upper = 10), x2 = p_dbl(lower = 0, upper = 15)),
          codomain = self$codomain,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        bbotk::ObjectiveRFunDt$new(
        fun = function(xdt) {
          y = (xdt[["x2"]] - ((5.1 / (4 * pi^2)) - 0.1 * (1 - xdt[["fidelity"]])) * xdt[["x1"]]^2 + (5 / pi) * xdt[["x1"]] - 6) ^ 2 +
            10 * (1 - (1 / (8 * pi))) * cos(xdt[["x1"]]) + 10
          data.table(y = y)
        },
        domain = self$param_set,
        codomain = self$codomain
        )
      }
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
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("branin", BenchmarkConfigBranin)



#' @export
BenchmarkConfigBraninSurrogate = R6Class("BenchmarkConfigBraninSurrogate",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "BraninSurrogate", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "branin_surrogate",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },
    get_objective = function(max_fidelity = FALSE, retrafo = TRUE) {
      codomain = self$codomain$clone(deep = TRUE)
      if (max_fidelity) {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = ps(x1 = p_dbl(lower = -5, upper = 10), x2 = p_dbl(lower = 0, upper = 15)),
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = self$opt_param_set,
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo
        )
      }
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_branin_surrogate(self)
      private$.data
    },
    param_set = function() {
      ps(
        x1 = p_dbl(lower = -5, upper = 10),
        x2 = p_dbl(lower = 0, upper = 15),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("branin_surrogate", BenchmarkConfigBraninSurrogate)



#' @export
# augmented Hartmann as described in Kandasamy et al. 2017
# p = 2, d = 3
# y_min = -3.86278 at x1 = 0.114614, x2 = 0.555649, x3 = 0.852547, fidelity = 1
# y_max?
BenchmarkConfigHartmann3d = R6Class("BenchmarkConfigHartmann3d",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Hartmann3d") {
      super$initialize(
        id,
        workdir = NULL,
        model_name = "hartmann3d",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
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

    get_objective = function(max_fidelity = FALSE) {
      if (max_fidelity) {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            xdt[, fidelity := fidelity]
            x = xdt[, - "fidelity"]
            alpha = c(1, 1.2, 3, 3.2)
            A = matrix(c(3, 10, 30,
                         0.1, 10, 35,
                         3, 10, 30,
                         0.1, 10, 35), nrow = 4, ncol = 3, byrow = TRUE)
            P = 10^(-4) * matrix(c(3689, 1170, 2673,
                                   4699, 4387, 7470,
                                   1091, 8732, 5547,
                                   381, 5743, 8828), nrow = 4, ncol = 3, byrow = TRUE)
            p = 2
            map_dtr(seq_len(NROW(xdt)), function(row_id) {
              y = 0
              for (i in 1:4) {
                ai = if (i <= p) 0.1 * (1 - xdt[row_id, ][["fidelity"]]) else 0
                y = y + ((alpha[i] - ai) * exp(- sum(A[i, ] * ((x[row_id, ] - P[i, ]) ^ 2))))
              }
              data.table(y = -y)
            })
          },
          domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1), x3 = p_dbl(lower = 0, upper = 1)),
          codomain = self$codomain,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))

        )
      } else {      
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt) {
            x = xdt[, - "fidelity"]
            alpha = c(1, 1.2, 3, 3.2)
            A = matrix(c(3, 10, 30,
                         0.1, 10, 35,
                         3, 10, 30,
                         0.1, 10, 35), nrow = 4, ncol = 3, byrow = TRUE)
            P = 10^(-4) * matrix(c(3689, 1170, 2673,
                                   4699, 4387, 7470,
                                   1091, 8732, 5547,
                                   381, 5743, 8828), nrow = 4, ncol = 3, byrow = TRUE)
            p = 2
            map_dtr(seq_len(NROW(xdt)), function(row_id) {
              y = 0
              for (i in 1:4) {
                ai = if (i <= p) 0.1 * (1 - xdt[row_id, ][["fidelity"]]) else 0
                y = y + ((alpha[i] - ai) * exp(- sum(A[i, ] * ((x[row_id, ] - P[i, ]) ^ 2))))
              }
              data.table(y = -y)
            })
          },
          domain = self$param_set,
          codomain = self$codomain
        )
      }
    }
  ),
  active = list(
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = 0, upper = 1),
        x3 = p_dbl(lower = 0, upper = 1),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("hartmann3d", BenchmarkConfigHartmann3d)



#' @export
BenchmarkConfigHartmann3dSurrogate = R6Class("BenchmarkConfigHartmann3dSurrogate",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Hartmann3dSurrogate", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "hartmann3d_surrogate",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },
    get_objective = function(max_fidelity = FALSE, retrafo = TRUE) {
      codomain = self$codomain$clone(deep = TRUE)
      if (max_fidelity) {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1), x3 = p_dbl(lower = 0, upper = 1)),
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = self$opt_param_set,
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo
        )
      }
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_hartmann_surrogate(self)
      private$.data
    },
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = 0, upper = 1),
        x3 = p_dbl(lower = 0, upper = 1),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("hartmann3d_surrogate", BenchmarkConfigHartmann3dSurrogate)



#' @export
# augmented Hartmann as described in Kandasamy et al. 2017
# p = 2, d = 6
# y_min = -3.322368 at x1 = 0.2016896, x2 = 0.1500236, x3 = 0.4768785, x4 = 0.2753365, x5 = 0.3116548, x6 = 0.6573037, fidelity = 1
# y_max?
BenchmarkConfigHartmann6d = R6Class("BenchmarkConfigHartmann6d",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Hartmann6d") {
      super$initialize(
        id,
        workdir = NULL,
        model_name = "hartmann6d",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
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

    get_objective = function(max_fidelity = FALSE) {
      if (max_fidelity) {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            xdt[, fidelity := fidelity]
            x = xdt[, - "fidelity"]
            alpha = c(1, 1.2, 3, 3.2)
            A = matrix(c(10, 3, 17, 3.5, 1.7, 8,
                         0.05, 10, 17, 0.1, 8, 14,
                         3, 3.5, 1.7, 10, 17, 8,
                         17, 8, 0.05, 10, 0.1, 14), nrow = 4, ncol = 6, byrow = TRUE)
            P = 10^(-4) * matrix(c(1312, 1696, 5569, 124, 8283, 5886,
                                   2329, 4135, 8307, 3736, 1004, 9991,
                                   2348, 1451, 3522, 2883, 3047, 6650,
                                   4047, 8828, 8732, 5743, 1091, 381), nrow = 4, ncol = 6, byrow = TRUE)
            p = 2
            map_dtr(seq_len(NROW(xdt)), function(row_id) {
              y = 0
              for (i in 1:4) {
                ai = if (i <= p) 0.1 * (1 - xdt[row_id, ][["fidelity"]]) else 0
                y = y + ((alpha[i] - ai) * exp(- sum(A[i, ] * ((x[row_id, ] - P[i, ]) ^ 2))))
              }
              data.table(y = -y)
            })
          },
          domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1), x3 = p_dbl(lower = 0, upper = 1),
                      x4 = p_dbl(lower = 0, upper = 1), x5 = p_dbl(lower = 0, upper = 1), x6 = p_dbl(lower = 0, upper = 1)),
          codomain = self$codomain,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))

        )
      } else {      
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt) {
            x = xdt[, - "fidelity"]
            alpha = c(1, 1.2, 3, 3.2)
            A = matrix(c(10, 3, 17, 3.5, 1.7, 8,
                         0.05, 10, 17, 0.1, 8, 14,
                         3, 3.5, 1.7, 10, 17, 8,
                         17, 8, 0.05, 10, 0.1, 14), nrow = 4, ncol = 6, byrow = TRUE)
            P = 10^(-4) * matrix(c(1312, 1696, 5569, 124, 8283, 5886,
                                   2329, 4135, 8307, 3736, 1004, 9991,
                                   2348, 1451, 3522, 2883, 3047, 6650,
                                   4047, 8828, 8732, 5743, 1091, 381), nrow = 4, ncol = 6, byrow = TRUE)
            p = 2
            map_dtr(seq_len(NROW(xdt)), function(row_id) {
              y = 0
              for (i in 1:4) {
                ai = if (i <= p) 0.1 * (1 - xdt[row_id, ][["fidelity"]]) else 0
                y = y + ((alpha[i] - ai) * exp(- sum(A[i, ] * ((x[row_id, ] - P[i, ]) ^ 2))))
              }
              data.table(y = -y)
            })
          },
          domain = self$param_set,
          codomain = self$codomain
        )
      }
    }
  ),
  active = list(
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = 0, upper = 1),
        x3 = p_dbl(lower = 0, upper = 1),
        x4 = p_dbl(lower = 0, upper = 1),
        x5 = p_dbl(lower = 0, upper = 1),
        x6 = p_dbl(lower = 0, upper = 1),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("hartmann6d", BenchmarkConfigHartmann6d)



#' @export
BenchmarkConfigHartmann6dSurrogate = R6Class("BenchmarkConfigHartmann6dSurrogate",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Hartmann6dSurrogate", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "hartmann6d_surrogate",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },
    get_objective = function(max_fidelity = FALSE, retrafo = TRUE) {
      codomain = self$codomain$clone(deep = TRUE)
      if (max_fidelity) {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1), x3 = p_dbl(lower = 0, upper = 1), x4 = p_dbl(lower = 0, upper = 1), x5 = p_dbl(lower = 0, upper = 1), x6 = p_dbl(lower = 0, upper = 1)),
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = self$opt_param_set,
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo
        )
      }
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_hartmann_surrogate(self)
      private$.data
    },
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = 0, upper = 1),
        x3 = p_dbl(lower = 0, upper = 1),
        x4 = p_dbl(lower = 0, upper = 1),
        x5 = p_dbl(lower = 0, upper = 1),
        x6 = p_dbl(lower = 0, upper = 1),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("hartmann6d_surrogate", BenchmarkConfigHartmann6dSurrogate)



#' @export
# augmented Currin as described in Kandasamy et al. 2017
# https://arxiv.org/abs/1703.06240
# y_min?
# y_max?
BenchmarkConfigCurrin = R6Class("BenchmarkConfigCurrin",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Currin") {
      super$initialize(
        id,
        workdir = NULL,
        model_name = "currin",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
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

    get_objective = function(max_fidelity = FALSE) {
      if (max_fidelity) {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            xdt[, fidelity := fidelity]
            y = (1 - 0.1 * (1 - xdt[["fidelity"]]) * exp(- 1 / (2 * xdt[["x2"]]))) * ((2300 * (xdt[["x1"]] ^ 3) + 1900 * (xdt[["x1"]] ^ 2) + 2092 * xdt[["x1"]] + 60) / ((100 * (xdt[["x1"]] ^ 3) + 500 * (xdt[["x1"]] ^ 2) + 4 * xdt[["x1"]] + 20)))
            data.table(y = - y)  # we want to minimize by default
          },
          domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1)),
          codomain = self$codomain,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))

        )
      } else {      
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt) {
            y = (1 - (0.1 * (1 - xdt[["fidelity"]]) * exp(- 1 / (2 * xdt[["x2"]])))) * ((2300 * (xdt[["x1"]] ^ 3) + 1900 * (xdt[["x1"]] ^ 2) + 2092 * xdt[["x1"]] + 60) / ((100 * (xdt[["x1"]] ^ 3) + 500 * (xdt[["x1"]] ^ 2) + 4 * xdt[["x1"]] + 20)))
            data.table(y = - y)  # we want to minimize by default
          },
          domain = self$param_set,
          codomain = self$codomain
        )
      }
    }
  ),
  active = list(
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = 0, upper = 1),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("currin", BenchmarkConfigCurrin)


#' @export
BenchmarkConfigCurrinSurrogate = R6Class("BenchmarkConfigCurrinSurrogate",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "CurrinSurrogate", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "currin_surrogate",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },
    get_objective = function(max_fidelity = FALSE, retrafo = TRUE) {
      codomain = self$codomain$clone(deep = TRUE)
      if (max_fidelity) {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1)),
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = self$opt_param_set,
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo
        )
      }
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_currin_surrogate(self)
      private$.data
    },
    param_set = function() {
      ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = 0, upper = 1),
        fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("currin_surrogate", BenchmarkConfigCurrinSurrogate)



#' @export
# augmented Borehole as described in Kandasamy et al. 2017
# y_min?
# y_max?
BenchmarkConfigBorehole = R6Class("BenchmarkConfigBorehole",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "Borehole") {
      super$initialize(
        id,
        workdir = NULL,
        model_name = "borehole",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
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

    get_objective = function(max_fidelity = FALSE) {
      if (max_fidelity) {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            xdt[, fidelity := fidelity]
            f2 = (2 * pi * xdt[["x3"]] * (xdt[["x4"]] - xdt[["x6"]])) / (log(xdt[["x2"]] / xdt[["x1"]]) * (1 + ((2 * xdt[["x7"]] * xdt[["x3"]]) / (log(xdt[["x2"]] / xdt[["x1"]]) * (xdt[["x1"]] ^ 2) * xdt[["x8"]])) + (xdt[["x3"]] / xdt[["x5"]])))
            f1 = (5 * xdt[["x3"]] * (xdt[["x4"]] - xdt[["x6"]])) / (log(xdt[["x2"]] / xdt[["x1"]]) * (1.5 + ((2 * xdt[["x7"]] * xdt[["x3"]]) / (log(xdt[["x2"]] / xdt[["x1"]]) * (xdt[["x1"]] ^ 2) * xdt[["x8"]])) + (xdt[["x3"]] / xdt[["x5"]])))
            y = xdt[["fidelity"]] * f2 + (1 - xdt[["fidelity"]]) * f1
            data.table(y = - y)  # we want to minimize by default
          },
          domain = ps(x1 = p_dbl(lower = 0.05, upper = 0.15), x2 = p_dbl(lower = 100, upper = 50 * 1000), x3 = p_dbl(lower = 63.07 * 1000, upper = 115.6 * 1000), x4 = p_dbl(lower = 990, upper = 1110),
                      x5 = p_dbl(lower = 63.1, upper = 116), x6 = p_dbl(lower = 700, upper = 820), x7 = p_dbl(lower = 1120, upper = 1680), x8 = p_dbl(lower = 9855, upper = 12045)),
          codomain = self$codomain,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))

        )
      } else {      
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt) {
            f2 = (2 * pi * xdt[["x3"]] * (xdt[["x4"]] - xdt[["x6"]])) / (log(xdt[["x2"]] / xdt[["x1"]]) * (1 + ((2 * xdt[["x7"]] * xdt[["x3"]]) / (log(xdt[["x2"]] / xdt[["x1"]]) * (xdt[["x1"]] ^ 2) * xdt[["x8"]])) + (xdt[["x3"]] / xdt[["x5"]])))
            f1 = (5 * xdt[["x3"]] * (xdt[["x4"]] - xdt[["x6"]])) / (log(xdt[["x2"]] / xdt[["x1"]]) * (1.5 + ((2 * xdt[["x7"]] * xdt[["x3"]]) / (log(xdt[["x2"]] / xdt[["x1"]]) * (xdt[["x1"]] ^ 2) * xdt[["x8"]])) + (xdt[["x3"]] / xdt[["x5"]])))
            y = xdt[["fidelity"]] * f2 + (1 - xdt[["fidelity"]]) * f1
            data.table(y = - y)  # we want to minimize by default
          },
          domain = self$param_set,
          codomain = self$codomain
        )
      }
    }
  ),
  active = list(
    param_set = function() {
      ps(
         x1 = p_dbl(lower = 0.05, upper = 0.15),
         x2 = p_dbl(lower = 100, upper = 50 * 1000),
         x3 = p_dbl(lower = 63.07 * 1000, upper = 115.6 * 1000),
         x4 = p_dbl(lower = 990, upper = 1110),
         x5 = p_dbl(lower = 63.1, upper = 116),
         x6 = p_dbl(lower = 700, upper = 820),
         x7 = p_dbl(lower = 1120, upper = 1680),
         x8 = p_dbl(lower = 9855, upper = 12045),
         fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("borehole", BenchmarkConfigBorehole)



#' @export
BenchmarkConfigBoreholeSurrogate = R6Class("BenchmarkConfigBoreholeSurrogate",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "BoreholeSurrogate", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "borehole_surrogate",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },
    get_objective = function(max_fidelity = FALSE, retrafo = TRUE) {
      codomain = self$codomain$clone(deep = TRUE)
      if (max_fidelity) {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = ps(x1 = p_dbl(lower = 0.05, upper = 0.15), x2 = p_dbl(lower = 100, upper = 50 * 1000), x3 = p_dbl(lower = 63.07 * 1000, upper = 115.6 * 1000), x4 = p_dbl(lower = 990, upper = 1110),
                      x5 = p_dbl(lower = 63.1, upper = 116), x6 = p_dbl(lower = 700, upper = 820), x7 = p_dbl(lower = 1120, upper = 1680), x8 = p_dbl(lower = 9855, upper = 12045)),
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = self$opt_param_set,
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo
        )
      }
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_borehole_surrogate(self)
      private$.data
    },
    param_set = function() {
      ps(x1 = p_dbl(lower = 0.05, upper = 0.15),
         x2 = p_dbl(lower = 100, upper = 50 * 1000),
         x3 = p_dbl(lower = 63.07 * 1000, upper = 115.6 * 1000),
         x4 = p_dbl(lower = 990, upper = 1110),
         x5 = p_dbl(lower = 63.1, upper = 116),
         x6 = p_dbl(lower = 700, upper = 820),
         x7 = p_dbl(lower = 1120, upper = 1680),
         x8 = p_dbl(lower = 9855, upper = 12045),
         fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("borehole_surrogate", BenchmarkConfigBoreholeSurrogate)



svm_on_phoneme = function(learner, task, train_id, test_id, psvals) {
  task_train = task$clone(deep = TRUE)$filter(rows = train_id)
  task_test = task$clone(deep = TRUE)$filter(rows = test_id)
  learner$param_set$values = mlr3misc::insert_named(learner$param_set$values, psvals)
  learner$train(task_train)
  p = learner$predict(task_test)
  data.table(y = unname(p$score(dictionary_sugar_get(mlr3::mlr_measures, "classif.logloss"))))
}

#' @export
# FIXME: can only be evaluated on the discrete fidelity steps below
BenchmarkConfigSVMPhoneme = R6Class("BenchmarkConfigSVMPhoneme",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "SVMPhoneme") {
    self$learner = mlr3learners::LearnerClassifSVM$new()
    self$learner$param_set$values$kernel = "radial"
    self$learner$param_set$values$type = "C-classification"
    self$learner$predict_type = "prob"
    self$task = mlr3misc::dictionary_sugar_get(mlr3::mlr_tasks, "oml", data_id = 1489)
    # different fidelities of train size, 1/(2^(0:9)) fidelity steps
    self$train_id = list("10" = 1:4864, "9" = 1:2432, "8" = 1:1216, "7" = 1:608, "6" = 1:304, "5" = 1:152, "4" = 1:76, "3" = 1:38, "2" = 1:19, "1" = 1:10)
    self$test_id = 4865:5404
      super$initialize(
        id,
        workdir = NULL,
        model_name = "svmphoneme",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = c("mlr3", "mlr3oml", "mlr3learners")
      )
    },

    learner = NULL,
    task = NULL,
    train_id = NULL,
    test_id = NULL,

    setup = function() {
      message("no setup necessary.")
    },

    get_objective = function(max_fidelity = FALSE) {
      if (max_fidelity) {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            xdt[, fidelity := fidelity]
            map_dtr(seq_len(nrow(xdt)), function(i) {
              psvals = as.list(xdt[i, - "fidelity"])
              train_id = self$train_id[[match(xdt[i, ][["fidelity"]], 1/(2^(0:9)))]]
              svm_on_phoneme(learner = self$learner, task = self$task, train_id = train_id, test_id = self$test_id, psvals = psvals)
            })
          },
          domain = ps(cost = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)), gamma = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)), tolerance = p_dbl(lower = -10, upper = log(2), tags = "log", trafo = function(x) exp(x))),
          codomain = self$codomain,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget")),
          check_values = FALSE
        )
      } else {
        bbotk::ObjectiveRFunDt$new(
          fun = function(xdt) {
            map_dtr(seq_len(nrow(xdt)), function(i) {
              psvals = as.list(xdt[i, - "fidelity"])
              train_id = self$train_id[[match(xdt[i, ][["fidelity"]], 1/(2^(0:9)))]]
              svm_on_phoneme(learner = self$learner, task = self$task, train_id = train_id, test_id = self$test_id, psvals = psvals)
            })
          },
          domain = self$param_set,
          codomain = self$codomain,
          check_values = FALSE
        )
      }
    }
  ),
  active = list(
    param_set = function() {
      ps(
         cost = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)),
         gamma = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)),
         tolerance = p_dbl(lower = -10, upper = log(2), tags = "log", trafo = function(x) exp(x)),
         fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("svmphoneme", BenchmarkConfigSVMPhoneme)



#' @export
BenchmarkConfigSVMPhonemeSurrogate = R6Class("BenchmarkConfigSVMPhonemeSurrogate",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "BenchmarkConfigSVMPhonemeSurrogate", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "svmphoneme_surrogate",
        param_set_file = "param_set.rds",
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = "y",
        codomain = ps(
          y = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    },
    get_objective = function(max_fidelity = FALSE, retrafo = TRUE) {
      codomain = self$codomain$clone(deep = TRUE)
      if (max_fidelity) {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = ps(cp = p_dbl(lower = 0.001, upper = 0.1), minsplit = p_int(lower = 1, upper = 30), maxdepth = p_int(lower = 1, upper = 30)),
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo,
          constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1, tags = "budget"))
        )
      } else {
        ObjectiveONNX$new(
          model_path = self$onnx_model_path,
          data_order = readRDS(self$data_order_path),
          trafo_dict = readRDS(self$dicts_path),
          domain = self$opt_param_set,
          full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
          codomain = codomain,
          task = NULL,
          retrafo = retrafo
        )
      }
    }
  ),
  active = list(
    data = function() {
      if (is.null(private$.data)) private$.data = preproc_svmphoneme_surrogate(self)
      private$.data
    },
    param_set = function() {
      ps(
         cp = p_dbl(lower = 0.001, upper = 0.1),
         minsplit = p_int(lower = 1, upper = 30),
         maxdepth = p_int(lower = 1, upper = 30),
         fidelity = p_dbl(lower = 1e-3, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("svmphoneme_surrogate", BenchmarkConfigSVMPhonemeSurrogate)



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
        workdir = NULL,
        model_name = "shekel",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
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
      bbotk::ObjectiveRFunDt$new(
        fun = function(xdt) {
          xdt[["fidelity"]] = 4 * xdt[["fidelity"]]  # scale to from [0, 1] to [0, 4]
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
        fidelity = p_dbl(lower = 1e-8, upper = 1, tags = "budget")
      )
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("shekel", BenchmarkConfigShekel)



#' @export
# https://esa.github.io/pagmo2/docs/cpp/problems/zdt.html
# 10d zdt6 (n = 10)
# Zitzler, Deb & Thiele, 2000
# Fidelity is incorporated in F1, see below
# Pareto-optimal solutions are nonuniformly distributed along the global Pareto front
# Pareto front is biased for solutions where F1 is near 1
# Density of solutions is low near Pareto front and highest away from it
# F1 is partially (the exp part) approximated via second order taylor expansion around a = 0.5
# and the extent of the second order part being incorporated is scaled by the
# fidelity (i.e, fidelity of 0 --> first order approximation, fidelity of 1 --> second order approximation)
BenchmarkConfigZDT6 = R6Class("BenchmarkConfigZDT6",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "ZDT6") {
      super$initialize(
        id,
        workdir = NULL,
        model_name = "zdt1",
        param_set_file = NULL,
        data_file = NULL,
        data_order_file = "NULL",
        dicts_file = NULL,
        keras_model_file = NULL,
        onnx_model_file = NULL,
        target_variables = c("F1", "F2"),
        codomain = ps(
          F1 = p_dbl(lower = -Inf, upper = Inf, tags = "minimize"),
          F2 = p_dbl(lower = -Inf, upper = Inf, tags = "minimize")
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
          F1_first = function(a) exp(-4 * a)
          F1_second = function(a) (sin(6 * pi * a) ^ 6)

          F1_first_d1 = function(a) -4 * exp(-4 * a)
          F1_first_d2 = function(a) 16 * exp(-4 * a)

          F1_true = 1 - (F1_first(xdt[["x1"]]) * F1_second(xdt[["x1"]]))
          F1_taylor_05 = 1 - (F1_first(0.5) + F1_first_d1(0.5) * (xdt[["x1"]] - 0.5) + xdt[["fidelity"]] * ((F1_first_d2(0.5) / 2) * ((xdt[["x1"]] - 0.5) ^ 2))) * F1_second(xdt[["x1"]])
          F1_taylor_05 = pmin(1, pmax(0, F1_taylor_05))  # force F1 between [0, 1]

          g = 1 + 9 * ((rowSums(xdt[, 2:10]) / 9) ^ (1 / 4))
          F2 = g * (1 - ((F1_taylor_05 / g) ^ 2))
          data.table(F1 = F1_taylor_05, F2 = F2, g = g, F1_true = F1_true)
        },
        domain = self$param_set,
        codomain = self$codomain
      )
    }
  ),
  active = list(
    param_set = function() {
      ps = map(1:10, function(i) ParamDbl$new(paste0("x", i), lower = 0, upper = 1))
      ps = append(ps, ParamDbl$new("fidelity", lower = 1e-8, upper = 1, tags = "budget"))
      ParamSet$new(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("zdt6", BenchmarkConfigZDT6)



#' @export
BenchmarkConfigRBv2SVM = R6Class("BenchmarkConfigRBv2SVM",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_SVM", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_svm",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        kernel = p_fct(levels = c("linear", "polynomial", "radial")),
        cost = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)),
        gamma = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x), depends = kernel == "radial"),
        tolerance = p_dbl(lower = -10, upper = log(2), tags = "log", trafo = function(x) exp(x)),
        degree = p_int(lower = 2, upper = 5, depends = kernel == "polynomial"),
        trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
        repl = p_int(lower = 1, upper = 10, tag = "budget"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
          "11", "1111", "12", "1220", "14", "1457", "1461", "1462", "1464",
          "1468", "1475", "1476", "1478", "1479", "1480", "1485", "1486",
          "1487", "1489", "1494", "1497", "15", "1501", "1510", "1515",
          "16", "18", "181", "182", "188", "22", "23", "23381", "24", "28",
          "29", "3", "300", "307", "31", "312", "32", "334", "37", "375",
          "377", "38", "40496", "40498", "40499", "40536", "40670", "40701",
          "40900", "40966", "40975", "40978", "40979", "40981", "40982",
          "40983", "40984", "40994", "41138", "41142", "41143", "41146",
          "41156", "41157", "41163", "41164", "41212", "41216", "4134",
          "4135", "4154", "42", "44", "4534", "4538", "458", "46", "469",
          "470", "50", "54", "60", "6332"),
          tags = "task_id")
        )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_svm(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_svm", BenchmarkConfigRBv2SVM)



#' @export
BenchmarkConfigRBv2ranger = R6Class("BenchmarkConfigRBv2ranger",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_ranger", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_ranger",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
       ps(
        num.trees = p_int(lower = 1, upper = 2000),
        # replace = p_lgl(),
        sample.fraction = p_dbl(lower = 0.1, upper = 1),
        mtry.power = p_int(lower = 0, upper = 1),
        respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
        min.node.size = p_int(lower = 1, upper = 100),
        splitrule = p_fct(levels = c("gini", "extratrees")),
        num.random.splits = p_int(lower = 1, upper = 100, default = 1L, depends = splitrule == "extratrees"),
        trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
        repl = p_int(lower = 1, upper = 10, tag = "budget"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
          "11", "1111", "12", "1220", "14", "1457", "1461", "1462", "1464",
          "1468", "1475", "1476", "1478", "1479", "1480", "1485", "1486",
          "1487", "1489", "1494", "1497", "15", "1501", "151", "1510",
          "1515", "1590", "16", "18", "181", "182", "188", "22", "23",
          "23381", "23512", "23517", "24", "28", "29", "3", "300", "307",
          "31", "312", "32", "334", "37", "375", "377", "38", "40496",
          "40498", "40499", "40536", "40668", "40670", "40685", "40701",
          "40900", "40927", "40966", "40975", "40978", "40979", "40981",
          "40982", "40983", "40984", "40994", "41027", "41138", "41142",
          "41143", "41146", "41150", "41156", "41157", "41159", "41161",
          "41162", "41163", "41164", "41165", "41166", "41168", "41212",
          "41216", "41278", "4134", "4135", "4154", "42", "44", "4534",
          "4538", "4541", "458", "46", "469", "470", "50", "54", "6", "60",
          "6332")
          , tags = "task_id"
        )
      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_ranger(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_ranger", BenchmarkConfigRBv2ranger)



#' @export
BenchmarkConfigRBv2glmnet = R6Class("BenchmarkConfigRBv2glmnet",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_glmnet", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_glmnet",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        alpha = p_dbl(lower = 0, upper = 1, default = 1),
        s = p_dbl(lower = -7, upper = 7, default = 0, tags = "log", trafo = function(x) exp(x)),
        trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
        repl = p_int(lower = 1, upper = 10, tag = "budget"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
          "11", "1111", "12", "14", "1461", "1462", "1464", "1468", "1475",
          "1476", "1478", "1479", "1480", "1485", "1486", "1487", "1489",
          "1494", "1497", "15", "1501", "1510", "1515", "1590", "16", "18",
          "181", "182", "188", "22", "23", "23381", "23512", "24", "28",
          "29", "3", "307", "31", "312", "32", "334", "37", "375", "377",
          "38", "40496", "40498", "40499", "40536", "40668", "40670", "40701",
          "40900", "40966", "40975", "40978", "40979", "40981", "40982",
          "40983", "40984", "40994", "41138", "41142", "41143", "41146",
          "41156", "41157", "41159", "41161", "41162", "41212", "41278",
          "4134", "4135", "4154", "42", "44", "4534", "4538", "4541", "458",
          "46", "469", "470", "50", "54", "60", "6332")
          , tags = "task_id"
        )
      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_glmnet(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_glmnet", BenchmarkConfigRBv2glmnet)



#' @export
BenchmarkConfigRBv2xgboost = R6Class("BenchmarkConfigRBv2xgboost",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_xgboost", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_xgboost",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        booster = p_fct(levels = c("gblinear", "gbtree", "dart")),
        nrounds = p_dbl(lower = 2, upper = 8, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x), depends = booster %in% c("dart", "gbtree")),
        gamma = p_dbl(lower = -10, upper = 2, tags = "log", trafo = function(x) exp(x), depends = booster %in% c("dart", "gbtree")),
        lambda = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
        alpha = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
        subsample = p_dbl(lower = 0.1, upper = 1),
        max_depth = p_int(lower = 1, upper = 15, depends = booster %in% c("dart", "gbtree")),
        min_child_weight = p_dbl(lower = 1, upper = 5, tags = "log", trafo = function(x) exp(x), depends = booster %in% c("dart", "gbtree")),
        colsample_bytree = p_dbl(lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
        colsample_bylevel = p_dbl(lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
        rate_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
        skip_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
        trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
        repl = p_int(lower = 1, upper = 10, tag = "budget"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
          "11", "1111", "12", "1220", "14", "1457", "1461", "1462", "1464",
          "1468", "1475", "1476", "1478", "1479", "1480", "1485", "1486",
          "1487", "1489", "1494", "1497", "15", "1501", "151", "1510",
          "1515", "1590", "16", "18", "181", "182", "188", "22", "23",
          "23381", "23512", "24", "28", "29", "3", "300", "307", "31",
          "312", "32", "334", "37", "375", "377", "38", "40496", "40498",
          "40499", "40536", "40668", "40670", "40701", "40900", "40927",
          "40966", "40975", "40978", "40979", "40981", "40982", "40983",
          "40984", "40994", "41138", "41142", "41143", "41146", "41150",
          "41156", "41157", "41159", "41161", "41162", "41163", "41164",
          "41165", "41166", "41212", "41216", "41278", "4134", "4135",
          "4154", "42", "44", "4534", "4538", "4541", "458", "46", "469",
          "470", "50", "54", "60", "6332"),
          tags = "task_id"
        )
      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_xgboost(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_xgboost", BenchmarkConfigRBv2xgboost)



#' @export
BenchmarkConfigRBv2rpart = R6Class("BenchmarkConfigRBv2rpart",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_glmnet", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_rpart",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps = ps(
        cp = p_dbl(lower = -7, upper = 0, default = log(0.01), tags = "log", trafo = function(x) exp(x)),
        maxdepth = p_int(lower = 1, upper = 30, default = 30),
        minbucket = p_int(lower = 1, upper = 100, default = 1),
        minsplit = p_int(lower = 1, upper = 100, default = 20),
        trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
        repl = p_int(lower = 1, upper = 10, tag = "budget"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
          "11", "1111", "12", "14", "1457", "1461", "1462", "1464", "1468",
          "1475", "1476", "1478", "1479", "1480", "1485", "1486", "1487",
          "1489", "1494", "1497", "15", "1501", "1510", "1515", "1590",
          "16", "18", "181", "182", "188", "22", "23", "23381", "23512",
          "24", "28", "29", "3", "300", "307", "31", "312", "32", "334",
          "37", "375", "377", "38", "40496", "40498", "40499", "40536",
          "40670", "40701", "40900", "40927", "40966", "40975", "40978",
          "40979", "40981", "40982", "40983", "40984", "40994", "41138",
          "41142", "41143", "41146", "41156", "41157", "41159", "41161",
          "41162", "41163", "41164", "41165", "41212", "4134", "4135",
          "4154", "42", "44", "4534", "4538", "458", "46", "469", "470",
          "50", "54", "60", "6332")
          , tags = "task_id"
        )
      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_rpart(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_rpart", BenchmarkConfigRBv2rpart)



#' @export
BenchmarkConfigRBv2aknn = R6Class("BenchmarkConfigRBv2aknn",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_aknn", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_aknn",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        k = p_int(lower = 1L, upper = 50L),
        distance = p_fct(levels = c("l2", "cosine", "ip"), default = "l2"),
        M = p_int(lower = 18L, upper = 50L),
        ef = p_dbl(lower = 2, upper = 6, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        ef_construction = p_dbl(lower = 2, upper = 7, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
        repl = p_int(lower = 1, upper = 10, tag = "budget"),
        num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
        task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
          "11", "1111", "12", "1220", "14", "1457", "1461", "1462", "1464",
          "1468", "1475", "1476", "1478", "1479", "1480", "1485", "1486",
          "1487", "1489", "1494", "1497", "15", "1501", "1510", "1515",
          "16", "18", "181", "182", "188", "22", "23", "23381", "24", "28",
          "29", "3", "300", "307", "31", "312", "32", "334", "37", "375",
          "377", "38", "40496", "40498", "40499", "40536", "40670", "40701",
          "40900", "40966", "40975", "40978", "40979", "40981", "40982",
          "40983", "40984", "40994", "41138", "41142", "41143", "41146",
          "41156", "41157", "41159", "41161", "41162", "41163", "41164",
          "41165", "41212", "41278", "4134", "4154", "42", "44", "4534",
          "4538", "458", "46", "469", "470", "50", "54", "60", "6332"),
          tags = "task_id"
        )
      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_aknn(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_aknn", BenchmarkConfigRBv2aknn)



#' @export
BenchmarkConfigSuperRBv2 = R6Class("BenchmarkConfigSuperRBv2",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "RBv2_super", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "rbv2_super",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("mmce", "f1", "auc", "logloss", "timetrain", "timepredict"),
        codomain = ps(
          mmce = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          f1 = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          auc = p_dbl(lower = 0, upper = 1, tags = "maximize"),
          logloss = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timetrain = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          timepredict = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      pc = ps(
          # svm
          svm.kernel = p_fct(levels = c("linear", "polynomial", "radial")),
          svm.cost = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)),
          svm.gamma = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x), depends = svm.kernel == "radial"),
          svm.tolerance = p_dbl(lower = -10, upper = log(2), tags = "log", trafo = function(x) exp(x)),
          svm.degree = p_int(lower = 2, upper = 5, depends = svm.kernel == "polynomial"),
          # glmnet
          glmnet.alpha = p_dbl(lower = 0, upper = 1, default = 1),
          glmnet.s = p_dbl(lower = -7, upper = 7, default = 0, tags = "log", trafo = function(x) exp(x)),
          # rpart
          rpart.cp = p_dbl(lower = -7, upper = 0, default = log(0.01), tags = "log", trafo = function(x) exp(x)),
          rpart.maxdepth = p_int(lower = 1, upper = 30, default = 30),
          rpart.minbucket = p_int(lower = 1, upper = 100, default = 1),
          rpart.minsplit = p_int(lower = 1, upper = 100, default = 20),
          # ranger
          ranger.num.trees = p_int(lower = 1, upper = 2000),
          ranger.sample.fraction = p_dbl(lower = 0.1, upper = 1),
          ranger.mtry.power = p_int(lower = 0, upper = 1),
          ranger.respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
          ranger.min.node.size = p_int(lower = 1, upper = 100),
          ranger.splitrule = p_fct(levels = c("gini", "extratrees")),
          ranger.num.random.splits = p_int(lower = 1, upper = 100, default = 1L, depends = ranger.splitrule == "extratrees"),
          # aknn
          aknn.k = p_int(lower = 1L, upper = 50L),
          aknn.distance = p_fct(levels = c("l2", "cosine", "ip"), default = "l2"),
          aknn.M = p_int(lower = 18L, upper = 50L),
          aknn.ef = p_dbl(lower = 2, upper = 6, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
          aknn.ef_construction = p_dbl(lower = 2, upper = 7, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
          # xgboost
          xgboost.booster = p_fct(levels = c("gblinear", "gbtree", "dart")),
          xgboost.nrounds = p_dbl(lower = 2, upper = 8, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
          xgboost.eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("dart", "gbtree")),
          xgboost.gamma = p_dbl(lower = -10, upper = 2, tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("dart", "gbtree")),
          xgboost.lambda = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
          xgboost.alpha = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
          xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
          xgboost.max_depth = p_int(lower = 1, upper = 15, depends = xgboost.booster %in% c("dart", "gbtree")),
          xgboost.min_child_weight = p_dbl(lower = 1, upper = 5, tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("dart", "gbtree")),
          xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, depends = xgboost.booster %in% c("dart", "gbtree")),
          xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, depends = xgboost.booster %in% c("dart", "gbtree")),
          xgboost.rate_drop = p_dbl(lower = 0, upper = 1, depends = xgboost.booster == "dart"),
          xgboost.skip_drop = p_dbl(lower = 0, upper = 1, depends = xgboost.booster == "dart"),
          # learner
          trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
          repl = p_int(lower = 1, upper = 10, tag = "budget"),
          num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
          learner = p_fct(levels = c("aknn", "glmnet", "ranger", "rpart", "svm", "xgboost")),
          task_id = p_fct(levels = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
            "11", "1111", "12", "14", "1461", "1462", "1464", "1468", "1475",
            "1476", "1478", "1479", "1480", "1485", "1486", "1487", "1489",
            "1494", "1497", "15", "1501", "1510", "1515", "16", "18", "181",
            "182", "188", "22", "23", "23381", "24", "28", "29", "3", "307",
            "31", "312", "32", "334", "37", "375", "377", "38", "40496",
            "40498", "40499", "40536", "40670", "40701", "40900", "40966",
            "40975", "40978", "40979", "40981", "40982", "40983", "40984",
            "40994", "41138", "41142", "41143", "41146", "41156", "41157",
            "41212", "4134", "4154", "42", "44", "4534", "4538", "458", "46",
            "469", "470", "50", "54", "60", "6332"),
            tags = "task_id"
          )
      )
      # Add dependencies
      map(pc$params$learner$levels, function(x) {
          nms = names(pc$params)[startsWith(names(pc$params), x)]
          map(nms, function(nm) pc$add_dep(nm, "learner", CondEqual$new(x)))
      })
      pc
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_rbv2_super(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "repl"))
      ps$add(ParamInt$new("repl", lower = 10, upper = 10, default = 10, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("rbv2_super", BenchmarkConfigSuperRBv2)


#' @export
BenchmarkConfigFCNet = R6Class("BenchmarkConfigFCNet",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "FCNet", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "fcnet_tabular_benchmarks",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("valid_loss", "runtime", "n_params"),
        codomain = ps(
          valid_loss = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          runtime = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          n_params = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        epoch = p_int(lower = 1L, upper = 100L, tags = "budget"),
        activation_fn_1 = p_fct(levels = c("relu", "tanh")),
        activation_fn_2 = p_fct(levels = c("relu", "tanh")),
        batch_size = p_int(lower = 8L, upper = 64L),
        dropout_1 = p_dbl(lower = 0, upper = 0.6),
        dropout_2 = p_dbl(lower = 0, upper = 0.6),
        init_lr = p_dbl(lower = -8, upper = -3, tags = "log", trafo = function(x) exp(x)),
        lr_schedule = p_fct(levels = c("const", "cosine")),
        n_units_1 = p_dbl(lower = 2, upper = 7, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        n_units_2 = p_dbl(lower = 2, upper = 7, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        replication = p_int(lower = 0L, upper = 4L, tags = "budget"),
        task = p_fct(levels = c("fcnet_protein_structure", "fcnet_parkinsons_telemonitoring", "fcnet_naval_propulsion", "fcnet_slice_localization"), tags = "task_id")

      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_fcnet(self)
      private$.data
    },
    opt_param_set = function() {
      ps = self$param_set$clone()
      ps$subset(setdiff(ps$ids(), "replication"))
      ps$add(ParamInt$new("replication", lower = 4, upper = 4, default = 4L, tags = "constant"))
      return(ps)
    }
  )
)
#' @include BenchmarkConfig.R
benchmark_configs$add("fcnet", BenchmarkConfigFCNet)



BenchmarkConfigTaskSet = R6Class("BenchmarkConfigTaskSet",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = "TaskSet", workdir) {
      super$initialize(
        id,
        workdir = workdir,
        model_name = "task_set",
        param_set_file = NULL,
        data_file = "data.rds",
        data_order_file = "data_order.rds",
        dicts_file = "dicts.rds",
        keras_model_file = "model.hdf5",
        onnx_model_file = "model.onnx",
        target_variables = c("train", "valid1", "valid2", "test"),
        codomain = ps(
          train = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          valid1 = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          valid2 = p_dbl(lower = 0, upper = Inf, tags = "minimize"),
          test = p_dbl(lower = 0, upper = Inf, tags = "minimize")
        ),
        packages = NULL
      )
    }
  ),
  active = list(
    param_set = function() {
      ps(
        epoch = p_int(lower = 1L, upper = 10000L, tags = "budget"),
        learning_rate = p_dbl(lower = -19, upper = 2, tags = "log", trafo = function(x) exp(x)),
        beta1 = p_dbl(lower = -9, upper = 0, tags = "log", trafo = function(x) exp(x)),
        beta2 = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x)),
        epsilon = p_dbl(lower = -23, upper = 7, tags = "log", trafo = function(x) exp(x)),
        l1 = p_dbl(lower = -19, upper = 2, tags = "log", trafo = function(x) exp(x)),
        l2 = p_dbl(lower = -19, upper = 2, tags = "log", trafo = function(x) exp(x)),
        linear_decay = p_dbl(lower = -16, upper = -9, tags = "log", trafo = function(x) exp(x)),
        exponential_decay = p_dbl(lower = -14, upper = -7, tags = "log", trafo = function(x) exp(x)),
        task_name = p_fct(levels =
          c("Associative_GRU128_BS128_Pairs10_Tokens50", "Associative_GRU256_BS128_Pairs20_Tokens50",
            "Associative_LSTM128_BS128_Pairs10_Tokens50", "Associative_LSTM128_BS128_Pairs20_Tokens50",
            "Associative_LSTM128_BS128_Pairs5_Tokens20", "Associative_LSTM256_BS128_Pairs20_Tokens50",
            "Associative_LSTM256_BS128_Pairs40_Tokens100", "Associative_VRNN128_BS128_Pairs10_Tokens50",
            "Associative_VRNN256_BS128_Pairs20_Tokens50", "Copy_GRU128_BS128_Length20_Tokens10",
            "Copy_GRU256_BS128_Length40_Tokens50", "Copy_LSTM128_BS128_Length20_Tokens10",
            "Copy_LSTM128_BS128_Length20_Tokens20", "Copy_LSTM128_BS128_Length5_Tokens10",
            "Copy_LSTM128_BS128_Length50_Tokens5", "Copy_LSTM256_BS128_Length40_Tokens50",
            "Copy_VRNN128_BS128_Length20_Tokens10", "Copy_VRNN256_BS128_Length40_Tokens50",
            "FixedImageConvAE_cifar10_32x32x32x32x32_bs128", "FixedImageConvAE_cifar10_32x64x8x64x32_bs128"
          ), tags = "task_id"
        )
      )
    },
    data = function() {
      if(is.null(private$.data)) private$.data = preproc_data_task_set(self)
      private$.data
    }
  )
)
#' @include BenchmarkConfig.R
#benchmark_configs$add("task_set", BenchmarkConfigTaskSet)


# Not sure whether to include kerasff
# If we do, fix trafos to only log trafos
# classif.kerasff = ps(
#   p_dbl(id = "epochs", lower = 3, upper = 7, trafo = function(x) round(2^x)),
#   p_fct(id = "optimizer", values = c("sgd", "rmsprop", "adam")),
#   p_dbl(id = "lr", lower = -5, upper = 0, trafo = function(x) 5^x),
#   p_dbl(id = "decay", lower = -8, upper = 0, trafo = function(x) 5^x),
#   p_dbl(id = "momentum", lower = -8, upper = 0,trafo = function(x) 5^x,
#     requires = quote(optimizer == "sgd")),
#   p_int(id = "layers", lower = 1L, upper = 4L),
#   p_fct(id = "batchnorm_dropout", values = c("batchnorm", "dropout", "none")),
#   p_dbl(id = "input_dropout_rate", lower = -5, upper = 0, requires = quote(batchnorm_dropout == "dropout"), trafo = function(x) 3^(x/2)),
#   p_dbl(id = "dropout_rate", lower = -5, upper = 0, requires = quote(batchnorm_dropout == "dropout"), trafo = function(x) 3^(x/2)),
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
