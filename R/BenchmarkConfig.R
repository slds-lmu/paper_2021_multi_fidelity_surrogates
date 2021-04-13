#' @export
BenchmarkConfig = R6Class("BenchmarkConfig",
  public = list(
    id = NULL,
    download_url = NULL,
    workdir = NULL,
    model_name = NULL,
    subdir = NULL,
    param_set_file = NULL,
    data_file = NULL,
    dicts_file = NULL,
    keras_model_file = NULL,
    onnx_model_file = NULL,
    budget_param = NULL,
    target_variables = NULL,
    codomain = NULL,
    packages = NULL,

    initialize = function(id, download_url, workdir, model_name, param_set_file = NULL, data_file, dicts_file, keras_model_file, onnx_model_file, budget_param, target_variables, codomain, packages) {
      self$id = assert_string(id)
      self$download_url = download_url
      self$workdir = workdir  # FIXME: assure that this end on /
      self$model_name = model_name
      self$subdir = if (!is.null(workdir) && !is.null(model_name)) paste0(workdir, model_name, "/") else NULL
      self$param_set_file = param_set_file
      self$data_file = data_file
      self$dicts_file = dicts_file
      self$keras_model_file = keras_model_file
      self$onnx_model_file = onnx_model_file
      self$budget_param = assert_string(budget_param)
      self$target_variables = assert_character(target_variables, min.len = 1L)
      self$codomain = assert_param_set(codomain)
      self$packages = assert_character(packages, null.ok = TRUE)
    },

    setup = function(force_download = FALSE) {
      assert_flag(force_download)
      if (!test_directory(self$workdir)) {
        dir.create(self$workdir, recursive = TRUE)
      }

      if (!test_directory(self$subdir)) {
        dir.create(self$subdir)
      }

      if (!test_file_exists(self$param_set_path) || force_download) {
        download.file(paste0(self$download_url, self$param_set_file), destfile = self$param_set_path)
      }

      if (!test_file_exists(self$dicts_path) || force_download) {
        download.file(paste0(self$download_url, self$dicts_file), destfile = self$dicts_path)
      }

      if (!test_file_exists(self$keras_model_path) || force_download) {
        download.file(paste0(self$download_url, self$keras_model_file), destfile = self$keras_model_path)
      }

      if (!test_file_exists(self$onnx_model_path) || force_download) {
        download.file(paste0(self$download_url, self$onnx_model_file), destfile = self$onnx_model_path)
      }
      message("setup sucessful.")
    },

    get_objective = function(task = NULL, target_variables = NULL) {
      assert_subset(target_variables, choices = self$target_variables, empty.ok = TRUE)
      codomain = self$codomain$clone(deep = TRUE)
      if (!is.null(target_variables)) {
        codomain = ParamSet$new(codomain$params[target_variables])
      }
      ObjectiveONNX$new(
        model_path = self$onnx_model_path,
        trafo_dict = readRDS(self$dicts_path),
        domain = self$param_set,
        full_codomain_names = self$codomain$ids(),  # needed to set the names
        codomain = codomain
      )
    },

    save_trafo_dict = function() {
        trafos = c(
          map(keep(self$data$xtrain, is.factor), function(x) {
            dt = data.table(level = levels(x), int = as.integer(factor(levels(x))), key = "level")
            if ("None" %in% dt$level) dt = rbind(dt, data.table(level = "None", int = max(dt$int)+1L))
            dt
          }),
          self$data$trafos
        )
        saveRDS(trafos, self$dicts_path)
    },
    plot = function() {
      stop("Abstract")
    },
    fit_surrogate = function(model_config = default_model_config(), overwrite = FALSE, plot = TRUE) {
      if (overwrite) self$save_trafo_dict()
      fit_surrogate(self, model_config, overwrite = overwrite, plot = plot)
    }
  ),
  active = list(
    data = function() {
      stop("Abstract")
    },
    data_path = function() {
      paste0(self$subdir, self$data_file)
    },
    param_set_path = function() {
      paste0(self$subdir, self$param_set_file)
    },
    param_set = function() {
      stop("Abstract")
    },
    dicts_path = function() {
      paste0(self$subdir, self$dicts_file)
    },
    keras_model_path = function() {
      paste0(self$subdir, self$keras_model_file)
    },
    onnx_model_path = function() {
      paste0(self$subdir, self$onnx_model_file)
    }
  ),
  private = list(
    .data = NULL
  )
)


#' @title Dictionary of Configurations
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [BenchmarkConfig].
#'
#' For a more convenient way to retrieve and construct tasks, see [tsk()]/[tsks()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#' @export
benchmark_configs = R6Class("DictionaryTask",
  inherit = mlr3misc::Dictionary,
  cloneable = FALSE
)$new()

cfgs = function(.key, ...) {
  mlr3misc::dictionary_sugar_get(benchmark_configs, .key, ...)
}