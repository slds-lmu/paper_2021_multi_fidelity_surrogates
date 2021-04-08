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
    param_set = NULL,    
    budget_param = NULL,
    target_variables = NULL,
    codomain = NULL,
    packages = NULL,

    initialize = function(id, download_url, workdir, model_name, param_set_file, data_file, dicts_file, keras_model_file, onnx_model_file, budget_param, target_variables, codomain, packages) {
      self$id = assert_string(id)
      self$download_url = download_url
      self$workdir = workdir  # FIXME: assure that this end on /
      self$model_name = model_name
      self$subdir = paste0(workdir, model_name, "/")
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

    setup = function() {
      if (!test_directory(self$workdir)) {
        dir.create(self$workdir, recursive = TRUE)
      }

      if (!test_directory(self$subdir)) {
        dir.create(self$subdir)
      }

      if (!test_file_exists(self$param_set_path)) {
        download.file(paste0(self$download_url, self$param_set_file), destfile = self$param_set_path)
      }

      if (!test_file_exists(self$dicts_path)) {
        download.file(paste0(self$download_url, self$dicts_file), destfile = self$dicts_path)
      }

      if (!test_file_exists(self$keras_model_path)) {
        download.file(paste0(self$download_url, self$keras_model_file), destfile = self$keras_model_path)
      }

      if (!test_file_exists(self$onnx_model_path)) {
        download.file(paste0(self$download_url, self$onnx_model_file), destfile = self$onnx_model_path)
      }

      self$param_set = readRDS(self$param_set_path)

      message("setup sucessful.")
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

    dicts_path = function() {
      paste0(self$subdir, self$dicts_file)
    },
    keras_model_path = function() {
      paste0(self$subdir, self$keras_model_file)
    },
    onnx_model_path = function() {
      paste0(self$subdir, self$onnx_model_file)
    },
    objective = function() {
      ObjectiveONNX$new(
        model_path = self$onnx_model_path,
        trafo_dict = readRDS(self$dicts_path),
        domain = self$param_set,
        codomain = self$codomain
      )
    }
  )
)

