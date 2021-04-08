#' @export
BenchmarkConfig = R6Class("BenchmarkConfig",
  public = list(
    id = NULL,
    download_url = NULL,
    workdir = NULL,
    subdirs = NULL,
    param_set = NULL,
    dicts_file = NULL,
    keras_model_file = NULL,
    onnx_model_file = NULL,
    budget_param = NULL,
    target_variables = NULL,
    codomain = NULL,
    packages = NULL,

    initialize = function(id, download_url, workdir, param_set, dicts_file, keras_model_file, onnx_model_file, budget_param, target_variables, codomain, packages) {
      self$id = assert_string(id)
      self$download_url = download_url
      self$workdir = workdir  # FIXME: strip latest "/", assert
      self$subdirs = paste0(workdir, "/", c("metadata", "model", "paramsets"))
      self$param_set = assert_param_set(param_set)
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

      for (subdir in self$subdirs) {
        if (!test_directory(subdir)) {
          dir.create(subdir)
        }
      }

      if (!test_file_exists(self$dicts_path)) {
        download.file(paste0(self$download_url, "/metadata", self$dicts_file), destfile = self$dicts_path)
      }

      if (!test_file_exists(self$keras_model_path)) {
        download.file(paste0(self$download_url, "/model/", self$keras_model_file), destfile = self$keras_model_path)
      }

      if (!test_file_exists(self$onnx_model_path)) {
        download.file(paste0(self$download_url, "/model/", self$onnx_model_file), destfile = self$onnx_model_path)
      }

      message("setup sucessful.")
    }
  ),
  active = list(
    dicts_path = function() paste0(self$subdirs[1L], "/", self$dicts_file),
    keras_model_path = function() paste0(self$subdirs[2L], "/", self$keras_model_file),
    onnx_model_path = function() paste0(self$subdirs[2L], "/", self$onnx_model_file),
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

