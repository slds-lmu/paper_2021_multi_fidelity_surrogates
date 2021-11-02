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
    data_order_file = NULL,
    keras_model_file = NULL,
    onnx_model_file = NULL,
    target_variables = NULL,  # FIXME: make ab to codomain$ids() ordered by data_order
    codomain = NULL,
    packages = NULL,
    # FIXME: think about easier an way
    all_task_ids_file = "task_ids.txt",
    eval_task_ids_file = "task_ids.txt",

    initialize = function(id, workdir, model_name, param_set_file = NULL, data_file, data_order_file, dicts_file, keras_model_file, onnx_model_file, target_variables, codomain, packages) {
      self$id = assert_string(id)
      self$download_url = paste0("https://syncandshare.lrz.de/dl/fi7HBiNodEa5DBTzP2e8tgmN/", model_name, "/")
      self$workdir = if (!is.null(workdir)) if (!endsWith(workdir, "/")) paste0(workdir, "/") else workdir
      self$model_name = model_name
      self$subdir = if (!is.null(workdir) && !is.null(model_name)) paste0(self$workdir, model_name, "/") else NULL
      self$param_set_file = param_set_file
      self$data_file = data_file
      self$data_order_file = data_order_file
      self$dicts_file = dicts_file
      self$keras_model_file = keras_model_file
      self$onnx_model_file = onnx_model_file
      self$target_variables = assert_character(target_variables, min.len = 1L)
      self$codomain = assert_param_set(codomain)
      self$packages = assert_character(packages, null.ok = TRUE)
      if (length(self$packages)) {
        mlr3misc::require_namespaces(self$packages)
      }
    },

    setup = function(force_download = FALSE, data = FALSE) {
      assert_flag(force_download)
      assert_flag(data)
      if (!test_directory(self$workdir)) {
        dir.create(self$workdir, recursive = TRUE)
      }

      if (!test_directory(self$subdir)) {
        dir.create(self$subdir)
      }

      if (data && !(is.null(self$data_file)) && (!test_file_exists(self$data_set_path) || force_download)) {
        download.file(paste0(self$download_url, self$data_file), destfile = self$data_path)
      }

      if (!(is.null(self$data_order_file)) && (!test_file_exists(self$data_order_path) || force_download)) {
        download.file(paste0(self$download_url, self$data_order_file), destfile = self$data_order_path)
      }

      if (!(is.null(self$param_set_file)) && (!test_file_exists(self$param_set_path) || force_download)) {
        download.file(paste0(self$download_url, self$param_set_file), destfile = self$param_set_path)
      }

      if (!(is.null(self$dicts_file)) && (!test_file_exists(self$dicts_path) || force_download)) {
        download.file(paste0(self$download_url, self$dicts_file), destfile = self$dicts_path)
      }

      if (!is.null(self$keras_model_file) & (!test_file_exists(self$keras_model_path) || force_download)) {
        download.file(paste0(self$download_url, self$keras_model_file), destfile = self$keras_model_path)
      }

      if (!is.null( self$onnx_model_file) & (!test_file_exists(self$onnx_model_path) || force_download)) {
        download.file(paste0(self$download_url, self$onnx_model_file), destfile = self$onnx_model_path)
      }
      message("setup sucessful.")
    },

    get_objective = function(task = NULL, target_variables = NULL, retrafo = TRUE) {
      assert_subset(target_variables, choices = self$target_variables, empty.ok = TRUE)
      assert_subset(task, choices = self$get_task_ids(), empty.ok = TRUE)
      codomain = self$codomain$clone(deep = TRUE)
      if (!is.null(target_variables)) {
        codomain = ParamSet$new(codomain$params[target_variables])
      }
      ObjectiveONNX$new(
        model_path = self$onnx_model_path,
        data_order = readRDS(self$data_order_path),
        trafo_dict = readRDS(self$dicts_path),
        domain = self$opt_param_set,
        full_codomain = self$codomain$clone(deep = TRUE),  # needed to set the names
        codomain = codomain,
        task = task,
        retrafo = retrafo
      )
    },
    save_data_order = function() {
      order = get_data_order(self)
      cat("Data order:\n")
      print(order)
      saveRDS(order, self$data_order_path)
    },
    save_trafo_dict = function() {
        trafos = c(
          {
            # mimick mlr3keras::reshape_data_embedding
            types = map_chr(self$data$xtrain, function(x) class(x)[[1L]])
            embed_vars = names(types)[types %in% c("ordered", "factor")]
            map(as.list(self$data$xtrain[, embed_vars, with = FALSE]), function(x) {
             level = levels(x)
             int = seq_along(level) - 1L
             data.table(level = level, int = int, key = "level")
            })
          },
          self$data$trafos
        )
        trafos = map(trafos, mlr3misc::crate)
        cat("Trafo:\n")
        print(trafos)
        saveRDS(trafos, self$dicts_path)
    },
    get_task_ids = function(eval=FALSE) {
      self$task_levels
    },
    print = function(...) {
      catf("BenchmarkConfig: <%s>", self$id)
      catf('Target variables: %s', paste0(self$target_variables, collapse = ", "))
      catf('Budget parameter: "%s"', self$budget_param)
      if (!is.null(self$task_col)) {
        catf('Task parameter (n): "%s" (%i)', self$task_col, length(self$param_set$params[[self$task_col]]$levels))
      }
      self$param_set$print()
      self$codomain$print()
    },
    plot = function() {
      stop("Abstract")
    },
    fit_surrogate = function(model_config = default_model_config(), overwrite = FALSE, plot = TRUE) {
      if (overwrite) {
        self$save_data_order()
        self$save_trafo_dict()
      }
      fit_surrogate(self, model_config, overwrite = overwrite, plot = plot)
    },
    tune_surrogate = function(continue = FALSE, save = FALSE, tune_munge = TRUE, n_evals = 10L) {
      tune_surrogate(self, continue = continue, save = save, tune_munge = tune_munge, n_evals = n_evals)
    },
    best_surrogate_config = function() {
      ins_path = paste0(self$subdir, "OptimInstance.rds")
      assert_file_exists(ins_path)
      ins = readRDS(ins_path)
      ins$archive$best()
    }
  ),
  active = list(
    data = function() {
      stop("Abstract")
    },
    data_path = function() {
      paste0(self$subdir, self$data_file)
    },
    data_order_path = function() {
      paste0(self$subdir, self$data_order_file)
    },
    param_set_path = function() {
      paste0(self$subdir, self$param_set_file)
    },
    param_set = function() {
      stop("Abstract")
    },
    opt_param_set = function() {
      self$param_set
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
    task_col = function() {
      x = unlist(imap(self$param_set$params, function(x, nm) ifelse("task_id" %in% x$tags, nm, NA_character_)))
      x = x[!is.na(x)]
      if (length(x)) {
        unname(x)
      } else {
        NULL
      }
    },
    budget_param = function() {
      x = unlist(imap(self$param_set$params, function(x, nm) ifelse("budget" %in% x$tags, nm, NA_character_)))
      x = x[!is.na(x)]
      if (length(x)) {
        unname(x)
      } else {
        NULL
      }
    },
    task_levels = function() {
      if (!is.null(self$task_col)) return(self$param_set$params[[self$task_col]]$levels)
      return(NULL)
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
#' For a more convenient way to retrieve and construct benchmarks, see [cfgs()]/[cfgs()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#' @export
benchmark_configs = R6Class("DictionaryTask",
  inherit = mlr3misc::Dictionary,
  cloneable = FALSE
)$new()

#' @export
cfgs = function(.key, ...) {
  mlr3misc::dictionary_sugar_get(benchmark_configs, .key, ...)
}
