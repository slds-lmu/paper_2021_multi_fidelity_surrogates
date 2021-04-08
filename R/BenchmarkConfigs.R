BenchmarkConfigNB301 = R6Class("BenchmarkConfigNB301",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id = 'NASBench301', workdir) {
      super$initialize(
        id,
        download_url = "https://syncandshare.lrz.de/getlink/fiWvjauTgurqRB8dXUfbgmXE",
        workdir = workdir,
        param_set = readRDS(paste0(workdir, "/metadata/nb_301_param_set.rds")),
        dicts_file = "nb_301_dicts.rds",
        keras_model_file = "nb_301_wide_and_deeper_50.hdf5",
        onnx_model_file = "nb_301_wide_and_deeper_50.onnx",
        budget_param = "epoch",
        target_variables = c("val_accuracy", "runtime"),
        codomain = ps(
          val_accuracy = p_dbl(lower = 0, upper = 1, tags = "minimize"),
          runtime = p_dbl(lower = 0, upper = 1, tags = "minimize")
        ),
        packages = NULL
      )
    }
  )
)
