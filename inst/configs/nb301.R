config = function() {
  list(
    # Param Set
    ps = readRDS("metadata/nb_301_param_set.rds"),
    
    # Load dicts for categorical variables: This maps categories to integers for the NN
    dicts = readRDS("metadata/nb_301_dicts.rds"),
    
    path = "metadata/nb_301_data.rds",
    budget_param = "epoch",
    target_variables = c("val_accuracy", "runtime"),
    
    keras_model_path = "nb_301_wide_and_deeper_50.hdf5",
    onnx_model_path = "nb_301_wide_and_deeper_50.onnx"
  )
}



BenchmarkConfigNB301 = R6Class("BenchmarkConfigNB301",
  inherit = BenchmarkConfig,
  public = list(
    initialize = function(id, workdir) {
      super$initialize(
        id, 
        download_url = "https://syncandshare.lrz.de/getlink/fiWvjauTgurqRB8dXUfbgmXE",
        workdir = workdir,
        param_set = psnb301,
        dicts_file = "nb_301_dicts.rds",
        keras_model_file = "nb_301_wide_and_deeper_50.hdf5",
        onnx_model_file = "nb_301_wide_and_deeper_50.onnx",
        budget_param = "epoch",
        target_variables = c("val_accuracy", "runtime"),
        packages = NULL
      )
    }
  )
)

