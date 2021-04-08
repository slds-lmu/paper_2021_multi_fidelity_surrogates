keras_to_onnx = function(model_path = "model.hdf5") {
  k2o = reticulate::import("keras2onnx")
  model = keras::load_model_hdf5(model_path)
  onnx = k2o$convert_keras(model, model$name)
  k2o$save_model(onnx, gsub("hdf5", "onnx", model_path))
}

run_with_mlflow = function(xs) {
  mlflow::mlflow_start_run()
  mlr3misc::imap(xs, function(value, name) {
    mlflow::mlflow_log_param(name, as.character(value))
  })
  output = callr::r_safe(run_fit_svm, xs)
  mlr3misc::imap(output, function(value, name) {
    mlflow::mlflow_log_metric(name, as.numeric(value))
  })
  mlflow::mlflow_end_run()
  return(output)
}

# # Map a character to the correct integer using a dict
# char_to_int = function(x, param_name, dict) {
#   x[is.na(x)] = "None"
#   matrix(dict[[param_name]][x,]$int)
# }

# Map a character to the correct integer using a dict
char_to_int = function(x, param_name, dict) {
  x[is.na(x)] = "None"
  dt = dict[[param_name]]
  rownames(dt) = dt$level
  matrix(dt[x,]$int)
}