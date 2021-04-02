
keras_to_onnx = function(model_path = "wide_and_deeper_50.hdf5") {
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
