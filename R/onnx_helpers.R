
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

# Map a character to the correct integer using a dict
char_to_int = function(x, param_name, dict) {
  x[is.na(x)] = "None"
  matrix(dict[[param_name]][x,]$int)
}

# Predict using a .onnx file
# inference time (1k samples):
# - open session: 15 ms
# - new session:  45 ms
predict_onnx = function(model, data, dicts) {

  li = c(
    mlr3misc::imap(keep(data, is.character), char_to_int, dicts),
    continuous = list(reticulate::r_to_py(as.matrix(keep(data, is.numeric)))$astype("float32"))
  )
  rt = reticulate::import("onnxruntime")
  sess = rt$InferenceSession(model)
  sess$run(NULL, li)[[1]]
}
