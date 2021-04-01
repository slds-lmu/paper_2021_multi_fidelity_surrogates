library(paradox)
library(mlr3misc)
library(data.table)

# Load Config File
source("inst/configs/nb301.R")

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

# Generate data points
test = paradox::generate_design_random(ps, 1000)$data
predict_onnx(model, data = test, dicts = dicts)
