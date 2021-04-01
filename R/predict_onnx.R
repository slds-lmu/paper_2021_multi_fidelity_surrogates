library(paradox)
library(mlr3misc)
library(data.table)

# Load ParamSet
ps = readRDS("metadata/nb_301_param_set.rds")
# Load dicts for categorical variables: This maps categories to integers for the NN
dicts = readRDS("metadata/nb_301_dicts.rds")

# Map a character to the correct integer using a dict
char_to_int = function(x, param_name, dict) {
  x[is.na(x)] = "None"
  matrix(dict[[param_name]][x,]$int)
}

# Predict using a .onnx file
# inference time (1k samples):
# - open session: 15 ms
# - new session:  45 ms
predict_onnx = function(model = "nb_301_wide_and_deeper_50.onnx", data, dicts) {
  li = c(
    imap(keep(data, is.character), char_to_int, dicts),
    continuous = list(reticulate::r_to_py(as.matrix(keep(data, is.numeric)))$astype("float32"))
  )
  rt = reticulate::import("onnxruntime")
  sess = rt$InferenceSession(model)
  sess$run(NULL, li)[[1]]
}

# Generate data points
test = paradox::generate_design_random(ps, 1000)$data
predict_onnx(data = test, dicts = dicts)
