# Param Set
ps = readRDS("metadata/nb_301_param_set.rds")

# Load dicts for categorical variables: This maps categories to integers for the NN
dicts = readRDS("metadata/nb_301_dicts.rds")

budget_param = "epoch"

target_variables = c("val_accuracy", "runtime")

model = "nb_301_wide_and_deeper_50.onnx"