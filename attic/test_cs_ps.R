devtools::load_all()
library(reticulate)

workdir = "../../multifidelity_data/"

### lcbench
config = cfgs("lcbench", workdir = workdir)
ps = config$param_set

psx = rbindlist(generate_design_random(ps, n = 10000L)$transpose())
psx[, OpenML_task_id := NULL]
psx[, epoch := NULL]

py_run_string("
import ConfigSpace.read_and_write
from ConfigSpace.read_and_write import json
with open('../src/configspaces/configspace_lcbench_drop_OpenML_task_id_epoch.json', 'r') as f:
    json_string = f.read()
    cs = json.read(json_string)
")

cs_sample = py$cs$sample_configuration(10000L)
cs_dicts = lapply(cs_sample, function(x) x$get_dictionary())
cs_dat = map_dtr(cs_dicts, function(x) as.data.table(x))

# same distributions achieved by random sampling
summary(psx)
summary(cs_dat)

# mimick what we conceptually do in python
cs_dat[, OpenML_task_id := "3945"]
cs_dat[, epoch := 20L]
data_order = readRDS(config$data_order_path)
trafo_dict = readRDS(config$dicts_path)

py_run_string("import onnxruntime")
session = py$onnxruntime$InferenceSession(paste0(workdir, "lcbench/model.onnx"))
li = convert_for_onnx(cs_dat, data_order = data_order, param_set = config$param_set, trafo_dict = trafo_dict)
li[["continuous"]] = reticulate::r_to_py(li[["continuous"]])$astype("float32")
res_ = as.data.table(session$run(NULL, li)[[1]])
res_ = as.data.table(retrafo_predictions(res_, target_names = config$target_variables, codomain = config$codomain, trafo_dict = trafo_dict))


# vs. what happens in R
objective = config$get_objective(task = "3945", target_variables = "val_cross_entropy")
cs_dat[, OpenML_task_id := NULL]
res = objective$eval_dt(cs_dat)

all(rowSums(abs(res - res_)) < sqrt(.Machine$double.eps))



### rbv2_super
config = cfgs("rbv2_super", workdir = workdir)
ps = config$param_set

psx = rbindlist(generate_design_random(ps, n = 10000L)$transpose(), fill = TRUE)
psx[, task_id := NULL]
psx[, repl := NULL]
psx[, trainsize := NULL]


py_run_string("
import ConfigSpace.read_and_write
from ConfigSpace.read_and_write import json
with open('../src/configspaces/configspace_rbv2_super_drop_trainsize_repl_task_id.json', 'r') as f:
    json_string = f.read()
    cs = json.read(json_string)
")

cs_sample = py$cs$sample_configuration(10000L)
cs_dicts = lapply(cs_sample, function(x) x$get_dictionary())
cs_dat = map_dtr(cs_dicts, function(x) as.data.table(x), .fill = TRUE)

# same distributions achieved by random sampling
summary(psx[, names(psx), with = FALSE])
summary(cs_dat[, names(psx), with = FALSE])

# mimick what we conceptually do in python
cs_dat[, task_id := "37"]
cs_dat[, trainsize := 0.5]
data_order = readRDS(config$data_order_path)
trafo_dict = readRDS(config$dicts_path)

py_run_string("import onnxruntime")
session = py$onnxruntime$InferenceSession(paste0(workdir, "rbv2_super/model.onnx"))
li = convert_for_onnx(cs_dat, data_order = data_order, param_set = config$param_set, trafo_dict = trafo_dict)
li[["continuous"]] = reticulate::r_to_py(li[["continuous"]])$astype("float32")
res_ = as.data.table(session$run(NULL, li)[[1]])
res_ = as.data.table(retrafo_predictions(res_, target_names = config$target_variables, codomain = config$codomain, trafo_dict = trafo_dict))


# vs. what happens in R
objective = config$get_objective(task = "37", target_variables = "logloss")
cs_dat[, task_id := NULL]
res = objective$eval_dt(cs_dat)

all(rowSums(abs(res - res_)) < sqrt(.Machine$double.eps))

