devtools::load_all()
devtools::load_all("/home/lps/paradox")

ids = c("rbv2_super", "rbv2_svm", "rbv2_rpart", "rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_xgboost", "nb301", "lcbench")
params = map(ids, function(id) {
  cfg = cfgs(id, workdir = "../../multifidelity_data")
  ps = cfg$param_set
  cs = ps_to_cs(ps, json_file = paste0(cfg$subdir, "config_space.json"))
  cs$get_hyperparameter_names()
})

