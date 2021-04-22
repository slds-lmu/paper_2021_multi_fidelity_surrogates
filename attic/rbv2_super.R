reticulate::use_condaenv('mlr3keras', required = TRUE)
devtools::load_all()
workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")


cfg_rbv2 = cfgs()
base_configs = c("rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_svm", "rbv2_xgboost")


cfg = base_configs[1]