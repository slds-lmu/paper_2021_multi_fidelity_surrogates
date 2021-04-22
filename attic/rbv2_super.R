reticulate::use_condaenv('mlr3keras', required = TRUE)
devtools::load_all()
workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")


cfg_rbv2 = cfgs()
base_configs = c("rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_svm", "rbv2_xgboost")


dts = map(base_configs[1:6], function(cfg) {
    cff = cfgs(cfg, workdir=workdir)
    dt = data.table(farff::readARFF(cff$data_path))
    # Drop irrelevant columns
    dt[, c("dataset", "learner") := NULL]
    dt[, task_id := as.factor(task_id)]
    dt[, num.impute.selected.cpo := as.factor(num.impute.selected.cpo)]
    # Rename colnames
    param_names = setdiff(intersect(colnames(dt), names(cff$param_set$params)), "task_id")
    colnames(dt)[colnames(dt) %in% param_names] = paste0(gsub("rbv2_", "", cfg), ".", param_names)
    dt
})
dts = rbindlist(dts, fill=TRUE)
saveRDS(dts, "C:/Users/flo/LRZ Sync+Share/multifidelity_data/rbv2_super/data.rds")
