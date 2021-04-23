reticulate::use_condaenv('mlr3keras', required = TRUE)
devtools::load_all()
workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")

cfg_rbv2 = cfgs()
base_configs = c("rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_svm", "rbv2_xgboost")
dts = map(base_configs, function(cfg) {
    cff = cfgs(cfg, workdir=workdir)
    learner = gsub("rbv2_", "", cfg)
    dt = data.table(farff::readARFF(cff$data_path))
    # Drop irrelevant columns
    dt[, c("dataset", "learner") := NULL]
    dt[, task_id := as.factor(task_id)]
    dt[, num.impute.selected.cpo := as.factor(num.impute.selected.cpo)]
    # Rename colnames
    param_names = setdiff(intersect(colnames(dt), names(cff$param_set$params)), c("task_id", "num.impute.selected.cpo"))
    colnames(dt)[colnames(dt) %in% param_names] = paste0(learner, ".", param_names)
    dt[, learner := as.factor(learner)]
    return(dt)
})
dts = rbindlist(dts, fill=TRUE)
dts[, svm.shrinking := as.logical(svm.shrinking)]
dts[, ranger.replace := as.logical(ranger.replace)]
dts[, fitted := NULL]
dts[, (colnames(keep(dts, is.character))) := map(.SD, as.factor), .SDcols = is.character]
saveRDS(dts, "C:/Users/flo/LRZ Sync+Share/multifidelity_data/rbv2_super/data.rds")
