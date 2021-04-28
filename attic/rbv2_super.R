reticulate::use_condaenv('mlr3keras', required = TRUE)
devtools::load_all()
workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")

cfg_rbv2 = cfgs()
base_configs = c("rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_svm", "rbv2_xgboost")
dts = map(base_configs, function(cfg) {
    cff = cfgs(cfg, workdir=workdir)
    learner = gsub("rbv2_", "", cfg)
    dt = data.table(readRDS(cff$data_path))
    # Drop irrelevant columns
    dt[, c("dataset", "learner") := NULL]
    dt[, task_id := as.factor(task_id)]
    dt[, num.impute.selected.cpo := as.factor(num.impute.selected.cpo)]
    dt = dt[repl %in% 1:10,]
    # Limit to tasks with > 500 obs.
    cnts = dt[, .N, by = task_id][N > 800,]
    dt = dt[task_id %in% cnts$task_id,]
    catf("Algo %s: %s datasets with > 800 evals", cfg, nrow(cnts))
    # Rename colnames
    param_names = setdiff(intersect(colnames(dt), names(cff$param_set$params)), c("task_id", "num.impute.selected.cpo", "repl", "trainsize"))
    colnames(dt)[colnames(dt) %in% param_names] = paste0(learner, ".", param_names)
    dt[, learner := as.factor(learner)]
    return(dt)
})
dts = rbindlist(dts, fill=TRUE)
cnts = dts[, .N, by = c("task_id", "learner")][N > 800,]
cnts = cnts[, .N, by = task_id][N >= 6,]
dts = dts[task_id %in% cnts$task_id,]
catf("Algo %s: %s datasets with > 800 evals", "rbv2_super", nrow(cnts))
dts[, svm.shrinking := as.logical(svm.shrinking)]
dts[, ranger.replace := as.logical(ranger.replace)]
dts[, task := NULL]
dts[, (colnames(keep(dts, is.character))) := map(.SD, as.factor), .SDcols = is.character]
saveRDS(dts, "C:/Users/flo/LRZ Sync+Share/multifidelity_data/rbv2_super/data.rds")


