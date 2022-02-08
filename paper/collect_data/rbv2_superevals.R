# cd /dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/supermuc_project/results
# cp -r di57qoy2@lrz2:/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/supermuc_project/results /home/flo/Documents

library(data.table)
library(mlr3misc)
basepath = paste0(path.expand("~"), "/Downloads/superevals")
metrics = c("f1", "mmce", "auc", "logloss", "timepredict", "timetrain")
fcts = c("num.impute.selected.cpo", "learner", "task")

# Map over chunks of the file
map_chunked = function(all, fct, chunks = 10L) {
  ixs = chunk_vector(seq_along(all), n_chunks = chunks)
  imap(ixs, function(x, i) {
    fct(all[x], i)
  })
  gc()
}

save_rds = function(lstix, i) {
  lstx = map(lstix, function(lst) {
    x = data.table(lst$performances)

    # Get rid of failed runs
    nas = apply(x[, intersect(metrics, colnames(x)), with = FALSE], 1, function(x) mean(is.na(x)))
    x = x[nas < 0.5,]
    if (all(is.na(x$trainsize))) return(NULL)

    # Convert columns
    x[, trainsize := round(trainsize / max(trainsize, na.rm = TRUE),2)]
    x[, repl := as.factor(seq_along(iter)), by = trainsize]
    x[, predictsize := NULL]
    x[, iter := NULL]
    x[, (names(lst$METADATA)) := lst$METADATA]
    x[, seed := NULL]
    x = cbind(x, rbindlist(map(x$point, function(xp) eval(parse(text = xp)))))
    x[, point := NULL]
    x[, SUPEREVAL := NULL]
    fcts = intersect(fcts, colnames(x))
    x[, (fcts) := map(.SD, as.factor), .SDcols = fcts]
    return(x)
  })
  dt = rbindlist(lstx, fill = TRUE)
  fileext = paste0("_", i, "_prep.rds")
  saveRDS(dt, gsub(".rds", fileext, filename))
}

split_task_col = function(x) {
  splits = strsplit(levels(x), split = ".", fixed = TRUE)
  dt = rbindlist(map(splits, function(x) {
    data.table(factor(x[length(x)]), factor(paste0(x[-length(x)], collapse=".")))
  }))
  dt[as.integer(x),]
}

# First we iterate over all files to save the prepared data in chunks
files = list.files(basepath, full.names = TRUE)
files = files[!endsWith(files, "_prep.rds")]
for (j in c(1, 4:10)) {
  filename = files[[j]]
  lsti = readRDS(filename)
  map_chunked(lsti, save_rds, chunks = ifelse(j != 10, 10, 30))
}


files = list.files(basepath, full.names = TRUE)
files = files[endsWith(files, "_prep.rds")]
devtools::load_all()
workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")

# glmnet
this_file = files[[1]]
dt = readRDS(this_file)
dt[,c("task_id", "dataset") := split_task_col(task)]
dt[,task := NULL]
cfg = cfgs("rbv2_glmnet", workdir = workdir)
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))
map(this_file, unlink)

# ranger
this_file = files[grepl("ranger.pow_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE)
dt[,c("task_id", "dataset") := split_task_col(task)]
dt[,task := NULL]
cfg = cfgs("rbv2_ranger", workdir = workdir)
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))

# svm
this_file = files[grepl("svm.*_\\d*_prep", files)]kb(lU2~bZ!Es*+ld[J
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
dt[,c("task_id", "dataset") := split_task_col(task)]
dt[,task := NULL]
dt[, fitted := NULL]
dt[, kernel := ifelse(is.na(kernel), "radial", kernel)]
dt[, shrinking := as.logical(shrinking)]
dt[, kernel := as.factor(kernel)]
dt[, learner := as.factor("classif.svm")]
cfg = cfgs("rbv2_svm", workdir = workdir)
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))

# xgboost
save_chunk = function(this_file, i) {
  dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
  dt[,c("task_id", "dataset") := split_task_col(task)]
  dt[, nthread := NULL]
  dt[, learner := factor("classif.xgboost")]
  cfg = cfgs("rbv2_xgboost", workdir = workdir)
  print(i)
  saveRDS(dt, gsub(".arff", paste0(i, ".rds"), cfg$data_path))
}
this_file = files[grepl("xgboost.*_\\d*_prep", files)]
map_chunked(this_file, save_chunk, chunks = 10L)

# And finally aggregate
cfg = cfgs("rbv2_xgboost", workdir = workdir)
files = list.files(cfg$subdir, "*.rds", full.names = TRUE)
dt = NULL
for (f in files) {
  dt = rbindlist(list(dt, readRDS(f)), fill=TRUE, use.names=TRUE)
  gc()
}
saveRDS(dt, gsub(".arff", "_full.rds", cfg$data_path))

# we also need a smaller version the other one might just be too big
dt = readRDS(gsub(".arff", "_full.rds", cfg$data_path))
dt = dt[!(task_id == "23517"), ]
dt = dt[repl %in% 1:10, ]
dt = dt[, sample_max(.SD, 10^5), by=task_id]
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))

# aknn
this_file = files[grepl("HNSW.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
dt[,c("task_id", "dataset") := split_task_col(task)]
dt[,task := NULL]
dt[, distance := as.factor(distance)]
cfg = cfgs("rbv2_aknn", workdir = workdir)
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))

# rpart
this_file = files[grepl("rpart.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
dt[,c("task_id", "dataset") := split_task_col(task)]
dt[,task := NULL]
cfg = cfgs("rbv2_rpart", workdir = workdir)
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))