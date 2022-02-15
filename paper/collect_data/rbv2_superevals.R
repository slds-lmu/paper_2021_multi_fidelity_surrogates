################################################################################
# This script ingests data from rbv2 and turns it into a single table for use 
# with yahpo gym. 
# - Preprocessing
# - Merge memory data from log files
# - Returns a flat table
################################################################################

library(data.table)
library(mlr3misc)
basepath = paste0(path.expand("~"), "/results")
mempath = paste0(path.expand("~"), "/results/memory_rbv2")
metrics = c("f1", "mmce", "auc", "logloss", "timepredict", "timetrain")
fcts = c("num.impute.selected.cpo", "learner", "task")

memfiles = list.files(mempath, full.names = TRUE)

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
    x = cbind(x, rbindlist(map(x$point, function(xp) eval(parse(text = xp)))))
    x[, point := NULL]
    x[, SUPEREVAL := NULL]
    fcts = intersect(fcts, colnames(x))
    x[, (fcts) := map(.SD, as.factor), .SDcols = fcts]
    x[,c("task_id", "dataset") := split_task_col(task)]
    x[,task := NULL]
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

##########################################################################################################
# Step 1: Convert to chunked .rds files
# Chunking is required since memory overhead is massive.
if (FALSE) {
  # First we iterate over all files to save the prepared data in chunks
  # (This is done to avoid memory problems)
  files = list.files(basepath, full.names = TRUE)
  files = files[endsWith(files, ".rds") & !grepl("_prep", files)]
  for (j in c(2:10)) {
    filename = files[[j]]
    lsti = readRDS(filename)
    map_chunked(lsti, save_rds, chunks = ifelse(j != 10, 2, 8))
  }
}

files = list.files(basepath, full.names = TRUE)
files = files[endsWith(files, "_prep.rds")]

# What do we keep for csv export?
metrics = c("timetrain", "timepredict", "acc", "bac", "auc", "multiclass.aunp", "brier", "multiclass.brier", "f1", "logloss", "M")
cols = c("dataset", "task_id", "trainsize","repl", "seed")
csv_path = "~/../LRZ Sync+Share/multifidelity_data/"

##########################################################################################################
# Step 2: Export collected data.
# Saves the full data (for future use) as well as a .csv that is used to fit surrogates

##########################################################################################################
# glmnet
learner = 'glmnet'
this_file = files[grepl(paste0(learner, "_\\d*_prep"), files)]
memfile = memfiles[grepl("glmnet", memfiles)]
dt = rbindlist(map(this_file, readRDS))
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, mems, by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), how="left")
# Save full data
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv for learning
pars = c("alpha", "s", "num.impute.selected.cpo")
# Decide on repl in 1:10 or repl mod 10 ...
dt = dt[, c(cols, pars, metrics), with=FALSE]
dt[, memory := M/1024][, M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# ranger
learner = "ranger"
this_file = files[grepl("ranger.pow_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE)[, learner := "classif.ranger"]
memfile = memfiles[grepl("ranger", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, mems, by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), how="left")
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv
pars = c("num.trees", "replace", "sample.fraction", "mtry.power", "respect.unordered.factors", "min.node.size", "splitrule", "num.impute.selected.cpo")
# Decide on repl in 1:10 or repl mod 10 ...
dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
dt[, memory := M/1024][, M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# svm
learner = 'svm'
this_file = files[grepl("svm.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)# [, learner := "classif.svm"]
dt[, fitted := NULL]
dt[, kernel := ifelse(is.na(kernel), "radial", kernel)]
dt[, shrinking := as.logical(shrinking)]
dt[, kernel := as.factor(kernel)]
memfile = memfiles[grepl("svm", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)][, learner_id := factor(ifelse(setting == "all", "classif.svm", "classif.svm.radial"))]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv
pars = c("cost", "gamma", "tolerance", "shrinking", "kernel", "degree", "num.impute.selected.cpo")
# Decide on repl in 1:10 or repl mod 10 ...
dt = dt[, c(cols, pars, metrics), with=FALSE]
dt[, memory := M/1024][, M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# xgboost
learner = "xgboost"
save_chunk = function(this_file, i) {
  dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
  dt[, nthread := NULL]
  browser()
  dt = merge(dt, unique(mems), 
    by.x =c("seed", "dataset", "task_id", "learner"), 
    by.y = c("seed", "task", "data_id", "learner_id"),
    all.x = TRUE, all.y = FALSE
  )
  dt[, learner := "classif.xgboost"][, setting := NULL]
  dt[, booster := as.factor(booster)]
  saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", paste0("_", i, "_\\1_full"), this_file[1]))
}
this_file = files[grepl("xgboost.*_\\d*_prep", files)]
memfile = memfiles[grepl("xgboost", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
mems[setting == "gbtree", learner_id := "classif.xgboost.gbtree"]
mems[setting == "gblinear", learner_id := "classif.xgboost.gblinear"]
mems[setting == "dart", learner_id := "classif.xgboost.dart"]
mems[, learner_id := factor(learner_id)]
map_chunked(this_file, save_chunk, chunks = 3L)


xgb_files = list.files(basepath, full.names = TRUE)
xgb_files = xgb_files[grepl("xgboost", xgb_files) & grepl("full.rds", xgb_files)]

dt = rbindlist(map(xgb_files, function(x) {
  dt = readRDS(x)
  dt[, booster := as.factor(booster)]
  pars = c("booster", "nrounds", "eta", "gamma", "lambda", "alpha", "subsample",  "max_depth", "min_child_weight", "colsample_bytree", "colsample_bylevel",  "rate_drop", "skip_drop", "num.impute.selected.cpo")
  pars = intersect(pars, colnames(dt))
  dt = dt[, c(cols, pars, metrics), with=FALSE]
  dt[, memory := M/1024][, M := NULL]
  dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
  dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
  gc()
  return(dt)
}), use.names = TRUE, fill = TRUE)

fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# aknn
learner = 'aknn'
this_file = files[grepl("HNSW.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
dt[, distance := as.factor(distance)][, RcppHNSW.M := M][, M:= NULL]
dt[, as.factor(learner)]
memfile = memfiles[grepl("Rcpp", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))
# Save csv

pars = c("k", "RcppHNSW.M", "ef_construction", "ef", "distance", "num.impute.selected.cpo")
# Decide on repl in 1:10 or repl mod 10 ...
dt = dt[, c(cols, pars, metrics), with=FALSE]
dt[, memory := M/1024][, M := NULL]
dt[, M := RcppHNSW.M]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# rpart
this_file = files[grepl("rpart.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
memfile = memfiles[grepl("rpart", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
# Save full data
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv for learning
pars = c("cp","maxdepth","minbucket","minsplit","num.impute.selected.cpo")
# Decide on repl in 1:10 or repl mod 10 ...
dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
dt[, memory := M/1024][, M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
fwrite(dt, "~/../LRZ Sync+Share/multifidelity_data/rbv2_rpart/data2.csv")
