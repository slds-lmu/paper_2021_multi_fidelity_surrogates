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

# glmnet
this_file = files[grepl("glmnet_\\d*_prep", files)]
memfile = memfiles[grepl("glmnet", memfiles)]
dt = rbindlist(map(this_file, readRDS))
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, mems, by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), how="left")
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# ranger
this_file = files[grepl("ranger.pow_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE)[, learner := "classif.ranger"]
memfile = memfiles[grepl("ranger", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, mems, by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), how="left")
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))


# svm
this_file = files[grepl("svm.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)# [, learner := "classif.svm"]
dt[, fitted := NULL]
dt[, kernel := ifelse(is.na(kernel), "radial", kernel)]
dt[, shrinking := as.logical(shrinking)]
dt[, kernel := as.factor(kernel)]
# [, learner := as.factor("classif.svm")]
memfile = memfiles[grepl("svm", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)][, learner_id := ifelse(setting == "all", "classif.svm", "classif.svm.radial")]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))


# xgboost
save_chunk = function(this_file, i) {
  dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
  dt[, nthread := NULL]
  dt = merge(dt, unique(mems), 
    by.x =c("seed", "dataset", "task_id", "learner"), 
    by.y = c("seed", "task", "data_id", "learner_id"),
    all.x = TRUE, all.y = FALSE
  )
  saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", paste0("_", i, "_\\1_full"), this_file[1]))
}
this_file = files[grepl("xgboost.*_\\d*_prep", files)]
memfile = memfiles[grepl("xgboost", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
mems[setting == "gbtree", learner_id := "classif.xgboost.gbtree"]
mems[setting == "gblinear", learner_id := "classif.xgboost.gblinear"]
mems[setting == "dart", learner_id := "classif.xgboost.dart"]
map_chunked(this_file, save_chunk, chunks = 10L)

# And finally aggregate
xgb_files = list.files(basepath, full.names = TRUE)
xgb_files = xgb_files[grepl("xgboost", xgb_files) & endsWith(xgb_files, "full.rds")]
mems = NULL

# This does not fit memory, so will be left in three parts
dt = NULL
i = 0
for (f in xgb_files) {
  i = i+1
  x = data.table(readRDS(f))[, setting := NULL][, learner := "classif.xgboost"]
  dt = rbindlist(list(dt, x), fill=TRUE, use.names=TRUE)
  if (i == 4){
    saveRDS(dt, gsub("_full", "_full_p1", xgb_files[1]))
    rm(dt)
    dt = NULL
  }
  if (i == 8){
    saveRDS(dt, gsub("_full", "_full_p2", xgb_files[1]))
    rm(dt)
    dt = NULL
  }
  if (i == 10){
    saveRDS(dt, gsub("_full", "_full_p3", xgb_files[1]))
  }
  rm(x)
  gc()
}

xgb_files = list.files(basepath, full.names = TRUE)
dt[, booster := as.factor(booster)]
xgb_files = xgb_files[grepl("xgboost", xgb_files) & grepl("full_p\\d.rds", xgb_files)]

dt = rbindlist(map(xgb_files, readRDS), fill=TRUE, use.names = TRUE)

# we also need a smaller version the other one might just be too big
dt = readRDS(gsub(".arff", "_full.rds", cfg$data_path))
dt = dt[!(task_id == "23517"), ]
dt = dt[repl %in% 1:10, ]
dt = dt[, sample_max(.SD, 10^5), by=task_id]
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))

# aknn
this_file = files[grepl("HNSW.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
dt[, distance := as.factor(distance)][, RcppHNSW.M := M][, M:= NULL]
dt[, as.factor(learner)]
memfile = memfiles[grepl("Rcpp", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# rpart
this_file = files[grepl("rpart.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
memfile = memfiles[grepl("rpart", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

