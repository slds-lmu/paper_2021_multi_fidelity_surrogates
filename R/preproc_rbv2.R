preproc_data_rbv2_svm = function(config, seed = 123L, frac=.1, n_max=5e6, n_min_task=800L) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  stopifnot(all(dt$repl == as.numeric(dt$repl)))
  dt[, repl := as.numeric(repl)]
  dt = dt[repl <= 10L,]
  # Limit to tasks with >= n_min_task obs.
  cnts = dt[, .N, by = task_id][N > n_min_task,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  dt[, shrinking := NULL]

  tt = split_by_col(dt, frac = frac)
  train = tt$train
  train = sample_max(train, n_max)
  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("cost", "gamma"), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_ranger = function(config, seed = 123L, frac=.1, n_max=5e6, n_min_task=800L) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, replace := NULL]
  dt[, repl := as.numeric(repl)]
  dt = dt[repl <= 10L,]
  # Limit to tasks with >= n_min_task obs.
  cnts = dt[, .N, by = task_id][N > n_min_task,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = sample_max(train, n_max)
  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("num.trees", "min.node.size", 'num.random.splits'), with = FALSE], scale_base, base = 2L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, config$target_variables := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }

  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_glmnet = function(config, seed = 123L, frac=.1, n_max=5e6, n_min_task=800L) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, repl := as.numeric(repl)]
  dt = dt[repl <= 10L,]
  # Limit to tasks with >= n_min_task obs.
  cnts = dt[, .N, by = task_id][N > n_min_task,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = sample_max(train, n_max)
  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("s"), with = FALSE], scale_base, base = 2L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }

  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_xgboost = function(config, seed = 123L, frac=.1, n_max=5e6, n_min_task=800L) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  if ("task" %in% colnames(dt)) dt[, task := NULL]
  dt[, repl := as.numeric(repl)]
  dt = dt[repl <= 10L,]
  # Limit to tasks with >= n_min_task obs.
  cnts = dt[, .N, by = task_id][N > n_min_task,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = sample_max(train, n_max)
  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("nrounds", "eta", "gamma", "lambda", "alpha", "min_child_weight"), with = FALSE], scale_base, base = 2L),
    map(train[, "max_depth", with = FALSE], scale_sigmoid, p=0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }

  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_rpart = function(config, seed = 123L, frac=.1, n_max=5e6, n_min_task=800L) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, repl := as.numeric(repl)]
  dt = dt[repl <= 10L,]
  # Limit to tasks with >= n_min_task obs.
  cnts = dt[, .N, by = task_id][N > n_min_task,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = sample_max(train, n_max)
  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, "cp", with = FALSE], scale_base, base = 2L),
    map(train[, c("maxdepth", "minsplit", "minbucket"), with = FALSE], scale_sigmoid, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_aknn = function(config, seed = 123L, frac=.1, n_max=5e6, n_min_task=800L) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, repl := as.numeric(repl)]
  dt = dt[repl <= 10L,]
  # Limit to tasks with >= n_min_task obs.
  cnts = dt[, .N, by = task_id][N > n_min_task,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = sample_max(train, n_max)

  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("k", "M"), with = FALSE], scale_sigmoid, p = 0),
    map(train[, c("ef", "ef_construction"), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }

  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}


preproc_data_rbv2_super = function(config, seed = 123L, frac=.1, n_max=5e6) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, repl := as.numeric(repl)]
  dt = sample_max(dt, 10*n_max)
  dt = dt[repl <= 10L,]
  dt[, num.threads := NULL]
  dt[, task_id := droplevels(task_id)]
  dt[, learner := droplevels(learner)]
  dt[, svm.shrinking := NULL]  
  dt[, ranger.replace := NULL]

  # Split into train and test
  tt = split_by_col(dt, frac = frac)
  train = tt$train
  train = sample_max(train, n_max)
  train = preproc_iid(train)
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss","timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("glmnet.s"), with = FALSE], scale_base, base = 2L),
    map(train[, c("aknn.k", "aknn.M"), with = FALSE], scale_sigmoid, p = 0),
    map(train[, c("aknn.ef", "aknn.ef_construction"), with = FALSE], scale_base),
    map(train[, "rpart.cp", with = FALSE], scale_base, base = 2L),
    map(train[, c("rpart.maxdepth", "rpart.minsplit", "rpart.minbucket"), with = FALSE], scale_sigmoid, p = 0),
    map(train[, paste0("xgboost.", c("nrounds", "eta", "gamma", "lambda", "alpha", "min_child_weight")), with = FALSE], scale_base, base = 2L),
    map(train[, "xgboost.max_depth", with = FALSE], scale_sigmoid, p=0),
    map(train[, c("ranger.num.trees", "ranger.min.node.size", 'ranger.num.random.splits'), with = FALSE], scale_base, base = 2L),
    map(train[, c("svm.cost", "svm.gamma"), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict"), sum = "timetrain", "repl", ignore=NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULL
  }
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}


#' @export
rbv2_test_tasks = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
"11", "1111", "12", "14", "1461", "1462", "1464", "1468", "1475",
"1476", "1478", "1479", "1480", "1485", "1486", "1487", "1489",
"1494", "1497", "15", "1501", "1510", "1515", "16", "18", "181",
"182", "188", "22", "23", "23381", "24", "28", "29", "3", "307",
"31", "312", "32", "334", "37", "375", "377", "38", "40496",
"40498", "40499", "40536", "40670", "40701", "40900", "40966",
"40975", "40978", "40979", "40981", "40982", "40983", "40984",
"40994", "41138", "41142", "41143", "41146", "41156", "41157",
"41212", "4134", "4154", "42", "44", "4534", "4538", "458", "46",
"469", "470", "50", "54", "60", "6332")
