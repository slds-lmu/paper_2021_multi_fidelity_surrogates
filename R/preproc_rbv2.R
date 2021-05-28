preproc_data_rbv2_svm = function(config, seed = 123L, frac = .1, n_max = 5e6, n_min_task = 800L) {
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("cost", "gamma"), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
    oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
    ytest = as.matrix(oob[, config$target_variables, with = FALSE])
    oob = oob[, (config$target_variables) := NULL]
  } else {
    oob = NULL
    ytest = NULLc
  }
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_ranger = function(config, seed = 123L, frac = .1, n_max = 5e6, n_min_task = 800L) {
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("num.trees", "min.node.size", 'num.random.splits'), with = FALSE], scale_base, base = 2L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, config$target_variables := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
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

preproc_data_rbv2_glmnet = function(config, seed = 123L, frac = .1, n_max = 5e6, n_min_task = 800L) {
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
    map(train[, c("s"), with = FALSE], scale_base, base = 2L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
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

preproc_data_rbv2_xgboost = function(config, seed = 123L, frac = .1, n_max = 5e6, n_min_task = 800L) {
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
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
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
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

preproc_data_rbv2_rpart = function(config, seed = 123L, frac = .1, n_max = 5e6, n_min_task = 800L) {
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
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
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
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

preproc_data_rbv2_aknn = function(config, seed = 123L, frac = .1, n_max = 5e6, n_min_task = 800L) {
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
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
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
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


preproc_data_rbv2_super = function(config, seed = 123L, frac = .1, n_max = 5e6) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  dt[, repl := as.numeric(repl)]
  dt = sample_max(dt, 2*n_max)
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
  train = apply_cummean_variance_param(train, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
  trafos = c(
    map(train[, c("mmce", "f1", "auc"), with = FALSE], scale_base_0_1, p = .01, base = 1),
    map(train[, c("logloss"), with = FALSE], scale_neg_exp),
    map(train[, c("timetrain", "timepredict"), with = FALSE], scale_base_0_1, p = .01, base = 10),
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
    oob = apply_cummean_variance_param(oob, mean = c("mmce", "f1", "auc", "logloss", "timepredict", "timetrain"), sum = NULL, fidelity_param = "repl", ignore = NULL)
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
