preproc_data_rbv2_svm = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, task_id := as.factor(task_id)]
  dt[, shrinking := as.logical(shrinking)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("cost", "gamma"), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
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

preproc_data_rbv2_ranger = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, task_id := as.factor(task_id)]
  dt[, replace := as.logical(replace)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("num.trees", "min.node.size", 'num.random.splits'), with = FALSE], scale_base, base = 2L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, config$target_variables := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
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

preproc_data_rbv2_glmnet = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, task_id := as.factor(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("s"), with = FALSE], scale_base, base = 2L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  if (frac) {
    oob = tt$test
    oob = preproc_iid(oob)
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

preproc_data_rbv2_xgboost = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, task_id := as.factor(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
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

preproc_data_rbv2_rpart = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, task_id := as.factor(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
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

preproc_data_rbv2_aknn = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  dt[, task_id := as.factor(task_id)]
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
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


preproc_data_rbv2_super = function(config, seed = 123L, frac=.1) {
  set.seed(seed)
  dt = data.table(readRDS(config$data_path))
  # Limit to tasks with > 500 obs.
  cnts = dt[, .N, by = task_id][N > 500,]
  dt = dt[task_id %in% cnts$task_id,]
  dt[, task_id := droplevels(task_id)]
  # Split into train and test
  tt = split_by_col(dt, frac = frac)

  train = tt$train
  train = preproc_iid(train)
  train = sample_max(train, 1e6)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
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