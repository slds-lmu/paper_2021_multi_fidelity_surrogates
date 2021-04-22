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