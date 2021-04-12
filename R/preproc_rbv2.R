preproc_data_rbv2_svm = function(config, seed = 123L) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  tt = split_by_col(dt)

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
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])
  oob = oob[, (config$target_variables) := NULL]
  
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_ranger = function(config, seed = 123L) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  tt = split_by_col(dt)

  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("num.trees", "min.node.size", 'num.random.splits'), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])
  oob = oob[, (config$target_variables) := NULL]
  
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_glmnet = function(config, seed = 123L) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  tt = split_by_col(dt)
  
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
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])
  oob = oob[, (config$target_variables) := NULL]
  
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

preproc_data_rbv2_xgboost = function(config, seed = 123L) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  tt = split_by_col(dt)
  
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("nrounds", "eta", "gamma", "lambda", "alpha", "min_child_weight"), with = FALSE], scale_base),
    map(train[, "max_depth", with = FALSE], scale_sigmoid, p=0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]
  
  # Preproc test data
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])
  oob = oob[, (config$target_variables) := NULL]
  
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

# FIXME: This was not started yet
preproc_data_rbv2_rpart = function(config, seed = 123L) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  tt = split_by_col(dt)
  
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("num.trees", "min.node.size", 'num.random.splits'), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]
  
  # Preproc test data
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])
  oob = oob[, (config$target_variables) := NULL]
  
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}

# FIXME: This was not started yet
preproc_data_rbv2_aknn = function(config, seed = 123L) {
  set.seed(seed)
  dt = data.table(farff::readARFF(config$data_path))
  dt[, c("dataset", "learner") := NULL]
  tt = split_by_col(dt)
  
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("num.trees", "min.node.size", 'num.random.splits'), with = FALSE], scale_base)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]
  
  # Preproc test data
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])
  oob = oob[, (config$target_variables) := NULL]
  
  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}


