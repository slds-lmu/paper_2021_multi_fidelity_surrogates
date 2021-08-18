preproc_branin_surrogate = function(config, seed = 123L, n_max = 2*10^6, frac = .1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  tt = split_by_col(dt, by = NULL, frac = 0.1)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, "y", with = FALSE], scale_base_0_1, base = 10, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
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

preproc_currin_surrogate = function(config, seed = 123L, n_max = 2*10^6, frac = .1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  tt = split_by_col(dt, by = NULL, frac = 0.1)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, "y", with = FALSE], scale_base_0_1, base = 1, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
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

preproc_hartmann_surrogate = function(config, seed = 123L, n_max = 2*10^6, frac = .1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  tt = split_by_col(dt, by = NULL, frac = 0.1)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, "y", with = FALSE], scale_base_0_1, base = 1, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
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

preproc_borehole_surrogate = function(config, seed = 123L, n_max = 2*10^6, frac = .1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  tt = split_by_col(dt, by = NULL, frac = 0.1)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, "y", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x1", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x2", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x3", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x4", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x5", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x6", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x7", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "x8", with = FALSE], scale_base_0_1, base = 1, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
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

preproc_rpart_surrogate = function(config, seed = 123L, n_max = 2*10^6, frac = .1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  # We get rid of larger loss in training. This leads to mathematical instabilities otherwise.
  #upper_outliers = which(dt$y > 1)
  #if (length(upper_outliers)) {
  #  dt = dt[-upper_outliers, ]
  #}
  tt = split_by_col(dt, by = NULL, frac = 0.1)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, "y", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "maxdepth", with = FALSE], scale_base_0_1, base = 1, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
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

preproc_glmnet_surrogate = function(config, seed = 123L, n_max = 2*10^6, frac = .1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  # We get rid of larger loss in training. This leads to mathematical instabilities otherwise.
  #upper_outliers = which(dt$y > 1)
  #if (length(upper_outliers)) {
  #  dt = dt[-upper_outliers, ]
  #}
  tt = split_by_col(dt, by = NULL, frac = 0.1)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  trafos = c(
    map(train[, "y", with = FALSE], scale_base_0_1, base = 1, p = 0),
    map(train[, "s", with = FALSE], scale_base_0_1, base = 1, p = 0)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
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


