# FIXME: This was not started yet
preproc_data_lcbench = function(config, seed = 123L, n_max = 2*10^6, frac=.1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  tt = split_by_col(dt, by = "OpenML_task_id", frac = frac)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  train = sample_max(train, n_max)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_sigmoid),
    map(train[, c("batch_size", "max_units"), with = FALSE], scale_sigmoid, p = 0)
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
