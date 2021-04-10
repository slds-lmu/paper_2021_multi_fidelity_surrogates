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
  # train = augment_with_munge(train, config$target_variables, 10^4)
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  # Preproc test data
  oob = tt$test
  oob = preproc_iid(oob)
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, config$target_variables, with = FALSE])

  list(
    xtrain = train,
    ytrain = y,
    xtest = oob,
    ytest = ytest,
    trafos = trafos
  )
}