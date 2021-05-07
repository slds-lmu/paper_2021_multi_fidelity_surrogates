preproc_data_task_set = function(config) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  dt[, optimizer := NULL]
  tt = list(test = dt[replication == 1L], train = dt[replication == 0L])
  tt$test[, replication := NULL]
  tt$train[, replication := NULL]

  # Preproc train data
  train = tt$train
  upper_outliers = which(rowSums(train[, map(.SD, function(tv) tv > quantile(tv, 0.99999)), .SDcols = config$target_variables]) >= 1)
  if (length(upper_outliers)) {
    train = train[-upper_outliers, ]
  }
  train = preproc_iid(train)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_base_0_1, base = 10),
    map(train[, c("epoch", "learning_rate", "epsilon", "l1", "l2", "linear_decay", "exponential_decay"), with = FALSE], scale_standard),
    map(train[, c("beta1", "beta2"), with = FALSE], scale_log_left_standard, constant = 1)
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
