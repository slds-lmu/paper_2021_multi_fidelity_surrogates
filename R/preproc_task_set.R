
preproc_data_task_set = function(config, seed = 123L, n_max = 5*10^6, frac=.1) {
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  dt[, optimizer := NULL]
  tt = split_by_col(dt, by = "task_name", frac=frac)

  # Preproc train data
  train = tt$train
  train = preproc_iid(train)
  train = sample_max(train, n_max)
  train = apply_cummean_variance_param(train, mean = c("train", "valid1", "valid2", "test"), sum = NULL, "replication", ignore=NULL)
  trafos = c(
    map(train[, config$target_variables, with = FALSE], scale_base_0_1, base = 10),
    map(train[, c("learning_rate","beta1", "beta2","epsilon", "l1", "l2", "linear_decay", "exponential_decay"), with = FALSE], scale_base, base = 10L)
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
