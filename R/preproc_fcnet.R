preproc_data_fcnet = function(config, seed = 123L, n_max = 5*10^6, frac = .1) {
  browser()
  set.seed(seed)
  path = config$data_path
  dt = readRDS(path)
  dt = dt[, valid_mse := NULL]
  tt = split_by_col(dt, by = "task", pars = get_pars(config), frac = frac)

  # Preproc train data
  train = tt$train
  # We get rid of some upper outliers in training. This leads to mathematical instabilities otherwise.
  upper_outliers = which(rowSums(train[, map(.SD, function(tv) tv > quantile(tv, 0.999)), .SDcols = config$target_variables]) >= 1)
  if (length(upper_outliers)) {
    train = train[-upper_outliers, ]
  }
  train = preproc_iid(train)
  train = sample_max(train, n_max)
  train = apply_cummean_variance_param(train, mean = c("runtime", "valid_loss"),  sum = NULL, fidelity_param = "replication", ignore="n_params")

  trafos = c(
    map(train[, c("n_params", "runtime"), with = FALSE], scale_base_0_1, base = 10),
    map(train[, c("valid_loss"), with = FALSE], scale_neg_exp),
    map(train[, c("batch_size", "n_units_1", "n_units_2"), with = FALSE], scale_sigmoid, p = 0),
    map(train[, c("init_lr"), with = FALSE], scale_base, base = 10L)
  )
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, config$target_variables, with = FALSE])
  train = train[, (config$target_variables) := NULL]

  if (frac) {
    # Preproc test data
    oob = tt$test
    oob = preproc_iid(oob, keep_cols = colnames(train))
    oob = apply_cummean_variance_param(oob, mean = c("runtime", "valid_loss"),  sum = NULL, fidelity_param = "replication", ignore="n_params")
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
