get_test = function(cfg) {
  data = cfg$data
  #data$xtest[sample(NROW(data$xtest), size = 10^5, replace = FALSE), ]
  data$xtest
}

get_trafos = function(cfg) {
  data = cfg$data
  data$trafos
}

predict_like_fitting = function(test, model_path) {
  # uses keras model
  # as done during fit_surrogate
  model = keras::load_model_hdf5(model_path, compile = FALSE)
  rs2 = mlr3keras::reshape_data_embedding(test)
  ptest = as.data.table(predict(model, rs2$data))
}

predict_objective = function(xdt, objective, trafos) {
  # uses ONNX model
  # as done in objective fun
  if (length(trafos)) {
    xdt[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$retrafo(x)}), .SDcols = names(trafos)]  # raw data because the objective applies the trafos
  }
  objective$eval_dt(xdt)
}

predictions_equal = function(cfg) {
  set.seed(1234)
  test = get_test(cfg)
  
  p1 = predict_like_fitting(test, model_path = paste0(cfg$subdir, cfg$keras_model_file))
  names(p1) = cfg$target_variables

  objective = cfg$get_objective(retrafo = FALSE)
  trafos = get_trafos(cfg)
  trafos = trafos[names(trafos) %in% names(test)]  # applicable trafos

  xdt = copy(test)
  xdt = xdt[, mlr3misc::shuffle(names(xdt)), with = FALSE]

  p2 = predict_objective(xdt, objective = objective, trafos = trafos)
  names(p2) = cfg$target_variables
  all(abs(p1 - p2 ) <= 1e-4)

  truth = cfg$data$ytest
  p1 = as.matrix(p1)
  p2 = as.matrix(p2)
  colnames(truth) = colnames(p1) = colnames(p2) = cfg$target_variables

  metrics1 = compute_metrics(truth, p1)
  metrics2 = compute_metrics(truth, p2)

  all(c(all(abs(p1 - p2 ) <= 1e-4), all(abs(metrics1[, -c(1, 2, 5)] - metrics2[, -c(1, 2, 5)]) <= 1e-4)))
}

test_that("predict equal", {
  skip_if_not(check_directory_exists(workdir))
  # FIXME: fcnet and task_set not stable yet
  cfgs = setdiff(benchmark_configs$keys(), c("branin", "shekel", "zdt6", "fcnet", "task_set"))
  for (cfg in cfgs) {
    config = benchmark_configs$get(cfg, workdir = workdir)
    expect_true(predictions_equal(config))
  }
})

