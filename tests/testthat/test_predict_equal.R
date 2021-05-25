get_test = function(cfg) {
  data = cfg$data
  data$xtest[sample(NROW(data$xtest), size = 10^5, replace = FALSE), ]
}

get_trafos = function(cfg) {
  data = cfg$data
  data$trafos
}

predict_like_fitting = function(test, model_path) {
  model = keras::load_model_hdf5(model_path)
  rs2 = mlr3keras::reshape_data_embedding(test)
  ptest = as.data.table(predict(model, rs2$data))
}

predict_objective = function(xdt, objective, trafos) {
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
  p2 = predict_objective(xdt, objective = objective, trafos = trafos)
  all(abs(p1 - p2 ) <= 1e-4)
}

test_that("predict_equal", {
  skip_if_not(check_directory_exists(workdir))
  expect_true(predictions_equal(BenchmarkConfigNB301$new(workdir = workdir)))
})

test_that("lcbench", {
  skip_if_not(check_directory_exists(workdir))
  expect_true(predictions_equal(BenchmarkConfigLCBench$new(workdir = workdir)))
})

test_that("rbv2_super,", {
  skip_if_not(check_directory_exists(workdir))
  expect_true(predictions_equal(BenchmarkConfigSuperRBv2$new(workdir = workdir)))
})

