fit_surrogate = function(problem_config, model_config = default_model_config(), overwrite = FALSE, plot = TRUE) {
  require_namespaces(c("keras", "mlr3keras"))
  data = problem_config$data
  data = munge_data(data, target_vars = problem_config$target_variables, munge_n = model_config$munge_n)
  rs = mlr3keras::reshape_data_embedding(data$xtrain)
  embd = make_embedding_dt(data$xtrain, emb_multiplier = model_config$emb_multiplier)

  input_shape = list(ncol(data$xtrain) - ncol(data$ytrain))
  output_shape = ncol(data$ytrain)
  model = make_architecture(embd, input_shape, output_shape, model_config)

  cbs = list(mlr3keras::cb_es(patience = 20L))
  history = model %>%
    fit(
      x = rs$data,
      y = data$ytrain,
      batch_size = model_config$batch_size,
      validation_split = 0.1,
      epochs = model_config$epochs,
      sample_weight = weights_from_target(data$ytrain),
      callbacks = cbs
    )
  # Save model
  if (overwrite) {
    keras::save_model_hdf5(model, problem_config$keras_model_path, overwrite = overwrite)
    keras_to_onnx(problem_config$keras_model_path, problem_config$onnx_model_path)
  }

  # Test Data Metrics & Plots
  rs2 = mlr3keras::reshape_data_embedding(data$xtest)
  ptest = as.matrix(predict(model, rs2$data))
  colnames(ptest) = problem_config$target_variables
  colnames(data$ytest) = problem_config$target_variables

  metrics = compute_metrics(data$ytest, ptest)
  print(metrics)

  if (plot) {
    require("ggplot2")
    require("patchwork")
    smp = sample(seq_along(ptest[,1]), min(length(ptest[,1]), 500L))
    p1 = ggplot(data.frame(x = data$ytest[smp,1], y = ptest[smp,1]), aes(x=x, y=y)) +
      geom_point() +
      geom_abline(slope = 1, color = "blue")
    p2 = plot(history)
    p = p1 + p2
    print(p)
    if (overwrite) ggsave(paste0(problem_config$subdir, "surrogate_test_metrics.pdf"), plot = p)
    if (overwrite) data.table::fwrite(metrics, paste0(problem_config$subdir, "surrogate_test_metrics.csv"))

  }
  return(metrics)
}

default_model_config = function() {
  list(
    activation = "elu",
    deep_u = c(512, 512),
    deeper_u = c(512, 512, 256, 128),
    optimizer = keras::optimizer_adam(3*10^-4),
    deep = TRUE,
    deeper = TRUE,
    batchnorm = FALSE,
    dropout = FALSE,
    dropout_p = 0.1,
    epochs = 150L,
    munge_n = NULL,
    batch_size = 512L,
    emb_multiplier = 1.6
  )
}

tune_surrogate = function(self, continue = FALSE, save = TRUE, tune_munge = TRUE) {
  ins_path = paste0(self$subdir, "OptimInstance.rds")
  if (save && test_file_exists(ins_path) && !continue) {
    stopf(paste(ins_path, "exists and saving would overwrite it and continue = FALSE."))
  }
  p = ps(
    activation = p_fct(levels = c("elu", "relu")),
    deep_u = p_int(lower = 6, upper = 9, trafo = function(x) rep(2^x, 2), depends = deep == TRUE),
    deeper_u = p_int(lower = 6, upper = 9, trafo = function(x) 2^c(x, x, x - 1, x - 2), depends = deeper == TRUE),
    deep = p_lgl(),
    deeper = p_lgl(),
    munge_n =  p_int(lower = 2L, upper = ifelse(tune_munge, 4L, 2L), trafo = function(x) {if (x <= 2L) {NULL} else {10^x}}),
    batchnorm = p_lgl(),
    emb_multiplier = p_dbl(lower = 1.5, upper = 2)
  )
  opt = bbotk::opt("random_search")
  obj = bbotk::ObjectiveRFun$new(
    fun = function(xs) {
      xs = mlr3misc::insert_named(default_model_config(), xs)
      ret = fit_surrogate(self, xs, overwrite = FALSE, plot = FALSE)
      keras::k_clear_session()
      list(rsq = ret[grp == "_full_",][1, ]$rsq, metrics = ret)
    },
    domain = p,
    codomain = ps(rsq = p_dbl(lower = 0, upper = 1, tags = "maximize")),
    check_values = FALSE
  )
  ins = bbotk::OptimInstanceSingleCrit$new(obj, terminator = bbotk::trm("evals", n_evals = 10L))
  if (continue) {
    assert_file_exists(ins_path)
    old_ins = readRDS(ins_path)
    ins$archive$data = old_ins$archive$data
    ins$terminator$param_set$values$n_evals = NROW(ins$archive$data) + ins$terminator$param_set$values$n_evals
  }
  opt$optimize(ins)
  if (save) {
    saveRDS(ins, paste0(self$subdir, "OptimInstance.rds"))
  }
  return(ins)
}
