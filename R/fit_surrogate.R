fit_surrogate = function(problem_config, model_config = default_model_config(), overwrite = FALSE, plot = TRUE) {
  data = problem_config$data
  data = munge_data(data, target_vars = problem_config$target_variables, munge_n = model_config$munge_n)
  rs = reshape_data_embedding(data$xtrain)
  embd = make_embedding_dt(data$xtrain)

  input_shape =  list(ncol(data$xtrain) - ncol(data$ytrain))
  output_shape = ncol(data$ytrain)
  model = make_architecture(embd, input_shape, output_shape, model_config)

  cbs = list(cb_es(patience = 20L))
  history = model %>%
    fit(
      x = rs$data,
      y = data$ytrain,
      batch_size = 512L,
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
  rs2 = reshape_data_embedding(data$xtest)
  ptest = as.matrix(predict(model, rs2$data))
  colnames(ptest) = cfg$target_variables
  colnames(data$ytest) = cfg$target_variables
  metrics = map_dtr(colnames(ptest), function(nms) {
    x = data$ytest[, nms]
    y = ptest[, nms]
    smp = sample(length(x), 1000L)
    data.table(
      variable = nms,
      rsq = mlr3measures::rsq(x,y),
      roh = mlr3measures::srho(x,y),
      ktau = mlr3measures::ktau(x[smp],y[smp]), # on sample since this is slow.
      mae = mlr3measures::mae(x,y)
    )
  })

  
  if (overwrite) data.table::fwrite(metrics, paste0(cfg$subdir, "surrogate_test_metrics.csv"))
  if (plot) {
    print(metrics)
    require("ggplot2")
    require("patchwork")
    p1 = ggplot(data.frame(x = data$ytest[,1], y = ptest[,1]), aes(x=x, y=y)) +
      geom_point() +
      geom_abline(slope = 1, color = "blue")
    p2 = plot(history)
    p = p1 + p2
    print(p)
    if (overwrite) ggsave(paste0(cfg$subdir, "surrogate_test_metrics.pdf"), plot = p)
  }
  return(metrics)
}

default_model_config = function() {
  list(
    activation = "relu",
    deep_u = c(512, 512),
    deeper_u = c(512, 512, 256, 128),
    optimizer = optimizer_adam(3*10^-4),
    deep = TRUE,
    deeper = TRUE,
    batchnorm = FALSE,
    dropout = FALSE,
    dropout_p = FALSE,
    epochs = 150L,
    munge_n = NULL
  )
}


tune_surrogate = function(self) {
  p = ps(
    activation = p_fct(levels = c("elu", "relu")),
    deep_u = p_int(lower = 6, upper = 9, trafo = function(x) rep(2^x, 2)),
    deeper_u = p_int(lower = 6, upper = 9, trafo = function(x) 2^c(x,x,x-1, x-2)), 
    deep = p_lgl(),
    deeper = p_lgl(),
    munge_n = p_int(lower = 1, upper = 4, trafo = function(x) {if (x == 1L) {NULL} else {10^x}}),
    batchnorm = p_lgl()
  )
  opt = bbotk::opt("random_search")
  obj = ObjectiveRFun$new(
    fun = function(xs) {
      xs = mlr3misc::insert_named(default_model_config(), xs)
      xs$epochs = 1L
      xs$munge_n = NULL
      ret = fit_surrogate(self, xs, plot = FALSE)
      list(rsq = ret[1,]$rsq, metrics = ret)
    }, 
    domain = p, 
    codomain = ps(rsq = p_dbl(lower = 0, upper = 1, tags = "maximize")),
    check_values = FALSE
  )
  ins = bbotk::OptimInstanceSingleCrit$new(obj, terminator = trm("evals", n_evals = 10L))
  opt$optimize(ins)
}