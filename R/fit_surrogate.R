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
    data.table(
      variable = nms,
      rsq = mlr3measures::rsq(x,y),
      roh = mlr3measures::srho(x,y),
      # ktau = mlr3measures::ktau(x,y),
      mae = mlr3measures::mae(x,y)
    )
  })
  data.table::fwrite(metrics, paste0(cfg$subdir, "surrogate_test_metrics.csv"))
  print(metrics)
  
  if (plot) {
    require("ggplot2")
    require("patchwork")
    p1 = ggplot(data.frame(x = data$ytest[,1], y = ptest[,1]), aes(x=x, y=y)) +
      geom_point() +
      geom_abline(slope = 1, color = "blue")
    p2 = plot(history)
    p = p1 + p2
    print(p)
    ggsave(paste0(cfg$subdir, "surrogate_test_metrics.pdf"), plot = p)
  }
}

default_model_config = function() {
  list(
    activation = "relu",
    deep_u = c(512, 512),
    deeper_u = c(512, 512, 256, 128),
    optimizer = optimizer_adam(3*10^-4),
    deep = TRUE,
    deeper = TRUE,
    batchnorm = TRUE,
    dropout = FALSE,
    dropout_p = FALSE,
    epochs = 150L,
    munge_n = NULL
  )
}