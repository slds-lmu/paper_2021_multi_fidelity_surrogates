fit_nb301_surrogate = function(config, wts_pow = 3L, overwrite = overwrite) {
  data = config$data
  rs = reshape_data_embedding(data$xtrain)
  embd = make_embedding_dt(data$xtrain)

  model = embd$layers
  input_shape =  list(ncol(data$xtrain) - ncol(data$ytrain))
  output_shape = ncol(data$ytrain)
  model = make_architecture(model, input_shape, output_shape)

  cbs = list(cb_es(patience = 20L))
  history = model %>%
    fit(
      x = rs$data,
      y = y,
      batch_size = 512L,
      validation_split = 0.2,
      epochs = 150L,
      sample_weight = weights_from_target(y),
      callbacks = cbs
    )
  # Save model
  keras::save_model_hdf5(model, config$keras_model_path, overwrite = TRUE)
  keras_to_onnx(config$keras_model_path, config$onnx_model_path)

  # Test Data Metrics & Plots
  rs2 = reshape_data_embedding(data$xtest)
  ptest = as.matrix(predict(model, rs2$data))
  for(nm in names(data$ytest)) {
    cat("RSq.", nm, ":", mlr3measures::rsq(data$ytest[,nm], ptest[,nm]))
  }

  if (plot) {
    require_namespaces("ggplot2")
    require_namespaces("patchwork")
    require("ggplot2")
    require("patchwork")
    p1 = ggplot(data.frame(x = data$ytest[,1], y = ptest[,1]), aes(x=x, y=y)) +
      geom_point() +
      geom_abline(slope = 1, color = "blue")
    p2 = plot(history)
    print(p1 + p2)
  }
}

preproc_data_nb301 = function(path, seed = 123L, n_max = 10^5) {
  set.seed(seed)
  dt = readRDS(path)

  # Preproc train data
  train = dt[method != "rs", ]
  train = sample_max(train, n_max)
  train = map_dtc(train, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  train[, val_accuracy := val_accuracy/100]
  trafos = map(train[, "runtime"], scale_sigmoid)
  train[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  y = as.matrix(train[, c("val_accuracy", "runtime")])
  train[, method :=NULL]
  train[, runtime := NULL]
  train[, val_accuracy := NULL]

  # Preproc test data
  oob = dt[method == "rs", ]
  oob = map_dtc(oob, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  oob[, val_accuracy := val_accuracy/100]
  oob[, names(trafos) := pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), .SDcols = names(trafos)]
  ytest = as.matrix(oob[, c("val_accuracy", "runtime")])
  if ("method" %in% colnames(oob)) oob[, method := NULL]
  oob[, runtime := NULL]
  oob[, val_accuracy := NULL]

  list(
    xtrain = train,
    yrain = y,
    xtest = oob,
    ytest = ytest
  )
}