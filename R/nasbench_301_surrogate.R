fit_nb301_surrogate = function(config, wts_pow = 3L) {

  data = preproc_data_nb301(config$path)
  rs = reshape_data_embedding(data$xtrain)

  embd = make_embedding_dt(data$xtrain, names(data$ytrain))
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
  keras::save_model_hdf5(model, config$model_path, overwrite = TRUE)

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


make_architecture = function(inputs, input_shape, output_shape,
  activation = "relu", units = c(512, 512), dropout_p = 0.5,
  batchnorm = FALSE, dropout = FALSE, deeper = TRUE) {
  
  # Wide part
  wide = inputs %>% layer_dense(output_shape)
  # Deep part
  deep = inputs
  for (i in seq_len(length(units))) {
    if (batchnorm) deep = deep %>% layer_batch_normalization()
    if (dropout) deep = deep %>% layer_dropout(dropout_p)
    deep = deep %>%
      layer_dense(
        units = units[i],
        input_shape = if (i == 1) input_shape else NULL,
        activation = activation
      )
  }
  model = layer_add(inputs = list(wide, deep %>% layer_dense(units = output_shape)))
  
  if (deeper) {
    units = c(512, 512, 256, 128)
    deeper = make_layers(inputs, units, batchnorm=batchnorm, dropout=dropout, dropout_p=dropout_p, activation=activation)
    model = layer_add(inputs = list(model, deeper %>% layer_dense(units = output_shape)))
  }
  model = model %>% layer_activation("sigmoid")
  model = keras_model(inputs = embd$inputs, outputs = model)
  model %>%
    compile(
      optimizer = optimizer_adam(3*10^-4),
      loss = "mean_squared_error"
    )
}

preproc_data_nb301 = function(path, seed = 123L, n_max = 10^6) {
  requireNamespace("mlr3misc")
  dt = readRDS(path)
  set.seed(123L)
  
  train = dt[method != "rs", ]
  train = sample_max(train, nmax)
  map_dtc(train, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  train[, val_accuracy := val_accuracy/100]
  trafos = map(train[, "runtime"], scale_sigmoid)
  train[, pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), names(trafos)]
  y = as.matrix(train[, c("val_accuracy", "runtime")])
  train[, method :=NULL]
  train[, runtime := NULL]
  
  
  oob = dt[method == "rs", ]
  oob = map_dtc(oob, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  oob[, val_accuracy := val_accuracy/100]
  train[, pmap(list(.SD, trafos), function(x, t) {t$trafo(x)}), names(trafos)]
  ytest = as.matrix(oob[, c("val_accuracy", "runtime")])
  oob[, method := NULL]
  oob[, runtime := NULL]
  oob[, val_accuracy := NULL]
  
  list(
    xtrain = train,
    yrain = y,
    xtest = oob,
    ytest = ytest
  )
}