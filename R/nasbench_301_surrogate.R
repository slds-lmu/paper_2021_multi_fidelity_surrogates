run_fit = function(batchnorm = FALSE, dropout = FALSE, deeper = TRUE, wts_pow = 3L) {
  ## -- Data
  n = 10^6
  dt = readRDS("metadata/nb_301_data.rds")
  set.seed(123L)
  ban = dt[method != "rs", ]
  ban = map_dtc(ban, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  ban = ban[sample(seq_len(nrow(ban)), min(n, nrow(ban))),]
  ban[, val_accuracy := val_accuracy/100]
  # ban[, runtime := log(runtime)]
  rt_min = min(ban$runtime) * 0.5
  rt_range = (max(ban$runtime) - min(ban$runtime)) * .9
  ban[, runtime := ((runtime - rt_min) / rt_range)]
  ban[, method :=NULL]
  y = as.matrix(ban[, c("val_accuracy", "runtime")])
  ban[, runtime := NULL]
  t = TaskRegr$new("ban", backend = ban, target = "val_accuracy")

  ## -- Hyperpars
  dropout_p = 0.5
  activation = "relu"
  input_shape =  list(t$ncol - 1L)
  output_shape = 2L

  embd = make_embedding(t)
  model = embd$layers

  wide = model %>% layer_dense(output_shape)

  units = c(512, 512)
  deep = model
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
  deeper_concat = TRUE
  if (deeper) {
      units = c(512, 512, 256, 128)
      deeper = model
      for (i in seq_len(length(units))) {
      if (batchnorm) deeper = deeper %>% layer_batch_normalization()
      if (dropout) deeper = deeper %>% layer_dropout(dropout_p)
      deeper = deeper %>%
        layer_dense(
          units = units[i],
          activation = activation
        )
    }
    model = layer_add(inputs = list(model, deeper %>% layer_dense(units = output_shape)))
  }
  # tf = reticulate::import("tensorflow")
  # mul = tf$constant(c(1, rt_range), "float32")
  # add = tf$constant(c(0, rt_min), "float32")
  #
  # Connect deep part and activate %>%
  model = model %>% layer_activation("sigmoid")
  # model = layer_lambda(model, function(x) {
  #   x * mul + add
  # })

  model = keras_model(inputs = embd$inputs, outputs = model)
  model %>%
    compile(
      optimizer = optimizer_adam(3*10^-4),
      loss = "mean_squared_error"
    )

  rs = reshape_data_embedding(ban[, val_accuracy := NULL])
  wts = y[,1]^wts_pow / sum(y[,1]^wts_pow) * nrow(y)
  cbs = list(cb_es(patience = 20L))
  history = model %>%
    fit(
      x = rs$data,
      y = y,
      batch_size = 512L,
      validation_split = 0.2,
      epochs =150L,
      sample_weight = wts,
      callbacks = cbs
    )

  keras::save_model_hdf5(model, "wide_and_deeper_50.hdf5")

  # In-sample
  p = predict(model, rs$data)
  rib = mlr3measures::rsq(y[,1], p[,1])

  oob = dt[method == "rs", ]
  oob = map_dtc(oob, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  oob[, val_accuracy := val_accuracy/100]
  oob[, runtime := ((runtime - rt_min) / rt_range)]
  if ("method" %in% names(oob)) oob[, method := NULL]
  ytest = as.matrix(oob[, c("val_accuracy", "runtime")])
  oob[, runtime := NULL]
  oob[, val_accuracy := NULL]
  rs2 = reshape_data_embedding(oob)

  ptest = as.matrix(predict(model, rs2$data))
  qp = quantile(ptest[,1], 0)
  t0 = mlr3measures::rsq(ytest[ptest[,1] > qp,1], ptest[ptest[,1] > qp,1])
  # qp = quantile(ptest[,1], 0.5)
  # t50 = mlr3measures::rsq(ytest[ptest[,1] > qp,1], ptest[ptest[,1] > qp,1])
  # qp = quantile(ptest[,1], 0.8)
  # t80 = mlr3measures::rsq(ytest[ptest[,1] > qp,1], ptest[ptest[,1] > qp,1])

  library(patchwork)
  library(ggplot2)
  p1 = ggplot(data.frame(x = ytest[,1], y = ptest[,1]), aes(x=x, y=y)) + geom_point() + geom_abline(slope = 1, color = "blue")
  p2 = plot(history)
  print(p1 + p2)
  return(list(y = t0, rsq_inbag = rib, hist = history))
}

run_with_mlflow = function(xs) {
  mlflow::mlflow_start_run()
  mlr3misc::imap(xs, function(value, name) {
    mlflow::mlflow_log_param(name, as.character(value))
  })
  output = callr::r_safe(run_fit, xs)
  mlr3misc::imap(output, function(value, name) {
    mlflow::mlflow_log_metric(name, as.numeric(value))
  })
  mlflow::mlflow_end_run()
  return(output)
}
