fit_rbv2_svm_surrogate = function(batchnorm = FALSE, dropout = FALSE, go_deeper = TRUE, wts_pow = 0L, optimizer = optimizer_adam(3e-4)) {

  ## -- Data
  n = 10^6
  dt = data.table(readARFF("../metadata/rbv2_mlr_classif.svm.arff"))
  # dt = dt[dataset %in% c("Internet.Advertisements", "sylva_prior", "pc4", "pc3"),]
  target_vars = c("perf.mmce", "perf.logloss", "traintime", "predicttime")
  dt[, task_id := NULL]
  dt[, learner := NULL]
  set.seed(123L)
  ban = copy(dt)
  rt_min = min(ban$traintime) * 0.5
  rt_range = (max(ban$traintime) - rt_min) * 1.05
  ban[, traintime := ((traintime - rt_min) / rt_range)]
  pt_min = min(ban$predicttime) * 0.5
  pt_range = (max(ban$predicttime) - pt_min) * 1.05
  ban[, predicttime := ((predicttime - pt_min) / pt_range)]
  ll_min = min(ban$perf.logloss) * 0.5
  ll_range = (max(ban$perf.logloss) - ll_min) * 1.05
  ban[, perf.logloss := ((perf.logloss - ll_min) / ll_range)]
  mm_min = min(ban$perf.mmce) * 0.5
  mm_range = (max(ban$perf.mmce) - mm_min) * 1.05
  ban[, perf.mmce := ((perf.mmce - mm_min) / mm_range)]
  ban[, cost := log(cost, base = 10) / 15]
  ban[, gamma := log(gamma, base = 10)/ 11]
  ban = map_dtc(ban, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.character(x)) x = as.factor(x)
    if (is.numeric(x) | is.integer(x)) x[is.na(x)] = 0
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
  test = sample(seq_len(nrow(ban)), 1000)
  x_test = ban[test, ]
  y_test = as.matrix(ban[test, ..target_vars])
  ban = ban[-test,]
  ban = augment_with_munge(ban, target_vars, 10^4)
  y = as.matrix(ban[, ..target_vars])
  ban[, which(colnames(ban) %in% target_vars[-1]) := NULL]
  #ban = ban[1:10,]
  t = TaskRegr$new("ban", backend = ban, target = target_vars[1])

  ## -- Hyperpars
  dropout_p = 0.5
  activation = "elu"
  input_shape =  list(t$ncol - 1L)
  output_shape = length(target_vars)

  embd = make_embedding(t) # this returns the final layer AND the inputs. Inputs are needed to construct the model later on
  model = embd$layers

  # Wide part: This is just a linear model
  wide = model %>% layer_dense(output_shape)

  # Deep part: This is a 2 hidden layer NN
  units = c(256, 256)
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

  # Deeper part: This is a 4 hidden layer NN
  model = layer_add(inputs = list(wide, deep %>% layer_dense(units = output_shape)))
  deeper_concat = TRUE
  if (go_deeper) {
    units = c(256, 256, 128, 128)
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
  model = model %>% layer_activation("sigmoid")
  model = keras_model(inputs = embd$inputs, outputs = model)
  model %>%
    compile(
      optimizer = optimizer,
      loss = "mean_squared_error"
    )

  rs = reshape_data_embedding(ban[, which(colnames(ban) %in% target_vars[1]) := NULL])

  # Save categorical lookup dict with model
  dicts = map(rs$fct_levels, function(x) {
    dt = data.table(level = x, int = seq_along(x) - 1L)
    setkey(dt, "level")
  })
  # saveRDS(dicts, "../metadata/rbv2_svm_dicts.rds")

  wts = (1-y[,1])^wts_pow / sum((1-y[,1])^wts_pow) * nrow(y)
  cbs = list(cb_es(patience = 20L))
  history = model %>%
    fit(
      x = rs$data,
      y = y,
      batch_size = 256L,
      validation_split = 0.2,
      epochs=100L,
      sample_weight = wts,
      callbacks = cbs
    )
  if (TRUE) {
    keras::save_model_hdf5(model, "rbv2_svm_wide_and_deeper_50.hdf5")

    # In-sample
    p = predict(model, rs$data)
    rib = mlr3measures::rsq(y[,1], p[,1])

    ytest = y_test
    x_test[, which(colnames(x_test) %in% target_vars) := NULL]
    rs2 = reshape_data_embedding(x_test)
    ptest = as.matrix(predict(model, rs2$data))
    qp = quantile(ptest[,1], 0)
    t0 = mlr3measures::rsq(ytest[ptest[,1] > qp,1], ptest[ptest[,1] > qp,1])

    library(patchwork)
    library(ggplot2)
    p1 = ggplot(data.frame(x = ytest[,1], y = ptest[,1]), aes(x=x, y=y)) + geom_point() + geom_abline(slope = 1, color = "blue")
    p2 = plot(history)
    print(p1 + p2)
    return(list(y = t0, rsq_inbag = rib, hist = history))
  } else {
    list(y = min(history$metrics$loss))
  }
}
