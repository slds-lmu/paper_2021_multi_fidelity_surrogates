fit_surrogate = function(config, model_config, wts_pow = 3L, overwrite = TRUE) {
  data = config$data
  rs = reshape_data_embedding(data$xtrain)
  embd = make_embedding_dt(data$xtrain)

  model = embd$layers
  input_shape =  list(ncol(data$xtrain) - ncol(data$ytrain))
  output_shape = ncol(data$ytrain)
  model = make_architecture(model, input_shape, output_shape, model_config)

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


preproc_data_rbv2_svm = function(config) {
  n = 10^6
  dt = config$data
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
  ban[, gamma := log(gamma, base = 10) / 11]
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
}