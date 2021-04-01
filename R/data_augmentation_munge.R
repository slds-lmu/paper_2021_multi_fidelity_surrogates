augment_with_munge = function(data, target_vars, n_augment = 10000, n_max_train = 10000, stratify = "dataset") {
  library(mlr3learners)
  rng = lrn("regr.ranger")

  news = map(unique(data[[stratify]]), function(stratum) {
    print(paste0("stratum:", stratum))
    dt = copy(data)[data[[stratify]] ==stratum,]
    set(dt, j=stratify, value=NULL)
    dt[, which(colnames(dt) %in% target_vars[-1]) := NULL]
    t = TaskRegr$new("ban", backend = dt, target = target_vars[1])
    tm = t$clone()$filter(sample(t$row_ids, min(length(t$row_ids), n_augment)))
    orig_rows = tm$row_ids
    while (tm$nrow < n_augment + length(orig_rows)) {
      x = munge_task(task = tm)
      tm$rbind(x)
    }
    tm$filter(setdiff(tm$row_ids, orig_rows))
    y_predict = map_dtc(1:4, function(i) {
      dt = copy(data)
      dt = dt[data[[stratify]] == stratum,]
      set(dt, j=stratify, value=NULL)
      dt = dt[sample(seq_len(nrow(dt)), min(nrow(dt), n_max_train)),]
      dt[, which(colnames(dt) %in% target_vars[-i]) := NULL]
      t = TaskRegr$new("ban", backend = dt, target = target_vars[i])
      l = rng$clone()
      l$train(t)
      print(l$predict(t)$score(list(msr("regr.rsq"), msr("regr.rmse"))))
      p = l$predict(tm)
      p$data$response
    })
    names(y_predict) = target_vars
    res = data.table(cbind(y_predict, tm$data(cols = tm$feature_names)))
    set(res, j = stratify, value = stratum)
    return(res)
  })
  na.omit(rbind(data, rbindlist(news)))
}

make_embedding = function(task, embed_size = NULL, embed_dropout = 0, embed_batchnorm = FALSE, emb_multiplier = 1.6) {
  typedt = task$feature_types
  data = as.matrix(task$data(cols = task$feature_names))
  target = task$data(cols = task$target_names)

  embed_vars = typedt[typedt$type %in% c("ordered", "factor", "character"),]$id
  n_cont = nrow(typedt) - length(embed_vars)

  # Embeddings for categorical variables: for each categorical:
  # - create a layer_input
  # - create an embedding
  # - apply dropout
  embds = list()
  if (length(embed_vars) > 0) {
    embds = map(.f = function(feat_name) {
      x = data[,feat_name]
      if (is.factor(x)) n_cat = length(levels(x)) else n_cat = length(unique(x))
      # Use heuristic from fast.ai https://github.com/fastai/fastai/blob/master/fastai/tabular/data.py
      # or a user supplied value
      if (length(embed_size) >= 2) embed_size = embed_size[feat_name]
      if (length(embed_size) == 0) embed_size = min(600L, round(emb_multiplier * n_cat^0.56))
      input = layer_input(shape = 1, dtype = "int32", name = feat_name)
      layers = input %>%
        layer_embedding(input_dim = as.numeric(n_cat), output_dim = as.numeric(embed_size),
                        input_length = 1L, name = paste0("embed_", feat_name),
                        embeddings_initializer = initializer_he_uniform()) %>%
        layer_dropout(embed_dropout, input_shape = as.numeric(embed_size)) %>%
        layer_flatten()
      return(list(input = input, layers = layers))
    }, embed_vars)
  }

  # Layer for the continuous variables
  # - apply batchnorm
  if (n_cont > 0) {
    input = layer_input(shape = n_cont, dtype = "float32", name = "continuous")
    layers = input
    if (embed_batchnorm) layers = layers %>% layer_batch_normalization(input_shape = n_cont, axis = 1)
    embds = c(embds, list(cont = list(input = input, layers = layers)))
  }

  # Concatenate all layers
  if (length(embds) >= 2)
    layers = layer_concatenate(unname(lapply(embds, function(x) x$layers)))
  else
    layers = unname(embds[[1]]$layers)
  return(list(inputs = lapply(embds, function(x) x$input), layers = layers))
}

keras_to_onnx = function(model_path = "wide_and_deeper_50.hdf5") {
  k2o = reticulate::import("keras2onnx")
  model = keras::load_model_hdf5(model_path)
  onnx = k2o$convert_keras(model, model$name)
  k2o$save_model(onnx, gsub("hdf5", "onnx", model_path))
}

run_with_mlflow = function(xs) {
  mlflow::mlflow_start_run()
  mlr3misc::imap(xs, function(value, name) {
    mlflow::mlflow_log_param(name, as.character(value))
  })
  output = callr::r_safe(run_fit_svm, xs)
  mlr3misc::imap(output, function(value, name) {
    mlflow::mlflow_log_metric(name, as.numeric(value))
  })
  mlflow::mlflow_end_run()
  return(output)
}
