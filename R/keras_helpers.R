# Generic deep and wide architecture
make_architecture = function(embedding, input_shape, output_shape, mcfg) {
  inputs = embedding$layers
  # Wide part
  wide = inputs %>% layer_dense(output_shape)
  # Deep part
  deep = inputs
  for (i in seq_len(length(mcfg$deep_u))) {
    if (mcfg$batchnorm) deep = deep %>% layer_batch_normalization()
    if (mcfg$dropout) deep = deep %>% layer_dropout(mcfg$dropout_p)
    deep = deep %>%
      layer_dense(
        units = mcfg$deep_u[i],
        input_shape = if (i == 1) input_shape else NULL,
        activation = mcfg$activation
      )
  }
  model = layer_add(inputs = list(wide, deep %>% layer_dense(units = output_shape)))
  if (mcfg$deeper) {
    deeper = make_layers(inputs, units=mcfg$deeper_u, batchnorm=mcfg$batchnorm, dropout = mcfg$dropout, dropout_p = mcfg$dropout_p, activation=mcfg$activation)
    model = layer_add(inputs = list(model, deeper %>% layer_dense(units = output_shape)))
  }
  model = model %>% layer_activation("sigmoid")
  model = keras_model(inputs = embedding$inputs, outputs = model)
  model %>%
    compile(
      optimizer = mcfg$optimizer,
      loss = "mean_squared_error"
    )
}

make_embedding_dt = function(dt, embed_size = NULL, embed_dropout = 0, embed_batchnorm = FALSE, emb_multiplier = 1.6) {
  emb_dt = copy(dt)[, tmp_target := runif(nrow(dt))]
  t = TaskRegr$new("train", backend = emb_dt, target = "tmp_target")
  emb = make_embedding(t, embed_size, embed_dropout, embed_batchnorm, emb_multiplier)
  return(emb)
}

make_embedding = function(task, embed_size = NULL, embed_dropout = 0, embed_batchnorm = FALSE, emb_multiplier = 1.6) {
  requireNamespace("keras")
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

make_layers = function(input, units, batchnorm, dropout, dropout_p, activation) {
  for (i in seq_len(length(units))) {
    if (batchnorm) input = input %>% layer_batch_normalization()
    if (dropout) input = input %>% layer_dropout(dropout_p)
    input = input %>%
      layer_dense(
        units = units[i],
        activation = activation
      )
  }
  return(input)
}

keras_to_onnx = function(keras_model, onnx_model) {
  k2o = reticulate::import("keras2onnx")
  model = keras::load_model_hdf5(model_path)
  onnx = k2o$convert_keras(model, model$name)
  k2o$save_model(onnx, gsub("hdf5", "onnx", model_path))
}
