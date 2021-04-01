classif.kerasff = ps(
  p_dbl(id = "epochs", lower = 3, upper = 7, trafo = function(x) round(2^x)),
  p_fct(id = "optimizer", values = c("sgd", "rmsprop", "adam")),
  p_dbl(id = "lr", lower = -5, upper = 0, trafo = function(x) 5^x),
  p_dbl(id = "decay", lower = -8, upper = 0, trafo = function(x) 5^x),
  p_dbl(id = "momentum", lower = -8, upper = 0,trafo = function(x) 5^x,
    requires = quote(optimizer == "sgd")),
  p_int(id = "layers", lower = 1L, upper = 4L),
  p_fct(id = "batchnorm_dropout", values = c("batchnorm", "dropout", "none")),
  p_dbl(id = "input_dropout_rate", lower = -5, upper = 0, requires = quote(batchnorm_dropout == "dropout"), trafo =  function(x) 3^(x/2)),
  p_dbl(id = "dropout_rate", lower = -5, upper = 0, requires = quote(batchnorm_dropout == "dropout"), trafo =  function(x) 3^(x/2)),
  # Neurons / Layers
  p_dbl(id = "units_layer1", lower = 3L, upper = 9, trafo = function(x) round(2^x)),
  p_dbl(id = "units_layer2", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 2)),
  p_dbl(id = "units_layer3", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 3)),
  p_dbl(id = "units_layer4", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 4)),
  # Activations
  p_fct(id = "act_layer", values = c("relu", "tanh")),
  # Initializers
  p_fct(id = "init_layer", values = c("glorot_normal", "glorot_uniform", "he_normal", "he_uniform")),
  # Regularizers
  p_dbl(id = "l1_reg_layer",
    lower = -10, upper = -2, trafo = function(x) 5^x),
  p_dbl(id = "l2_reg_layer",
    lower = -10, upper = -2, trafo = function(x) 5^x),
  p_lgl(id = "learning_rate_scheduler", default = FALSE),
  p_fct(id = "init_seed", values = c(1L, 11L, 101L, 131L, 499L)),
  num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
)
classif.kerasff.fixed_pars = list(early_stopping_patience = 0L, validation_split = 0, nthread = 1L)

preproc.pipeline <- pSS(
  num.impute.selected.cpo: discrete [impute.mean, impute.median, impute.hist]  # numeric feature imputation to use
)