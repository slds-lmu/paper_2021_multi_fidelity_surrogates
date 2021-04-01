library(paradox)


classif.glmnet = ps(
  # alpha: [-Inf;0] L1, [1; Inf] L2, [0;1] elasticnet (15/15/70)% approx.
  p_dbl("alpha", lower = 0, upper = 1, default = 1, trafo = function(x) max(0, min(1, x))),
  p_dbl("s", lower = -10, upper = 10, default = 0, trafo = function(x) 2^x)
)

classif.rpart = ps(
  p_dbl("cp", lower = -10, upper = 0, default = log2(0.01), trafo = function(x) 2^x),
  p_int("maxdepth", lower = 1, upper = 30, default = 30),
  p_int("minbucket", lower = 1, upper = 100, default = 1),
  p_int("minsplit", lower = 1, upper = 100, default = 20)
)





classif.svm.fixed_pars = list("fitted" = FALSE)

classif.svm.radial = ps( # Only radial basis function kernel
  p_dbl("cost", lower = -12, upper = 12, trafo = function(x) 2^x),
  p_dbl("gamma", lower = -12, upper = 12, trafo = function(x) 2^x),
  p_dbl("tolerance", lower = -12, upper = -3, trafo = function(x) 2^x),
  p_lgl("shrinking")
)
classif.svm.radial.fixed_pars = list("fitted" = FALSE)

# => See RLearner.classif.ranger.pow.R
classif.ranger.pow = ps(
  num.trees = p_int("num.trees", lower = 1, upper = 2000),
  replace = p_lgl("replace"),
  p_dbl("sample.fraction", lower = 0.1, upper = 1),
  p_int("mtry.power", lower = 0, upper = 1),
  p_fct("respect.unordered.factors", values = c("ignore", "order", "partition")),
  p_int("min.node.size", lower = 1, upper = 100),
  p_fct("splitrule", values = c("gini", "extratrees")),
  p_int("num.random.splits", lower = 1, upper = 100, default = 1L, requires = quote(splitrule == "extratrees")))
classif.ranger.pow.fixed_pars = list("num.threads" = 1L)

classif.xgboost.gblinear = ps(
  p_int("nrounds", lower = 3, upper = 11, trafo = function(x) round(2^x)),
  p_dbl("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  p_dbl("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
  p_dbl("subsample",lower = 0.1, upper = 1)
)
classif.xgboost.gblinear.fixed_pars = list("nthread" = 1L, booster = "gblinear")

classif.xgboost.gbtree = ps(
  p_int("nrounds", lower = 3, upper = 11, trafo = function(x) round(2^x)),
  p_dbl("eta",   lower = -10, upper = 0, trafo = function(x) 2^x),
  p_dbl("gamma", lower = -15, upper = 3, trafo = function(x) 2^x),
  p_dbl("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  p_dbl("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
  p_dbl("subsample",lower = 0.1, upper = 1),
  p_int("max_depth", lower = 1, upper = 15),
  p_dbl("min_child_weight",  lower = 0, upper = 7, trafo = function(x) 2^x),
  p_dbl("colsample_bytree",  lower = 0.01, upper = 1),
  p_dbl("colsample_bylevel", lower = 0.01, upper = 1)
  # p_fct("tree_method", values = c("exact", "auto", "approx", "hist")), # CURRENTLY NOT IMPLEMENTED IN MLR
)
classif.xgboost.gbtree.fixed_pars = list("nthread" = 1L, booster = "gbtree")

classif.xgboost.dart = ps(
  p_int("nrounds", lower = 3, upper = 11, trafo = function(x) round(2^x)),
  p_dbl("eta",   lower = -10, upper = 0, trafo = function(x) 2^x),
  p_dbl("gamma", lower = -15, upper = 3, trafo = function(x) 2^x),
  p_dbl("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  p_dbl("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
  p_dbl("subsample",lower = 0.1, upper = 1),
  p_int("max_depth", lower = 1, upper = 15),
  p_dbl("min_child_weight",  lower = 0, upper = 7, trafo = function(x) 2^x),
  p_dbl("colsample_bytree",  lower = 0.01, upper = 1),
  p_dbl("colsample_bylevel", lower = 0.01, upper = 1),
  p_dbl("rate_drop", lower = 0, upper = 1),
  p_dbl("skip_drop", lower =  0, upper = 1))
classif.xgboost.dart.fixed_pars = list("nthread" = 1L, booster = "dart")

# Leave this out for now as there is no benefit over glmnet (LiblineaR svm can not do probs)
# # => See RLearner.classif.LiblineaR.R
# classif.LiblineaR = ps(
#   p_fct(id = "type", default = 0L, values = 0:7),
#   p_dbl(id = "cost", default = 10, lower = -10, upper = 10, trafo = function(x) 2^x),
#   p_dbl(id = "epsilon", default = log2(0.01), lower = -12, upper = 0, trafo = function(x) 2^x),
#   p_lgl(id = "bias", default = TRUE)
# )


classif.RcppHNSW = ps(
  p_int(id = "k", lower = 1L, upper = 50),
  p_fct(id = "distance", values = c("l2", "cosine", "ip"), default = "l2"),
  p_int(id = "M", lower = 18, upper = 50),
  p_dbl(id = "ef", lower = 3, upper = 8, trafo = function(x) round(2^x)),
  p_dbl(id = "ef_construction", lower = 4, upper = 9, trafo = function(x) round(2^x))
)

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
      p_fct(id = "init_seed", values = c(1L, 11L, 101L, 131L, 499L))
    )
classif.kerasff.fixed_pars = list(early_stopping_patience = 0L, validation_split = 0, nthread = 1L)
