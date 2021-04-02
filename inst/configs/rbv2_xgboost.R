ps = ps(
  booster = p_fct(levels = c("gblinear", "gbtree", "dart")),
  nrounds = p_int(lower = 3, upper = 11, trafo = function(x) round(2^x)),
  eta = p_dbl(lower = -10, upper = 0, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
  gamma = p_dbl(lower = -15, upper = 3, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
  lambda = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x),
  alpha = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x),
  subsample = p_dbl(lower = 0.1, upper = 1),
  max_depth = p_int(lower = 1, upper = 15), depends = booster %in% c("dart", "gbtree"),
  min_child_weight = p_dbl(lower = 0, upper = 7, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
  colsample_bytree = p_dbl(lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
  colsample_bylevel = p_dbl("", lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
  rate_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
  skip_drop = p_dbl(lower =  0, upper = 1, depends = booster == "dart")
)
# classif.xgboost.fixed_pars = list("nthread" = 1L)
target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime")