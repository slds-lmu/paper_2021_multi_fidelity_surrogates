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

preproc.pipeline <- pSS(
  num.impute.selected.cpo: discrete [impute.mean, impute.median, impute.hist]  # numeric feature imputation to use
)