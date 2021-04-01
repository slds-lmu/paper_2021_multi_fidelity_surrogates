ps = ps( # Only radial basis function kernel
  kernel = p_fct(levels = c("linear", "polynomial", "radial")),
  cost =  p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x),
  gamma = p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x, depends = kernel == "radial"),
  tolerance = p_dbl(lower = -12, upper = -3, trafo = function(x) 2^x),
  degree = p_int(lower = 2, upper = 5, depends = kernel == "polynomial"),
  shrinking = p_lgl(),
  num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
)
# classif.svm.fixed_pars = list("fitted" = FALSE)

target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime")