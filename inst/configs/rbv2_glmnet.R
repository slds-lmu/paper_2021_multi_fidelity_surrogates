ps = ps(
  alpha = p_dbl(lower = 0, upper = 1, default = 1, trafo = function(x) max(0, min(1, x))),
  s = p_dbl(lower = -10, upper = 10, default = 0, trafo = function(x) 2^x)
)

preproc.pipeline <- pSS(
  num.impute.selected.cpo: discrete [impute.mean, impute.median, impute.hist]  # numeric feature imputation to use
)