ps = ps(
  cp = p_dbl(lower = -10, upper = 0, default = log2(0.01), trafo = function(x) 2^x),
  maxdepth = p_int(lower = 1, upper = 30, default = 30),
  minbucket = p_int(lower = 1, upper = 100, default = 1),
  minsplit = p_int(lower = 1, upper = 100, default = 20)
)

preproc.pipeline <- pSS(
  num.impute.selected.cpo: discrete [impute.mean, impute.median, impute.hist]  # numeric feature imputation to use
)