# => See RLearner.classif.ranger.pow.R
classif.ranger.pow = ps(
  num.trees = p_int("num.trees", lower = 1, upper = 2000),
  replace = p_lgl("replace"),
  p_dbl("sample.fraction", lower = 0.1, upper = 1),
  p_int("mtry.power", lower = 0, upper = 1),
  p_fct("respect.unordered.factors", values = c("ignore", "order", "partition")),
  p_int("min.node.size", lower = 1, upper = 100),
  p_fct("splitrule", values = c("gini", "extratrees")),
  p_int("num.random.splits", lower = 1, upper = 100, default = 1L, requires = quote(splitrule == "extratrees")),
  num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
)
classif.ranger.pow.fixed_pars = list("num.threads" = 1L)

preproc.pipeline <- pSS(
  num.impute.selected.cpo: discrete [impute.mean, impute.median, impute.hist]  # numeric feature imputation to use
)