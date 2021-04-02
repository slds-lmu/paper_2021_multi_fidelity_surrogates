ps = ps(
  num.trees = p_int(lower = 1, upper = 2000),
  replace = p_lgl(),
  sample.fraction = p_dbl(lower = 0.1, upper = 1),
  mtry.power = p_int(lower = 0, upper = 1),
  respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
  min.node.size = p_int(lower = 1, upper = 100),
  splitrule = p_fct(levels = c("gini", "extratrees")),
  num.random.splits = p_int(lower = 1, upper = 100, default = 1L, depends = splitrule == "extratrees"),
  num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
)
# classif.ranger.pow.fixed_pars = list("num.threads" = 1L)

target_variables = c("perf.mmce", "perf.logloss", "traintime", "predicttime")