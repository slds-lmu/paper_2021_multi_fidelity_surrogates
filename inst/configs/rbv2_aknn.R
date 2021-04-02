ps = ps(
  k = p_int(lower = 1L, upper = 50),
  distance = p_fct(levels = c("l2", "cosine", "ip"), default = "l2"),
  M = p_int(lower = 18, upper = 50),
  ef = p_dbl(lower = 3, upper = 8, trafo = function(x) round(2^x)),
  ef_construction = p_dbl(ower = 4, upper = 9, trafo = function(x) round(2^x)),
  num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist"))
)
