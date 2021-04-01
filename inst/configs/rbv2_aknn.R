classif.RcppHNSW = ps(
  p_int(id = "k", lower = 1L, upper = 50),
  p_fct(id = "distance", values = c("l2", "cosine", "ip"), default = "l2"),
  p_int(id = "M", lower = 18, upper = 50),
  p_dbl(id = "ef", lower = 3, upper = 8, trafo = function(x) round(2^x)),
  p_dbl(id = "ef_construction", lower = 4, upper = 9, trafo = function(x) round(2^x))
)
