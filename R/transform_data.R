# Sample rows up to a maximum of nmax
sample_max = function(dt, n_max) {
  dt[sample(seq_len(nrow(dt)), min(n_max, nrow(dt))),]
}

# Scale to ~[0, 1] with some leeway e.g. scale to [0.05, 0.95] depending on params.
scale_sigmoid = function(x, mul_min = 0.5, mul_range = 1.05) {
  rt_min = min(x) * mul_min
  rt_range = (max(x) - min(x)) * mul_range
  list(
    trafo = function(x) {(x - rt_min) / rt_range},
    retrafo = function(x) {x * rt_range + rt_min}
  )
}

# Get weights vector by exponentiating
weights_from_target = function(y, minimize = TRUE, j = 1L, wts_pow = 0L) {
  if (wts_pow) {
    if (minimize) {
      yy = (1  - y[,j])
    } else {
      yy = y[,j]
    }
    yy = yy^wts_pow
    yy / sum(yy) * nrow(y)  
  } else {
    rep(0, nrow(y))
  }
}