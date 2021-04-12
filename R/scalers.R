# Scale to ~[0, 1] with some leeway e.g. scale to [0.05, 0.95] depending on params.
scale_sigmoid = function(x, p = 0.03) {
  rt_min = min(x)
  rt_range = (max(x) - min(x)) / (1-2*p)
  cat("Scaling [", min(x), ";", max(x), "] to [", (min(x) - rt_min) / rt_range + p, ";", (max(x) - rt_min) / rt_range + p,"]\n")
  list(
    trafo = function(x) {(x - rt_min) / rt_range + p},
    retrafo = function(x) {(x-p) * rt_range + rt_min}
  )
}

scale_base = function(x, base = 10) {
  x = ifelse(x == 0, 1, x)
  div = max(abs(log(c(min(x), max(x)), base = base)))
  cat("Log-", base, "-scaling [", min(x), ";", max(x), "] to [", log(min(x), base)/div, ";", log(max(x), base)/div ,"]\n")
  list(
    trafo = function(x) {x = ifelse(x == 0, 1, x); log(x, base = base) / div},
    retrafo = function(x) {base ^ (x*div)}
  )
}