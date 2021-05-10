# Scale to ~[0, 1] with some leeway e.g. scale to [0.05, 0.95] depending on params.
scale_sigmoid = function(x, p = 0.03) {
  assert_number(p, upper = 0.49)
  rt_min = min(x)
  rt_range = (max(x) - min(x)) / (1-2*p)
  cat("Scaling [", min(x), ";", max(x), "] to [", (min(x) - rt_min) / rt_range + p, ";", (max(x) - rt_min) / rt_range + p,"]\n")
  list(
    trafo = function(x) {
      x[is.na(x)] = rt_min
      (x - rt_min) / rt_range + p
    },
    retrafo = function(x) {(x-p) * rt_range + rt_min}
  )
}

scale_standard = function(x) {
  x = ifelse(x == 0, 1, x)
  mu = mean(x)
  sigma = sd(x)
  list(
    trafo = function(x) {x = ifelse(x == 0 | is.na(x), 1, x); (x - mu) / sigma},
    retrafo = function(x) {(x * sigma) + mu}
  )
}

scale_log_left_standard = function(x, constant = 1) {
  assert_number(constant, lower = 0)
  x = ifelse(x == 0, 1, x)
  x_trafoed = log(constant - x)
  mu = mean(x_trafoed)
  sigma = sd(x_trafoed)
  list(
    trafo = function(x) {x = ifelse(x == 0 | is.na(x), 1, x); (log(constant - x) - mu) / sigma},
    retrafo = function(x) {constant - exp(x * sigma + mu)}
  )
}

scale_base = function(x, base = 10) {
  assert_number(base, lower = 0)
  x = ifelse(x == 0, 1, x)
  div = max(abs(log(c(min(x), max(x)), base = base)))
  cat("Log-", base, "-scaling [", min(x), ";", max(x), "] to [", log(min(x), base)/div, ";", log(max(x), base)/div ,"]\n")
  list(
    trafo = function(x) {x = ifelse(x == 0 | is.na(x), 1, x); log(x, base = base) / div},
    retrafo = function(x) {base ^ (x*div)}
  )
}

scale_base_0_1 = function(x, base = 10, p = 0.01) {
  assert_number(p, upper = 0.49)
  assert_number(base, lower = 0)
  x = ifelse(x == 0, 1, x)
  rt_range = (max(x) - min(x)) / (1-2*p)
  rt_min = min(x)
  cat("Log-", base, "-[0,1]-scaling [", min(x), ";", max(x), "] to [",p,";",1-p,"]\n")
  list(
    trafo = function(x) {
      x = ifelse(x == 0 | is.na(x), 1, x);
      if (base > 1) x = log(x, base = base)
      (x - rt_min) / rt_range + p
    },
    retrafo = function(x) {
      ((base^x)-p) * rt_range + rt_min
    }
  )
}
