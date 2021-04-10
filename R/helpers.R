# Map a character to the correct integer using a dict and impute NA's
char_to_int = function(x, param_name, dict) {
  x[is.na(x)] = "None"
  matrix(dict[[param_name]][x,]$int)
}

# Trafo numerics using a trafo dict and impute NA's
trafo_numerics = function(xdt, trafo_dict) {
  l = mlr3misc::keep(xdt, is.numeric)
  imap(l, function(x, nm) {
    x[is.na(x)] = 0
    if (nm %in% names(trafo_dict)) {
      trafo_dict[[nm]]$trafo(x)
    } else {
      x
    }
  })
}

# Sample rows up to a maximum of nmax
sample_max = function(dt, n_max) {
  dt[sample(seq_len(nrow(dt)), min(n_max, nrow(dt))),]
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
    return(yy / sum(yy) * nrow(y))
  } else {
    return(rep(1L, nrow(y)))
  }
}

split_by_col = function(dt, by = 'task_id', frac = 0.1) {
  dt[, rn := seq_len(nrow(dt))]
  test_idx = dt[, .(rn = sample(rn, ceiling(.N * frac))), keyby = by]
  dt[, "rn" := NULL]
  list(
    test = dt[test_idx$rn,],
    train = dt[!test_idx$rn,]
  )
}

preproc_iid = function(dt) {
    dt = map_dtc(dt, function(x) {
    if (is.logical(x)) x = as.numeric(x)
    if (is.character(x)) x = as.factor(x)
    if (is.numeric(x) | is.integer(x)) x[is.na(x)] = 0
    if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
    if (length(unique(x)) == 1) x = NULL
    return(x)
  })
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

scale_base = function(x, base = 10) {
  div = max(abs(log(c(min(x), max(x)), base = base)))
  list(
    trafo = function(x) {log(x, base = base) / div},
    retrafo = function(x) {base ^ (x*div)}
  )
}