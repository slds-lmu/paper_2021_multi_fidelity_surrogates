ps = ps(
  x1 = p_dbl(lower = -5, upper = 10),
  x2 = p_dbl(lower = 0, upper = 15),
  fid = p_dbl(lower = 0, upper = 1)
)

branin = function(x1, x2, fid) {
  a = 1
  b = 5.1/(4 * pi^2)
  c = 5/pi
  d = 6
  e = 10
  f = 1/(8 * pi)
  x1 = x1 + (1 - fid)
  x2 = x2 * (1 - fid)
  y = ((x2 - b * x1^2 + c * x1 - d)^2 + e * (1 - f) * cos(x1) + e)
  t = (fid - 0.5) * 10
  0.5 * y  * ifelse(t == 5, 1, (1 + e^-t)) + 0.5 * y
}


plot_branin = function() {
  dt = expand.grid(
    x = seq(from = -5, to = 10, length.out = 50),
    y = seq(from =  0, to = 15, length.out = 50),
    z = seq(from = 0, to = 1, length.out = 10)
  )
  dt$val = NA

  for (i in seq_len(nrow(dt))) {
    dt$val[i] = branin(dt[i,1],dt[i,2],dt[i,3])
  }

  library(ggplot2)
  ggplot(dt[y == 0]) +
    geom_contour_filled(aes(x = x, y = z, z = log(val)))
}