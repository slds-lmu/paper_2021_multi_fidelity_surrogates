test_that("apply_cummean_var", {
  dt = data.table(
      m1 = runif(1000),
      m2 = runif(1000),
      s1 = runif(1000),
      repl = rep(1:10,  100),
      id = rep(1:100, each = 10)
  )
  d1 = apply_cummean_variance_param(
    copy(dt[id == 1,]),
    mean = c("m1", "m2"),
    sum = "s1", "repl", 
    ignore=NULL
  )
  d100 = apply_cummean_variance_param(
    copy(dt),
    mean = c("m1", "m2"),
    sum = "s1", "repl", 
    ignore=NULL
  )
  expect_true(all(d100[id == 1,] == d1))
  expect_true(d1[repl == 10, s1] ==  sum(dt[id == 1,]$s1))
  expect_true(d1[repl == 10, m1] == mean(dt[id == 1,]$m1))
  expect_true(d1[repl == 10, m2] == mean(dt[id == 1,]$m2))

  head(dt[repl == 1,]) - head(d100[repl == 1,])
})

test_that("cummean", {
    x = runif(1000)
    xm = cummean(x)
    expect_numeric(xm, lower = 0, upper = 1, len = 1000)
    expect_true(abs(xm[1000] - 0.5) < 0.05)
    expect_equal(xm /cumsum(x), 1 / seq_len(1000), tolerance = .01)

    x = rnorm(1000, mean = 0.5, sd =.1)
    xm = cummean(x)
    expect_true(abs(xm[1000] - 0.5) < 0.05)
    expect_equal(xm /cumsum(x), 1 / seq_len(1000), tolerance = .01)
})

