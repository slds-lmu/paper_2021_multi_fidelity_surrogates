test_trafo = function(x, scale_fun, range = c(0,1), ...) {
  expect_output({tfs = scale_fun(x, ...)})
  tfd = tfs$trafo(x)
  if (!is.null(range)) expect_equal(range(tfd), range)
  expect_equal(tfs$retrafo(tfd), x)
}

test_that("sigmoid", {
  for (p in c(0, 0.03, 0.1)) {
    x = rnorm(10, rnorm(1), runif(1, 0, 10))
    test_trafo(x, scale_sigmoid, range = c(p, 1-p), p=p)
  }
  test_trafo(10^-3:3, scale_sigmoid, range = c(0.03, 0.97))
  expect_error(scale_sigmoid(x, p = 0.5))
})

test_that("base", {
  for (base in c(2, 10)) {
    x = abs(rnorm(10, rnorm(1), runif(1, 0, 10)))^base
    test_trafo(x, scale_base, range=NULL, base = base)
  }
  test_trafo(10^-3:3, scale_base, range = NULL)
  expect_error(scale_base(x, base = -1))
})

test_that("scale_base", {
  x = runif(100, 0, 100)
  expect_output({sc = scale_base(x, base = 10)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))

  x = runif(100, 0, 100)^2
  expect_output({sc = scale_base(x, base = 2)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-6))

  x = runif(100, 0, 100)
  expect_output({sc = scale_base(x, base = 2)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))
})



test_that("scale_base_0_1", {
  x = runif(100)
  expect_output({sc = scale_base_0_1(x)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))

  x = runif(100, 0, 1000)
  expect_output({sc = scale_base_0_1(x)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))

  x = runif(100)
  expect_output({sc = scale_base_0_1(x, p = 0, base = 2)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))

  x = runif(100, 0, 1000)
  expect_output({sc = scale_base_0_1(x, p = 0, base = 2)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))

  x = runif(100)
  expect_output({sc = scale_base_0_1(x, p = .1, base = 1)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))

  x = runif(100, 0, 1000)
  expect_output({sc = scale_base_0_1(x, p = .1, base = 1)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))
})

test_that("neg_exp", {
  x = runif(100, 0, 100)
  expect_output({sc = scale_neg_exp(x)})
  xs = sc$trafo(x)
  xr = sc$retrafo(xs)
  expect_true(all((x - xr) < 1e-12))
})
