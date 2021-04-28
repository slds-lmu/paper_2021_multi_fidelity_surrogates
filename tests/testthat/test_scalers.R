test_trafo = function(x, scale_fun, range = c(0,1), ...) {
  expect_output({tfs = scale_fun(x, ...)})
  tfd = tfs$trafo(x)
  if (!is.null(range)) expect_equal(range(tfd), range)
  expect_equal(tfs$retrafo(tfd),x)
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
  for (base in c(0.1, 2, 10)) {
    xx = rnorm(10, rnorm(1), runif(1, 0, 10))^base
    test_trafo(x, scale_base, range=NULL, base = base)
  }
  test_trafo(10^-3:3, scale_base, range = NULL)
  expect_error(scale_base(x, base = -1))
})

# test_that("base_0_1", {
#   for (p in c(0, 0.03, 0.1)) {
#     for (base in c(2, 10)) {
#       x = rnorm(10, rnorm(1), runif(1, 0, 10))^base
#       test_trafo(x, scale_base_0_1, range = c(p, 1-p), p=p, base = base)
#     }
#   }
#   test_trafo(10^-3:3, scale_sigmoid, range = c(0.03, 0.97))
#   expect_error(scale_sigmoid(x, p = 0.5))
# })
