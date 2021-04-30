test_that("Objectives", {
  skip_if_not(check_directory_exists(workdir))
  keys = cfgs()$keys()
  for (k in keys) {
    cfg = cfgs(k, workdir=workdir)
    obj = cfg$get_objective("3945")
    des = paradox::generate_design_random(cfg$param_set, 10)
    data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
    out = obj$eval_dt(data)
    expect_data_table(out, min.rows=10L)
    data = data[, sample(seq_along(data)), with = FALSE]
    out = obj$eval_dt(data)
    expect_data_table(out, min.rows=10L)
  }
})

test_that("Objectives-Fix task", {
  skip_if_not(check_directory_exists(workdir))
  cfg = cfgs("lcbench", workdir=workdir)
  obj = cfg$get_objective("3945")
  des = paradox::generate_design_random(cfg$param_set, 10)
  data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
  out = obj$eval_dt(data)
  expect_data_table(out, min.rows=10L)
  data = data[, sample(seq_along(data)), with = FALSE]
  out = obj$eval_dt(data)
  expect_data_table(out, min.rows=10L)
})
