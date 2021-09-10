test_that("Objectives", {
  skip_if_not(check_directory_exists(workdir))
  cfgs = c(grep("rbv2", benchmark_configs$keys(), value = TRUE), "lcbench", "nb301")
  for (cfg in cfgs) {
    cfg = cfgs(cfg, workdir=workdir)
    obj = cfg$get_objective()
    des = paradox::generate_design_random(cfg$opt_param_set, 10)
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

test_that("Benchmark Settings", {
  skip_if_not(check_directory_exists(workdir))

  cfg = cfgs("lcbench", workdir=workdir)
  obj = cfg$get_objective("3945", retrafo =TRUE)
  des = paradox::generate_design_random(cfg$opt_param_set, 10)
  data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
  out = obj$eval_dt(data)
  expect_data_table(out, min.rows=10L)
  data = data[, sample(seq_along(data)), with = FALSE]
  out2 = obj$eval_dt(data)
  expect_data_table(out2, min.rows=10L)
  expect_true(all(out == out2))


  cfg = cfgs("nb301", workdir=workdir)
  obj = cfg$get_objective(retrafo =TRUE)
  des = paradox::generate_design_random(cfg$opt_param_set, 10)
  data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
  out = obj$eval_dt(data)
  expect_data_table(out, min.rows=10L)
  data = data[, sample(seq_along(data)), with = FALSE]
  out2 = obj$eval_dt(data)
  expect_data_table(out2, min.rows=10L)
  expect_true(all(out == out2))

  cfg = cfgs("rbv2_super", workdir=workdir)
  obj = cfg$get_objective("377", retrafo =TRUE)
  des = paradox::generate_design_random(cfg$opt_param_set, 10)
  data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
  out = obj$eval_dt(data)
  expect_data_table(out, min.rows=10L)
  data = data[, sample(seq_along(data)), with = FALSE]
  out2 = obj$eval_dt(data)
  expect_data_table(out2, min.rows=10L)
  expect_true(all(out == out2))

  cfg = cfgs("fcnet", workdir=workdir)
  obj = cfg$get_objective("fcnet_protein_structure", retrafo =TRUE)
  des = paradox::generate_design_random(cfg$opt_param_set, 10)
  data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
  out = obj$eval_dt(data)
  expect_data_table(out, min.rows=10L)
  data = data[, sample(seq_along(data)), with = FALSE]
  out2 = obj$eval_dt(data)
  expect_data_table(out2, min.rows=10L)
  expect_true(all(out == out2))
})
