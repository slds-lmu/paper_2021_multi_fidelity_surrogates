test_that("Objectives", {
  skip_if_not(check_directory_exists(workdir))
  keys = cfgs()$keys()
  keys = setdiff(keys, "rbv2_super")
  for (k in keys) {
    print(cfg)
    cfg = cfgs(k, workdir=workdir)
    obj = cfg$get_objective()
    des = paradox::generate_design_random(cfg$param_set, 10)
    data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
    out = obj$eval_dt(data)
    expect_data_table(out, min.rows=10L)
  }
})
