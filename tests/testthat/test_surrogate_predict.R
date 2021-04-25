test_that("Objectives", {
  skip_if_not(check_directory_exists(workdir))
  keys = cfgs()$keys()
  k = "rbv2_svm"
  cfg = cfgs(k, workdir=workdir)
  obj = cfg$get_objective()
  des = paradox::generate_design_random(cfg$param_set, 10)
  data = rbindlist(des$transpose(FALSE))[,names(obj$domain$params), with=FALSE]
  obj$eval_dt(data)
})
