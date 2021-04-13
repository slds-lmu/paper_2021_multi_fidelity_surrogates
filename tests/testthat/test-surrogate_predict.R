test_that("Objectives", {
  workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data")
  skip_if_not(check_directory_exists(workdir))
  keys = cfgs()$keys()
  k = "rbv2_aknn"
  cfg = cfgs(k, workdir=paste0(workdir, "/"))
  obj = cfg$get_objective()
  des = paradox::generate_design_random(cfg$param_set, 10)
  obj$eval_dt(des$data)
})
