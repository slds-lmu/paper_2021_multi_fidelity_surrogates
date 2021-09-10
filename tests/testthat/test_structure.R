test_that("target variables in order", {
  skip_if_not(check_directory_exists(workdir))
  cfgs = c(grep("rbv2", benchmark_configs$keys(), value = TRUE), "lcbench", "nb301")
  for (cfg in cfgs) {
    config = benchmark_configs$get(cfg, workdir = workdir)
    expect_true(all(config$target_variables == config$codomain$ids()))
    expect_true(all(colnames(config$data$ytrain) == config$target_variables))
    expect_true(all(colnames(config$data$ytest) == config$target_variables))
  }
})

