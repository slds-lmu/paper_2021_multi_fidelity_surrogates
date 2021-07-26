source("real_tabular_surrogate_helpers.R")

# Branin

repls = 10
results = map_dtr(seq_len(repls), function(repl) {

  ins_real = get_ins("real", budget = 20)
  ins_tabular = get_ins("tabular", budget = 20)
  ins_surrogate = get_ins("surrogate", budget = 20)
  
  opt("random_search", batch_size = 1)$optimize(ins_real)
  real_rs = get_trace(ins_real$archive, "real", "rs")
  
  opt("random_tabular", table = ins_tabular$table, batch_size = 1)$optimize(ins_tabular)
  tabular_rs = get_trace(ins_tabular$archive, "tabular", "rs")
  
  opt("random_search", batch_size = 1)$optimize(ins_surrogate)
  surrogate_rs = get_trace(ins_surrogate$archive, "surrogate", "rs")
  
  ins_real$clear()
  ins_tabular$clear()
  ins_surrogate$clear()
  
  bayesopt_soo(
    instance  = ins_real,
    acq_function = AcqFunctionEI$new(SurrogateSingleCritLearner$new(lrn("regr.ranger"))),
    acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000)),
    n_design = 4
  )
  real_bo = get_trace(ins_real$archive, "real", "bo")
  
  ins_tabular$eval_batch(ins_tabular$table[sample(seq_len(NROW(ins_tabular$table)), size = 4, replace = FALSE), ins_tabular$x_cols, with = FALSE])
  bayesopt_soo(
    instance  = ins_tabular,
    acq_function = AcqFunctionEI$new(SurrogateSingleCritLearner$new(lrn("regr.ranger"))),
    acq_optimizer = AcqOptimizer$new(opt("random_tabular", table = ins_tabular$table, batch_size = 1000), trm("evals", n_evals = 1000))
  )
  tabular_bo = get_trace(ins_tabular$archive, "tabular", "bo")
  
  bayesopt_soo(
    instance  = ins_surrogate,
    acq_function = AcqFunctionEI$new(SurrogateSingleCritLearner$new(lrn("regr.ranger"))),
    acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000)),
    n_design = 4
  )
  surrogate_bo = get_trace(ins_surrogate$archive, "surrogate", "bo")
  
  results = rbind(real_rs, tabular_rs, surrogate_rs, real_bo, tabular_bo, surrogate_bo)
  results[, normalized_regret := (best - min(best)) / diff(range(best)), by = .(method)]
  results[, repl := repl]
  results
})

results_agg = setNames(results[, mean(normalized_regret), by = .(iteration, method, optimizer)], c("iteration", "method", "optimizer", "mean_normalized_regret"))
results_agg[, sd_normalized_regret := results[, sd(normalized_regret), by = .(iteration, method, optimizer)][["V1"]]]
results_agg$lower = results_agg$mean_normalized_regret - results_agg$sd_normalized_regret
results_agg$upper = results_agg$mean_normalized_regret + results_agg$sd_normalized_regret

ggplot(aes(x = iteration, y = mean_normalized_regret, fill = optimizer, colour = optimizer), data = results_agg) +
  geom_line() +
  geom_ribbon(aes(min = lower, max = upper), alpha = 0.5, colour = NA) +
  facet_grid(~ method)

