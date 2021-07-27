source("real_tabular_surrogate_helpers.R")

# FIXME: rs, bo full budget
# FIXME: hartmann

# Branin

repls = 100
results = map_dtr(seq_len(repls), function(repl) {

  ins_real = get_ins("real", budget = 100)
  ins_tabular = get_ins("tabular", budget = 100)
  ins_surrogate = get_ins("surrogate", budget = 100)

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

  ins_real$clear()
  ins_tabular$clear()
  ins_surrogate$clear()

  param_set_hb = ParamSet$new(ins_real$search_space$params[-3])

  opt("hyperband")$optimize(ins_real)
  real_hb = get_trace(ins_real$archive, "real", "hb")

  opt("hyperband", sampler = SamplerRandomTabular$new(table = data_tabular[, c("x1", "x2", "fidelity"), with = FALSE], param_set = param_set_hb))$optimize(ins_tabular)
  tabular_hb = get_trace(ins_tabular$archive, "tabular", "hb")

  opt("hyperband")$optimize(ins_surrogate)
  surrogate_hb = get_trace(ins_surrogate$archive, "surrogate", "hb")

  results = rbind(real_rs, tabular_rs, surrogate_rs, real_bo, tabular_bo, surrogate_bo, real_hb, tabular_hb, surrogate_hb)
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

ranks = map_dtr(unique(results$repl), function(r) {
  map_dtr(c("real", "tabular", "surrogate"), function(m) {
    tmp = results[repl == r & method == m, min(best), by = .(method, optimizer, repl)]
    tmp$V1 = order(tmp$V1)
    setNames(tmp, c("method", "optimizer", "repl", "rank"))
  })
})
ranks_agg = setNames(ranks[, mean(rank), by = .(method, optimizer)], c("method", "optimizer", "mean_rank"))
ranks_agg$sd = ranks[, sd(rank), by = .(method, optimizer)][["V1"]]
ranks_agg$se = ranks_agg$sd / sqrt(repls)

ranks_half = map_dtr(unique(results$repl), function(r) {
  map_dtr(c("real", "tabular", "surrogate"), function(m) {
    tmp = results[repl == r & method == m & cumbudget <= 16, min(best), by = .(method, optimizer, repl)]
    tmp$V1 = order(tmp$V1)
    setNames(tmp, c("method", "optimizer", "repl", "rank"))
  })
})
ranks_half_agg = setNames(ranks_half[, mean(rank), by = .(method, optimizer)], c("method", "optimizer", "mean_rank"))
ranks_half_agg$sd = ranks_half[, sd(rank), by = .(method, optimizer)][["V1"]]
ranks_half_agg$se = ranks_half_agg$sd / sqrt(repls)

ranks_tq = map_dtr(unique(results$repl), function(r) {
  map_dtr(c("real", "tabular", "surrogate"), function(m) {
    tmp = results[repl == r & method == m & cumbudget <= 24, min(best), by = .(method, optimizer, repl)]
    tmp$V1 = order(tmp$V1)
    setNames(tmp, c("method", "optimizer", "repl", "rank"))
  })
})
ranks_tq_agg = setNames(ranks_tq[, mean(rank), by = .(method, optimizer)], c("method", "optimizer", "mean_rank"))
ranks_tq_agg$sd = ranks_tq[, sd(rank), by = .(method, optimizer)][["V1"]]
ranks_tq_agg$se = ranks_tq_agg$sd / sqrt(repls)


g = ggplot(aes(x = method, y = mean_rank, fill = optimizer), data = ranks_agg) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_rank - se, ymax = mean_rank + se), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "32/32 budget")

ggsave("rts_32_32.png", plot = g)
