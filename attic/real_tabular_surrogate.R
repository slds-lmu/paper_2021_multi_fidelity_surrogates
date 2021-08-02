source("real_tabular_surrogate_helpers.R")

# FIXME: add eggholder or schwefel (more local minima)
# FIXME: HPOlib

repls = 4

#learner = GraphLearner$new(ppl("robustify") %>>% lrn("regr.km", covtype = "matern3_2", optim.method = "gen", nugget.stability = 10^-8))
learner = lrn("regr.km", covtype = "matern3_2", optim.method = "gen", nugget.stability = 10^-8)
surrogate = SurrogateSingleCritLearner$new(learner)

# Branin
results_branin = map_dtr(seq_len(repls), function(repl) {

  ### random search full budget
  ins_real = get_ins("branin", method = "real", full_budget = TRUE, budget = 100)
  ins_tabular = get_ins("branin", method = "tabular", full_budget = TRUE, budget = 100)
  ins_surrogate = get_ins("branin", method = "surrogate", full_budget = TRUE, budget = 100)

  opt("random_search", batch_size = 1)$optimize(ins_real)
  real_rs_full = get_trace(ins_real$archive, "real", "rs_full", full_budget = TRUE)
  
  opt("random_tabular", table = ins_tabular$table, batch_size = 1)$optimize(ins_tabular)
  tabular_rs_full = get_trace(ins_tabular$archive, "tabular", "rs_full", full_budget = TRUE)
  
  opt("random_search", batch_size = 1)$optimize(ins_surrogate)
  surrogate_rs_full = get_trace(ins_surrogate$archive, "surrogate", "rs_full", full_budget = TRUE)

  ### BO full budget
  ins_real = get_ins("branin", method = "real", full_budget = TRUE, budget = 100)
  ins_tabular = get_ins("branin", method = "tabular", full_budget = TRUE, budget = 100)
  ins_surrogate = get_ins("branin", method = "surrogate", full_budget = TRUE, budget = 100)
  
  bayesopt_soo(
    instance = ins_real,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = AcqOptimizer$new(opt("nloptr", algorithm = "NLOPT_LN_BOBYQA"), trm("none")),
    n_design = 10
  )
  real_bo_full = get_trace(ins_real$archive, "real", "bo_full", full_budget = TRUE)
  
  tabular_init = ins_tabular$table[fidelity == 1]
  ins_tabular$eval_batch(tabular_init[sample(seq_len(NROW(tabular_init)), size = 10, replace = FALSE), ins_tabular$x_cols, with = FALSE])
  bayesopt_soo(
    instance = ins_tabular,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = AcqOptimizer$new(opt("random_tabular", table = ins_tabular$table, batch_size = 1000, fb_acqo = TRUE), trm("evals", n_evals = NROW(tabular_init)))
  )
  tabular_bo_full = get_trace(ins_tabular$archive, "tabular", "bo_full", full_budget = TRUE)
  
  bayesopt_soo(
    instance = ins_surrogate,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = AcqOptimizer$new(opt("nloptr", algorithm = "NLOPT_LN_BOBYQA"), trm("none")),
    n_design = 10
  )
  surrogate_bo_full = get_trace(ins_surrogate$archive, "surrogate", "bo_full", full_budget = TRUE)

  ### HB
  ins_real = get_ins("branin", method = "real", full_budget = FALSE, budget = 100)
  ins_tabular = get_ins("branin", method = "tabular", full_budget = FALSE, budget = 100)
  ins_surrogate = get_ins("branin", method = "surrogate", full_budget = FALSE, budget = 100)

  param_set_hb = ParamSet$new(ins_real$search_space$params[-3])

  opt("hyperband")$optimize(ins_real)
  real_hb = get_trace(ins_real$archive, "real", "hb")

  opt("hyperband", sampler = SamplerRandomTabular$new(table = data_tabular_branin[, c(paste0("x", 1:2), "fidelity"), with = FALSE], param_set = param_set_hb))$optimize(ins_tabular)
  tabular_hb = get_trace(ins_tabular$archive, "tabular", "hb")

  opt("hyperband")$optimize(ins_surrogate)
  surrogate_hb = get_trace(ins_surrogate$archive, "surrogate", "hb")

  results = rbind(
    real_rs_full, tabular_rs_full, surrogate_rs_full,
    real_bo_full, tabular_bo_full, surrogate_bo_full,
    real_hb, tabular_hb, surrogate_hb)
  results[, normalized_regret := (best - min(best)) / diff(range(best)), by = .(method)]
  results[, repl := repl]
  results
})

# Hartmann
results_hartmann = map_dtr(seq_len(repls), function(repl) {

  ### random search full budget
  ins_real = get_ins("hartmann", method = "real", full_budget = TRUE, budget = 100)
  ins_tabular = get_ins("hartmann", method = "tabular", full_budget = TRUE, budget = 100)
  ins_surrogate = get_ins("hartmann", method = "surrogate", full_budget = TRUE, budget = 100)

  opt("random_search", batch_size = 1)$optimize(ins_real)
  real_rs_full = get_trace(ins_real$archive, "real", "rs_full", full_budget = TRUE)
  
  opt("random_tabular", table = ins_tabular$table, batch_size = 1)$optimize(ins_tabular)
  tabular_rs_full = get_trace(ins_tabular$archive, "tabular", "rs_full", full_budget = TRUE)
  
  opt("random_search", batch_size = 1)$optimize(ins_surrogate)
  surrogate_rs_full = get_trace(ins_surrogate$archive, "surrogate", "rs_full", full_budget = TRUE)

  ### BO full budget
  ins_real = get_ins("hartmann", method = "real", full_budget = TRUE, budget = 100)
  ins_tabular = get_ins("hartmann", method = "tabular", full_budget = TRUE, budget = 100)
  ins_surrogate = get_ins("hartmann", method = "surrogate", full_budget = TRUE, budget = 100)
  
  bayesopt_soo(
    instance = ins_real,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = AcqOptimizer$new(opt("nloptr", algorithm = "NLOPT_LN_BOBYQA"), trm("none")),
    n_design = 30
  )
  real_bo_full = get_trace(ins_real$archive, "real", "bo_full", full_budget = TRUE)
  
  tabular_init = ins_tabular$table[fidelity == 1]
  ins_tabular$eval_batch(tabular_init[sample(seq_len(NROW(tabular_init)), size = 30, replace = FALSE), ins_tabular$x_cols, with = FALSE])
  bayesopt_soo(
    instance = ins_tabular,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = AcqOptimizer$new(opt("random_tabular", table = ins_tabular$table, batch_size = 1000, fb_acqo = TRUE), trm("evals", n_evals = NROW(tabular_init)))
  )
  tabular_bo_full = get_trace(ins_tabular$archive, "tabular", "bo_full", full_budget = TRUE)
  
  bayesopt_soo(
    instance = ins_surrogate,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = AcqOptimizer$new(opt("nloptr", algorithm = "NLOPT_LN_BOBYQA"), trm("none")),
    n_design = 30
  )
  surrogate_bo_full = get_trace(ins_surrogate$archive, "surrogate", "bo_full", full_budget = TRUE)

  ### HB
  ins_real = get_ins("hartmann", method = "real", full_budget = FALSE, budget = 100)
  ins_tabular = get_ins("hartmann", method = "tabular", full_budget = FALSE, budget = 100)
  ins_surrogate = get_ins("hartmann", method = "surrogate", full_budget = FALSE, budget = 100)

  param_set_hb = ParamSet$new(ins_real$search_space$params[-7])

  opt("hyperband")$optimize(ins_real)
  real_hb = get_trace(ins_real$archive, "real", "hb")

  opt("hyperband", sampler = SamplerRandomTabular$new(table = data_tabular_hartmann[, c(paste0("x", 1:6), "fidelity"), with = FALSE], param_set = param_set_hb))$optimize(ins_tabular)
  tabular_hb = get_trace(ins_tabular$archive, "tabular", "hb")

  opt("hyperband")$optimize(ins_surrogate)
  surrogate_hb = get_trace(ins_surrogate$archive, "surrogate", "hb")

  results = rbind(
    real_rs_full, tabular_rs_full, surrogate_rs_full,
    real_bo_full, tabular_bo_full, surrogate_bo_full,
    real_hb, tabular_hb, surrogate_hb)
  results[, normalized_regret := (best - min(best)) / diff(range(best)), by = .(method)]
  results[, repl := repl]
  results
})

# Analysis
results = results_branin

results_agg = setNames(results[, mean(normalized_regret), by = .(iteration, method, optimizer, cumbudget)], c("iteration", "method", "optimizer", "cumbudget", "mean_normalized_regret"))
results_agg[, sd_normalized_regret := results[, sd(normalized_regret), by = .(iteration, method, optimizer, cumbudget)][["V1"]]]
results_agg$se_normalized_regret = results_agg$sd_normalized_regret / sqrt(repls)
results_agg$lower = results_agg$mean_normalized_regret - results_agg$se_normalized_regret
results_agg$upper = results_agg$mean_normalized_regret + results_agg$se_normalized_regret

g = ggplot(aes(x = cumbudget, y = mean_normalized_regret, fill = optimizer, colour = optimizer), data = results_agg) +
  geom_line() +
  geom_ribbon(aes(min = lower, max = upper), alpha = 0.5, colour = NA) +
  facet_grid(~ method)

ranks = map_dtr(unique(results$repl), function(r) {
  map_dtr(c("real", "tabular", "surrogate"), function(m) {
    tmp = results[repl == r & method == m, min(best), by = .(method, optimizer, repl)]
    tmp$V1 = match(tmp$V1, sort(tmp$V1))
    setNames(tmp, c("method", "optimizer", "repl", "rank"))
  })
})
ranks_agg = setNames(ranks[, mean(rank), by = .(method, optimizer)], c("method", "optimizer", "mean_rank"))
ranks_agg$sd = ranks[, sd(rank), by = .(method, optimizer)][["V1"]]
ranks_agg$se = ranks_agg$sd / sqrt(repls)

g = ggplot(aes(x = method, y = mean_rank, fill = optimizer), data = ranks_agg) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_rank - se, ymax = mean_rank + se), width = 0.2, position = position_dodge(0.9))

transpositions = map_dtr(unique(ranks$repl), function(r) {
  real_rank = paste0(ranks[repl == r & method == "real", ][["rank"]], collapse = "")
  tabular_rank = paste0(ranks[repl == r & method == "tabular", ][["rank"]], collapse = "")
  surrogate_rank = paste0(ranks[repl == r & method == "surrogate", ][["rank"]], collapse = "")
  data.table(repl = r, r_r = stringdist(real_rank, real_rank), r_t = stringdist(real_rank, tabular_rank), r_s = stringdist(real_rank, surrogate_rank))
})
colMeans(transpositions)

