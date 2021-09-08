library(bbotk)
library(paradox)
library(mlr3hyperband)
library(mlr3learners)
devtools::load_all()
devtools::load_all("/home/lps/workshop/mlr3mbo")
library(miesmuschel)
library(ggplot2)
source("/home/lps/paper_2021_benchmarking_special_issue/irace/optimization.R")

cfg = cfgs("branin")

rs = function(full = FALSE) {
  ins = OptimInstanceSingleCrit$new(
    objective = cfg$get_objective(),
    terminator = trm("budget", budget = 100)
  )
  if (full) {
    xdt = generate_design_random(ins$search_space, n = 100L)$data
    xdt[, fidelity := 1]
    for (i in seq_len(nrow(xdt))) {
      ins$eval_batch(xdt[i, ])
    }
  } else {
    opt("random_search")$optimize(ins)
  }
  x = ins$archive$data
  y = map_dbl(seq_len(nrow(ins$archive$data)), function(x) min(ins$archive$data[1:x][[ins$archive$cols_y]]))
  x$best = y
  x[, ccost := cumsum(cost)]
  x[, reg := log(best - 0.398, base = 10)]
  x
}

hb = function() {
  ins = OptimInstanceSingleCrit$new(
    objective = cfg$get_objective(),
    terminator = trm("budget", budget = 100)
  )
  opt("hyperband")$optimize(ins)
  x = ins$archive$data
  y = map_dbl(seq_len(nrow(ins$archive$data)), function(x) min(ins$archive$data[1:x][[ins$archive$cols_y]]))
  x$best = y
  x[, ccost := cumsum(cost)]
  x[, reg := log(best - 0.398, base = 10)]
  x
}

irace_result = readRDS("/home/lps/paper_2021_benchmarking_special_issue/irace/data/data_31_05_single/irace_instance.rda")
lambda = irace_result$result_x_domain

sm = function(lambda) {
  objective = cfg$get_objective()

  # create search space
  domain = objective$domain
  param_ids = domain$ids()
  budget_idx = which(domain$tags %in% c("budget", "fidelity"))
  budget_id = param_ids[budget_idx]
  budget_lower = domain$params[[budget_id]]$lower
  budget_upper = domain$params[[budget_id]]$upper
  params_to_keep = param_ids[- budget_idx]

  search_space = ParamSet$new(domain$params[params_to_keep])
  search_space$add(ParamDbl$new(id = budget_id, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))
  domain_tafo = domain$trafo
  search_space$trafo = function(x, param_set) {
    if (!is.null(domain_tafo)) x = domain_tafo(x, param_set)
    x[budget_id] = if (domain$params[[budget_id]]$class == "ParamInt") as.integer(exp(x[[budget_id]])) else exp(x[[budget_id]])
    x
  }
  search_space$deps = domain$deps

  budget_limit = 100

  # call smashy with configuration parameter in xs
  ins = mlr3misc::invoke(opt_objective, objective = objective, budget_limit = budget_limit, search_space = search_space, .args = lambda)
  x = ins$archive$data
  y = map_dbl(seq_len(nrow(ins$archive$data)), function(x) min(ins$archive$data[1:x][[ins$archive$cols_y]]))
  x$best = y
  x[, ccost := cumsum(cost)]
  x[, reg := log(best - 0.398, base = 10)]
  x
}

mbo = function() {
  ins = OptimInstanceSingleCrit$new(
    objective = cfg$get_objective(),
    terminator = trm("budget", budget = 100)
  )

  learner = lrn(
     "regr.km",
    optim.method = "gen",
    fallback = mlr3::lrn("regr.featureless"),
    encapsulate = c(train = "evaluate", predict = "evaluate")
  )
  acqf = AcqFunctionEI$new(SurrogateSingleCritLearner$new(learner))
  optimizer = opt("random_search", batch_size = 1000)
  terminator = trm("evals", n_evals = 10000)
  acqo = AcqOptimizer$new(optimizer, terminator = terminator)

  ins = bayesopt_soo(ins, acq_function = acqf, acq_optimizer = acqo, n_design = 10L)
  x = ins$archive$data
  y = map_dbl(seq_len(nrow(ins$archive$data)), function(x) min(ins$archive$data[1:x][[ins$archive$cols_y]]))
  x$best = y
  x[, ccost := cumsum(cost)]
  x[, reg := log(best - 0.398, base = 10)]
  x
}


res = map_dtr(seq_len(100), function(repl) {
  res_rs_full = rs(full = TRUE)[, method := "rs_full"][, c("reg", "ccost", "method")]
  res_rs_full[, id := paste0(method, seq_len(.N))]

  res_hb = hb()[, method := "hb"][, c("reg", "ccost", "method")]
  res_hb[, id := paste0(method, seq_len(.N))]

  res_sm = sm(lambda)[, method := "sm"][, c("reg", "ccost", "method")]
  res_sm[, id := paste0(method, seq_len(.N))]

  res = rbind(res_rs_full, res_hb, res_sm)
  res$repl = repl
  res
})

res_agg = setNames(res[, mean(reg, na.rm = TRUE), by = .(id, method, ccost)], c("id", "method", "ccost", "mean"))
res_agg$sd = res[, sd(reg, na.rm = TRUE), by = .(id, method, ccost)][["V1"]]
res_agg$n = res[, sum(!is.na(reg)), by = .(id, method, ccost)][["V1"]]
res_agg$upper = res_agg$mean + (res_agg$sd / sqrt(res_agg$n))
res_agg$lower = res_agg$mean - (res_agg$sd / sqrt(res_agg$n))

g = ggplot(aes(y = mean, x= ccost, fill = method), data = res_agg) +
  geom_ribbon(aes(min = lower, max = upper), alpha = 0.5) +
  xlab("Cumulative Cost") +
  ylab("log10(Simple Regret)")

ggsave("branin.png", plot = g)
