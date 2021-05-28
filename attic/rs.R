devtools::load_all()
library(bbotk)
library(paradox)

tasks = readRDS("inst/instances.rds")

# LCBench
res = map(tasks[cfg == "lcbench" & test == TRUE][["level"]], .f = function(task) {
  cfg = BenchmarkConfigLCBench$new(workdir = "../multifidelity_data/")
  ins = OptimInstanceSingleCrit$new(
    objective = cfg$get_objective(task = task, target_variables = "val_cross_entropy"),
    terminator = trm("evals", n_evals = 100000L)
  )
  opt("random_search", batch_size = 10L)$optimize(ins)
  ins
})

best = map(res, .f = function(x) {
  tmp = map_dtr(unique(x$archive$data$batch_nr), .f = function(bn) {
    x$archive$best(batch = 1:bn)
  })
  tmp$batch = seq_len(max(x$archive$data$batch_nr))
  tmp
})

names(best) = tasks[cfg == "lcbench" & test == TRUE][["level"]]

best = imap_dtr(best, .f = function(x, nm) {
  x$OpenML_task_id = nm
  x
})

library(ggplot2)
ggplot(best, aes(x = batch, y = val_cross_entropy, colour = OpenML_task_id)) +
  geom_line()

