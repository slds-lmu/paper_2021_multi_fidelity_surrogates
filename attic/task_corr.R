library(mfsurrogates)
library(bbotk)
library(paradox)
library(ggcorrplot)

# lcbench
cfg = cfgs("lcbench", workdir = "/home/lps/multifidelity_data")
ins = OptimInstanceSingleCrit$new(
  objective = cfg$get_objective(target = "val_cross_entropy"),
  terminator = trm("none")
)
data = generate_design_random(ins$search_space, 10000L)$data

map(cfg$task_levels, function(level) {
  tmp = copy(data)
  tmp[, OpenML_task_id := level]
  ins$eval_batch(tmp)
})

dat = ins$archive$data[, c("OpenML_task_id", "val_cross_entropy")]
dat = map_dtc(split(dat, f = dat$OpenML_task_id), function(x) x$val_cross_entropy)
M = cor(dat)
rownames(M) = colnames(M) = as.factor(gsub("X", "I", names(dat)))
p = ggcorrplot(M, method = "circle", type = "lower")
ggsave("corrlcbench.png", plot = p)
summary(M[upper.tri(M, diag = FALSE)])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.3008  0.1104  0.3014  0.3069  0.5095  0.9585

# rbv2_super
cfg = cfgs("rbv2_super", workdir = "/home/lps/multifidelity_data")
ins = OptimInstanceSingleCrit$new(
  objective = cfg$get_objective(target = "logloss"),
  terminator = trm("none")
)
data = generate_design_random(ins$search_space, 10000L)$data

map(cfg$task_levels, function(level) {
  tmp = copy(data)
  tmp[, task_id := level]
  ins$eval_batch(tmp)
})

dat = ins$archive$data[, c("task_id", "logloss")]
dat = map_dtc(split(dat, f = dat$task_id), function(x) x$logloss)
M = cor(dat)
rownames(M) = colnames(M) = as.factor(gsub("X", "I", names(dat)))
p = ggcorrplot(M, method = "circle", type = "lower")
ggsave("corrrbv2_super.png", plot = p)
summary(M[upper.tri(M, diag = FALSE)])
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.1936  0.3607  0.6174  0.5654  0.7951  0.9848
