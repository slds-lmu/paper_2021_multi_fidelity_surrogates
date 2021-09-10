devtools::load_all()
library(checkmate)
library(paradox)
library(bbotk)
library(data.table)

library(ggplot2)

cfg = cfgs("rbv2_super", workdir = "../multifidelity_data/")

objective = cfg$get_objective(task = "46", target_variables = "logloss")

ins = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none"),
  check_values = FALSE
)

design_1 = generate_design_random(objective$domain, 1000L)$data
design_1[, trainsize := 1/27]
ins$eval_batch(design_1)

design_2 = generate_design_random(objective$domain, 1000L)$data
design_2[, trainsize := 1/9]
ins$eval_batch(design_2)

design_3 = generate_design_random(objective$domain, 1000L)$data
design_3[, trainsize := 1/3]
ins$eval_batch(design_3)

data = ins$archive$data
data[, n := seq_len(.N)]

g = ggplot(aes(y = logloss, x= n, colour = as.factor(trainsize), shape = as.factor(learner)), data = ins$archive$data) + geom_point()
ggsave("rbv2_super_46.png", plot = g, device = "png")

