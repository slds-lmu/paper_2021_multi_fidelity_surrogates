devtools::load_all()
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) #@cubist_mars
library(mlr3pipelines)
library(paradox)
library(bbotk)
library(ggplot2)
library(ggpubr)

n_init = 100
workdir = "../../multifidelity_data/"

surrogate_measures = function(n_init, replicates = 10) {
  imputepl = po("imputeoor", offset = 1, multiplier = 10) %>>% po("fixfactors") %>>% po("imputesample")
  imputepl_cubist =  po("colapply", applicator = as.integer, affect_columns = selector_type("logical")) %>>% imputepl
  imputepl_mars = po("colapply", applicator = as.integer, affect_columns = selector_type("logical")) %>>% po("encode") %>>% imputepl
  
  kknn = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))
  kknn_local = kknn$clone(deep = TRUE)
  kknn_local$param_set$values$regr.kknn.k = 1
  kknn_local$id = paste0(kknn_local$id, ".1")
  
  surrogates = list(
    ranger = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.ranger", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
    knn = kknn,
    kknn_local = kknn_local,
    cubist = GraphLearner$new(imputepl_cubist %>>% mlr3::lrn("regr.cubist", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
    mars = GraphLearner$new(imputepl_mars %>>% mlr3::lrn("regr.mars", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))
  )

  inst = readRDS("../inst/instances.rds")[cfg %in% c("lcbench", "rbv2_super") & test == TRUE]

  map_dtr(seq_len(NROW(inst)), function(i) {
    tmp = map_dtr(seq_len(replicates), function(j) {
      cfg = inst[i, ]$cfg
      task = inst[i, ]$level
  
      config = cfgs(cfg, workdir = workdir)
      target_variables = if (cfg == "lcbench") "val_cross_entropy" else "logloss"
      ins = OptimInstanceSingleCrit$new(
        objective = config$get_objective(task = task, target_variables = target_variables),
        terminator = trm("evals", n_evals = 1000 + n_init)
      )
      opt("random_search", batch_size = 1000 + n_init)$optimize(ins)
  
      backend = ins$archive$data[, c(ins$archive$cols_x, ins$archive$cols_y), with = FALSE]
      to_fct = names(which(map_lgl(backend, is.character)))
      if (length(to_fct)) {
        backend[, (to_fct) := lapply(.SD, function(x) as.factor(x)), .SDcols = to_fct]
      }
      
      task = TaskRegr$new("id", backend = backend, target = ins$archive$cols_y)
      ids_train = sample(task$row_ids, size = n_init, replace = FALSE)
      ids_test = setdiff(task$row_ids, ids_train)
      task_train = task$clone()$filter(ids_train)
      task_test = task$clone()$filter(ids_test)
      tmp = task_test$data(cols = c("..row_id", ins$archive$cols_y))
      setorderv(tmp, ins$archive$cols_y, order = 1L)  # val_cross_entropy / logloss is minimized
      task_test_10 = task_test$clone()$filter(tmp[1:100, ]$"..row_id")
     
      tmp = map_dtr(surrogates, function(surrogate) {
        surrogate$train(task_train)
        p = surrogate$predict(task_test)
        surrogate$train(task_train)  # FIXME: cubist errors on rbv2_super if we don't retrain...
        p_10 = surrogate$predict(task_test_10)
        data.table(
          id = surrogate$id,
          rsq = p$score(msr("regr.rsq")),
          srho = p$score(msr("regr.srho")),
          srho_10 = p_10$score(msr("regr.srho")),
          ktau = p$score(msr("regr.ktau")),
          ktau_10 = p_10$score(msr("regr.ktau")))
      }, .fill = TRUE)
      tmp$cfg = inst[i, ]$cfg
      tmp$task = inst[i, ]$level
      tmp
    }, .fill = TRUE)
    tmp_mean = tmp[,  lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = c("rsq", "srho", "srho_10", "ktau", "ktau_10")]
    tmp_sd = tmp[,  lapply(.SD, sd, na.rm = TRUE), by = id, .SDcols = c("rsq", "srho", "srho_10", "ktau", "ktau_10")][, c("rsq", "srho", "srho_10", "ktau", "ktau_10")]
    colnames(tmp_sd) = paste0(colnames(tmp_sd), "_sd")
    tmp = cbind(tmp_mean, tmp_sd)
    tmp$cfg = inst[i, ]$cfg
    tmp$task = inst[i, ]$level
    tmp
  }, .fill = TRUE)
}

res = surrogate_measures(n_init = n_init)

saveRDS(res, paste0("surrogates_res_", n_init, ".rds"))

res = map_dtr(c(10, 20, 50, 100), .f = function(x) {
  tmp = readRDS(paste0("surrogates_res_", x, ".rds"))
  tmp$n_init = x
  tmp
})

variables = c("rsq", "srho", "srho_10", "ktau", "ktau_10")

plots = map(variables, .f = function(variable) {
  p = ggplot(res, aes_string(x = "id", y = variable)) +
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2), aes(colour = cfg)) +
    facet_grid(~ n_init) +
    scale_x_discrete(labels = c("mars", "cubist", "kknn (7)", "kknn (1)", "ranger")) +
    xlab("Surrogate Learner")
  p
})

g = ggarrange(plotlist = plots, nrow = 5, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave(file = "surrogates.png", g, width = 10, height = 12)

