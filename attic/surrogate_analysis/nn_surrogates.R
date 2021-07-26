devtools::load_all()
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(paradox)
library(bbotk)
library(ggplot2)
library(ggpubr)

workdir = "../../multifidelity_data/"

#budgets = 1:52
#budgets = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1.0)

#separate = c("26-1", "52-1", "52-26")
#separate = c("4-1", "7-1", "7-4")

get_measures = function(model, x, y, id, budget_id, budgets, tid, target_variables, task_levels, trafos, retrafo = FALSE) {
  map_dtr(task_levels, function(task) {
    res = map_dtr(budgets, function(budget) {
      x[, row_id := seq_len(.N)]
      ids = x[get(tid) == task & get(budget_id) == budget][["row_id"]]
      x[, row_id := NULL]
      tmp_p = as.matrix(predict(model, mlr3keras::reshape_data_embedding(x[ids, ])$data))
      tmp_y = y[ids, ]
      colnames(tmp_p) = colnames(tmp_y) = target_variables
      if (retrafo) {
        for (variable in target_variables) {
        tmp_p[, variable] = trafos[[variable]]$retrafo(tmp_p[, variable])
        tmp_y[, variable] = trafos[[variable]]$retrafo(tmp_y[, variable])
        }
      }
      res = compute_metrics(tmp_y, tmp_p)
      res$budget = budget
      res
    })
    res$cfg = config$id
    res$level = task
    res
  })
}

get_all_measures = function(cfg, workdir, budgets) {
  config = cfgs(cfg, workdir = workdir)
  data = config$data
  model = keras::load_model_hdf5(paste0(config$subdir, config$keras_model_file), compile = FALSE)
  budget_param = config$param_set$params[[which(config$param_set$tags == "budget")[1]]]

  measures_train = get_measures(
    model,
    x = data$xtrain,
    y = data$ytrain,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos
  )
  measures_train$split = "train"

  measures_train_retrafo = get_measures(
    model,
    x = data$xtrain,
    y = data$ytrain,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos,
    retrafo = TRUE
  )
  measures_train_retrafo$split = "train"

  measures_test = get_measures(
    model,
    x = data$xtest,
    y = data$ytest,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos
  )
  measures_test$split = "test"

  measures_test_retrafo = get_measures(
    model,
    x = data$xtest,
    y = data$ytest,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos,    
    retrafo = TRUE
  )
  measures_test_retrafo$split = "test"

  measures = rbind(measures_train, measures_test)
  measures_retrafo = rbind(measures_train_retrafo, measures_test_retrafo)

  list(measures = measures, measures_retrafo = measures_retrafo)
}

plot_measures = function(measures, postfix = "", cfg, workdir) {
  config = cfgs(cfg, workdir = workdir)
  for (key in c("rsq", "rho", "ktau")) {
    plots = map(config$task_levels, function(task) {
      p = ggplot(aes_string(x = "budget", y = key, colour = "variable"), data = measures[level == task]) +
        facet_grid(~ split) +
        geom_smooth() +
        labs(title = task) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        coord_cartesian(ylim = c(0, 1)) +
        geom_hline(yintercept = 1, linetype = 1, size = 2)
      p
    })

    rc = n2mfrow(length(plots), asp = 16 / 9)
    g = ggarrange(plotlist = plots, nrow = rc[1], ncol = rc[2], common.legend = TRUE)
    ggsave(file = paste0("measures_", config$id, "_", key, "_", postfix, ".png"), plot = g, width = rc[1] * 5, height = rc[2] * 3)
  }
}

plot_measures_heat = function(measures, postfix = "", cfg, workdir) {
  config = cfgs(cfg, workdir = workdir)
  ntask = length(config$task_levels)
  plots = map(config$target_variables, function(var) {
    map(c("rsq", "rho", "ktau"), function(key) {
      measures_agg = aggregate(as.formula(paste0(key, " ~ level")), FUN = mean, data = measures[variable == var])
      x = y = seq_len(ceiling(sqrt(ntask)))
      data = expand.grid(X = x, Y = y)
      data$Z = NA
      data$Z[seq_len(ntask)] = measures_agg[[key]]
      data$nms = NA_character_
      data$nms[seq_len(ntask)] = measures_agg$level
    
      p = ggplot(data, aes(X, Y, fill = Z)) +
        geom_tile() +
        geom_text(aes(label = data$nms), size = 2.5) +
        theme_minimal() +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        labs(title = paste0(config$id, " - ", var), fill = key) +
        scale_fill_gradient(limits = c(0, 1), low = "yellow", high = "red")
      p
    })
  })
  g = ggarrange(plotlist = unlist(plots, recursive = FALSE), nrow = length(plots), ncol = 3)
  ggsave(file = paste0("measures_heat_", config$id, "_", postfix, ".png"), plot = g, width = 3 * 5, height = length(plots) * 4)
}
   
get_py_lags = function(model, x, y, id, budget_id, budgets, tid, target_variables, task_levels, trafos, retrafo = FALSE) {
  map_dtr(task_levels, function(task) {
    tmp = vector("list", length = length(budgets))

    for (i in seq_along(budgets)) {
      budget = budgets[i]
      ids = which(x[[tid]] == task & x[[budget_id]] == budget)
      X = x[ids, ]
      p = as.matrix(predict(model, mlr3keras::reshape_data_embedding(X)$data))
      Y = y[ids, ]
      colnames(p) = colnames(Y) = target_variables
      if (retrafo) {
        for (variable in target_variables) {
        p[, variable] = trafos[[variable]]$retrafo(p[, variable])
        Y[, variable] = trafos[[variable]]$retrafo(Y[, variable])
        }
      }

      X[, (budget_id) := NULL]
      X[, hash := do.call(paste0, c(.SD, sep = "_"))]
      tmp[[i]] = list(p = p, y = Y, X = X)
    }

    lag_1 = map_dtr(seq_len(length(budgets) - 1), function(i) {
      res = map_dtc(target_variables, function(variable) {
        X = merge(tmp[[i]]$X, tmp[[i + 1]]$X)
        id_i = match(X$hash, tmp[[i]]$X$hash)
        id_i1 = match(X$hash, tmp[[i + 1]]$X$hash)
        c(rho_p = cor(tmp[[i]]$p[id_i, variable], tmp[[i + 1]]$p[id_i1, variable]), rho_y = cor(tmp[[i]]$y[id_i, variable], tmp[[i + 1]]$y[id_i1, variable]))
      })
      colnames(res) = target_variables
      res = cbind(res, set = c("p", "y"), lag = paste0(i + 1, "-", i))
    })

    min = 1
    mid = round(length(budgets) / 2)
    max = length(budgets)

    lag_mid_min = map_dtc(target_variables, function(variable) {
        X = merge(tmp[[min]]$X, tmp[[mid]]$X)
        id_i = match(X$hash, tmp[[min]]$X$hash)
        id_i1 = match(X$hash, tmp[[mid]]$X$hash)
        c(rho_p = cor(tmp[[min]]$p[id_i, variable], tmp[[mid]]$p[id_i1, variable]), rho_y = cor(tmp[[min]]$y[id_i, variable], tmp[[mid]]$y[id_i1, variable]))
    })
    colnames(lag_mid_min) = target_variables
    lag_mid_min = cbind(lag_mid_min, set = c("p", "y"), lag = paste0(mid, "-", min))

    lag_max_mid = map_dtc(target_variables, function(variable) {
        X = merge(tmp[[mid]]$X, tmp[[max]]$X)
        id_i = match(X$hash, tmp[[mid]]$X$hash)
        id_i1 = match(X$hash, tmp[[max]]$X$hash)
        c(rho_p = cor(tmp[[mid]]$p[id_i, variable], tmp[[max]]$p[id_i1, variable]), rho_y = cor(tmp[[mid]]$y[id_i, variable], tmp[[max]]$y[id_i1, variable]))
    })
    colnames(lag_max_mid) = target_variables
    lag_max_mid = cbind(lag_max_mid, set = c("p", "y"), lag = paste0(max, "-", mid))

    lag_max_min = map_dtc(target_variables, function(variable) {
        X = merge(tmp[[min]]$X, tmp[[max]]$X)
        id_i = match(X$hash, tmp[[min]]$X$hash)
        id_i1 = match(X$hash, tmp[[max]]$X$hash)
        c(rho_p = cor(tmp[[min]]$p[id_i, variable], tmp[[max]]$p[id_i1, variable]), rho_y = cor(tmp[[min]]$y[id_i, variable], tmp[[max]]$y[id_i1, variable]))
    })
    colnames(lag_max_min) = target_variables
    lag_max_min = cbind(lag_max_min, set = c("p", "y"), lag = paste0(max, "-", min))

    lags = rbind(lag_1, lag_mid_min, lag_max_mid, lag_max_min)
    lags$cfg = id
    lags$level = task
    lags
  })
}

get_all_py_lags = function(cfg, workdir, budgets) {
  config = cfgs(cfg, workdir = workdir)
  data = config$data
  model = keras::load_model_hdf5(paste0(config$subdir, config$keras_model_file), compile = FALSE)
  budget_param = config$param_set$params[[which(config$param_set$tags == "budget")[1]]]

  py_lags_train = get_py_lags(
    model,
    x = data$xtrain,
    y = data$ytrain,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos
  )
  py_lags_train$split = "train"

  py_lags_train_retrafo = get_py_lags(
    model,
    x = data$xtrain,
    y = data$ytrain,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos,
    retrafo = TRUE
  )
  py_lags_train_retrafo$split = "train"

  py_lags_test = get_py_lags(
    model,
    x = data$xtest,
    y = data$ytest,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos
  )
  py_lags_test$split = "test"

  py_lags_test_retrafo = get_py_lags(
    model,
    x = data$xtest,
    y = data$ytest,
    id = config$id,
    budget_id = budget_param$id,
    budgets = budgets,
    tid = config$task_col,
    target_variables = config$target_variables,
    task_levels = config$task_levels,
    trafos = data$trafos,
    retrafo = TRUE
  )
  py_lags_test_retrafo$split = "test"

  py_lags = rbind(py_lags_train, py_lags_test)
  py_lags_retrafo = rbind(py_lags_train_retrafo, py_lags_test_retrafo)

  list(py_lags = py_lags, py_lags_retrafo = py_lags_retrafo)
}

plot_py_lags = function(py_lags, postfix = "", cfg, workdir, separate, discrete = FALSE) {
  config = cfgs(cfg, workdir = workdir)
  for (var in config$target_variables) {
    plots = map(config$task_levels, function(task) {
      dat = py_lags[level == task & lag %nin% separate]
      dat$lag_numeric = as.numeric(factor(dat$lag, levels = unique(dat$lag)))
      p = if (discrete) {
        ggplot(aes_string(x = "lag_numeric", y = var, colour = "set", linetype = "split", pch = "split"), data = dat) +
          geom_point(size = 3) +
          labs(title = task) +
          xlab("budget (i)") +
          ylab(paste0("rho(", var, "[i], ", var, "[i + 1])")) +
          theme_minimal() +
          theme(axis.title = element_text(size = rel(0.75)), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          coord_cartesian(ylim = c(0, 1)) +
          geom_hline(yintercept = 1, linetype = 1, size = 2)
      } else {
        ggplot(aes_string(x = "lag_numeric", y = var, colour = "set", linetype = "split", pch = "split"), data = dat) +
          geom_smooth() +
          labs(title = task) +
          xlab("budget (i)") +
          ylab(paste0("rho(", var, "[i], ", var, "[i + 1])")) +
          theme_minimal() +
          theme(axis.title = element_text(size = rel(0.75)), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          coord_cartesian(ylim = c(0, 1)) +
          geom_hline(yintercept = 1, linetype = 1, size = 2)
      }
      p
    })
  
    rc = n2mfrow(length(plots), asp = 16 / 9)
    g = ggarrange(plotlist = plots, nrow = rc[1], ncol = rc[2], common.legend = TRUE)
    ggsave(file = paste0("py_lags_", config$id, "_", var, "_", postfix, ".png"), plot = g, width = rc[1] * 5, height = rc[2] * 3)

    plots_ = map(config$task_levels, function(task) {
      dat = py_lags[level == task & lag %in% separate]
      p = ggplot(aes_string(x = "lag", y = var, colour = "set", pch = "split"), data = dat) +
        geom_point(size = 3) +
        labs(title = task) +
        xlab("budget (i) - budget(j)") +
        ylab(paste0("rho(", var, "[i], ", var, "[j])")) +
        theme_minimal() +
        theme(axis.title = element_text(size = rel(0.75))) +
        coord_cartesian(ylim = c(-1, 1))
      p
    })

    g = ggarrange(plotlist = plots_, nrow = rc[1], ncol = rc[2], common.legend = TRUE)
    ggsave(file = paste0("py_lags_mmm_", config$id, "_", var, "_", postfix, ".png"), plot = g, width = rc[1] * 5, height = rc[2] * 3)
  }
}

