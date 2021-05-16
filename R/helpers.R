# Map a character to the correct integer using a dict and impute NA's
char_to_int = function(x, param_name, dict) {
  if (anyNA(x)) x[is.na(x)] = "None"
  if (typeof(x) != "character") x = as.character(x) # lcbench has integer factor task ids.
  matrix(dict[[param_name]][x,]$int)
}

# Trafo numerics using a trafo dict and impute NA's
trafo_numerics = function(xdt, trafo_dict) {
  xdt = map_if(xdt, is.logical, as.numeric)
  l = mlr3misc::keep(xdt, is.numeric)
  l = imap(l, function(x, nm) {
    x[is.na(x)] = 0
    if (nm %in% names(trafo_dict)) {
      trafo_dict[[nm]]$trafo(x)
    } else {
      x
    }
  })
  do.call("cbind", l)
}

# Sample rows up to a maximum of nmax
sample_max = function(dt, n_max) {
  dt[sample(seq_len(nrow(dt)), min(n_max, nrow(dt))),]
}

# Get weights vector by exponentiating
weights_from_target = function(y, minimize = TRUE, j = 1L, wts_pow = 0L) {
  if (wts_pow) {
    if (minimize) {
      yy = (1  - y[,j])
    } else {
      yy = y[,j]
    }
    yy = yy^wts_pow
    return(yy / sum(yy) * nrow(y))
  } else {
    return(rep(1L, nrow(y)))
  }
}

# get the parameters of a config (only search space without `task_id`, `budget`)
get_pars = function(config) {
  ps = config$param_set
  ids = ps$ids()
  setdiff(ids, c(ps$ids(tags = "task_id"), ps$ids(tags = "budget")))
}

# split data into train and test stratified by column `by`
# if `pars` is not null, additionally blocking is performed with respect to `pars` and `by`,
# e.g, a specific config for a specific task will always belong either to train or test with all its budget
# only meaningful if a single budget parameter is present (e.g., epoch)
split_by_col = function(dt, by = "task_id", pars = NULL, frac = 0.1) {
  if (!is.null(pars)) {
    dt[, id  := .GRP, keyby = c(by, pars)]
    uids = unique(dt$id)
    test_idx = dt[id %in% sample(uids, size = ceiling(length(uids) * frac))]
  } else {
    dt[, id := seq_len(nrow(dt))]
    test_idx = dt[, .(id = sample(id, ceiling(.N * frac))), keyby = by]
  }
  dt[, "id" := NULL]
  list(
    test = dt[test_idx$id, ],
    train = dt[!test_idx$id, ]
  )
}

preproc_iid = function(dt, keep_cols = NULL) {
    dt = imap_dtc(dt, function(x, nm) {
      if (is.logical(x)) x = as.numeric(x)
      if (is.character(x)) x = as.factor(x)
      if (is.numeric(x) | is.integer(x)) x[is.na(x)] = 0
      if (is.factor(x)) x = fct_drop(fct_explicit_na(x, "None"))
      if (length(unique(x)) == 1 && !(nm %in% keep_cols)) x = NULL
      return(x)
    })
}

# cfg: A config
# min_evals: minimum number of evals in the data to be used
# set: Intersect with 'set'
save_task_ids = function(cfg, min_evals = 50L, set = NULL, out_name = "task_ids.txt") {
  dt = cfg$data$xtest
  cnts = dt[, .N, by = task_id]
  if (!is.null(set)) cnts = cnts[task_id %in% set, ]
  print(cnts)
  write(as.integer(as.character(unique(cnts[N >= min_evals,]$task_id))), paste0(cfg$subdir, "task_ids.txt"))
}


# Some fidelity parameters do not introduce bias but instead reduce variance.
# Examples: Replications, CV-folds
# We model this by introducing an arbitrary ordering on the replications
# and computing the cummulative mean from the first to last replication.
# By increasing the number of replications, we can thus go from minimal to maximal fidelity.
apply_cummean_variance_param = function(dt, mean, sum, fidelity_param, ignore = NULL) {
  hpars = setdiff(colnames(dt), c(mean, sum, fidelity_param, ignore))
  setorderv(dt, fidelity_param)
  if (!is.null(mean)) {
    dt[, (mean) := map(.SD, function(x) {cummean(x)}), by = hpars, .SDcols  = mean]
  }
  if (!is.null(sum)) {
    dt[, (sum) := map(.SD, cumsum), by = hpars, .SDcols  = sum]
  }
  return(dt)
}

compute_metrics = function(response, prediction, stratify = factor("_full_")) {
  if(is.null(stratify)) return(NULL)
  map_dtr(
    levels(stratify),
    function(grp) {
      map_dtr(colnames(prediction), function(nms) {
      if (grp == "_full_") {
        idx = rep(TRUE, nrow(prediction))
      } else {
        idx = (stratify == grp)
      }
      x = response[idx, nms]
      y = prediction[idx, nms]
      smp = sample(seq_along(x), min(length(x), 500L))
      data.table(
        variable = nms,
        grp = as.character(grp),
        #rsq = mlr3measures::rsq(x,y),
        rsq = rsq_(x,y),
        roh = mlr3measures::srho(x,y),
        ktau = mlr3measures::ktau(x[smp],y[smp]), # on sample since this is slow.
        mae = mlr3measures::mae(x,y)
      )
    })
  })
}

# classical rsq
rsq_ = function(truth, response) {
  mlr3measures::srho(truth, response) ^ 2
}

cummean = function(x) {
  cumsum(x) / seq_len(length(x))
}
