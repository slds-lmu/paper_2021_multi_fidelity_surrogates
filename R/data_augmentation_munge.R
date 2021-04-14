munge_data = function(data, target_vars, munge_n = NULL) {
  if (!is.null(munge_n)) {
    train = augment_with_munge(cbind(data$xtrain, data$ytrain), target_vars = target_vars, n_augment = munge_n)
    data$ytrain = as.matrix(train[, target_vars, with = FALSE])
    data$xtrain = train[, (target_vars) := NULL]
  }
  return(data)
}


augment_with_munge = function(data, target_vars, n_augment = 10000, n_max_train = 10000, stratify = "task_id") {
  require_namespaces(c("mlr3learners", "distillery"))

  rng = lrn("regr.ranger")
  if (is.null(stratify)) {
    stratum = FALSE
  } else {
    strats = unique(data[[stratify]])
  }
  news = map(strats, function(stratum) {
    print(paste0("stratum:", stratum))
    if (!is.character(stratum)) {
      dt = copy(data)
    } else {
      dt = copy(data)[data[[stratify]] == stratum,]
      set(dt, j=stratify, value=NULL)
    }
    dt[, which(colnames(dt) %in% target_vars[-1]) := NULL]
    t = TaskRegr$new("ban", backend = dt, target = target_vars[1])
    tm = t$clone()$filter(sample(t$row_ids, min(length(t$row_ids), n_augment)))
    orig_rows = tm$row_ids
    while (tm$nrow < n_augment + length(orig_rows)) {
      x = distillery::munge_task(task = tm)
      tm$rbind(x)
    }
    tm$filter(setdiff(tm$row_ids, orig_rows))
    y_predict = map_dtc(1:4, function(i) {
      dt = copy(data)
      dt = dt[data[[stratify]] == stratum,]
      set(dt, j=stratify, value=NULL)
      dt = dt[sample(seq_len(nrow(dt)), min(nrow(dt), n_max_train)),]
      dt[, which(colnames(dt) %in% target_vars[-i]) := NULL]
      t = TaskRegr$new("ban", backend = dt, target = target_vars[i])
      l = rng$clone()
      l$train(t)
      # print(l$predict(t)$score(list(msr("regr.rsq"), msr("regr.rmse"))))
      p = l$predict(tm)
      p$data$response
    })
    names(y_predict) = target_vars
    res = data.table(cbind(y_predict, tm$data(cols = tm$feature_names)))
    set(res, j = stratify, value = stratum)
    return(res)
  })
  na.omit(rbind(data, rbindlist(news)))
}
