keys = cfgs()$keys()
out = setNames(map(keys[5:11], function(k) {
  cfg = cfgs(k, workdir=workdir)
  obj = cfg$get_objective()
  obj$trafo_dict$task_id$level
}), keys[5:11])

tab = table(unlist(out))
