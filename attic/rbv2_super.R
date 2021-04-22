reticulate::use_condaenv('mlr3keras', required = TRUE)
devtools::load_all()
workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")


cfg_rbv2 = cfgs()
base_configs = c("rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_svm", "rbv2_xgboost")

psets = map(base_configs, function(x) {
    cfgs(x, workdir=workdir)$param_set
})


# collect param set
pc = ps(
    # svm 
    svm.kernel = p_fct(levels = c("linear", "polynomial", "radial")),
    svm.cost =  p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x),
    svm.gamma = p_dbl(lower = -12, upper = 12, trafo = function(x) 2^x, depends = kernel == "radial"),
    svm.tolerance = p_dbl(lower = -12, upper = -3, trafo = function(x) 2^x),
    svm.degree = p_int(lower = 2, upper = 5, depends = kernel == "polynomial"),
    svm.shrinking = p_lgl(),
    svm.num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
    # glmnet
    glmnet.alpha = p_dbl(lower = 0, upper = 1, default = 1, trafo = function(x) max(0, min(1, x))),
    glmnet.s = p_dbl(lower = -10, upper = 10, default = 0, trafo = function(x) 2^x),
    glmnet.num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
    # rpart
    rpart.cp = p_dbl(lower = -10, upper = 0, default = log2(0.01), trafo = function(x) 2^x),
    rpart.maxdepth = p_int(lower = 1, upper = 30, default = 30),
    rpart.minbucket = p_int(lower = 1, upper = 100, default = 1),
    rpart.minsplit = p_int(lower = 1, upper = 100, default = 20),
    rpart.num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
    # ranger
    ranger.num.trees = p_int(lower = 1, upper = 2000),
    ranger.replace = p_lgl(),
    ranger.sample.fraction = p_dbl(lower = 0.1, upper = 1),
    ranger.mtry.power = p_int(lower = 0, upper = 1),
    ranger.respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
    ranger.min.node.size = p_int(lower = 1, upper = 100),
    ranger.splitrule = p_fct(levels = c("gini", "extratrees")),
    ranger.num.random.splits = p_int(lower = 1, upper = 100, default = 1L, depends = splitrule == "extratrees"),
    ranger.num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
    # aknn
    aknn.k = p_int(lower = 1L, upper = 50L),
    aknn.distance = p_fct(levels = c("l2", "cosine", "ip"), default = "l2"),
    aknn.M = p_int(lower = 18L, upper = 50L),
    aknn.ef = p_dbl(lower = 3, upper = 8, trafo = function(x) round(2^x)),
    aknn.ef_construction = p_dbl(lower = 4, upper = 9, trafo = function(x) round(2^x)),
    aknn.num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
    # xgboost
    xgboost.booster = p_fct(levels = c("gblinear", "gbtree", "dart")),
    xgboost.nrounds = p_int(lower = 3, upper = 11, trafo = function(x) round(2^x)),
    xgboost.eta = p_dbl(lower = -10, upper = 0, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
    xgboost.gamma = p_dbl(lower = -15, upper = 3, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
    xgboost.lambda = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x),
    xgboost.alpha = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x),
    xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
    xgboost.max_depth = p_int(lower = 1, upper = 15, depends = booster %in% c("dart", "gbtree")),
    xgboost.min_child_weight = p_dbl(lower = 0, upper = 7, trafo = function(x) 2^x, depends = booster %in% c("dart", "gbtree")),
    xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
    xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, depends = booster %in% c("dart", "gbtree")),
    xgboost.rate_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
    xgboost.skip_drop = p_dbl(lower =  0, upper = 1, depends = booster == "dart"),
    xgboost.num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
    # learner
    learner = p_fct(levels = c("aknn", "glmnet", "ranger", "rpart", "svm", "xgboost")),
    task_id = p_fct(levels = c(1, 2, 3))
)
# Add dependencies
map(pc$params$learner$levels, function(x) {
    nms = names(pc$params)[startsWith(names(pc$params), x)]
    print(nms)
    map(nms, function(nm) pc$add_dep(nm, "learner", CondEqual$new(x)))
})

generate_design_random(pc, 10)
