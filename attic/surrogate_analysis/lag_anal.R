library(data.table)
lcb = readRDS("py_lags_lcbench.rds")$py_lags_retrafo

tmp = lcb[lag == "2-1" & split == "test"]
res = tmp[, .(mce = mean(val_cross_entropy)), by = .(level, set)]
agg = res[, .(m = mean(mce), s = sd(mce), n = length(mce)), by = .(set)]
agg$se = agg$s / sqrt(agg$n)

rb = readRDS("py_lags_rbv2_super.rds")$py_lags_retrafo

tmp = rb[lag == "2-1" & split == "test"]
res = tmp[, .(mce = mean(logloss)), by = .(level, set)]
agg = res[, .(m = mean(mce), s = sd(mce), n = length(mce)), by = .(set)]
agg$se = agg$s / sqrt(agg$n)
