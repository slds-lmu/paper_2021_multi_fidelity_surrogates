# See https://github.com/google-research/google-research/tree/master/task_set
# Florian has a script to download more data.
library(data.table)
library(mlr3misc)
dt = fread("~/Downloads/task_set_8p_1_20.csv")  
head(dt)

dt[,(colnames(keep(dt, is.character))) := map(.SD, as.factor), .SDcols= is.character]
dt[, epoch := as.integer(epoch)]
dt[, replication := as.integer(replication)]
dt = dt[!is.na(train), ]
dt[, V1 := NULL]
summary(dt)

basepath = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/", "task_set/")
saveRDS(dt, paste0(basepath, "data.rds"))
