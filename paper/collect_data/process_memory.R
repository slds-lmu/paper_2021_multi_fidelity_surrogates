library(data.table)
library(mlr3misc)
basepath = paste0(path.expand("~"), "/results/memory_rbv2")




fread(list.files(basepath, full = TRUE)[1])


files = list.files(paste0(path.expand("~"), "/results"), full = T,pattern="_prep")
this_file = files[grepl("glmnet", files)]
map(this_file, fread)

dt = readRDS(this_file)
dt[,c("task_id", "dataset") := split_task_col(task)]
dt[,task := NULL]
cfg = cfgs("rbv2_glmnet", workdir = workdir)
saveRDS(dt, gsub(".arff", ".rds", cfg$data_path))
map(this_file, unlink)