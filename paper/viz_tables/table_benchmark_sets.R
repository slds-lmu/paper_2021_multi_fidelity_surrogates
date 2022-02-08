# Creates the table describing benchmark sets in the appendix.
library(data.table)
library(ggplot2)
library(mlr3misc)
library(latex2exp)
library(RJSONIO)

data_path = "~/Documents/repos/yahpo_data/"

out = discard(map(list.files(data_path, full.names = TRUE), list.files, pattern = "onnx.csv", full.names = TRUE), function(x) length(x) == 0)
instance_stats = rbindlist(map(out, function(file) {
    dt = fread(file)
    dt = dt[instance != "all", ]
    dt[, scenario := basename(dirname(file))]
    return(dt)
}))

tabs = map(list.files(paste0(data_path, "/benchmark_tasks"), full.names = TRUE), function(path) {
    ll = RJSONIO::fromJSON(path)
    dt = rbindlist(map(ll, function(x) {
        xx = mean(merge(as.data.table(x), instance_stats, on = c("instance", "scenario", "target"))$spearman)
        dt = data.table(x$scenario, x$instance, paste0(x$target, collapse = ","))
        dt[, rho := xx]    
    }))
    colnames(dt) = c("scenario", "instances", "target(s)", "$\\rho$")
    return(dt)
})



xx1 = knitr::kable(tabs[[2]], format = "latex", digits = 3, label = "tab:yahposo", 
caption = "\\textbf{YAHPO-SO} (v1): Collection of single-objective benchmark instances.")
xx2 = knitr::kable(tabs[[1]], format = "latex", digits = 3, label = "tab:yahpomo", 
  caption = "\\textbf{YAHPO-MO} (v1): Collection of multi-objective benchmark instances.")


con = file("paper/viz_tables/tables_benchmark_instances.tex", open = "wt")
writeLines(xx1, con)
close(con)
con = file("paper/viz_tables/tables_benchmark_instances.tex", open = "at")
writeLines(xx2, con)
close(con)



