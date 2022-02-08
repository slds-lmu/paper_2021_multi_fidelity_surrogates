library(data.table)
library(ggplot2)
library(mlr3misc)
library(latex2exp)
data_path = "~/Documents/repos/yahpo_data"
out = discard(map(list.files(data_path, full.names = TRUE), list.files, pattern = "onnx.csv", full.names = TRUE), function(x) length(x) == 0)
 
global_stats = rbindlist(map(out, function(file) {
    dt = fread(file)
    dt = dt[instance == "all", ]
    dt[, instance := NULL][, scenario := basename(dirname(file))]
    return(dt)
}))

fwrite(global_stats, "paper/viz_tables/global_surrogate_stats.csv")


instance_stats = rbindlist(map(out, function(file) {
    dt = fread(file)
    dt = dt[instance != "all", ]
    dt[, scenario := basename(dirname(file))]
    return(dt)
}))

make_names = function(x) {
    x = gsub("balanced_", "b", x)
    x = gsub("cross_entropy", "ce", x)
    x = gsub("accuracy", "acc", x)
    x = gsub("test", "test", x)
    return(x)
}

# Decide which instances to drop:
rho_cutoff = 0.7
drop_idx = instance_stats[, list(sp = mean(spearman), scenario = unique(scenario)), by = "instance"][sp < rho_cutoff]
# lcbench : 167083, 167184

instance_stats = instance_stats[!drop_idx, on = c("instance", "scenario")]
instance_stats[, target := make_names(target)]

p = ggplot(instance_stats, aes(x=target, y = spearman)) + 
  geom_boxplot(outlier.shape =NA) +
  geom_jitter(width = .1, alpha = .3) +
  theme_bw() +
  xlab("") +
  ylab(TeX("Spearman's $\\rho$")) +
  facet_wrap(~scenario) +
  coord_cartesian(ylim = c(0.5, 1)) +
  theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
  )
ggsave("paper/viz_tables/boxplot_surrogate_rho.pdf")
  
