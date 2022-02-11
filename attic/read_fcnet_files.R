library(data.table)
library(mlr3misc)
library(jsonlite)
library(rhdf5)

basepath = paste0(path.expand("~"), "/LRZ Sync+Share/multifidelity_data/", "fcnet/")

# Map over chunks of the file
map_chunked = function(all, fct, chunks = 10L) {
  ixs = chunk_vector(seq_along(all), n_chunks=chunks)
  imap(ixs, function(x, i) {
    fct(all[x], i)
  })
  gc()
}

# Iterate over all elements and convert to data.table
collect_hdf5 = function(all, i) {
  lst = imap(all, function(el, json) {
    lst = c(
      imap(el[c("valid_loss", "valid_mse")], function(x, nm) {
      dt = data.table(x)[, epoch := 1:100]
      dt = melt(dt, id.vars = "epoch", variable.name = "replication", value.name = nm)[, replication := as.numeric(as.factor(replication))]
      }),
      list(rbindlist(map(seq_len(100), function(x) {
      data.table(
        epoch = x,
        replication = 1:4,
        runtime = unlist(el[["runtime"]] / 100 * x),
        n_params = el[["n_params"]]
      )
      }))))
      dt = Reduce(merge, lst)
      imap(jsonlite::fromJSON(json), function(x, nm) {
        set(dt, j = nm, value = x)
      })
      return(dt)
  })
  dt = rbindlist(lst)
  filep = paste0(basepath, "_", i, gsub(".hdf5", ".csv", filename, fixed = TRUE))
  fwrite(dt, file = filep)
  return(filep)
}


filename = "fcnet_protein_structure_data.hdf5"
file = paste0(basepath, filename)
# Read full .hdf5 file
all = h5read(file, "/")
# Dump in chunks
chunk_files = map_chunked(all, collect_hdf5)
# Rbind chunks, dump as .rds and rm chunks
dt = rbindlist(map(chunk_files, fread))
saveRDS(dt, gsub(".hdf5", ".rds", file))
map(chunk_files, unlink)



filename = "fcnet_slice_localization_data.hdf5"
file = paste0(basepath, filename)
# Read full .hdf5 file
all = h5read(file, "/")
# Dump in chunks
chunk_files = map_chunked(all, collect_hdf5)
# Rbind chunks, dump as .rds and rm chunks
dt = rbindlist(map(chunk_files, fread))
saveRDS(dt, gsub(".hdf5", ".rds", file))
map(chunk_files, unlink)



filename = "fcnet_parkinsons_telemonitoring_data.hdf5"
file = paste0(basepath, filename)
# Read full .hdf5 file
all = h5read(file, "/")
# Dump in chunks
chunk_files = map_chunked(all, collect_hdf5)
# Rbind chunks, dump as .rds and rm chunks
chunk_files = list.files(basepath, "telemonitoring.*.csv", full.names = TRUE)
dt = rbindlist(map(chunk_files, fread))
saveRDS(dt, gsub(".hdf5", ".rds", file))
map(chunk_files, unlink)



filename = "fcnet_naval_propulsion_data.hdf5"
file = paste0(basepath, filename)
# Read full .hdf5 file
all = h5read(file, "/")
# Dump in chunks
chunk_files = map_chunked(all, collect_hdf5)
# Rbind chunks, dump as .rds and rm chunks
chunk_files = list.files(basepath, "naval.*.csv", full.names = TRUE)
dt = rbindlist(map(chunk_files, fread))
saveRDS(dt, gsub(".hdf5", ".rds", file))
map(chunk_files, unlink)


# Smaller sample and save full data jointly.
rds_files = list.files(basepath, ".rds", full.names = TRUE)

set.seed(123L)
dt = rbindlist(map(rds_files, function(x) {
  nm = basename(x)
  x = readRDS(x)
  x = x[sample(nrow(x), 0.05 * ceiling(nrow(x))), ]
  x[, activation_fn_1 := as.factor(activation_fn_1)]
  x[, activation_fn_2 := as.factor(activation_fn_2)]
  x[, lr_schedule := as.factor(lr_schedule)]
  x[, task := factor(gsub("_data.rds", "", nm))]
  return(x)
}), fill = TRUE)
saveRDS(dt, paste0(basepath, "data.rds"))