# Description: Load a file to Synapse.
# Author: Haley Hunter-Zinck
# Date: September 2, 2021

# input -----------------------------

args <- commandArgs(trailingOnly = T)
if (length(args) != 6) {
  cat("Usage: R = f to_synapse.R --args <file_name> <parent_id> <prov_name> <prov_desc> <prov_used> <prov_exec>\n")
  quit(save = "no")
}

file_name <- args[1]
parent_id <- args[2]
prov_name <- args[3]
prov_desc <- args[4]
prov_used <- strsplit(args[5], split = ",")[[1]]
prov_exec <- args[6]

# setup ----------------------------

tic = as.double(Sys.time())

library(synapser)
synLogin()
library(glue)

# functions ----------------------------

save_to_synapse <- function(file_name, parent_id, prov_name = "", prov_desc = "", prov_used = "", prov_exec = "") {
  file <- File(path = file_name, parentId = parent_id)
  act <- Activity(name = prov_name,
                  description = prov_desc,
                  used = prov_used,
                  executed = prov_exec)
  file <- synStore(file, activity = act)
  
  return(T)
}

# main ----------------------------

res <- save_to_synapse(file_name, parent_id, prov_name, prov_desc, prov_used, prov_exec)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
