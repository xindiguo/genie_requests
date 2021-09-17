# Description: Sample 50 patients from the BPC NSCLC 2.1 consortium release.  
# Author: Haley Hunter-Zinck
# Date: 2021-09-16
# Request:
#     Source: Shawn Sweeney
#     Request date: 2021-09-16
#     Message:  randomly select 50 patients from the BPC NSCLC 2.1-consortium 
#              release and generate the 8 release files for just those 50 patients


# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(dplyr)

# synapse
synid_file_manifest <- "syn26209101"
synid_files_data <- c("syn25985885", "syn25985887", "syn25985888")
synid_folder_sample <- "syn26209100"

# functions ----------------------------

get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}


save_to_synapse <- function(path, parent_id, file_name = NA, prov_name = NA, prov_desc = NA, prov_used = NA, prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  
  return(T)
}

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA"), header = T) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, header = header)
  return(data)
}


# read ----------------------------

patient_ids <- get_synapse_entity_data_in_csv(synid_file_manifest, header = F)

datas <- list()
for (synid in synid_files_data) {
  datas[[synid]] <- get_synapse_entity_data_in_csv(synid)
}

# main ----------------------------

sampled <- list()

for (i in 1:length(datas)) {
  sampled[[names(datas)[i]]] <- datas[[i]] %>% 
    filter(is.element(record_id, unlist(patient_ids)))
}

lapply(sampled, dim)

# write -----------------------------

# save to synapse
for (i in 1:length(sampled)) {
  
  file_output <- gsub(pattern = ".csv", replacement = "_sample.csv", 
                    x = get_synapse_entity_name(names(sampled)[i]))
  
  write.csv(sampled[[i]], row.names = F, file = file_output)
  save_to_synapse(path = file_output, parent_id = synid_folder_sample)
  file.remove(file_output)
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
