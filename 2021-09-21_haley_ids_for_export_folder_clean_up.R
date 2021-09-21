# Description: Create folder structure by structure and site within the "IDs for Export" folder
#    to facilitate file organization.
# Author: Haley Hunter-Zinck
# Date: 2021-09-21
# Request: 
#    Source: self-initialized
#    Date requested: 2021-09-21
#    Message: automate folder structure creation

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_folder_staging <- "syn26127146"

# parameters
cohort_sites <- list()
cohort_sites[["BLADDER"]] <- c("DFCI", "MSK", "UHN", "VICC")
cohort_sites[["BrCa"]] <- c("DFCI", "MSK", "VICC")
cohort_sites[["CRC"]] <- c("DFCI", "MSK", "VICC")
cohort_sites[["NSCLC"]] <- c("DFCI", "MSK", "UHN", "VICC")
cohort_sites[["PANC"]] <- c("DFCI", "MSK", "UHN", "VICC")
cohort_sites[["Prostate"]] <- c("DFCI", "MSK", "UHN", "VICC")


# functions ----------------------------

get_synapse_folder_children <- function(synapse_id) {
  
  
  ent <- as.list(synGetChildren(synapse_id))
  
  children <- c()
  
  if (length(ent) > 0) {
    for (i in 1:length(ent)) {
      children[ent[[i]]$name] <- ent[[i]]$id
    }
  }
  
  return(children)
}

create_synapse_folder <- function(name, parentId) {
  
  # check if folder already exists
  children <- get_synapse_folder_children(parentId)
  if(is.element(name, names(children))) {
    return(as.character(children[name]))
  }
  
  concreteType <- "org.sagebionetworks.repo.model.Folder"
  uri <- "/entity"
  payload <- paste0("{", glue("'name':'{name}', 'parentId':'{parentId}', 'concreteType':'{concreteType}'"), "}")
  ent <- synRestPOST(uri = uri, body = payload)
  return(ent$id)
}

# main ----------------------------

synid_folders <- matrix(NA, nrow = length(cohort_sites), ncol = max(unlist(lapply(cohort_sites, length))) + 1, 
                        dimnames = list(sort(names(cohort_sites)), c("root", sort(unique(unlist(cohort_sites))))))

for (cohort in names(cohort_sites)) {
  
  synid_folders[cohort, "root"] <- create_synapse_folder(name = cohort, parentId = synid_folder_staging)
  
  for (site in cohort_sites[[cohort]]) {
    synid_folders[cohort,site] <- create_synapse_folder(name = site, parentId = synid_folders[cohort, "root"])
  }
}

# close out ----------------------------

print("Folders created: ")
print(synid_folders)

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
