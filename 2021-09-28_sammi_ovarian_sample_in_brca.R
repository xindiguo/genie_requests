# Description: Determine if ovarian sample in BrCa cohort that should have been removed is 
#     is present in clinical files and original data upload. 
# Author: Haley Hunter-Zinck
# Date: 2021-09-28
# Request: 
#     Source: Sammi in Freedcamp task https://freedcamp.com/view/2995682/tasks/42253884
#     Date of request: 2021-09-28
#     Message: Hi, I see that the ovarian sample (cpt_oncotree_code = LGSOC) is still 
#               included in the breast data in today's data cut (9/28/2021)

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_ca <- "syn26253370"
synid_file_vicc_brca_upload <- "syn24201984"
code_ovarian = 'LGSOC'

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA")) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep)
  return(data)
}

# read ----------------------------

data_clinical <- get_synapse_entity_data_in_csv(synid_file_ca)
data_upload <- get_synapse_entity_data_in_csv(synid_file_vicc_brca_upload)

# main ----------------------------

data_clinical %>% 
  filter(cpt_oncotree_code == code_ovarian) %>%
  select(cohort, record_id, institution, cpt_number, cpt_genie_sample_id, cpt_oncotree_code)

data_upload %>% 
  filter(cpt_oncotree_code == code_ovarian) %>%
  select(record_id, cpt_genie_sample_id, cpt_oncotree_code)


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
