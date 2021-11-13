# Description: Get sample sequenced for patients used in phase 1. 
# Author: Haley Hunter-Zinck
# Date: 2021-11-12

# pre-setup  ---------------------------

library(optparse)

waitifnot <- function(cond, msg) {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}

# user input ----------------------------

option_list <- list( 
  make_option(c("-c", "--cohort"), type = "character",
              help="BPC cohort"),
  make_option(c("-s", "--site"), type = "character",
              help="BPC site"),
  make_option(c("-u", "--save_synapse"), action="store_true", default = FALSE, 
              help="Save output to Synapse")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$cohort) && !is.null(opt$site),
          msg = "Rscript template.R -h")

cohort <- opt$cohort
site <- opt$site
save_synapse <- opt$save_synapse


# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_bpc <- "syn21446700"
synid_table_pat <- "syn7517669"
synid_table_sam <- "syn7517674"

# parameters
codes <- c('NSCLC', 'CMPT', 'LCLC', 'LUAD', 'LUPC', 'LUSC', 'NSCLCPD', 'LUAS', 'LUMEC', 'LECLC', 'CCLC', 'RLCLC', 'GCLC', 'BLCLC')
year_min <- "2018"
year_max <- "2019"

# functions ----------------------------

# read ----------------------------

query <- glue("SELECT record_id AS PATIENT_ID FROM {synid_table_bpc} WHERE cohort = '{cohort}' AND redcap_data_access_group = '{site}'")
bpc_ids <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

query <- glue("SELECT PATIENT_ID, SAMPLE_ID, SEQ_YEAR, ONCOTREE_CODE, AGE_AT_SEQ_REPORT FROM {synid_table_sam}")
sam_inf <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

query <- glue("SELECT PATIENT_ID, YEAR_DEATH, BIRTH_YEAR  FROM {synid_table_pat}")
pat_inf <-  as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

data <- bpc_ids %>% 
  inner_join(pat_inf, by = "PATIENT_ID") %>%
  inner_join(sam_inf, by = "PATIENT_ID") %>%
  filter(is.element(ONCOTREE_CODE, codes) & SEQ_YEAR <= YEAR_DEATH & SEQ_YEAR >= year_min & SEQ_YEAR <= year_max) %>%
  select(PATIENT_ID, SAMPLE_ID)

# write ----------------------------

file_output <- tolower(glue("{cohort}_{site}_phase1_additional_samples.csv"))
write.csv(data, file = file_output, row.names = F)

# close out ----------------------------

print(glue("Cohort: {cohort}"))
print(glue("Site: {site}"))
print(glue("Number of samples: {nrow(data)}"))
print(glue("Output written to '{file_output}'"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
