# Description: Determine sites with unmasked drugs.
# Author: Haley Hunter-Zinck
# Date: 2021-09-20
# Request:
#     Source: verbal request from Katya during drug masking meeting
#     Date of request: 2021-09-20
#     Message: "several unmasked investigational agents were noted: for example, GDC0941  and MK2206."  
#             Determine sites and record keys to report to sites for masking

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_drug <- "syn21446703"

# parameters
drug_names_check <- c("GDC0941", "MK2206")
drug_names_control <- c("Investigational Drug", "Carboplatin")


# read ----------------------------

drug_columns <- c(paste0("drugs_drug_", c(1:5)), paste0("drugs_drug_oth_", c(1:5)))
drug_columns_str <- paste0("'", paste0(drug_columns, collapse = "','"), "'")
query <- glue("SELECT cohort, record_id, redcap_repeat_instance, redcap_data_access_group, {drug_columns_str} FROM {synid_table_drug}")
data <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

# real check
res_check <- data %>%
  filter(grepl(pattern = drug_names_check[1], x = drugs_drug_1) | grepl(pattern = drug_names_check[2], x = drugs_drug_1) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_2) | grepl(pattern = drug_names_check[2], x = drugs_drug_2) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_3) | grepl(pattern = drug_names_check[2], x = drugs_drug_3) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_4) | grepl(pattern = drug_names_check[2], x = drugs_drug_4) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_5) | grepl(pattern = drug_names_check[2], x = drugs_drug_5) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_oth_1) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_1) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_oth_2) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_2) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_oth_3) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_3) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_oth_4) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_4) |
           grepl(pattern = drug_names_check[1], x = drugs_drug_oth_5) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_5))

# control
res_control <- data %>%
  filter(grepl(pattern = drug_names_control[1], x = drugs_drug_1) | grepl(pattern = drug_names_control[2], x = drugs_drug_1) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_2) | grepl(pattern = drug_names_control[2], x = drugs_drug_2) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_3) | grepl(pattern = drug_names_control[2], x = drugs_drug_3) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_4) | grepl(pattern = drug_names_control[2], x = drugs_drug_4) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_5) | grepl(pattern = drug_names_control[2], x = drugs_drug_5) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_oth_1) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_1) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_oth_2) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_2) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_oth_3) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_3) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_oth_4) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_4) |
            grepl(pattern = drug_names_control[1], x = drugs_drug_oth_5) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_5))

# close out ----------------------------

print(glue("Number of rows in control: {nrow(res_control)}"))
print(glue("Number of rows in check: {nrow(res_check)}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
