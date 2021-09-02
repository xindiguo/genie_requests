# Description: Determine why WAKE has no variants in GENIE consortium release
#    11.2.
# Author: Haley Hunter-Zinck
# Date: September 1, 2021
# slack message:
#   from: Tom Yu
#   date: August 30, 2021
#   msg: "Can you take a look at the 11.2-consortium release: 
#          https://www.synapse.org/#!Synapse:syn26134642
#         WAKE has 0 variants because none of their SAMPLE_IDs 
#         in the mutation file match whats in their clinical file…"
# Notes: 
#   CNA sample IDs match for 86%.  Mut for 0.
#   Letters in clinical are C and F but only F for mutation
#   Clincal sample ID middle numbers range from 1001-2005 but mutation sample IDs from 1499-2445
#   Changing letter from F to C of mutation sample IDs does not increase overlap
#   Subtracting 1000 from the mutation sample IDs middle number increases overlap to 77%.  


# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(dplyr)

# synapse
synid_clinical <- "syn7392892"
synid_wake_mutation <- "syn22268698"
synid_wake_cna <- "syn22271205"
synid_sample <- "syn9734573"

# parameters
center <- "WAKE"

# functions ----------------------------


# read ----------------------------

cli <- read.csv(synGet(synid_clinical)$path, stringsAsFactors = F, sep = "\t")
mut <- read.csv(synGet(synid_wake_mutation)$path, stringsAsFactors = F, sep = "\t")
cna <- read.csv(synGet(synid_wake_cna)$path, stringsAsFactors = F, sep = "\t")
sam <- read.csv(synGet(synid_sample)$path, sep = "\t")

# mutation_extended ----------------------------

# verify intersection is 0 --> yes
sam_sam <- as.character(unlist(sam %>% 
 filter(grepl(pattern = center, x = sam[,"Sample.Identifier"]))  %>%
 select("Sample.Identifier")))
sam_cli <- as.character(unlist(cli %>% 
  filter(CENTER == center) %>%
  select("Sample.Identifier" )))
sam_mut <- as.character(unlist(mut %>%
  select(Tumor_Sample_Barcode) %>%
  distinct()))
length(intersect(sam_cli, sam_mut))
length(intersect(sam_sam, sam_mut))

# browse
head(sam_cli)
head(sam_mut)

# number of sample IDs
length(sam_cli)
length(sam_mut)

# number of characters per sample ID
table(nchar(sam_cli))
table(nchar(sam_mut))

# letters
table(substr(sam_cli, 12, 12))
table(substr(sam_mut, 12, 12))

# numbers, middle
num_cli <- as.double(substr(sam_cli, 13, 16))
num_mut <- as.double(substr(sam_mut, 13, 16))
length(intersect(num_cli, num_mut))
par(mfrow = c(2,1))
hist(num_cli)
hist(num_mut)
quantile(num_cli)
quantile(num_mut)
quantile(num_mut-1000)

# numbers, suffix
suf_cli <- substr(sam_cli, 18, 19)
suf_mut <- substr(sam_mut, 18, 19)
table(suf_cli)
table(suf_mut)

# replacement of F for C in mutation file? only 6 in intersection after letter swap
sam_mut_c <- gsub(pattern = "F", replacement = "C", x = sam_mut)
length(intersect(sam_mut_c, sam_cli))

# subtract 1000?
length(intersect(num_cli, num_mut)) / length(num_mut) * 100
length(intersect(num_cli, num_mut-1000)) / length(num_mut) * 100
sam_mut_grand <- glue("{substr(sam_mut, 1, 12)}{num_mut - 1000}{substr(sam_mut, 17, 19)}")
length(intersect(sam_cli, sam_mut_grand)) / length(sam_mut) * 100


# cna -----------------------------------

# to CNA samples match clinical file?  yes, ~ 86% at least
sam_cli <- as.character(unlist(cli %>% 
                                 filter(CENTER == center)  %>%
                                 select(SAMPLE_ID)))
sam_cna <- gsub(pattern = ".", replacement = "-", x = colnames(cna)[2:length(cna)], fixed = T)
length(intersect(sam_cli, sam_cna)) / length(sam_cna) * 100

# check previous version to see where change ocurred ----------------------------

version_current <- synGet(synid_wake_mutation, downloadFile = F)$properties$versionNumber
sam_ver <- list()

for (version_index in 1:version_current) {
  sam_ver[[version_index]] <- read.csv(synGet(synid_wake_mutation, version = version_index)$path, stringsAsFactors = F, sep = "\t")$Tumor_Sample_Barcode
}

# how do numbers of recorded mutations and distinct samples differ over versions?  just last is a drop
unlist(lapply(sam_ver, length))
unlist(lapply(sam_ver, function(x) {length(unique(x))}))
unlist(lapply(sam_ver, function(x) {length(intersect(sam_cli, x))}))


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
