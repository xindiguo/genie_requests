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

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()




# functions ----------------------------


# main ----------------------------


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
