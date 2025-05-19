# Calculate MMI
# needed epa token to install... see email
# devtools::install_github("USEPA/aquametBio")

library(aquametBio)
library(tidyverse)

# vignette/workflow 
# https://github.com/USEPA/aquametBio/blob/main/vignettes/CalculateFishMetrics.Rmd

# NRSA DATA
########
# NRSA data directory
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

# need site info for aggregated ecoregions
site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)%>%
  select("UID", "AG_ECO9")

# need watershed area from MW... 


#fish Count file -- Prepared by KB/NARS IM team-- merged with Ecosregions
FishCnts <- dir_NRSA_2324%>%
  paste0("/data/tabfiles/nrsa2324_fishcount_data.tab") %>%
  read.table(sep = "\t", header = T) %>%
  mutate(FINAL_CT = TOTAL)%>%
  select("UID", "TAXA_ID", "FINAL_CT", "ANOM_CT", "IS_DISTINCT", "NON_NATIVE")%>%
  left_join(site.info,by="UID")

#this should be the most recent taxa list
FishTaxa <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)


#########################################

# calculate fish MMI metrics
##########
# This function calculates metrics for only the MMI for the ecoregion of each site.
outMets <- calcNRSA_FishMMImets(FishCnts, FishTaxa, "UID",
                                ecoreg =  "AG_ECO9",
                                dist = "IS_DISTINCT", ct = "FINAL_CT",
                                taxa_id = "TAXA_ID", tol = "TOLERANCE_NRSA",
                                vel = "VEL_NRSA", habitat = "HABITAT_NRSA",
                                trophic = "TROPHIC_NRSA", migr = "MIGR_NRSA", nonnat = "NON_NATIVE",
                                reprod = "REPROD_NRSA", family = "FAMILY", genus = "GENUS",
                                comname = "FINAL_NAME")

#########################################

# calculate fish MMI
############
# We have to supply the log10 of watershed area because some metrics in each MMI are adjusted for watershed size.
#outMets.1 <- merge(outMets...)
outMMI <- calcFishMMI(outMets.1, sampID = "UID", ecoreg = "ECO9", lwsarea = "LWSAREA")
#########################################

# assign condition
##########
# Additional variables, including watershed area (km2) and total number of individuals 
# collected would be required to assign condition only if we had missing MMI values.
outCond <- ?assignFishCondition(outMMI, sampID = "UID", ecoreg = "ECO9", mmi = "MMI_FISH")
#########################################
