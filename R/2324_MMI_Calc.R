# Calculate MMI
# needed epa token to install... see email
# devtools::install_github("USEPA/aquametBio")
rm(list = ls())
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
  select("UID", "SITE_ID", "AG_ECO9","WSAREASQKM")%>%
  filter(!is.na(WSAREASQKM))

#fish Count file -- Prepared by KB/NARS IM team-- merged with Ecoregions
FishCnts <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fishcount_data.tab") %>%
  read.table(sep = "\t", header = T) %>%
  mutate(FINAL_CT = TOTAL) %>%
  select("UID", "TAXA_ID", "FINAL_CT", "ANOM_CT", "IS_DISTINCT", "NON_NATIVE","UNIQUE_ID")%>%
  inner_join(site.info, by = "UID")

#this should be the most recent taxa list
FishTaxa <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)
#########################################

# calculate fish MMI metrics
##########
# This function calculates metrics for each ecoregion's MMI.
outMets <- calcNRSA_FishMMImets(indata = FishCnts, 
                                inTaxa = FishTaxa, 
                                sampID = "UID",
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
# We have to supply the log10 of watershed area because some metrics are adjusted for watershed size.
outMets.1 <- merge(outMets[1:1267,], site.info[, c("UID", "SITE_ID", "WSAREASQKM")], by = "UID")%>%
  mutate(LWSAREA = log10(WSAREASQKM), ECO9 = AG_ECO9)%>%
  select(-c("AG_ECO9", "WSAREASQKM"))

outMMI <- calcFishMMI(inMets = outMets.1, sampID = "UID", ecoreg = "ECO9", lwsarea = "LWSAREA")
#########################################

# assign condition
##########
# Additional variables, including watershed area (km2) and total number of individuals 
# collected would be required to assign condition only if we had missing MMI values.
outCond <- assignFishCondition(outMMI, sampID = "UID", ecoreg = "ECO9", mmi = "MMI_FISH")
#########################################

# identify/remove sites that were not sufficiently sampled 
Condition <- read.csv("Data/2324_Fish_Sampling_Sufficent.csv") %>%
  select("UID","FISH_SAMPLING_SUFFICIENT_CORRECTED") %>%
  right_join(outCond, by = "UID") %>%
  mutate(COND_CHECK = case_when(
    str_detect(FISH_SAMPLING_SUFFICIENT_CORRECTED, regex("YES", ignore_case=TRUE)) ~ FISH_MMI_COND,
    str_detect(FISH_SAMPLING_SUFFICIENT_CORRECTED, regex("NO", ignore_case=TRUE)) ~ "Not Assessed",
    str_detect(FISH_SAMPLING_SUFFICIENT_CORRECTED, regex("SEINING ONLY", ignore_case=TRUE)) ~ "Not Assessed"))



library(ggplot2)
library(ggpattern)
a <- tapply(Condition$COND_CHECK, Condition$ECO9, function(s) round(prop.table(table(s)),3))
b <- do.call(rbind, lapply(a, function(x) data.frame(x)))

b$ECO <- substring(rownames(b), 0, 3)
b <- rbind(b,setNames(data.frame(round(prop.table(table(Condition$COND_CHECK)),2), 
                    ECO = "ALL"),c("s","Freq", "ECO")))

b$s <- factor(b$s, levels = c("Not Assessed", "Poor", "Fair","Good"))
P1 <- ggplot(b, aes(x = ECO, 
                     y = Freq, 
                     fill = s))+
  geom_bar(stat="identity", 
           color='black') +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(
    values = c("orange","white", "grey", "black")) +
  
  labs(
    title = "Fish assemblage condition - Proportion of sites, unweighted", 
    fill = "") + 
  theme(
    legend.title = element_text(hjust = 0.1), 
    legend.position = "top")+
  ylab("Percentage of Sites") +
  xlab("Ecoregion") 

ggsave(
  filename = "Figures/FishAssemblageCondition.jpeg", 
  P1, 
  height = 5, 
  width = 6, 
  units = "in")
