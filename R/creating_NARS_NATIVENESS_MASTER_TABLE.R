
################################################################################
library(sf)
library(tidyverse)
library(nhdplusTools)
library(tmap)
################################################################################


# lookup tables for nativeness assignments. 
# Files obtained from USGS NAS program -- email saved Matt from NAS.
# and PiSCES (M. Cyrterski)
#########
# renamed fields to match NRSA fish collection files

# NatureServe/NAS
NATIVE_HUC <- read.csv("HUC8TaxaLists/native fish HUC list.csv") %>%
  mutate(HUC8 = ifelse(nchar(HUC8) == 7,
                       paste0("H0", HUC8),
                       paste0("H", HUC8)),
         common_name = toupper(common_name),
         source = data_source,
         scientific_name = scientific_name,
         NON_NATIVE = "N")%>%
  select(HUC8, scientific_name, common_name, source, NON_NATIVE)

# NAS
NONNATIVE_HUC <- read.csv("HUC8TaxaLists/nonnative fish HUC list.csv") %>%
  mutate(HUC8 = ifelse(nchar(HUC8_number_char)==7,
                       paste0("H0",HUC8_number_char),
                       paste0("H", HUC8_number_char)),
         common_name = toupper(common_name),
         source = "NAS",
         scientific_name = scientific_name,
         NON_NATIVE = "Y") %>%
  select(HUC8, scientific_name, common_name, source, NON_NATIVE)

# PiSCES
library(readxl)
PiSCES_HUC <- read_xlsx("HUC8TaxaLists/PiSCES_fish_HUCs.xlsx") %>%
  mutate(HUC8 = ifelse(nchar(HUC)==7,
                       paste0("H0", HUC),
                       paste0("H", HUC)),
         common_name = toupper(Common_name),
         source = "PiSCES",
         scientific_name = Scientific_name,
         NON_NATIVE = ifelse(Origin == "Native", "N", 
                             ifelse(Origin=="Introduced", "Y", "E")))%>%
  select(HUC8, scientific_name, common_name, source, NON_NATIVE)

# NRSA
# Files pulled from IM 9/6/2024
allTheNRSA <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/allTheNRSA/data/tabfiles"

allTheNRSA.site.info <- paste0(allTheNRSA, "/NRSA0809-1819_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)

allTheNRSA.fishCts <- paste0(allTheNRSA, "/NRSA0809-1819_fishCts_alltheNRSA.tab") %>%
  read.table(sep = "\t", header = T)

allTheNRSA.NATIVENESS_HUC8 <- allTheNRSA.site.info %>%
  select(c("UID", "SITE_ID", "HUC8","DSGN_CYCLE")) %>%
  inner_join(allTheNRSA.fishCts, by = "UID") %>%
  select(TAXA_ID, HUC8, NON_NATIVE, DSGN_CYCLE) %>%
  inner_join(nars_taxa_list[,c("TAXA_ID", "FINAL_NAME", "NRSA_SPNAME")], 
             by = "TAXA_ID") %>%
  mutate(DSGN_CYCLE = factor(DSGN_CYCLE, 
                             levels = c("2018-19", "2013-14", "2008-09")))%>%
  distinct() %>%
  rowwise() %>%
  mutate(common_name = toupper(FINAL_NAME),
         scientific_name = NRSA_SPNAME, 
         source = DSGN_CYCLE)%>%
  select(HUC8, scientific_name, common_name, source, NON_NATIVE)
########################################

#create master table
##########
Nativeness_Master_Table <- do.call(
  rbind,
  list(allTheNRSA.NATIVENESS_HUC8, 
       PiSCES_HUC, 
       NONNATIVE_HUC, 
       NATIVE_HUC))

#some of these sources overlap, priority is given to NAS/NatureServe because these files are most recent, 
#PiCSES only includes NRSA 0809, so gave priority to 1819
Nativeness_Master_Table$source <- 
  factor(Nativeness_Master_Table$source, 
         levels =  c("NAS", "NatureServe", "2018-19",
                     "PiSCES", "2013-14", "2008-09"))
Nativeness_Master_Table$index <- 
  paste0(Nativeness_Master_Table$HUC8, "_", Nativeness_Master_Table$common_name)

Nativeness_Master_Table <- split(Nativeness_Master_Table,
                                 Nativeness_Master_Table$index)

# if a HUC8 X SPECIES was surveyed multiple times, select the most recent entry
Nativeness_Master_Table <- lapply(
  Nativeness_Master_Table, function(x) 
    x[order(x$source)[1],])

Nativeness_Master_Table <- do.call(rbind, Nativeness_Master_Table)
# drop index file
Nativeness_Master_Table <- select(Nativeness_Master_Table, -index)
#add NRSA taxa ID's to master table--- 
Nativeness_Master_Table <- merge(Nativeness_Master_Table,
                                 nars_taxa_list[,c("FINAL_NAME", "TAXA_ID")], 
                                 by.x = c("common_name"),
                                 by.y = c("FINAL_NAME"),
                                 all.x = T)
Nativeness_Master_Table[is.na(Nativeness_Master_Table$TAXA_ID), "TAXA_ID"] <- ""
Nativeness_Master_Table<-Nativeness_Master_Table[Nativeness_Master_Table$NON_NATIVE!="E",]
###############################################
write.csv(Nativeness_Master_Table, "NRSA_Nativeness_Master_Table.csv", row.names = F)