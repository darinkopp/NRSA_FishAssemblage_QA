# Range check and native status

# For each species, query natureserve, non-indigenous aquatic species,
# previous NRSA surveys and PiSCES. 

# Native designated if the HUC where the taxon was collected is also listed in 
# the native range

# Non-native if the HUC where the taxon was collected is in NAS Non-native range. 

# When a HUC was surveyed during a previous cycle, assign the native/non-native 
# designation based on PiSCES and previous surveys-- 
# This is essentially leveraging expert opinion (i.e. D. Peck et al. and M. Cyterski)

# A manual check of unmatched taxa could indicate the the reported taxa is out 
# the species range conflicting assignments. 

################################################################################
library(sf)
library(tidyverse)
library(nhdplusTools)
library(tmap)
################################################################################


################################################################################
# Required data
################################################################################

# Site info -- locations and HUC8 needed to designate native/non-native status
######
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

# note /data/ are the outputs from IM
site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)

# add missing HUCs -- sometimes this could happen because locations are not yet
# finalized. the site info file will likely be updated by MW in next version

HUCs <- site.info %>%
  filter(HUC8 == "" & 
           !is.na(LON_DD83) & 
           !is.na(LAT_DD83)) %>%
  select(UID, SITE_ID, LON_DD83, LAT_DD83)

pt <- st_as_sf(HUCs,
               coords = c("LON_DD83", "LAT_DD83"), 
               crs = 4269)

# query HUC12 from nhdplustools
HUCs$HUC8 <- NA
for(i in 1:nrow(pt)){
  #i<-1
  HUCs$HUC8[i] <- get_huc(pt[i,], type = "huc08") %>%
    select(huc8) %>% 
    st_set_geometry(NULL)
}
HUCs$HUC8 <- unlist(HUCs$HUC8)

# add missing HUC8 to the site.info 
for (i in HUCs$UID){
  #i<-2022239
  site.info[site.info$UID==i, "HUC8"] <- paste0("H", HUCs[HUCs$UID == i, "HUC8"])
}

######################


# NRSA taxa list -- most recent. This is needed to crosswalk 
# taxa_id to species names 
########
#this should be the most recent taxa list
nars_taxa_list <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)

# check errors in taxa files
if(anyDuplicated(nars_taxa_list$FINAL_NAME)){
  stop("duplicated FINAL_NAME in nrsa2324_fish_taxa.tab")
}

# add species name
nars_taxa_list <- nars_taxa_list %>%
  rowwise()%>%
  mutate(NRSA_SPNAME = str_to_sentence(paste0(c(GENUS, SPECIES), collapse = " ")))
####################################################

# 2324 fish collections with reconciled names 
########
fish_col <-read.table("nrsa2324_fishcollectionWide_fish_Corrected.tab",
                      sep = "\t") %>%
  #filter(LINE_CORRECTED != "DELETE") %>%
  select(-TAXA_ID)%>%
  distinct()%>%
  mutate(FINAL_NAME = NAME_COM_CORRECTED)


# All taxa should have a TAXA_ID before reaching this step in the future
if(!all(fish_col$NAME_COM_CORRECTED %in% nars_taxa_list$FINAL_NAME))
  {stop("All taxa should have a TAXA_ID; update autecology file")}
fish_col <- merge(fish_col, 
                  nars_taxa_list[,c("FINAL_NAME","TAXA_ID","NRSA_SPNAME")], 
                  by.x = "NAME_COM_CORRECTED", 
                  by.y = "FINAL_NAME")

fish_col <- merge(fish_col, site.info[,c("UID","HUC8")], by="UID", all.x=T)
####################################################
dim(fish_col)

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

################################################################################
# Range Check and Nativeness 
################################################################################

#add new taxa to maser
###################################################  

#if you want to see the new occurrences added during 2324 use
#Nativeness_Master_Table <- read.csv("NRSA_Nativeness_Table.csv")
Nativeness_Master_Table <- read.csv("2324NRSA_Nativeness_Table.csv")

# look for occurrences with 6 and 4 digit HUCS. if there are no occurrences, 
# could potential indicate that the specimen was outside the range. 
#######
# initialize loop
MissingTaxa_HUC <- merge(fish_col, 
                         Nativeness_Master_Table, 
                         by.x = c("HUC8", "TAXA_ID"),
                         by.y = c("HUC8", "TAXA_ID"),
                         all.x = T)
MissingTaxa_HUC[MissingTaxa_HUC$TAXA_ID == 99999, "NON_NATIVE"] <- ""
#GU and Selawik do not have HUCs 
MissingTaxa_HUC[is.na(MissingTaxa_HUC$HUC8), "NON_NATIVE"] <- ""
MissingTaxa_HUC[is.na(MissingTaxa_HUC$HUC8), "HUC8"] <- ""

# these taxa and Huc8 need to ba added to the table
# by searching for occurrences at the HUC6 and then HUC4
# levels. 

MissingTaxa_HUC <- MissingTaxa_HUC %>%
  filter(is.na(NON_NATIVE) & HUC8 != "")%>%
  select(c("HUC8", "TAXA_ID"))

# Iterator to select the huc level
n = 7
# new taxa to add to master nativeness table
add2master <- data.frame()
while(n >= 5){
  
  # add New HUC (6 or 4)
  MissingTaxa_HUC <- MissingTaxa_HUC %>%
    mutate(HUC_NEW = substring(HUC8, 1, n)) %>% 
    distinct() 
  
  # This calculates percentage of HUC8s with native/nonnative
  # occurrences within the larger HUC
  LargerHUC <- Nativeness_Master_Table %>%
    mutate(HUC_NEW = substring(HUC8, 1, n)) %>%
    group_by(HUC_NEW, TAXA_ID) %>% 
    reframe(TAXA_ID = unique(TAXA_ID),
            NON_NATIVE = mean(NON_NATIVE == "Y"), 
            #source = "2023-24",
            COMMENTS = paste0(unique(source), collapse = "/"))
  
  # Use this to update nativeness maps, this resolves any potentially 
  # conflicting designations
  LargerHUC <- merge(MissingTaxa_HUC, 
                     LargerHUC, by = c("HUC_NEW", "TAXA_ID"))%>%
    filter(NON_NATIVE > 0.8 | NON_NATIVE < 0.2) %>%
    mutate(NON_NATIVE = ifelse(NON_NATIVE > 0.8, "Y", 
                               ifelse(NON_NATIVE < 0.2, 
                                      "N", NA)),
           source = "2023-24",
           COMMENTS = paste0(COMMENTS, " [HUC", n-1, "]"),
           scientific_name = "" ,
           common_name = "")%>%
    select(-HUC_NEW)
  
  # list of taxa to rbind to Nativeness_Master_Table
  add2master <- rbind(add2master, LargerHUC)
  
  #up data for next iteration
  MissingTaxa_HUC <- merge(MissingTaxa_HUC,
                           add2master[c("HUC8", "TAXA_ID", "NON_NATIVE")],
                           by = c("HUC8", "TAXA_ID"), 
                           all.x = T) %>%
    filter(is.na(NON_NATIVE))%>%
    select(-NON_NATIVE)
 
  n = n-2
}
#########################################

# resolve potential range flags
if(nrow(add2master)==0){print("no new taxa to add")
  } else {print("new taxa, proceed with writing new master table")}
########
tmp <- rbind(add2master[,-6], Nativeness_Master_Table)

b <- merge(fish_col, tmp, 
           by = c("HUC8","TAXA_ID"), 
           all.x = T)
# these sites do not have coordinates or are outside CONUS
b[b$TAXA_ID == "99999", "NON_NATIVE"] <- ""

dim(b)
sum(b$HUC8!="" & !is.na(b$HUC8) & is.na(b$NON_NATIVE))

# these need to be manually checked. The species were not found 
# within the HUC4 by any of the sources. Could be a new taxon or a indication 
# of a range violation
CHECK_new <- b[b$HUC8!="" & !is.na(b$HUC8) & is.na(b$NON_NATIVE),
               c("HUC8","TAXA_ID", "UID", "NAME_COM_CORRECTED", 
                 "SITE_ID", "NON_NATIVE", "source")]

CHECK_new$COMMENTS <- ""
CHECK_new$NAME_CORRECTION <- ""


# DONT NOT overwrite!!! add date to file after written to ensure no problems
#write.csv(CHECK_new, "checks/NONNATIVE_CHECK_NEW_FILE_XXXXXXX.csv")

# It is sometimes to map the location of the HUC8 to identify major river
# to decipher whether it is native
library(tmap)
WBD <- st_read(dsn = "Supporting Information/WBD_National_GDB/WBD_National_GDB.gdb", layer = "WBDHU8")
tmap_mode("view")
p <- WBD[WBD$huc8 == "10270101", "name"]
tm_shape(p)+
  tm_borders()
###########################################

newTaxa <- read.csv("checks/NONNATIVE_CHECK_NEW_FILE_04042025.csv")
#correct lines that had range violation
########
updates <- newTaxa[newTaxa$NAME_CORRECTION != "",
        c("UID","NAME_COM_CORRECTED", "SITE_ID", "NON_NATIVE","NAME_CORRECTION")]
for (i in 1:nrow(updates)){
  #i<-1
  #fish_col[fish_col$UID=="2022757" & fish_col$NAME_COM_CORRECTED == "RIVER CHUB", "NAME_COM_CORRECTED"]  
  fish_col[fish_col$UID==updates$UID[i] & 
             fish_col$NAME_COM_CORRECTED == updates$NAME_COM_CORRECTED[i], 
           "NAME_COM_CORRECTED"]<-updates$NAME_CORRECTION[i]  
}

searched_taxa <- unique(data.frame(newTaxa[,c("HUC8", "TAXA_ID", "NON_NATIVE")],
           common_name = newTaxa$NAME_COM_CORRECTED,
           scientific_name = "",
           COMMENTS = "SEARCHED by DK",
           source = "2023-24")) 

if(anyDuplicated(searched_taxa[,c("HUC8","TAXA_ID")])){
  stop("duplicated taxaXHUC8")
}
###################################
add2master <- rbind(add2master, searched_taxa)

#add new taxa to Nativeness Master Table
##############
NRSA_Nativeness_Table <- rbind(Nativeness_Master_Table, add2master[,-6])%>%
  filter(TAXA_ID != "") %>%
  select(c("HUC8", "source", "NON_NATIVE", "TAXA_ID"))
#########################################
#write.csv(NRSA_Nativeness_Table, "2324NRSA_Nativeness_Table.csv")

#add native/non-native to fish collection file
finished <- merge(fish_col, 
                  NRSA_Nativeness_Table[, c("HUC8", "TAXA_ID", "NON_NATIVE")], 
                  by = c("HUC8", "TAXA_ID"), all.x = T)

#write.csv(finished, "2324NRSA_Fish_collection.csv")


# Fun map of nativeness database
###########
library(tmap)
WBD <- st_read(dsn = "Supporting Information/WBD_National_GDB/WBD_National_GDB.gdb", layer = "WBDHU8")
head(NRSA_Nativeness_Table)
CONUS <- c(1:18)
CONUS <- ifelse(nchar(CONUS)==1, paste0("0",CONUS),CONUS)
HUCS<-HUCS[substring(HUCS,1,2)%in%CONUS]

NON_NativeP <- NRSA_Nativeness_Table%>%
  group_by(HUC8)%>%
  summarize(NNP = mean(NON_NATIVE=="Y"))%>%
  mutate(HUC8 = substring(HUC8,2))%>%
  filter(substring(HUC8,1,2) %in% CONUS)
nrow(NON_NativeP)
tmap_mode("view")
tmap_mode("plot")

p <- merge(WBD,NON_NativeP, by.x = "huc8", by.y = "HUC8")

p2 <- WBD[WBD$huc8%in%substring(NRSA2324,2),]
windows()
T1<-tm_shape(p)+
  tm_polygons("NNP")+
  tm_shape(p2)+
  tm_borders(col = "green")
##########################################
#tmap_save(T1,"NativenessExample.jpeg", width = 12, height = 10, units = "in")

