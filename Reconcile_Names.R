#QA for Fish Assemblages 

# 1) Reconcile names - Merge NAME_COM with FINAL_NAME from NRSA Taxa List

# Common Names are provided by field crew. In some instances there are simple 
# misspellings or typos that prevent direct matching with existing NRSA Taxa list 
# 
# First checked "Unknown" or unidentified taxa against UNKNOWNS in FINAL_NAME
# using grep

# Second checked NAME_COM against most similar taxa listed in FINAL_NAME using 
# Fuzzy Matching. Obvious spelling errors were corrected 

# Third NAME_COM was compared with taxa previously collected from the state 
# during 1819 NRSA. This was used to reconcile ambiguous naming in NAME_COM  

# Fourth NAME_COM was compared against AFS names (see: Names-of-Fishes-8-Table1.pdf)
# accepted names are added as new records in database

library(tidyverse)
library(sf)
library(tmap)
library(stringdist)

################################################################################
# Data Files 
################################################################################

# Files from NRSA 1819: allNRSA_fishTaxa.tab contains Autecology data 
# nars_taxa_col_1819 contains records of taxa collected previously. These files 
# were used to identify if an ambiguous taxon was collected previously from the 
# state
######
dir_NRSA_1819 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa1819/data/"

nars_taxa_list <- paste0(dir_NRSA_1819, "allNRSA_fishTaxa.tab") %>%
  read.table(sep = "\t", header = T)

nars_taxa_col_1819 <-  paste0(dir_NRSA_1819, "nrsa1819_fishCount_newUid.tab") %>%
  read.table(sep = "\t", header = T)
#######################

# Site info -- locations needed to designate native/non-native status
######
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

# note /data/ are the outputs from IM
site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)

# lon83 was negative for handpicked sites, 
# KB corrected in next version
site.info[!is.na(site.info[,c("LON_DD83")]) & site.info[,c("LON_DD83")]>0, "LON_DD83"] <- 
  site.info[!is.na(site.info[,c("LON_DD83")]) & site.info[,c("LON_DD83")]>0, "LON_DD83"] * -1
#######################

# Fish Files
# removed any 2024 samples because they are not completed with the
# identification. NRS23-Selawik-XX has dashes instead of underscores-- Not sure 
# if this is a problem, Fish samples from NM and CO are missing.
######
fish_col <- dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_fishcollectionWide_fish.tab") %>%
  read.table(sep = "\t", header = T)%>%
  filter(grepl("2023", DATE_COL)) %>% # filters 2024 which may have incomplete identification  
  mutate(SITE_ID_Mod = gsub("-","_", SITE_ID)) %>% # NRS23-Selawik-09 has dashes?
  mutate(STATE = unlist(lapply(strsplit(SITE_ID_Mod,"_"), "[[", 2)))

#seem to be missing states
state.abb[!state.abb%in%fish_col$STATE]


# "nrsa2324_fishinfoWide.tab" sampling info. Did they sample to completion? Gear type etc.
# "nrsa2324_fishcollectionWide_fplg.tab" Fish plug info -- unnecessary at the moment
# "nrsa2324_fishcollectionWide_vert.tab" Fish voucher specimens 
#######################

################################################################################
# Quick tmap to view 2023 fish records. Locations with in fish collection file
# and site.info file
################################################################################

#WGS84 for Guam and AK -- Map sites with fish collection data NRSA 23
######
site.info_WGS84 <- site.info%>%
  filter(!is.na(LON_DD84)& !is.na(LAT_DD84) & UID %in% fish_col$UID) %>%
  select(c(UID, SITE_ID, VISIT_NO, YEAR, HUC8, DATE_COL,
           LON_DD84, LAT_DD84))%>%
  mutate_at(vars(YEAR), 
            list(factor))%>%
  st_as_sf(coords = c("LON_DD84", "LAT_DD84"), crs = 4326)%>%
  st_transform(4269)

#######################
tmap_mode("view")
tm_shape(site.info_WGS84)+
  tm_dots("YEAR")

#NAD83 for CONUS -- Map sites with fish collection data NRSA 23
#######
site.info_NAD83 <- site.info %>%
  filter(!is.na(LON_DD83) & !is.na(LAT_DD83) & UID %in% fish_col$UID) %>%
  select(c(UID, SITE_ID, VISIT_NO, YEAR, HUC8, DATE_COL,
           LON_DD83, LAT_DD83)) %>%
  st_as_sf(coords = c("LON_DD83", "LAT_DD83"), crs = 4269)
########################
tmap_mode("view")
tm_shape(site.info_NAD83) +
  tm_dots("UID")

################################################################################
# reconcile names
################################################################################

# save original file as check 
fish_col_original <- fish_col

# Function to update Fish Collection Table 
# the inputs are:
# FIELD_Name = NAME_COM (name given in field)
# FINAL_NAME = the reconciled name (typically 1819 FINAL_NAME)
# STATE = State that the record should be updated. (In some instances 
# PEARL DACE was named differently (e.g. Northern, or Allegheny) depending on 
# where it was collected)

updateRecord <- function(df, FIELD_Name, FINAL_NAME, STATE = "ALL"){
  if(STATE=="ALL"){
    df[df$NAME_COM_UPR %in% FIELD_Name, "FINAL_NAME"] <- FINAL_NAME    
  } else{
    df[df$NAME_COM_UPR %in% FIELD_Name & df$STATE==STATE, "FINAL_NAME"] <- FINAL_NAME 
  }
  return(df)
}

# Update direct matches. Records that have a match in NRSA taxa list 
#######
# Add NAME_COM_UPR, upper case of NAME_COM for matching with final name
fish_col <- fish_col%>%
  mutate(NAME_COM_UPR = toupper(NAME_COM), 
         FINAL_NAME = "")

# Updates FINAL_NAME in fish collection file with matching NRSA 1819 FINAL_NAME 
ind <- fish_col$NAME_COM_UPR %in% nars_taxa_list$FINAL_NAME
fish_col$FINAL_NAME[ind] <- fish_col$NAME_COM_UPR[ind]
##########################################################

# Check unknown taxa: 
# Used grep function to compare the taxa with 
# "unknown" entry in NAME_COM 
#######
# Sites from lower 48 without matching FINAL Name
NewFish <- fish_col %>%
  filter(FINAL_NAME=="" & 
           NAME_COM!="" & 
           !STATE%in%c("Selawik","GU"))%>%
  select(NAME_COM_UPR) %>%
  distinct()
###########################################################
grep("UNKNOWN|SP.|UNIDENTIFIED", NewFish$NAME_COM_UPR, value = T)
grep("UNKNOWN", nars_taxa_list$FINAL_NAME, value = T)

# Update records for unknown taxa (after visual inspection) 
#######
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN 3","?"), 
               FINAL_NAME = "UNKNOWN")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNKNOWN SCULPIN", 
               FINAL_NAME = "UNKNOWN COTTUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN CYPRINDS", 
                              "UNKNOWN CYPRINID"), 
               FINAL_NAME = "UNKNOWN MINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNIDENTIFIED SUCKER"), 
               FINAL_NAME = "UNKNOWN SUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN REDHORSE",
                              "UNIDENTIFIED MOXOSTOMA", 
                              "MOXOSTOMA SP."), 
               FINAL_NAME = "UNKNOWN MOXOSTOMA")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("SHINER A",
                              "SHINER B"), 
               FINAL_NAME = "UNKNOWN SHINER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("CYPRINELLA A"), 
               FINAL_NAME = "UNKNOWN CYPRINELLA")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ICTIOBUS SP.", 
               FINAL_NAME = "UNKNOWN ICTIOBUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ICHTHYOMYZON SP.", 
               FINAL_NAME = "UNKNOWN ICHTHYOMYZON")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CARPIODES SP.", 
               FINAL_NAME = "UNKNOWN CARPIODES")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "HYBOGNATHUS SP.", 
               FINAL_NAME = "UNKNOWN HYBOGNATHUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MADTOM", 
               FINAL_NAME = "UNKNOWN NOTURUS") 
#############################################

# Check for misspellings/typos:  
# Conduct fuzzy search between NAME_COM and FINAL NAME 
# identify field IDs with misspellings or extra spaces

# Inspect fuzzy results update accordingly. The list names is the NAME_COM
# The elements are the 15 closest matching records in FINAL_NAME Dataset
# Any name that was not obvious was left unchanged 
#######
#update taxa with missing taxa names
NewFish <- fish_col %>%
  filter(FINAL_NAME=="" & 
           NAME_COM!="" & 
           !STATE%in%c("Selawik","GU"))%>%
  select(NAME_COM_UPR) %>%
  distinct()

#NRSA taxa list as vector
NARS_FINAL_NAME <- nars_taxa_list$FINAL_NAME

Fuzzy_result <- sapply(NewFish$NAME_COM_UPR, 
                       function(x){stringdist(x, 
                                              NARS_FINAL_NAME, 
                                              method = 'cosine', 
                                              q = 1)})
rownames(Fuzzy_result) <- NARS_FINAL_NAME
#selects the 15 NRSA taxa with closest match
Fuzzy_result <- apply(Fuzzy_result, 2, 
                      function(x) list(names(x)[order(x)[1:15]]))
#############################################
Fuzzy_result

# Updates records after fuzzy matching
######
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LOG PERCH", 
               FINAL_NAME = "LOGPERCH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "WESTERN MOSQUITO FISH", 
               FINAL_NAME = "WESTERN MOSQUITOFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNTNOSE MINNOW.", 
               FINAL_NAME = "BLUNTNOSE MINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ORANGE-SPOTTED SUNFISH", 
               FINAL_NAME = "ORANGESPOTTED SUNFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LAHONTAN REDSIDE SHINER", 
               FINAL_NAME = "LAHONTAN REDSIDE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LONG NOSE DACE", 
               FINAL_NAME = "LONGNOSE DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "YELLOWSTONE CUTTHROAT", 
               FINAL_NAME = "YELLOWSTONE CUTTHROAT TROUT")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "TIGER MUSKY", 
               FINAL_NAME = "TIGER MUSKELLUNGE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "RIVER CARP SUCKER", 
               FINAL_NAME = "RIVER CARPSUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COMMON  CARP", 
               FINAL_NAME = "COMMON CARP")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHORT NOSE GAR", 
               FINAL_NAME = "SHORTNOSE GAR")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SNUBNOSE  DARTER", 
               FINAL_NAME = "SNUBNOSE DARTER")
##############################################

# Checking names against 1819 survey 
# For each ambiguous taxa against taxa that were collected from the same 
# during the 2018-2019 cycle. 

# Inspect results.  Each list element contains the most similar FINAL_Names 
# for each NAME_COM and state combination
#####
# identified unmatched taxa, with state 
NewFish <- fish_col %>%
  filter(FINAL_NAME=="" & 
           NAME_COM!="" & 
           !STATE%in%c("Selawik", "GU"))%>%
  select(NAME_COM_UPR, STATE) %>%
  distinct()

# iterate through each species and state combination
# and used fuzzy matching to identify FINAL_NAMR that is most similar
# Each list element is a species/state combination
Fuzzy_list <- list()
for(nf in 1:nrow(NewFish)){
  
  unknsp <- NewFish[nf,]
  lname <- paste0(unknsp, collapse = "_")
  
  #Extract all records from the state where unknown was recorded
  #used a comparision vector
  NARS_FINAL_NAME_1819 <- nars_taxa_col_1819 %>%
    filter(STATE %in% unknsp$STATE)%>%
    select(c(FINAL_NAME, STATE))%>%
    distinct()
  
  #head(grep("PEARL DACE", NARS_FINAL_NAME_1819[,1],))
  
  Fuzzy_result <- sapply(unknsp$NAME_COM_UPR, 
                         function(x){stringdist(x, NARS_FINAL_NAME_1819$FINAL_NAME, 
                                                method = 'cosine', q = 1)})
  
  rownames(Fuzzy_result) <- NARS_FINAL_NAME_1819$FINAL_NAME
  
  Fuzzy_list[[lname]]<-rownames(Fuzzy_result)[order(Fuzzy_result[,1])[1:15]]
}
##############################################
Fuzzy_list

# update results based on taxa collected in the state
#####
# Only Northern was collected from WI
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "WI",
               FINAL_NAME = "NORTHERN PEARL DACE")

# checked via range maps, Pearl Dace was not collected previously in SD, 
# but range maps from NatureServe suggest that its Northern
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "ND",
               FINAL_NAME = "NORTHERN PEARL DACE")

# only minnow from illinois
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNTNOSE",
               STATE = "IL",
               FINAL_NAME = "BLUNTNOSE MINNOW")

# only Allegeny collected previously
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "VT",
               FINAL_NAME = "ALLEGHENY PEARL DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CRAPPIE",
               STATE = "MT",
               FINAL_NAME = "BLACK CRAPPIE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PIKEMINNOW",
               STATE = "ID",
               FINAL_NAME = "NORTHERN PIKEMINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNT NOSE WITH HEAD INJURY",
               STATE = "ND",
               FINAL_NAME = "BLUNTNOSE MINNOW")

# Ozark Darter is not listed in AFS. It is part of the Orangethroat Darter
# Etheostoma spectabile complex
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "OZARK DARTER",
               STATE = "AR",
               FINAL_NAME = "ORANGETHROAT DARTER")



# CA species --- kalamath and coastal rainbows... 
# most of these are subspecies that were not recognized by AFS. I could add as
# new taxa or call below. This the first time these taxa were collected

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CENTRAL CALIFORNIA COAST STEELHEAD",
               STATE = "CA",
               FINAL_NAME = "RAINBOW TROUT (STEELHEAD)")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "KLAMATH MOUNTAIN PROVINCE (STEELHEAD)",
               STATE = "CA",
               FINAL_NAME = "RAINBOW TROUT (STEELHEAD)")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "KLAMATH SPECKLED DACE",
               STATE = "CA",
               FINAL_NAME = "SPECKLED DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LOWER KLAMATH MARBLED SCULPIN",
               STATE = "CA",
               FINAL_NAME = "MARBLED SCULPIN")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COASTAL RAINBOW TROUT",
               STATE = "CA",
               FINAL_NAME = "RAINBOW TROUT")
########################################################

# Check remaining names against AFS accepted common Names 
# see (Names-of-Fishes-8-Table1.pdf). if the NAME_COM is accepted taxa should
# be a new record in NRSA List.
#####
NewFish <- fish_col %>%
  filter(FINAL_NAME=="" & 
           NAME_COM!="" & 
           !STATE%in%c("Selawik", "GU"))%>%
  select(NAME_COM_UPR, STATE) %>%
  distinct()
#######################################
NewFish

# Update Names 
##########
# Searched Common name in AFS 2023 
# PINFISH    LA -- Lagodon rhomboides 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PINFISH",
               STATE = "LA",
               FINAL_NAME = "Lagodon rhomboides -- NR")

# SHEEPSHEAD (MARINE)    LA -- Archosargus probatocephalus
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHEEPSHEAD (MARINE)",
               STATE = "LA",
               FINAL_NAME = "Archosargus probatocephalus -- NR")

# STRIPED MOJARRA -- Eugerres plumieri
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "STRIPED MOJARRA",
               STATE = "FL",
               FINAL_NAME = "Eugerres plumieri -- NR")
# BAYOU TOPMINNOW    MS -- Fundulus nottii
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BAYOU TOPMINNOW",
               STATE = "MS",
               FINAL_NAME = "Fundulus nottii -- NR")

# BUTTERFLY PEACOCK BASS -- Cichla ocellaris
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BUTTERFLY PEACOCK BASS",
               STATE = "FL",
               FINAL_NAME = "Cichla ocellaris -- NR")

# HEADWATER DARTER    KY -- Etheostoma lawrencei
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "HEADWATER DARTER",
               STATE = "KY",
               FINAL_NAME = "Etheostoma lawrencei -- NR")
# ORANGEFIN MADTOM    VA -- Noturus gilberti 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ORANGEFIN MADTOM",
               STATE = "VA",
               FINAL_NAME = "Noturus gilberti -- NR")

# UNKNOWN CATFISH    FL -- UNKNOWN ICTALURIDAE 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNKNOWN CATFISH",
               STATE = "FL",
               FINAL_NAME = "UNKNOWN ICTALURIDAE -- NR")

# CHUB A    VA -- UNKNOWN SQUALIUS
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CHUB A",
               STATE = "VA",
               FINAL_NAME = "UNKNOWN SQUALIUS -- NR")

# "UNKNOWN SPOT FIN SMALL" is this spotfin shiner? --- 
# SHEEPSHEAD    LA -- Assuming that this is the minnow bc above

#Hybrids
#COMMON SHINER X HORNYHEAD CHUB    MI
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COMMON SHINER X HORNYHEAD CHUB",
               STATE = "MI",
               FINAL_NAME = "COMMON SHINER X HORNYHEAD CHUB -- NR")

#REDEAR SUNFISH X REDBREAST SUNFISH    VA
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDEAR SUNFISH X REDBREAST SUNFISH",
               STATE = "VA",
               FINAL_NAME = "REDBREAST SUNFISH X REDEAR SUNFISH -- NR")

#REDEYE BASS X ALABAMA BASS    SC
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDEYE BASS X ALABAMA BASS",
               STATE = "SC",
               FINAL_NAME = "ALABAMA BASS X REDEYE BASS -- NR")
################################################


# append TAXA_ID to fish collection file
#########
fish_col <- merge(fish_col, 
      unique(nars_taxa_list[,c("TAXA_ID","FINAL_NAME")]),
      by = "FINAL_NAME", 
      all.x = T)

# Shows new records that need to be added to NRSA taxa list were not collected 
# from GU
unique(fish_col[is.na(fish_col$TAXA_ID) & 
                  fish_col$NAME_COM_UPR!="" & 
                  fish_col$STATE!="GU",])

write.csv(fish_col, "Reconciled_Taxa_Names.csv")
View(read.csv("Reconciled_Taxa_Names.csv"))
fish_col<-read.csv("Reconciled_Taxa_Names.csv")

# We need to nkow to get back to Brian and Chris to look at 23 data, Maybe we need updates with NAME_COM 
# Need Names ASAP

grep("COUNT", names(Reconciled_Taxa), value = T)
c("COUNT_18","COUNT_19","COUNT_6","COUNT_12")
length(unique(Reconciled_Taxa[Reconciled_Taxa$NAME_COM=="","UID"]))
#Check FINAL_NAME assignments
#Cleaned and sent to RM (8/28) for assistance with ambiguous taxa
Reconciled_Taxa <- fish_col%>%
  filter(!STATE%in%c("Selawik", "GU")&
           NAME_COM_UPR!=FINAL_NAME)%>%
  select(c("SITE_ID", "DATE_COL", "NAME_COM", "FINAL_NAME"))



write.csv(Reconciled_Taxa, 
          "Reconciled_Taxa_NRSA_23.csv", 
          row.names = F)




