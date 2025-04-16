rm(list=ls())
library(tidyverse)
library(sf)
library(tmap)
library(stringdist)


# Files from allTheNRSA
######
allTheNRSA <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/allTheNRSA/data/tabfiles/"
allTheNRSA_Fishcts <- paste0(allTheNRSA, "NRSA0809_fishCts_alltheNRSA.tab") %>%
  read.table(sep = "\t", header = T)

SI<-paste0(allTheNRSA, "NRSA0809-1819_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)

#######################

# Site info file -- locations for 2023-24
######
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

# note /data/ are the outputs from IM
site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)

allTheNARS_Sites <- rbind(SI[,c("SITE_ID","PSTL_CODE","LAT_DD83","LON_DD83", "DATE_COL", "COMID")],
      site.info[,c("SITE_ID", "PSTL_CODE","LAT_DD83","LON_DD83", "DATE_COL", "COMID")])

#this should be the most recent taxa list
nars_taxa_list <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)

#######################

#fish collection file
######
fish_col <- dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_fishcollectionWide_fish.tab") %>%
  read.table(sep = "\t", header = T) %>% 
  rowwise()%>%
  mutate(HAS_COUNT = ifelse(sum(c(COUNT_6, COUNT_12, COUNT_18, COUNT_19), 
                                na.rm = T) > 0, "Y", "N"))

# # add name of fish taxonomist
# FishTaxonomist <- dir_NRSA_2324%>%
#   paste0("/raw/tabfiles/nrsa2324_crew.tab") %>%
#   read.table(sep = "\t", header = T) %>%
#   filter(PARAMETER == "FISH_TAXONOMIST") %>%
#   mutate(FISH_TAXONOMIST = RESULT) %>%
#   select(UID,FISH_TAXONOMIST)

FishTaxonomist <- dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_verificationWide.tab") %>%
  read.table(sep = "\t", header = T) %>%
  select(UID, CREW)

fish_col <- merge(fish_col, FishTaxonomist, by = "UID", all.x = T)

FISH_COMMENTS <- dir_NRSA_2324 %>%
  paste0("/raw/tabfiles/nrsa2324_comments.tab") %>%
  read.table(sep = "\t", header = T) %>%
  filter(grepl("FISH_COMMENT", FLAG))


fish_col <- merge(fish_col, 
                  FISH_COMMENTS[,c("SITE_ID","VISIT_NO","FLAG","COMMENT")], 
                  by = c("SITE_ID","VISIT_NO","FLAG"), 
                  all.x = T)
#######################

# Function to update Fish Collection
#######
# Parameters:
# FIELD_Name = NAME_COM (name given in field)
# NAME_COM_CORRECTED = the reconciled name (typically 1819 NAME_COM_CORRECTED)
# STATE = State that the record should be updated. Default updates all records 
#     but beware, in some instances (uncommon) a species could be named differently 
#     depending on were it was collected (i.e. PEARL DACE, Northern, or Allegheny?) 

updateRecord <- function(df, FIELD_Name, NAME_COM_CORRECTED, STATE = "ALL"){
  if(!any(names(df)=="PSTL_CODE")){
    stop("must have PSTL_CODE as column heading")
  }
  if(STATE=="ALL"){
    df[df$NAME_COM_UPR %in% FIELD_Name, "NAME_COM_CORRECTED"] <- NAME_COM_CORRECTED    
  } else{
    df[df$NAME_COM_UPR %in% FIELD_Name & df$PSTL_CODE==STATE, "NAME_COM_CORRECTED"] <- NAME_COM_CORRECTED 
  }
  return(df)
}
######################################

################################################################################
# reconcile names
################################################################################

# save original file as check to ensure all records are accounted for 
fish_col_original <- fish_col

dim(fish_col_original)
# Update direct matches. Records that have a match in NRSA taxa list 
#######
# Add NAME_COM_UPR, upper case of NAME_COM for matching with final name
fish_col <- fish_col%>%
  mutate(NAME_COM_UPR = toupper(NAME_COM), 
         NAME_COM_CORRECTED = "")
#fish_col[,"NAME_COM_CORRECTED"] <- fish_col$FINAL_NAME

# Updates NAME_COM_CORRECTED in fish collection file with matching NRSA 1819 NAME_COM_CORRECTED 
ind <- fish_col$NAME_COM_UPR %in% nars_taxa_list$FINAL_NAME
fish_col$NAME_COM_CORRECTED[ind] <- fish_col$NAME_COM_UPR[ind]
##########################################################
dim(fish_col); names(fish_col)

# add NO FISH when no count--- In some instances when names are updated by field 
# taxonomists, the count is changed to zero. Updating these records to NO FISH.  
# if other fish were collected from a site then this value will be dropped. This 
# is accomplished in the count validation below
fish_col[fish_col$HAS_COUNT=="N", "NAME_COM_CORRECTED"] <- "NO FISH"

# Check unknown taxa: 
# Used grep function to compare the taxa with 
# "unknown" entry in NAME_COM 
#######
# Sites with NAME_COM but no matching NAME_COM_CORRECTED
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED == "" & NAME_COM != "") %>%
  select(NAME_COM_UPR, COMMENT) %>%
  distinct()
###########################################################


# Update records for unknown taxa after visual inspection of 
# Unknown taxa in NRSA table) 
sort(grep("UNKNOWN|SP.$|UNIDENTIFIED|UNKWN|SPP.|UNID.|UNK", 
          NewFish$NAME_COM_UPR, value = T))
sort(grep("UNKNOWN", nars_taxa_list$FINAL_NAME, value = T))

#######
fish_col[fish_col$NAME_COM_UPR == "" & 
           fish_col$HAS_COUNT == "Y", 
         "NAME_COM_CORRECTED"] <- "UNKNOWN"

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CARPIODES SP.", 
               NAME_COM_CORRECTED = "UNKNOWN CARPIODES")


fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ICHTHYOMYZON SPP.", 
               NAME_COM_CORRECTED = "UNKNOWN ICHTHYOMYZON")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LAMPETRA SPP.", 
               NAME_COM_CORRECTED = "UNKNOWN LAMPETRA")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNID. SUNFISH", 
               NAME_COM_CORRECTED = "UNKNOWN LEPOMIS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNK LAMPREY","LAMPREY"), 
               NAME_COM_CORRECTED = "UNKNOWN LAMPREY")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNK AMMOCOETE"), 
               NAME_COM_CORRECTED = "LAMPREY AMMOCOETE")


# Unknown should be recorded to family 
# should be UNKNOWN CYPRINIDAE ???
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN CYPRINID",
                              "UNKWN MINNOW"), 
               NAME_COM_CORRECTED = "UNKNOWN MINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNKNOWN HYBOGNATHUS SP.", 
               NAME_COM_CORRECTED = "UNKNOWN HYBOGNATHUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN, POSSIBLE FHM, BOTTLE B"), 
               NAME_COM_CORRECTED = "UNKNOWN")

# extra unknowns
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("SCULPIN"), 
               NAME_COM_CORRECTED = "UNKNOWN COTTUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNID. DARTER"), 
               NAME_COM_CORRECTED = "UNKNOWN ETHEOSTOMA")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("SUCKER"), 
               NAME_COM_CORRECTED = "UNKNOWN CATOSTOMUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("MORONE", "MORONE SP"), 
               NAME_COM_CORRECTED = "UNKNOWN MORONE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("CARP"), 
               NAME_COM_CORRECTED = "UNKNOWN CARPIODES")


fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CARPIODES SP.", 
               NAME_COM_CORRECTED = "UNKNOWN CARPIODES")

#fish_col <- fish_col %>%
# updateRecord(df = ., 
#             FIELD_Name = "HYBOGNATHUS SP.", 
#            NAME_COM_CORRECTED = "UNKNOWN HYBOGNATHUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ICHTHYOMYZON SP.", 
               NAME_COM_CORRECTED = "UNKNOWN ICHTHYOMYZON")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ICTIOBUS SP.", 
               NAME_COM_CORRECTED = "UNKNOWN ICTIOBUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN REDHORSE",
                              "UNIDENTIFIED MOXOSTOMA", 
                              "MOXOSTOMA SP."), 
               NAME_COM_CORRECTED = "UNKNOWN MOXOSTOMA")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNIDENTIFIED SUCKER"), 
               NAME_COM_CORRECTED = "UNKNOWN SUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN 3"), 
               NAME_COM_CORRECTED = "UNKNOWN")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNKNOWN SCULPIN", 
               NAME_COM_CORRECTED = "UNKNOWN COTTUS")

#############################################


# Check for misspellings/typos:  
# Conduct fuzzy search between NAME_COM and FINAL NAME 
# identify field IDs with misspellings or extra spaces

# Inspect fuzzy results update accordingly. The list names is the NAME_COM
# The elements are the 15 closest matching records in NAME_COM_CORRECTED Dataset
# Any name that was not obvious was left unchanged 

#######
#update taxa with missing taxa names
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED == "" & 
           NAME_COM != "") %>%
  select(NAME_COM_UPR, COMMENT) %>%
  distinct()

#NRSA taxa list as vector
NARS_NAME_COM_CORRECTED <- nars_taxa_list$FINAL_NAME

Fuzzy_result <- sapply(NewFish$NAME_COM_UPR, 
                       function(x){stringdist(x, 
                                              NARS_NAME_COM_CORRECTED, 
                                              method = 'cosine', 
                                              q = 1)})
rownames(Fuzzy_result) <- NARS_NAME_COM_CORRECTED
#selects the 15 NRSA taxa with closest match
Fuzzy_result <- apply(Fuzzy_result, 2, 
                      function(x) list(names(x)[order(x)[1:15]]))
#############################################

# Visually inspect NAME_COM to closest matching NAME_COM_CORRECTED in NRSA
# autecology dataset. 
Fuzzy_result

#Updates records after fuzzy matching
######
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CHATTAHOOCHIE BASS", 
               NAME_COM_CORRECTED = "CHATTAHOOCHEE BASS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "WESTERN MOSQUITO FISH", 
               NAME_COM_CORRECTED = "WESTERN MOSQUITOFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LARGE MOUTH BASS", 
               NAME_COM_CORRECTED = "LARGEMOUTH BASS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "EASTERN MOSQUITOFUSH", 
               NAME_COM_CORRECTED = "EASTERN MOSQUITOFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "AFRICAN JEWEL FISH", 
               NAME_COM_CORRECTED = "AFRICAN JEWELFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNTFACE SHINER SP", 
               NAME_COM_CORRECTED = "BLUNTFACE SHINER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "WESTSLOPE CUTTHROAT", 
               NAME_COM_CORRECTED = "WESTSLOPE CUTTHROAT TROUT")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COLUMBIA SPINY SCULPIN", 
               NAME_COM_CORRECTED = "COLUMBIA SCULPIN")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "TROUT PERCH", 
               NAME_COM_CORRECTED = "TROUT-PERCH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "QUILLBACK CARPSUCKER", 
               NAME_COM_CORRECTED = "QUILLBACK")


fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SPOTFIN SHINERS", 
               NAME_COM_CORRECTED = "SPOTFIN SHINER")


fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LOG PERCH", 
               NAME_COM_CORRECTED = "LOGPERCH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "TAILED FROG LARVAE", 
               NAME_COM_CORRECTED = "TAILED FROG")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "TAILED FROG TADPOLE", 
               NAME_COM_CORRECTED = "TAILED FROG (TADPOLE)")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PLAINS MINNOW OR             WESTERN SILVERY MINNOW", 
               NAME_COM_CORRECTED = "PLAINS MINNOW OR WESTERN SILVERY MINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SMALLMOUTHX BASS", 
               NAME_COM_CORRECTED = "SMALLMOUTH BASS")
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SMALLMOUTHX BASS", 
               NAME_COM_CORRECTED = "SMALLMOUTH BASS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SNUBNOSE  DARTER", 
               NAME_COM_CORRECTED = "SNUBNOSE DARTER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LAHONTAN REDSIDE SHINER", 
               NAME_COM_CORRECTED = "LAHONTAN REDSIDE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ORANGE-SPOTTED SUNFISH", 
               NAME_COM_CORRECTED = "ORANGESPOTTED SUNFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COMMON  CARP", 
               NAME_COM_CORRECTED = "COMMON CARP")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "RIVER CARP SUCKER", 
               NAME_COM_CORRECTED = "RIVER CARPSUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "TIGER MUSKY", 
               NAME_COM_CORRECTED = "TIGER MUSKELLUNGE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "YELLOWSTONE CUTTHROAT", 
               NAME_COM_CORRECTED = "YELLOWSTONE CUTTHROAT TROUT")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LONG NOSE DACE", 
               NAME_COM_CORRECTED = "LONGNOSE DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LOG PERCH", 
               NAME_COM_CORRECTED = "LOGPERCH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "WESTERN MOSQUITO FISH", 
               NAME_COM_CORRECTED = "WESTERN MOSQUITOFISH")


fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNTNOSE MINNOW.", 
               NAME_COM_CORRECTED = "BLUNTNOSE MINNOW")


fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHORT NOSE GAR", 
               NAME_COM_CORRECTED = "SHORTNOSE GAR")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PUMPKINSEED SUNFISH", 
               NAME_COM_CORRECTED = "PUMPKINSEED")

##############################################

# Checking names against previous survey 
# For each ambiguous taxa against taxa that were 
# collected from previous surveys. 
#####
names(fish_col)
# identified unmatched taxa, with state 
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED == "" & NAME_COM != "")%>%
  select(NAME_COM_UPR, PSTL_CODE, COMMENT) %>%
  distinct()

# iterate through each species and state combination
# and use fuzzy matching to identify FINAL_NAMR that is most similar
# Each list element is a species/state combination
Fuzzy_list <- list()
for(nf in 1:nrow(NewFish)){
  #nf<-1
  unknsp <- NewFish[nf,]
  lname <- paste0(unknsp, collapse = "_")
  
  #Extract all records from the state where unknown was recorded
  #used a comparison vector
  NARS_NAME_COM_CORRECTED <- allTheNRSA_Fishcts %>%
    filter(PSTL_CODE %in% unknsp$PSTL_CODE)%>%
    select(c(FINAL_NAME, PSTL_CODE))%>%
    distinct()
  
  # previous records from GU are not in previous collections
  if(nrow(NARS_NAME_COM_CORRECTED)>0){
    #head(grep("PEARL DACE", NARS_NAME_COM_CORRECTED_1819[,1],))
    
    Fuzzy_result <- sapply(unknsp$NAME_COM_UPR, 
                           function(x){stringdist(x, NARS_NAME_COM_CORRECTED$FINAL_NAME, 
                                                  method = 'cosine', q = 1)})
    
    rownames(Fuzzy_result) <- NARS_NAME_COM_CORRECTED$FINAL_NAME
    
    Fuzzy_list[[lname]] <- rownames(Fuzzy_result)[order(Fuzzy_result[,1])[1:15]]  
  }
}
##############################################

# Inspect results.  Each list element is named with the NAME_COM and STATE 
# abbreviation where it was collected. The values in each list element are 
# the most similar NAME_COM_CORRECTEDs for each NAME_COM collected within the state 
Fuzzy_list

# update results based on taxa collected in the state during previous surveys
#####
# In MN it should be Northern. Allegheny should be reserved for populations in 
# allegheny basin
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "MN",
               NAME_COM_CORRECTED = "NORTHERN PEARL DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PIKEMINNOW",
               STATE = "MT",
               NAME_COM_CORRECTED = "NORTHERN PIKEMINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "FATHEAD CHUB",
               STATE = "NM",
               NAME_COM_CORRECTED = "FLATHEAD CHUB")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "VT",
               NAME_COM_CORRECTED = "ALLEGHENY PEARL DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MOSQUITO FISH",
               STATE = "FL",
               NAME_COM_CORRECTED = "EASTERN MOSQUITOFISH")


# update results based on taxa collected in the state
# Only Northern was collected from WI
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "WI",
               NAME_COM_CORRECTED = "NORTHERN PEARL DACE")

# checked via range maps, Pearl Dace was not collected previously in SD, 
# but range maps from NatureServe suggest that its Northern
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "ND",
               NAME_COM_CORRECTED = "NORTHERN PEARL DACE")

# only minnow from IL
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNTNOSE",
               STATE = "IL",
               NAME_COM_CORRECTED = "BLUNTNOSE MINNOW")

# only Allegeny collected previously
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PEARL DACE",
               STATE = "VT",
               NAME_COM_CORRECTED = "ALLEGHENY PEARL DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CRAPPIE",
               STATE = "MT",
               NAME_COM_CORRECTED = "BLACK CRAPPIE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PIKEMINNOW",
               STATE = "ID",
               NAME_COM_CORRECTED = "NORTHERN PIKEMINNOW")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BLUNT NOSE WITH HEAD INJURY",
               STATE = "ND",
               NAME_COM_CORRECTED = "BLUNTNOSE MINNOW")

# CA species --- kalamath and coastal rainbows... 
# most of these are subspecies that were not recognized by AFS. I could add as
# new taxa or call below. This the first time these taxa were collected

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CENTRAL CALIFORNIA COAST STEELHEAD",
               STATE = "CA",
               NAME_COM_CORRECTED = "RAINBOW TROUT (STEELHEAD)")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "KLAMATH MOUNTAIN PROVINCE (STEELHEAD)",
               STATE = "CA",
               NAME_COM_CORRECTED = "RAINBOW TROUT (STEELHEAD)")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "KLAMATH SPECKLED DACE",
               STATE = "CA",
               NAME_COM_CORRECTED = "SPECKLED DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LOWER KLAMATH MARBLED SCULPIN",
               STATE = "CA",
               NAME_COM_CORRECTED = "MARBLED SCULPIN")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COASTAL RAINBOW TROUT",
               STATE = "CA",
               NAME_COM_CORRECTED = "RAINBOW TROUT")
#######################################


# Check remaining names against AFS accepted common Names 
# see (Names-of-Fishes-8-Table1.pdf). if the NAME_COM is accepted taxa should
# be a new record in NRSA List.
#####
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED=="" & NAME_COM!="") %>%
  select(NAME_COM_UPR, PSTL_CODE) %>%
  distinct()
#######################################
NewFish

# Update the names in the fish collection file 
##########
# Guam taxa -- note invertebrates submitted with fish are assigned "NO FISH"
# and eventually removed by LINE_CORRECTED
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ANGUILLA MARMORATA",
               NAME_COM_CORRECTED = "GIANT MOTTLED EEL")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "RHINELLA MARINUS",
               NAME_COM_CORRECTED = "CANE TOAD")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "OREOCHROMIS MOSSAMBICUS",
               NAME_COM_CORRECTED = "MOZAMBIQUE TILAPIA") 

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CLARIAS BATRACHUS",
               NAME_COM_CORRECTED = "WALKING CATFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "AWAOUS GUAMENSIS",
               NAME_COM_CORRECTED = "PACIFIC RIVER GOBY")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDIGOBIUS BIKOLANUS",
               NAME_COM_CORRECTED = "SPECKLED GOBY")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SICYOPTERUS LAGOCEPHALUS",
               NAME_COM_CORRECTED = "RED-TAILED GOBY")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "STIPHODON",
               NAME_COM_CORRECTED = "UNKNOWN STIPHODON")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SICYOPUS",
               NAME_COM_CORRECTED = "UNKNOWN SICYOPUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("KUHLIA RUPETRIS",
                              "KUHLIA RUPESTRIS",
                              "KULIA RUPESTRIS",
                              "KHULIA RUPESTRIS"),
               NAME_COM_CORRECTED = "ROCK FLAGTAIL")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LUTJANUS ARGENTIMACULATUS",
               NAME_COM_CORRECTED = "MANGROVE RED SNAPPER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("PEOCILIA RETICULATA", 
                              "POECILIA RETICULATA"),
               NAME_COM_CORRECTED = "GUPPY")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LIVEBEARERS",
               NAME_COM_CORRECTED = "LIVEBEARERS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MICROPHIS LEIASPIS",
               NAME_COM_CORRECTED = "BARHEAD PIPEFISH")

# these taxa are invertebrates. They will be updated to No Fish 
# and corrected with line number below
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("MACROBRACHIUM LAR",
                              "NERITINA VARIEGATA",
                              "NERITINA PULLIGERA",
                              "NERITINA SQUAMAEPICTA",
                              "NERITINA PETITION",
                              "NERITINA PETITII",
                              "ATYID", "THIARID",
                              "NERITINA PETITTI",
                              "ATYID SHRIMP"),
               NAME_COM_CORRECTED = "NO FISH")


# Add remaining names for new CONUS fish

# It appears that the Clear lake roach California Roach -- Hesperoleucus SP. CF. symmetricus
# ClearLakeRoach -- Hesperoleucus CF. symmetricus.pdf
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CLEAR LAKE ROACH",
               STATE = "CA",
               NAME_COM_CORRECTED = "CLEAR LAKE ROACH")

# GREENBACK CUTTHROAT TROUT -- Recognized Subspecies Oncorhynchus clarki stomias
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "GREENBACK CUTTHROAT TROUT",
               STATE = "CO",
               NAME_COM_CORRECTED = "GREENBACK CUTTHROAT TROUT")

# Florida Bass inherits Scientific Name as Largemouth Bass
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "FLORIDA BASS",
               STATE = "GA",
               NAME_COM_CORRECTED = "FLORIDA BASS")

# COASTAL CHUB seems to be unrecognized subspecies Hybopsis CF. winchelli
# similar to Clear Chub
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COASTAL CHUB",
               NAME_COM_CORRECTED = "COASTAL CHUB")

# GRAYFIN REDHORSE is undescribed subspecies of blacktail redhorse Moxostoma sp. cf. poecilurum
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "GRAYFIN REDHORSE",
               NAME_COM_CORRECTED = "GRAYFIN REDHORSE")

#Etowah Darter GA -- Nothonotus etowahae
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ETOWAH DARTER",
               NAME_COM_CORRECTED = "ETOWAH DARTER")

# NEOSHO MADTOM -- Noturus placidus
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "NEOSHO MADTOM",
               STATE = "KS",
               NAME_COM_CORRECTED = "NEOSHO MADTOM")

# KENTUCKY ARROW DARTER    KY -- Etheostoma spilotum
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "KENTUCKY ARROW DARTER",
               STATE = "KY",
               NAME_COM_CORRECTED = "KENTUCKY ARROW DARTER")


#COMMON SHINER Luxilus cornutus X FALLFISH (Semotilus corporalis)
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COMMON SHINER X FALLFISH",
               STATE = "ME",
               NAME_COM_CORRECTED = "COMMON SHINER X FALLFISH")

# PLAINS SUCKER   MT -- Pantosteus jordani
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PLAINS SUCKER",
               STATE = "MT",
               NAME_COM_CORRECTED = "PLAINS SUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MUDSKIPPER",
               STATE = "NV",
               NAME_COM_CORRECTED = "UNKNOWN MUDSKIPPER")

# ALABAMA BASS X BARTRAM’S BASS -- BARTRAMS Bass is unrecognized subspecies of Micropterus sp. cf. coosae
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ALABAMA BASS X BARTRAM’S BASS",
               STATE = "SC",
               NAME_COM_CORRECTED = "ALABAMA BASS X BARTRAMS BASS")

# SLENDER MOJARRA  NV -- Eucinostomus jonesii
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SLENDER MOJARRA",
               STATE = "SC",
               NAME_COM_CORRECTED = "SLENDER MOJARRA")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "WARMOUTH X PUMPKINSEED",
               STATE = "SC",
               NAME_COM_CORRECTED = "PUMPKINSEED X WARMOUTH")

# GUMBO DARTER    TX -- Etheostoma thompsoni 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "GUMBO DARTER",
               STATE = "TX",
               NAME_COM_CORRECTED = "GUMBO DARTER")

# NOCOMIS    VA --
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNKNOWN NOCOMIS",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN NOCOMIS")

#PINK SALMON WI -- Oncorhynchus gorbuscha
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "PINK SALMON",
               STATE = "WI",
               NAME_COM_CORRECTED = "PINK SALMON")

# SHEEPSHEAD (MARINE)    LA -- Archosargus probatocephalus
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("SHEEPSHEAD (MARINE)","SHEEPSHEAD"),
               STATE = "LA",
               NAME_COM_CORRECTED = "SHEEPSHEAD (MARINE)")

#REDEYE BASS X ALABAMA BASS    SC
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDEYE BASS X ALABAMA BASS",
               STATE = "SC",
               NAME_COM_CORRECTED = "ALABAMA BASS X REDEYE BASS")

#REDEAR SUNFISH X REDBREAST SUNFISH    VA
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDEAR SUNFISH X REDBREAST SUNFISH",
               STATE = "VA",
               NAME_COM_CORRECTED = "REDBREAST SUNFISH X REDEAR SUNFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MADTOM",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN NOTURUS")

###################################

# all names should have a record in the autecology file 
if(!all(fish_col$NAME_COM_CORRECTED %in% nars_taxa_list$FINAL_NAME)){
  stop("Need to update autecology file")
} else {
  print("all taxa in autecology file")}

# Copy autecology file from O: drive and update names/autecology file manually
# this should be the file from the previous years survey... but because we added
# taxa during the 2023 QA evaluation, we can used the taxa list written to the
# 2324 directory
#########
# do not copy unless you are prepared to overwrite
# file.copy("O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324/data/tabfiles/nrsa2324_fish_taxa.tab",
#           "nrsa2324_fish_taxa_NewTaxa.tab")


# open copied file in excel and create .csv file. Add any new taxa with the 
# appropriate information. Confirm "NAME_COM" is among AFS accepted names 
# (Names-of-Fishes-8-Table1.pdf)), otherwise identify the most similar species 
# using "SP. CF." in the scientific name. 

# to add autecology for new taxa its best (easiest) to sort by family, genus 
# and species and copy traits for closely related taxa. 

# If there are no closely related taxa then google. If there is uncertainty in 
# an assignment then leave blank
#######################################

# check to confirm that all taxa on fish collection file have autecology information
#########
# read in new taxa list if update were made
#nars_taxa_list <- read.table("nrsa2324_fish_taxa_NewTaxa.tab", sep = "\t", header=T)


# confirm that all taxa in the autecology file have correct names 
if(all(fish_col$NAME_COM_CORRECTED %in% nars_taxa_list$FINAL_NAME)){
  print("All taxa in autecology file")
} else{
  print("You're missing taxa")
  fish_col[!fish_col$NAME_COM_CORRECTED %in% nars_taxa_list$FINAL_NAME, ]
}

# Check all rows are accounted for
if(nrow(fish_col_original) == nrow(fish_col)){
  print("No records lost")
}
###################################

fish_col$DATE_COL <- as.character(fish_col$DATE_COL)

# send unknown taxa to field taxonomists for comments, 
# Any updates received will be made directly in IM and exported to NRSA folde
##############
Check_Taxa <- fish_col %>%
  filter(NAME_COM_UPR != NAME_COM_CORRECTED |
           str_detect(NAME_COM_CORRECTED, "UNKNOWN"))%>%
  filter(NAME_COM_CORRECTED != "NO FISH") %>%
  #filter(grepl("2024", DATE_COL))%>%
  select(c("UID", "SITE_ID", "VISIT_NO", "DATE_COL", 
           "TAG","LINE", "NAME_COM", 
           "NAME_COM_CORRECTED","VOUCH_NUM", "VOUCH_PHOTO", 
           "VOUCH_UNK", "VOUCH_QA", "COUNT_6", "COUNT_12", 
           "COUNT_18", "COUNT_19", "HAS_COUNT", 
           "CREW", "FLAG", "TAXA_ID", 
           "COMMENT"))
# Vise list
View(Check_Taxa)

################################################################
#write.csv(Check_Taxa, "Check_Taxa_NRSA_Reconciled_Names_2324_unknowns.csv", row.names = F)


# check MORT_CT: DP indicated that this field can be unreliable. 
# When it exceeds the number of fish collected set to NA
############
LineTotal <- apply(fish_col[,c("COUNT_6", "COUNT_12", "COUNT_18", "COUNT_19")], 
                   1, sum, na.rm = T)
fish_col$MORT_CT_CORRECTED <- ifelse(fish_col$MORT_CT > LineTotal & 
                                       !is.na(fish_col$MORT_CT), 
                                         "DELETE", " ")
####################################

# check counts: Identify lines without counts and add to delete from dataset. 
# counts are zeroed if identification is updated or the taxa was not 
# a fish (i.e. Guam has invertebrate taxa in the sample)
##########
# sometimes no fish were collected at a single transect. NO FISH is reserved for 
# sites that did not collect any fish. LINE CORRECTED should remove any Lines 
# that recorded no fish (either zero counts and/or blank names) but collected 
# fish recorded on other lines

# Split data into site and visit number
siteSplit <- split(fish_col, list(fish_col$SITE_ID, fish_col$VISIT_NO))
# identify sites that have fish
siteSplit <- siteSplit[unlist(lapply(siteSplit, function(x) nrow(x) > 0))]

siteSplit <- lapply(siteSplit, function(x) {
  # sites and visits that have "NO FISH" in NAME_COM_CORRECTED but have
  # collected other fish at site
  if(any(x$NAME_COM_CORRECTED != "NO FISH") & 
     any(x$NAME_COM_CORRECTED == "NO FISH")){
    index <- rownames(x)[x$NAME_COM_CORRECTED == "NO FISH"]
    df1 <- data.frame(index, x[x$NAME_COM_CORRECTED == "NO FISH", 
                               c("UID", "SITE_ID", "VISIT_NO", "LINE")], 
                      LINE_CORRECTED = "Y")
    
    #appends any other fish collected at the site, during the same visit
    if(any(x$NAME_COM_CORRECTED != "NO FISH")){
      index <- rownames(x)[x$NAME_COM_CORRECTED != "NO FISH"]
      df1 <- rbind(df1,
                   data.frame(index, 
                              x[x$NAME_COM_CORRECTED != "NO FISH", 
                                c("UID", "SITE_ID", "VISIT_NO", "LINE")], 
                              LINE_CORRECTED = "N"))
    }
    
  } else {
    index <- rownames(x)
    df1 <- data.frame(index, 
                      x[, c("UID", "SITE_ID", "VISIT_NO", "LINE")], 
                      LINE_CORRECTED = "N")}
  return(df1)
})

LINE_CORRECT <- do.call(rbind, siteSplit)
rownames(LINE_CORRECT) <- LINE_CORRECT$index
fish_col$LINE_CORRECTED <- LINE_CORRECT[rownames(fish_col), "LINE_CORRECTED"]
fish_col[fish_col$LINE_CORRECTED == "N", "LINE_CORRECTED"] <- ""
fish_col[fish_col$LINE_CORRECTED == "Y", "LINE_CORRECTED"] <- "DELETE"
####################################


# names that needed to be corrected that were identified downstream in the analysis
#####
# found comment mentioning that this was Hybrid, BARTRAMS BASS
fish_col[fish_col$SITE_ID == "NRS23_SC_10074" & 
           fish_col$LINE == 20, "NAME_COM_CORRECTED"] <- "ALABAMA BASS X BARTRAMS BASS"

# removing (YOY) - causes issues with duplicated common names in taxa table
fish_col[fish_col$NAME_COM_CORRECTED=="LARGEMOUTH BASS (YOY)","NAME_COM_CORRECTED"]<-"LARGEMOUTH BASS"
fish_col[fish_col$NAME_COM_CORRECTED=="CHINOOK SALMON (YOY)","NAME_COM_CORRECTED"]<-"CHINOOK SALMON"

# 99 visits should have been deleted during the 2023
fish_col[fish_col$VISIT_NO=="99","LINE_CORRECTED"]<-"DELETE"
#######################################

all(nrow(fish_col) == nrow(fish_col_original))

# write corrected file
#write.table(fish_col, "nrsa2324_fishcollectionWide_fish_Corrected.tab", sep="\t")

# view the lines that will be removed from the dataset
view(fish_col[fish_col$LINE_CORRECTED == "DELETE",])
View(fish_col)