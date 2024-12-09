#QA for Fish Assemblages -- Name Reconciliation 

# 1) Reconcile names - Merge NAME_COM with NAME_COM_CORRECTED from NRSA Taxa List

# Common Names are provided by field crew. There could be simple 
# misspellings or typos that prevent direct matching with existing NRSA Taxa list 
# 
# First check "Unknown" or unidentified taxa against UNKNOWNS in NRSA NAME_COM_CORRECTED
# using grep

# Second check NAME_COM against most similar taxa listed in NAME_COM_CORRECTED using 
# Fuzzy Matching. Correct/Update obvious spelling errors 

# Third compare NAME_COM to taxa previously collected from the state 
# during 1819 NRSA. This was used to reconcile ambiguous naming in NAME_COM because
# the other possibly suitable NAME_COM_CORRECTED was not observed previously

# Fourth compare NAME_COM against AFS accepted names (see: Names-of-Fishes-8-Table1.pdf). 
# accepted names are to be added as new records in NRSA database and need autecology 
# information

library(tidyverse)
library(sf)
library(tmap)
library(stringdist)

################################################################################
# Data Files 
################################################################################

# Files from NRSA 1819: "allNRSA_fishTaxa.tab" contains autecology data and
# "nars_taxa_col_1819" contains records of taxa collected previously. 
######
dir_NRSA_1819 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa1819/data/"

nars_taxa_list <- paste0(dir_NRSA_1819, "allNRSA_fishTaxa.tab") %>%
  read.table(sep = "\t", header = T)

nars_taxa_col_1819 <-  paste0(dir_NRSA_1819, "nrsa1819_fishCount_newUid.tab") %>%
  read.table(sep = "\t", header = T) %>%
  select(STATE)%>%
  distinct()
#######################

# Site info file -- locations for 2023-24
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
  #filter(grepl("2023", DATE_COL)) %>% # filters 2024 which may have incomplete identification  
  mutate(SITE_ID_Mod = gsub("-", "_", SITE_ID)) %>% # NRS23-Selawik-09 has dashes?
  mutate(STATE = unlist(lapply(strsplit(SITE_ID_Mod,"_"), "[[", 2))) %>% 
  rowwise()%>%
  mutate(HAS_COUNT = ifelse(sum(c(COUNT_6, COUNT_12, COUNT_18, COUNT_19),na.rm = T)>0,
                                "Y","N"))

# Fish voucher specimens? 
dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_fishcollectionWide_vert.tab") %>%
  read.table(sep = "\t", header = T)%>%
  filter(!is.na(SAMPLE_ID))%>%
  dim()

#######################


# Function to update Fish Collection Table directly
# the inputs are:
# FIELD_Name = NAME_COM (name given in field)
# NAME_COM_CORRECTED = the reconciled name (typically 1819 NAME_COM_CORRECTED)
# STATE = State that the record should be updated. (In some instances 
# PEARL DACE was named differently (e.g. Northern, or Allegheny) depending on 
# where it was collected)

updateRecord <- function(df, FIELD_Name, NAME_COM_CORRECTED, STATE = "ALL"){
  if(STATE=="ALL"){
    df[df$NAME_COM_UPR %in% FIELD_Name, "NAME_COM_CORRECTED"] <- NAME_COM_CORRECTED    
  } else{
    df[df$NAME_COM_UPR %in% FIELD_Name & df$STATE==STATE, "NAME_COM_CORRECTED"] <- NAME_COM_CORRECTED 
  }
  return(df)
}


################################################################################
# reconcile names
################################################################################

# save original file as check to ensure all records are accounted for 
fish_col_original <- fish_col


# Compare names provided by field taxonomist to names provided by the lab
#############
# Site, Visit, Tag for QA file --- the objective here is to pull out the list
# with matching records. Biggest challenge was that crews sometimes did not 
# record the "TAG" of the specimen that they submitted. In most instances 
# these were resolved using the "LINE" but a few instances required detective
# work that compared the LAB QA with the SITE in the collection file.


# QA Lab file provided by RM. Photo vouchers are found in Field Crews directory  
LAB_QA <- read.csv("Fish_QA_MBI_2023_11_30.csv")


# Adding TAG to sites/specimens that were submitted for voucher

# there were 26 TAG.NUMBER in LAB QA file -- assume line number is appropriate
fish_col[fish_col$SITE_ID == "NRS23_GA_10041" & is.na(fish_col$TAG),"TAG"] <- 
  fish_col[fish_col$SITE_ID == "NRS23_GA_10041" & is.na(fish_col$TAG),"LINE"]

# there were 3 TAG.NUMBER in LAB QA file -- assume line number is appropriate
fish_col[fish_col$SITE_ID=="NRS23_ID_10192"&is.na(fish_col$TAG),"TAG"] <- 
  fish_col[fish_col$SITE_ID=="NRS23_ID_10192"&is.na(fish_col$TAG),"LINE"]

# there were 4 TAG.NUMBER in LAB QA file -- assume line number is appropriate
fish_col[fish_col$SITE_ID=="NRS23_MT_10069"&is.na(fish_col$TAG),"TAG"] <-
  fish_col[fish_col$SITE_ID=="NRS23_MT_10069" & is.na(fish_col$TAG),"LINE"]

# there were 5 TAG.NUMBER in LAB QA file -- assume line number is appropriate. 
# Site was also revisited, so filter to visit 1 which was included in LAB QA 
fish_col[fish_col$SITE_ID == "NRS23_MT_10013" & is.na(fish_col$TAG)& 
           fish_col$VISIT_NO==1, "TAG"] <- 
  fish_col[fish_col$SITE_ID=="NRS23_MT_10013" & is.na(fish_col$TAG) & 
             fish_col$VISIT_NO==1, "LINE"]

# -- assume line number is appropriate
fish_col[fish_col$SITE_ID == "NRS23_TX_10306" & is.na(fish_col$TAG)& 
           fish_col$VISIT_NO==1, "TAG"] <- 
  fish_col[fish_col$SITE_ID=="NRS23_TX_10306" & is.na(fish_col$TAG) & 
             fish_col$VISIT_NO==1, "LINE"]

# TAG == 0 in collection file -- assume they meant 44, as listed LAB QA file
fish_col[fish_col$SITE_ID == "NRS23_SD_10017" & 
           !is.na(fish_col$TAG) & 
           fish_col$TAG==0, "TAG"] <- 44

# TAG not provided by field crew, assuming its 10 from LAB QA file
fish_col[fish_col$SITE_ID == "NRS23_NY_HP002" & 
           is.na(fish_col$TAG), "TAG"] <- 10


# Merge LAB QA with collection file. HAS_COUNT indicates that field crews 
# recorded counts for the taxa they collected. is.na(fish_col$TAG) removed 
# duplicate values 
MergedData <- merge(fish_col[fish_col$HAS_COUNT == "Y" & 
                               !is.na(fish_col$TAG),
                             c("SITE_ID", "VISIT_NO","LINE", "TAG", 
                               "VOUCH_NUM", "VOUCH_PHOTO", "VOUCH_UNK", 
                               "VOUCH_QA", "NAME_COM")],
                    LAB_QA[,c("SITE.ID", "VISIT.NUMBER", "TAG.NUMBER", 
                     "COMMON.NAME", "COMMENTS")], 
           by.y =c("SITE.ID", "VISIT.NUMBER", "TAG.NUMBER"), 
           by.x =  c("SITE_ID", "VISIT_NO", "TAG"), 
           all.y = T)

differences <- MergedData[trimws(toupper(MergedData$COMMON.NAME)) != 
                            trimws(toupper(MergedData$NAME_COM)),]

# add photo directory. 
differences$PHOTO_Directory <- NA
for (i in differences[differences$VOUCH_PHOTO=="Y","SITE_ID"]){
  # i <- "NRS23_WA_10011"
  PhotoFiles <- grep(i, list.files("Field Crews", recursive = T), value = T)
  directory <- unique(unlist(lapply(strsplit(PhotoFiles,"/"),
                       function(x) paste(x[-length(x)], collapse = "/"))))
  
  if(length(directory)>1){
    directory <- paste(directory, collapse = " - OR - ")
  }
  
  if(length(directory)==1){
    differences[differences$SITE_ID == i & 
                  differences$VOUCH_PHOTO=="Y", "PHOTO_Directory"] <- directory
  }
}


##################################################

# Pause here: tempted to update all differences to name provided by taxonomist, 
# meeting with RM and LR for thoughts...
# Lou is going to look through the records and provide a new column to indicate
# whether QA taxonomist, Field taxonomist or further reconciliation is needed
write.csv(differences,"Fish_QA_MBI_2023_11_30_wField_ID_Differences.csv")

MergedData[toupper(MergedData$COMMON.NAME) == MergedData$NAME_COM,]

# in some instances, multiple taxa were identified by the lab taxonomist. 
# these need to be added as new records. Following DP use decimals to 
# insert LINE and keep the same TAG number. Allocate total count between 
# the species based on the percentage of voucher specimens. This needs to be 
# included below. 



# Update direct matches. Records that have a match in NRSA taxa list 
#######
# Add NAME_COM_UPR, upper case of NAME_COM for matching with final name
fish_col <- fish_col%>%
  mutate(NAME_COM_UPR = toupper(NAME_COM), 
         NAME_COM_CORRECTED = "")

# Updates NAME_COM_CORRECTED in fish collection file with matching NRSA 1819 NAME_COM_CORRECTED 
ind <- fish_col$NAME_COM_UPR %in% nars_taxa_list$FINAL_NAME
fish_col$NAME_COM_CORRECTED[ind] <- fish_col$NAME_COM_UPR[ind]
##########################################################
dim(fish_col)


# Check unknown taxa: 
# Used grep function to compare the taxa with 
# "unknown" entry in NAME_COM 
#######
# Sites from lower 48 with NAME_COM but no matching NAME_COM_CORRECTED
# from NRSA Autecology dataset -- all Selawik TOTAL = 0
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED == "" & 
           NAME_COM != "" & 
           !STATE%in%c("Selawik", "GU")&
           grepl("2023", DATE_COL))%>% #filters out 2024 data
  select(NAME_COM_UPR) %>%
  distinct()
###########################################################


# Update records for unknown taxa after visual inspection of 
# Unknown taxa in NRSA table) 
sort(grep("UNKNOWN|SP.$|UNIDENTIFIED", NewFish$NAME_COM_UPR, value = T))
sort(grep("UNKNOWN", nars_taxa_list$FINAL_NAME, value = T))

#######
fish_col[fish_col$NAME_COM_UPR == "" & fish_col$HAS_COUNT=="Y", "NAME_COM_CORRECTED"] <- "UNKNOWN"

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CARPIODES SP.", 
               NAME_COM_CORRECTED = "UNKNOWN CARPIODES")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "HYBOGNATHUS SP.", 
               NAME_COM_CORRECTED = "UNKNOWN HYBOGNATHUS")

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

# Unknown should be recorded to family 
# UNKNOWN CATOSTOMIDAE -- not change here. 
# Need to confirm with Karen B. 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNIDENTIFIED SUCKER"), 
               NAME_COM_CORRECTED = "UNKNOWN SUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN 3"), 
               NAME_COM_CORRECTED = "UNKNOWN")


# Unknown should be recorded to family 
# UNKNOWN CATOSTOMIDAE -- not change here
# should be UNKNOWN CYPRINIDAE ???
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = c("UNKNOWN CYPRINDS", 
                              "UNKNOWN CYPRINID"), 
               NAME_COM_CORRECTED = "UNKNOWN MINNOW")

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
  filter(NAME_COM_CORRECTED=="" & 
           NAME_COM!="" & 
           !STATE%in%c("Selawik","GU") &
           grepl("2023", DATE_COL)) %>%
  select(NAME_COM_UPR) %>%
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
               FIELD_Name = "ORANGE-SPOTTED SUNFISH", 
               NAME_COM_CORRECTED = "ORANGESPOTTED SUNFISH")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LAHONTAN REDSIDE SHINER", 
               NAME_COM_CORRECTED = "LAHONTAN REDSIDE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "LONG NOSE DACE", 
               NAME_COM_CORRECTED = "LONGNOSE DACE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "YELLOWSTONE CUTTHROAT", 
               NAME_COM_CORRECTED = "YELLOWSTONE CUTTHROAT TROUT")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "TIGER MUSKY", 
               NAME_COM_CORRECTED = "TIGER MUSKELLUNGE")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "RIVER CARP SUCKER", 
               NAME_COM_CORRECTED = "RIVER CARPSUCKER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COMMON  CARP", 
               NAME_COM_CORRECTED = "COMMON CARP")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHORT NOSE GAR", 
               NAME_COM_CORRECTED = "SHORTNOSE GAR")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SNUBNOSE  DARTER", 
               NAME_COM_CORRECTED = "SNUBNOSE DARTER")
##############################################

# Checking names against 1819 survey 
# For each ambiguous taxa against taxa that were collected from the same 
# during the 2018-2019 cycle. 
#####
# identified unmatched taxa, with state 
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED=="" & 
           NAME_COM!="" & 
           !STATE %in% c("Selawik", "GU")&
           grepl("2023", DATE_COL))%>%
  select(NAME_COM_UPR, STATE) %>%
  distinct()

# iterate through each species and state combination
# and used fuzzy matching to identify FINAL_NAMR that is most similar
# Each list element is a species/state combination
Fuzzy_list <- list()
for(nf in 1:nrow(NewFish)){
  #nf<-1
  unknsp <- NewFish[nf,]
  lname <- paste0(unknsp, collapse = "_")
  
  #Extract all records from the state where unknown was recorded
  #used a comparison vector
  NARS_NAME_COM_CORRECTED_1819 <- nars_taxa_col_1819 %>%
    filter(STATE %in% unknsp$STATE)%>%
    select(c(FINAL_NAME, STATE))%>%
    distinct()
  
  #head(grep("PEARL DACE", NARS_NAME_COM_CORRECTED_1819[,1],))
  
  Fuzzy_result <- sapply(unknsp$NAME_COM_UPR, 
                         function(x){stringdist(x, NARS_NAME_COM_CORRECTED_1819$FINAL_NAME, 
                                                method = 'cosine', q = 1)})
  
  rownames(Fuzzy_result) <- NARS_NAME_COM_CORRECTED_1819$FINAL_NAME
  
  Fuzzy_list[[lname]] <- rownames(Fuzzy_result)[order(Fuzzy_result[,1])[1:15]]
}
##############################################

# Inspect results.  Each list element is named with the NAME_COM and STATE 
# abbreviation where it was collected. The values in each list element are 
# the most similar NAME_COM_CORRECTEDs for each NAME_COM collected within the state 
Fuzzy_list

# update results based on taxa collected in the state
#####
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

# Ozark Darter is not listed in AFS. It is part of the Orangethroat Darter
# Etheostoma spectabile complex -- Formerly there were five named 
# subspecies: uniporum, fragi, pulchellum, squamosum, and spectabile, 
# the latter "with several races" (Page and Burr 1991). Ceas and Page (1997) 
# elevated uniporum and fragi (Strawberry Darter) to species status and split off four new species 
# (E. burri, E. tecumsehi, E. kantuckeense, and E. bison) from the southeastern 
# and southcentral parts of the range of E. spectabile. 
# Ceas (1997) recognized several undescribed species in this complex: 
# Ozark darter, headwater darter (now E. lawrencei), Sheltowee darter, 
# Cumberland darter, and Caney Fork darter.

# uniporum and fragi (Strawberry Darter) both can occur in AR and strawberry darter, 
# was collected during a previous sruvey, BUT from above "Ozark Darter" 
# is an unrecognized as part of E. burri (only Missouri), E. tecumsehi (Kentucky), 
# E. kantuckeense (Kentucky or TN), or E. bison (Kentucky and TN). Thus, Orangethroat 
# darter seems like the best choice. see https://www.jstor.org/stable/1447555?seq=18
# for more. 

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "OZARK DARTER",
               STATE = "AR",
               NAME_COM_CORRECTED = "ORANGETHROAT DARTER")

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
########################################################

# Check remaining names against AFS accepted common Names 
# see (Names-of-Fishes-8-Table1.pdf). if the NAME_COM is accepted taxa should
# be a new record in NRSA List.
#####
NewFish <- fish_col %>%
  filter(NAME_COM_CORRECTED=="" & 
           NAME_COM!="" & 
           !STATE%in%c("Selawik", "GU")&
           grepl("2023", DATE_COL))%>%
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
               NAME_COM_CORRECTED = "PINFISH -- NR")

# SHEEPSHEAD (MARINE)    LA -- Archosargus probatocephalus
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHEEPSHEAD (MARINE)",
               STATE = "LA",
               NAME_COM_CORRECTED = "SHEEPSHEAD (MARINE) -- NR")

# STRIPED MOJARRA -- Eugerres plumieri
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "STRIPED MOJARRA",
               STATE = "FL",
               NAME_COM_CORRECTED = "STRIPED MOJARRA -- NR")

# BAYOU TOPMINNOW    MS -- Fundulus nottii
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BAYOU TOPMINNOW",
               STATE = "MS",
               NAME_COM_CORRECTED = "BAYOU TOPMINNOW -- NR")

# BUTTERFLY PEACOCK BASS -- Cichla ocellaris
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "BUTTERFLY PEACOCK BASS",
               STATE = "FL",
               NAME_COM_CORRECTED = "BUTTERFLY PEACOCK BASS -- NR")

# HEADWATER DARTER    KY -- Etheostoma lawrencei
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "HEADWATER DARTER",
               STATE = "KY",
               NAME_COM_CORRECTED = "HEADWATER DARTER -- NR")

# ORANGEFIN MADTOM    VA -- Noturus gilberti 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "ORANGEFIN MADTOM",
               STATE = "VA",
               NAME_COM_CORRECTED = "ORANGEFIN MADTOM -- NR")

# UNKNOWN CATFISH    FL -- UNKNOWN ICTALURIDAE 
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "UNKNOWN CATFISH",
               STATE = "FL",
               NAME_COM_CORRECTED = "UNKNOWN CATFISH -- NR")

# "UNKNOWN SPOT FIN SMALL" is this spotfin shiner? --- 
# SHEEPSHEAD    LA -- Assuming that this is the minnow bc above

#Hybrids
#COMMON SHINER X HORNYHEAD CHUB    MI
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "COMMON SHINER X HORNYHEAD CHUB",
               STATE = "MI",
               NAME_COM_CORRECTED = "COMMON SHINER X HORNYHEAD CHUB -- NR")

#REDEAR SUNFISH X REDBREAST SUNFISH    VA
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDEAR SUNFISH X REDBREAST SUNFISH",
               STATE = "VA",
               NAME_COM_CORRECTED = "REDBREAST SUNFISH X REDEAR SUNFISH -- NR")

#REDEYE BASS X ALABAMA BASS    SC
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "REDEYE BASS X ALABAMA BASS",
               STATE = "SC",
               NAME_COM_CORRECTED = "ALABAMA BASS X REDEYE BASS -- NR")

# CHUB A    VA -- most state records seem to be part of Nocomis sp. but cheek chub is Semoltilus
# assigning as "UNKNOWN MINNOW" aka Cyprinidae family
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CHUB A",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN NOCOMIS -- NR")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MADTOM",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN NOTURUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "MADTOM",
               STATE = "GA",
               NAME_COM_CORRECTED = "UNKNOWN NOTURUS")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHINER B",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN SHINER")
fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "SHINER A",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN SHINER")

fish_col <- fish_col %>%
  updateRecord(df = ., 
               FIELD_Name = "CYPRINELLA A",
               STATE = "VA",
               NAME_COM_CORRECTED = "UNKNOWN CYPRINELLA")

################################################


# append TAXA_ID to fish collection file by merging with NAME_COM_CORRECTED
#########
fish_col <- merge(fish_col, 
      unique(nars_taxa_list[,c("TAXA_ID","FINAL_NAME")]),
      by.x = "NAME_COM_CORRECTED", by.y = "FINAL_NAME",
      all.x = T)
fish_col[fish_col$NAME_COM_CORRECTED=="UNKNOWN",]
# Check all rows are accounted for
nrow(fish_col_original)==nrow(fish_col)


names(fish_col)

# This is the list of taxa that can be passed through to nativeness/range checks
write.csv(fish_col, "Reconciled_Taxa_Names.csv")

z<-fish_col[fish_col$NAME_COM==""&fish_col$HAS_COUNT=="Y",]
# Query instances where NAME_COM is different from NAME_COM_CORRECTED for 2023 
# collections. This list can be shared with partners to see of the 
# name change is appropriate. 
Check_Taxa <- fish_col%>%
  filter(!STATE%in%c("Selawik", "GU")&
           NAME_COM_UPR != NAME_COM_CORRECTED|str_detect(NAME_COM_CORRECTED, "UNKNOWN")&#NAME_COM_CORRECTED=="UNKNOWN"&
           HAS_COUNT=="Y" & 
           grepl("2023", DATE_COL))%>%
  select(c("SITE_ID", "VISIT_NO", "DATE_COL", "NAME_COM", "NAME_COM_CORRECTED",
           "VOUCH_PHOTO", "VOUCH_UNK", "VOUCH_QA","TAG","LINE"))

wVOUCH <- Check_Taxa%>%
  filter(VOUCH_PHOTO=="Y"|VOUCH_UNK=="Y"|VOUCH_QA=="Y")

LAB_QA <- read.csv("Fish_QA_MBI_2023_11_30.csv")
View(LAB_QA)
wVOUCH[!wVOUCH$SITE_ID%in%LAB_QA$SITE.ID,]

wVOUCH[wVOUCH$SITE_ID%in%LAB_QA$SITE.ID,]

View(LAB_QA[LAB_QA$SITE.ID=="NRS23_PA_10029",])
NRS23_CA_10072

write.csv(Check_Taxa, "Check_Taxa_NRSA_Reconciled_Names_2023.csv", row.names = F)





################################################################################
# Quick map to view fish records. Locations with in fish collection file
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

#NAD83 for CONUS -- Map sites with >0 fish collection data 
#######
fishUID <- fish_col%>%filter(HAS_COUNT=="Y")%>%select(UID)%>%distinct()
site.info_NAD83 <- site.info %>%
  filter(!is.na(LON_DD83) & !is.na(LAT_DD83) & UID %in% fishUID$UID) %>%
  select(c(UID, SITE_ID, VISIT_NO, YEAR, HUC8, DATE_COL,
           LON_DD83, LAT_DD83)) %>% 
  mutate(YEAR = as.factor(YEAR))%>%
  st_as_sf(coords = c("LON_DD83", "LAT_DD83"), crs = 4269)
########################
tmap_mode("view")
tm_shape(site.info_NAD83) +
  tm_dots("YEAR")



