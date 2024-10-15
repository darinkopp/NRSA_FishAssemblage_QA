# Range check and native status

# For each species, query nature serve shapefiles, 
# non-indigenous aquatic species and previous NRSA surveys. 
# Extract HUC8 for each record. Then compare to the list of HUC8's 
# where the 2324 taxa was collected
#
# Native designated if the HUC where the taxon was collected is also listed in 
# the native range

# Non-native if the HUC where the taxon was collected is in NAS Non-native range. 

# When a HUC was surveyed during a previous cycle, assign the native/non-native 
# designation. This is essentially leveraging expert opinion from previous NRSA
# (i.e. Dave Peck et al.)

# A manual check of taxa that were unmatched and species could be give 
# conflicting assignments. 

# Unmatched taxa could occur if the Scientific Name is not in NatureServe or 
# NAS databases, They occur outside Either the HUC8 was not within any 
# of the sources of the species did not have a record. 

library(sf)
library(tidyverse)
library(nhdplusTools)

################################################################################
# Functions
################################################################################

# Check Nature serve files
# Path2Map is the location of NatureServe Digital distribution Maps
######
Check_NatureServe <- function(SpeciesName, Path2Map){
  require(sf)
  #NatureServe shapefiles are named with "_" 
  sp <- gsub(" ", "_", SpeciesName)
  
  # Check if nature serve polygon exist for the species
  NS_Range <- try(read_sf(paste0(Path2Map, "/", sp, ".shp")), silent = T)
  
  if(any(class(NS_Range) == "try-error")){
    
    NS_Range <- data.frame(HUC = "-9999999")
    
  } else {
    
    NS_Range <- NS_Range %>%
      select(HUC) %>%
      st_set_geometry(NULL)%>%
      mutate(HUC = ifelse(nchar(HUC)==7, paste0(0,HUC), paste(HUC))) %>%
      distinct()
    
  }
  
  NATIVE <- data.frame(SpeciesName, 
                       HUC8 = NS_Range$HUC, 
                       NON_NATIVE = "N", 
                       Source = "NatureServe")
  return(NATIVE)
}
Check_NatureServe(SpeciesName = "Etheostoma douglasi", Path2Map = NatureServeDirect)
Check_NatureServe(SpeciesName = "Moxostoma sp. Cf. Poecilurum", Path2Map = NatureServeDirect)
Check_NatureServe(SpeciesName = "Fundulus diaphanus",Path2Map = NatureServeDirect)
#######################################

# check NAS 
# USGS NAS API query for nonindigeous occurrences. 
# Returns HUC of ocurrence
######
Check_NAS <- function(SpeciesName){
  NM <- strsplit(SpeciesName, " ") %>% unlist()
  
  # API Query
  NAS_Range <- httr::GET("http://nas.er.usgs.gov/api/v2/occurrence/search",
                         query = list(genus=NM[1], species=NM[2]))
  
  # converts results into something usable
  NAS_Range <- jsonlite::fromJSON(rawToChar(NAS_Range$content))
  
  # extracts HUC8
  NAS_Range <- unique(NAS_Range$results$huc8)
  
  if(is.null(NAS_Range)){NAS_Range<-"-9999999"}
  
  NONNATIVE <- data.frame(SpeciesName, HUC8 = NAS_Range, NON_NATIVE = "Y", Source = "NAS")  
  
  return(NONNATIVE)
}
#HUC8==-9999999 indicates that species is not found in data base
Check_NAS(SpeciesName = "Etheostoma douglasi")
Check_NAS(SpeciesName = "Atractosteus spatula")
Check_NAS(SpeciesName = "Fundulus diaphanus")
#######################################



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

# lon83 was negative for handpicked sites, 
# KB corrected in next version
site.info[!is.na(site.info[,c("LON_DD83")]) & site.info[,c("LON_DD83")]>0, "LON_DD83"] <- 
  site.info[!is.na(site.info[,c("LON_DD83")]) & site.info[,c("LON_DD83")]>0, "LON_DD83"] * -1

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
  HUCs$HUC8[i] <- get_huc(pt[i,]) %>%
    select(huc12) %>% 
    st_set_geometry(NULL)
}
HUCs$HUC8 <- unlist(HUCs$HUC8)

# add missing HUC8 to the site.info 
for (i in HUCs$UID){
  site.info[site.info$UID==i, "HUC8"] <- paste0("H", substring(HUCs[HUCs$UID == i, "HUC8"],0,8))
}

######################

# Prior survey results -- if a huc8 was surveyed previously, how were the taxa 
# classified. This is analogous to PBJ used by NAS to assemble their records
# Files pulled from IM 9/6/2024
#######
allTheNRSA <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/allTheNRSA/data/tabfiles"

allTheNRSA.site.info <- paste0(allTheNRSA, "/NRSA0809-1819_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)

allTheNRSA.fishCts <- paste0(allTheNRSA, "/NRSA0809-1819_fishCts_alltheNRSA.tab") %>%
  read.table(sep = "\t", header = T)

allTheNRSA.NATIVENESS_HUC8 <- allTheNRSA.site.info %>%
  select(c("UID", "SITE_ID", "HUC8","DSGN_CYCLE")) %>%
  inner_join(allTheNRSA.fishCts, by = "UID")%>%
  select(TAXA_ID, HUC8, NON_NATIVE, DSGN_CYCLE)%>%
  distinct()
####################################################

# NRSA taxa list -- most recent from 2018/19. This is needed to assign species 
# names which are required to match with NatureServe and NAS 
########
dir_NRSA_1819 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa1819/data/"
nars_taxa_list <- paste0(dir_NRSA_1819, "allNRSA_fishTaxa.tab") %>%
  read.table(sep = "\t", header = T)

nars_taxa_list[grep("BLUNT", nars_taxa_list$FINAL_NAME),]

# add species name
nars_taxa_list <- nars_taxa_list %>%
  rowwise()%>%
  mutate(NRSA_SPNAME = str_to_sentence(paste0(c(GENUS, SPECIES), collapse = " ")))
####################################################

# Path to NatureServe distribution maps
########
#(https://www.natureserve.org/products/digital-distribution-native-us-fishes-watershed)
NatureServeDirect <- "L:/Public/dpeck/NRSA 2018-19 FISH DATA CLEANUP/NATURESERVE FISH DISTRIBUTION SHAPEFILES 2020/Shapefiles"
####################################################

# 2324 fish collections with reconciled names 
########
fish_col <- read.csv("Reconciled_Taxa_Names.csv", row.names = "X")
# new taxa that need to be added added to the NARSA taxa list are 
# removed. These will be changed if they are added to the table. All taxa 
# should have a TAXA_ID before reaching this step in the future
nars2324_Taxa <- unique(fish_col$TAXA_ID[!is.na(fish_col$TAXA_ID)])
####################################################


################################################################################
# Range Check and Nativeness 
################################################################################

# Future steps to create a new lookup table for nativeness assignments. 
########
# avoid using the NAS API

# the problem is that we have species specific shapefiles from NatureServe, 
# the polygons are redundant. Better to store .csv visits and used to query 
# HUC8. Second, files from NAS require web queries. Third, it is not easy to used
# "expert" opinion from previous surveys. Create something that can change over 
# time

# I want to create a native/non-native species list by combining Nature Serve, 
# NAS and previous NRSA Surveys this will be a csv table, with FINAL NAME, HUC8 
# and NATIVE/NON_NATIVE STATUS the HUC8 will create a crosswalk to the WBD. 
# for each HUC8 in the US i want to know whether the taxon is native or not.
###################################################  


# iterate through each taxon collected from 2324 and check range 
# and nativeness status using the HUC8
#########
#these taxa cannot be queried from NAS
APITimeout_NAS <- c(105, 143, 301, 373, 374, 382, 5179, 213)
nars2324_Taxa <- nars2324_Taxa[!nars2324_Taxa %in% APITimeout_NAS]

nars2324_Taxa[which(nars2324_Taxa==213)]
Nativeness_RangeMaps <- data.frame()
for (id in nars2324_Taxa[163:length(nars2324_Taxa)]){
  # id <- 473
  print(id)
  
  # select species name for TAXA_ID
  species <- nars_taxa_list %>%
    filter(TAXA_ID==id) %>%
    select(NRSA_SPNAME) %>%
    as.character()
  
  # compare species name to databases
  NATIVE <- Check_NatureServe(SpeciesName = species, Path2Map = NatureServeDirect)
  NONNATIVE <- Check_NAS(SpeciesName = species)
  
  # check prior survey for new hucs 
  PriorSurvey <- allTheNRSA.NATIVENESS_HUC8 %>%
    filter(TAXA_ID == id) %>%
    mutate(HUC8 = substring(HUC8, 2, nchar(HUC8))) %>%
    distinct()
  
  # NRSA is source only if HUC8 was not included in other sources - avoids duplicated entries
  if(nrow(PriorSurvey)>0){
    PriorSurvey <- data.frame(SpeciesName = species, 
                              HUC8 = PriorSurvey$HUC8, 
                              NON_NATIVE = PriorSurvey$NON_NATIVE, 
                              Source = PriorSurvey$DSGN_CYCLE)%>%
      list(., NONNATIVE, NATIVE)%>%
      do.call(rbind, .)%>% 
      group_by(HUC8, NON_NATIVE) %>% 
      filter(n()==1 & !Source%in%c("NAS","NatureServe"))
    
    out <- do.call(rbind, list(NONNATIVE, NATIVE, PriorSurvey))
    
  } else {
    
    out <- do.call(rbind, list(NONNATIVE, NATIVE))
    
  }
  
  # collapse duplicated/conflicting results: Nativeness could change within a HUC
  out <- out %>%
    group_by(HUC8) %>% 
    summarise(SpeciesName = unique(SpeciesName),
              NON_NATIVE = paste0(NON_NATIVE, collapse = "/"), 
              Source = paste0(Source, collapse = "/"))
  
  HUC6 <- out %>%
    mutate(HUC6 = substring(HUC8, 1, 6)) %>%
    group_by(HUC6) %>% 
    summarise(SpeciesName = unique(SpeciesName),
              NON_NATIVE = paste0(NON_NATIVE, collapse = "_"), 
              Source = paste0(unique(Source), collapse = "/"))
  
  # identify the site where focal fish was collected
  # there should be only one fish record per site.
  UID <- fish_col %>%
    filter(TAXA_ID == id & !is.na(TAXA_ID)) %>%
    select(c("UID", "SITE_ID","TAXA_ID",
             "FINAL_NAME", "INTRODUCED")) %>%
    distinct()
  
  # extract HUC8 of location
  UID <- merge(UID, site.info[,c("UID", "HUC8")], 
               by = "UID", all.x = T)
  
  # trim leading "H" to a numeric value for matching
  UID$HUC8_num <- substring(UID$HUC8, 2, 9)
  UID$HUC6 <- substring(UID$HUC8_num, 1, 6)
  
  
  # merge the HUC8 of the site where the fish was collected to the checks
  # All sites are where the fish were collected are maintained
  tmp <- merge(UID, out, 
               by.x = c("HUC8_num"), 
               by.y = c("HUC8"), 
               all.x = T)
  HUC8s <- tmp %>%
    filter(!is.na(NON_NATIVE))%>%
    mutate(HUC_LEVEL = "HUC8")
  
  HUC6s <- tmp %>%
    filter(is.na(NON_NATIVE))%>%
    select(all_of(c("HUC8_num", "UID", "TAXA_ID", 
                    "FINAL_NAME", "SITE_ID",
                    "INTRODUCED", "HUC8", "HUC6"))) %>%
    mutate(HUC_LEVEL = "HUC6")
  
  HUC6s <- merge(HUC6s, HUC6, by = c("HUC6"), all.x = T)
  
  tmp <- rbind(HUC8s, HUC6s)
  
  Nativeness_RangeMaps <- rbind(Nativeness_RangeMaps, tmp)
}
###################################################

# check nativeness table
######
# The native range taxa should have a record for each taxon and site 
# collected for the 2324 survey
length(unique(Nativeness_RangeMaps$TAXA_ID)) == length(nars2324_Taxa)
sitesEval <- fish_col %>%
  filter(TAXA_ID %in% nars2324_Taxa) %>%
  select(UID) %>%
  distinct()
all(unique(Nativeness_RangeMaps$UID) %in% sitesEval[,1])
###################################################

# Use crew identification when no other information exists
#######
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$NON_NATIVE)&
                       Nativeness_RangeMaps$INTRODUCED=="Y", "HUC_LEVEL"] <- "FIELD"
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$Source)&
                       Nativeness_RangeMaps$INTRODUCED=="Y", "Source"] <- "CREW"
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$NON_NATIVE)&
                       Nativeness_RangeMaps$INTRODUCED=="Y", "NON_NATIVE"] <- "Y"

###################################################

################################################################################
# Explain/interpret table
# generate document to share with partners
# taxa that have no-native status should be checked/flagged 
# taxa that have ambiguous designations at either HUC8 or HUC6 levels should 
# also be checked
################################################################################


#write.csv(Nativeness_RangeMaps,"Nativeness_RangeMaps_10092024.csv")
Nativeness_RangeMaps <- read.csv("Nativeness_RangeMaps_10092024.csv")
View(Nativeness_RangeMaps)
