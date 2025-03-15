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
# USGS NAS API query for indigenous occurrences. 
# Returns HUC of occurrence
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
Check_NAS(SpeciesName = "Strongylura marina")
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

# NRSA taxa list -- most recent. This is needed to assign species 
# names which are required to match with NatureServe and NAS 
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

# Path to NatureServe distribution maps
########
#(https://www.natureserve.org/products/digital-distribution-native-us-fishes-watershed)
#NatureServeDirect <- "L:/Public/dpeck/NRSA 2018-19 FISH DATA CLEANUP/NATURESERVE FISH DISTRIBUTION SHAPEFILES 2020/Shapefiles"
####################################################


# 2324 fish collections with reconciled names 
########
fish_col <-read.table("nrsa2324_fishcollectionWide_fish_Corrected.tab",
                   sep = "\t") %>%
  filter(LINE_CORRECTED != "DELETE") %>%
  select(-TAXA_ID)%>%
  distinct()


# new taxa that need to be added added to the NARSA taxa list are 
# removed. These will be changed if they are added to the table. All taxa 
# should have a TAXA_ID before reaching this step in the future
fish_col <- merge(fish_col, 
                  nars_taxa_list[,c("FINAL_NAME","TAXA_ID","NRSA_SPNAME")], 
                  by.x = "NAME_COM_CORRECTED", 
                  by.y = "FINAL_NAME")
fish_col <- merge(fish_col, site.info[,c("UID","HUC8")], by="UID", all.x=T)

####################################################

# lookup tables for nativeness assignments. 
# Files obtained from USGS NAS program -- email saved Matt from NAS. 
#########
# renamed fields to match NRSA fish collection files
NATIVE_HUC <- read.csv("native fish HUC list.csv") %>%
  mutate(HUC8=ifelse(nchar(HUC8) == 7,
                     paste0("H0",HUC8),
                     paste0("H", HUC8)),
         common_name=toupper(common_name))%>%
  mutate(Source=data_source)%>%
  mutate(SpeciesName=scientific_name)

NONNATIVE_HUC <- read.csv("nonnative fish HUC list.csv") %>%
  mutate(HUC8 = ifelse(nchar(HUC8_number_char)==7,
                       paste0("H0",HUC8_number_char),
                       paste0("H", HUC8_number_char)),
         common_name = toupper(common_name))%>%
  mutate(Source="NAS")%>%
  mutate(SpeciesName=scientific_name)
###############################################

################################################################################
# Range Check and Nativeness 
################################################################################



########
# I want to create a native/non-native species list by combining Nature Serve, 
# NAS and previous NRSA Surveys this will be a csv table, with FINAL NAME, HUC8 
# and NATIVE/NON_NATIVE STATUS the HUC8 will create a crosswalk to the WBD. 
# for each HUC8 in the US i want to know whether the taxon is native or not.
###################################################  


# iterate through each taxon collected from 2324 and check range 
# and nativeness status using the HUC8
#########

nars2324_Taxa <- unique(fish_col$TAXA_ID) 
Nativeness_RangeMaps <- data.frame()
scientificName = T
for (id in nars2324_Taxa){
  # id <- 253
  #print(id)
  
  # identify the site where focal fish was collected
  # there should be only one fish record per site.
  UID <- fish_col %>%
    filter(TAXA_ID == id & !is.na(TAXA_ID)) %>%
    select(c("UID", "SITE_ID","TAXA_ID",
             "FINAL_NAME", "INTRODUCED")) %>%
    distinct()
  
  # extract HUC8 of location
  UID <- merge(UID, site.info[,c("UID", "LAT_DD83", "LON_DD83", "HUC8")], 
               by = "UID", all.x = T)
  UID$HUC6 <- substring(UID$HUC8, 1, 7)
  UID$HUC4 <- substring(UID$HUC8, 1, 5)
  UID$HUC2 <- substring(UID$HUC8, 1, 3)
  
  if(scientificName){
    # select species name for TAXA_ID
    species <- nars_taxa_list %>%
      filter(TAXA_ID == id) %>%
      select(NRSA_SPNAME) %>%
      as.character()
    NATIVE <- data.frame(NATIVE_HUC[NATIVE_HUC$scientific_name == species, c("SpeciesName", "HUC8", "Source")])
    NONNATIVE <- NONNATIVE_HUC[NONNATIVE_HUC$scientific_name==species, c("SpeciesName", "HUC8", "Source")]
  } else {
    species <- nars_taxa_list %>%
      filter(TAXA_ID == id) %>%
      select(FINAL_NAME) %>%
      as.character()
    NATIVE <- data.frame(NATIVE_HUC[toupper(NATIVE_HUC$common_name) == species, c("common_name", "HUC8", "Source")])
    colnames(NATIVE)[1] <- "SpeciesName"
    NONNATIVE <- NONNATIVE_HUC[toupper(NONNATIVE_HUC$common_name)==species, c("common_name", "HUC8", "Source")]
    colnames(NONNATIVE)[1] <- "SpeciesName"
  }
    
    # compare species name to databases
    #NATIVE <- Check_NatureServe(SpeciesName = species, Path2Map = NatureServeDirect)
    
    if(nrow(NATIVE)==0){
    NATIVE <- data.frame(SpeciesName = species, 
                         HUC8 = "-9999999", 
                         NON_NATIVE = " ", 
                         Source = " ")
  } else {
    NATIVE <- data.frame(NATIVE, NON_NATIVE = "N")
  }
  
    #NONNATIVE <- Check_NAS(SpeciesName = species)
    
    if(nrow(NONNATIVE)==0){
    NONNATIVE <- data.frame(SpeciesName = species, 
                         HUC8 = "-9999999", 
                         NON_NATIVE = " ", 
                         Source = " ")
  } else {
    NONNATIVE <- data.frame(NONNATIVE, NON_NATIVE = "Y")
  }
  
    # check prior survey for new hucs 
    PriorSurvey <- allTheNRSA.NATIVENESS_HUC8 %>%
      filter(TAXA_ID == id) %>%
      #mutate(HUC8 = substring(HUC8, 2, nchar(HUC8))) %>%
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
    reframe(SpeciesName = unique(SpeciesName),
              NON_NATIVE = paste0(NON_NATIVE, collapse = "/"), 
              Source = paste0(Source, collapse = "/"))
  
  HUC6 <- out %>%
    mutate(HUC6 = substring(HUC8, 1, 7)) %>%
    group_by(HUC6) %>% 
    summarise(SpeciesName = unique(SpeciesName),
              NON_NATIVE = paste0(NON_NATIVE, collapse = "/"), 
              Source = paste0(unique(Source), collapse = "/"))
  
  HUC4 <- out %>%
    mutate(HUC4 = substring(HUC8, 1, 5)) %>%
    group_by(HUC4) %>% 
    summarise(SpeciesName = unique(SpeciesName),
              NON_NATIVE = paste0(NON_NATIVE, collapse = "/"), 
              Source = paste0(unique(Source), collapse = "/"))
  
  HUC2 <- out %>%
    mutate(HUC2 = substring(HUC8, 1, 3)) %>%
    group_by(HUC2) %>% 
    summarise(SpeciesName = unique(SpeciesName),
              NON_NATIVE = paste0(NON_NATIVE, collapse = "/"), 
              Source = paste0(unique(Source), collapse = "/"))

  
  
  # merge the HUC8 of the site where the fish was collected to the checks
  # All sites are where the fish were collected are maintained
  tmp <- merge(UID, out, 
               by.x = c("HUC8"), 
               by.y = c("HUC8"), 
               all.x = T)

  HUC8s <- tmp %>%
    filter(!is.na(NON_NATIVE))%>%
    mutate(HUC_LEVEL = "HUC8")
  
  HUC6s <- tmp %>%
    filter(is.na(NON_NATIVE))%>%
    select(all_of(c("UID", "TAXA_ID",
                    "FINAL_NAME", "SITE_ID", 
                    "LAT_DD83",  "LON_DD83",
                    "INTRODUCED", "HUC8", 
                    "HUC6","HUC4","HUC2"))) %>%
    mutate(HUC_LEVEL = "HUC6")
  
  HUC6s <- merge(HUC6s, HUC6, by = c("HUC6"), all.x = T)
  
  HUC4s <- HUC6s %>%
    filter(is.na(NON_NATIVE))%>%
    select(all_of(c("UID", "TAXA_ID",
                    "FINAL_NAME", "SITE_ID", 
                    "LAT_DD83",  "LON_DD83",
                    "INTRODUCED", "HUC8", 
                    "HUC6","HUC4","HUC2"))) %>%
    mutate(HUC_LEVEL = "HUC4")
  
  HUC4s <- merge(HUC4s, HUC4, by = c("HUC4"), all.x = T)
  
  HUC2s <- HUC4s %>%
    filter(is.na(NON_NATIVE))%>%
    select(all_of(c("UID", "TAXA_ID",
                    "FINAL_NAME", "SITE_ID", 
                    "LAT_DD83",  "LON_DD83",
                    "INTRODUCED", "HUC8", 
                    "HUC6","HUC4","HUC2"))) %>%
    mutate(HUC_LEVEL = "HUC2")
  
  HUC2s <- merge(HUC2s, HUC2, by = c("HUC2"), all.x = T)
  
  tmp <- rbind(HUC8s, 
               HUC6s[!is.na(HUC6s$NON_NATIVE),], 
               HUC4s[!is.na(HUC4s$NON_NATIVE),],
               HUC2s)
  
  Nativeness_RangeMaps <- rbind(Nativeness_RangeMaps, tmp)
}
###################################################

# check all species and sites were evaluated in nativeness table
# The native range taxa should have a record for each taxon and site 
# collected for the 2324 survey
if(nrow(unique(fish_col[,c("UID","TAXA_ID")])) != nrow(Nativeness_RangeMaps)){
  stop("rows differ between input and NATIVENESS")
  #length(unique(Nativeness_RangeMaps$TAXA_ID)) == length(nars2324_Taxa)
  
  # sitesEval <- fish_col %>%
  #   filter(TAXA_ID %in% nars2324_Taxa) %>%
  #   select(UID) %>%
  #   distinct()
  # all(unique(Nativeness_RangeMaps$UID) %in% sitesEval[,1])
  
  #which(duplicated(Nativeness_RangeMaps[,c("UID","TAXA_ID")]))
  #Nativeness_RangeMaps[19086,c("UID","TAXA_ID")]
 # Nativeness_RangeMaps[Nativeness_RangeMaps$UID==2022724&
  #                       Nativeness_RangeMaps$TAXA_ID==253,]
}


# Use crew identification when no other information exists
#######
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$NON_NATIVE)&
                       Nativeness_RangeMaps$INTRODUCED=="Y", "HUC_LEVEL"] <- "FIELD"
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$Source)&
                       Nativeness_RangeMaps$INTRODUCED=="Y", "Source"] <- "CREW"
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$NON_NATIVE)&
                       Nativeness_RangeMaps$INTRODUCED=="Y", "NON_NATIVE"] <- "Y"
###################################################
dim(Nativeness_RangeMaps)


################################################################################
# Explain/interpret table
# generate document to share with partners
# taxa that have no-native status should be checked/flagged 
# taxa that have ambiguous designations at either HUC8 or HUC6 levels should 
# also be checked
################################################################################

Nativeness_RangeMaps[Nativeness_RangeMaps$TAXA_ID=="99999", "NON_NATIVE_FINAL"] <- ""
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$NON_NATIVE), "HUC_LEVEL"] <- "RANGE"

#RANGE need to be checked-- it indicates that the taxa was not observed within a 
#HUC2 and could possibly be a mistaken entry


Prop <- lapply(strsplit(Nativeness_RangeMaps[,"NON_NATIVE"],"/"), function(x) data.frame(PropY = sum(x=="Y")/length(x), Count=length(x)))
Prop <- do.call(rbind,Prop)
Nativeness_RangeMaps <- data.frame(Nativeness_RangeMaps, Prop)
df<-Nativeness_RangeMaps

updateNonNativeStatus <- function(df, UID, TAXA_ID, STATUS){
  if(!any(names(df)=="NON_NATIVE_FINAL")){
    stop("must have NON_NATIVE_FINAL as column heading")
  }
    df[df$UID %in% UID & df$TAXA_ID == TAXA_ID, "NON_NATIVE_FINAL"] <- STATUS    
  
  return(df)
}


# these are direct matches
Nativeness_RangeMaps[
  Nativeness_RangeMaps$HUC_LEVEL == "HUC8" &
    Nativeness_RangeMaps$Count == 1, "NON_NATIVE_FINAL"] <- 
  Nativeness_RangeMaps[
    Nativeness_RangeMaps$HUC_LEVEL == "HUC8" &
      Nativeness_RangeMaps$Count == 1,"NON_NATIVE"]

view(Nativeness_RangeMaps)
strsplit(Nativeness_RangeMaps$NON_NATIVE)
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$SpeciesName)&Nativeness_RangeMaps$SpeciesName=="Cyprinus carpio",]
# make sure all records are accounded for. 
# this table will be to query
addColNames<-names(Nativeness_RangeMaps)[!names(Nativeness_RangeMaps)%in%names(fish_col)]


Nativeness <- merge(fish_col, 
                    Nativeness_RangeMaps[,c("UID","TAXA_ID",addColNames)], 
                    by = c("UID","TAXA_ID"),all.x = T)

unique(Nativeness[is.na(Nativeness$NON_NATIVE) & Nativeness$TAXA_ID != 99999, c("TAXA_ID", "NAME_COM_CORRECTED")])



#These are range flags --- the taxon have not been observed within this 2digit HUC 
# from a previous survey or 
view(Nativeness[is.na(Nativeness$NON_NATIVE),
                c("UNIQUE_ID", "SITE_ID", "PSTL_CODE", 
                  "VISIT_NO", "DATE_COL", "FINAL_NAME",
                  "NAME_COM_CORRECTED", "TAXA_ID", 
                  "NON_NATIVE", "Source","HUC_LEVEL", "HUC2")])


table(Nativeness[,"HUC_LEVEL"])
           
Nativeness[!is.na(Nativeness$NON_NATIVE) & 
             Nativeness$HUC_LEVEL=="HUC2",
           c("UNIQUE_ID", "SITE_ID", "PSTL_CODE", 
             "VISIT_NO", "DATE_COL", "FINAL_NAME",
             "NAME_COM_CORRECTED", "TAXA_ID", 
             "NON_NATIVE", "Source","HUC_LEVEL", "HUC2")]
NAME
Nativeness

orderNames <- c("PUBLICATION_DATE","UNIQUE_ID", "SITE_ID", "VISIT_NO", "DATE_COL", 
                "HUC8_num", "HUC8", "HUC6", 
                "PSTL_CODE", "STATE", "SITE_ID_Mod", "SAMPLE_TYPE", "PAGE", "LINE", 
                "UID","NAME_COM", "NAME_COM_UPR", "FINAL_NAME", "TAXA_ID", "SpeciesName",   
                "FLAG", "INTRODUCED_NOT_EVAL", "INTRODUCED", "VOUCH_PHOTO",        
                "VOUCH_UNK", "VOUCH_NUM", "HYBRID", "TAG", "VOUCH_QA", "REVIEW", 
                "FISH_REVIEW", 
                "COUNT_6","COUNT_12", "COUNT_18", "COUNT_19", "MORT_CT", "ANOM_COUNT",
                "TOTAL","NON_NATIVE", "Source","HUC_LEVEL")


# write.csv(Nativeness[,orderNames],"Nativeness_10272024_B.csv")

Nativeness <- read.csv("Nativeness_10272024_B.csv", row.names = "X")

# Filter 2023 data
NRSA_2023 <- Nativeness[Nativeness$TOTAL > 0&
                          Nativeness$YEAR == 2023&
                          Nativeness$STATE != "GU",]


write.csv(NRSA_2023, "NRSA_2023.csv", row.names = F)

 View(Nativeness[Nativeness$TOTAL > 0 &
                 Nativeness$VISIT_NO!=99&
                 Nativeness$YEAR == 2023 &
                 Nativeness$STATE != "GU" &
                 is.na(Nativeness$NON_NATIVE),])


