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

site.info[site.info$SITE_ID=="NRS23_TX_10453",c("LAT_DD83","LON_DD83")]<-c(33.12121461,-96.11233232)
site.info[site.info$SITE_ID=="NRS23_TX_10504",c("LAT_DD83","LON_DD83")]<-c(33.3825256,-95.12624982)
site.info[site.info$SITE_ID=="NRS23_TX_10650",c("LAT_DD83","LON_DD83")]<-c(30.73226399,-95.60260147)
site.info[site.info$SITE_ID=="NRS23_TX_10671",c("LAT_DD83","LON_DD83")]<-c(31.26277343,-94.18733703)

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

# 2324 fish collections with reconciled names 
########
fish_col <-read.table("nrsa2324_fishcollectionWide_fish_Corrected.tab",
                      sep = "\t") %>%
  filter(LINE_CORRECTED != "DELETE") %>%
  select(-TAXA_ID)%>%
  distinct()%>%
  mutate(FINAL_NAME = NAME_COM_CORRECTED)


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
# and nativeness status using the HUC8 -- this is a master table
# any taxa/HUC8 combinations will need 
#########

nars2324_Taxa <- unique(fish_col$TAXA_ID) 
Nativeness_RangeMaps <- data.frame()
scientificName = T
for (id in nars2324_Taxa){
  # id <- 99999
  #print(id)
  
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
  
  if(nrow(NATIVE)>0){
    NATIVE <- data.frame(NATIVE, NON_NATIVE = "N")
  }
  
  if(nrow(NONNATIVE)>0){
    NONNATIVE <- data.frame(NONNATIVE, NON_NATIVE = "Y")
  }
  
  # check prior survey for new hucs 
  PriorSurvey <- allTheNRSA.NATIVENESS_HUC8 %>%
    filter(TAXA_ID == id) %>%
    distinct()
  
  # if a HUC8 was surveyed multiple times, select the most recent entry
  PriorSurvey <- split(PriorSurvey, PriorSurvey$HUC8)
  PriorSurvey <- lapply(
    PriorSurvey, function(x) x[order(x$DSGN_CYCLE, decreasing=T)[1],])
  PriorSurvey<-do.call(rbind,PriorSurvey)
  
  #set NO Fish to blank 0809 records have non-native == N
  if(id == 99999){PriorSurvey$NON_NATIVE <- ""}
  
  if(!is.null(PriorSurvey)){
    PriorSurvey <- data.frame(SpeciesName = species, 
                              HUC8 = PriorSurvey$HUC8, 
                              NON_NATIVE = PriorSurvey$NON_NATIVE, 
                              Source = PriorSurvey$DSGN_CYCLE)
  }
  
  datalist <- list(NONNATIVE, NATIVE, PriorSurvey)
  ind <- unlist(lapply(datalist, function(x) nrow(x)>0))
  
  if(any(ind)){
    out <- do.call(rbind, datalist[ind])
  } else {
    out <- data.frame(SpeciesName = species, 
                      HUC8 = "-9999999", 
                      NON_NATIVE = " ", 
                      Source = " ")
  }
  
  # collapse duplicated/conflicting results for HUC8: Priotitize NAS, NATURESERVE and previous surveys
  out$Source <- factor(out$Source, levels =  c("NAS", "NatureServe", "2018-19", "2013-14", "2008-09"))
  out <- split(out, out$HUC8)
  out <- lapply(out, function(x) x[order(x$Source)[1],])
  out <- do.call(rbind,out)
  
  Nativeness_RangeMaps <- rbind(Nativeness_RangeMaps, data.frame(TAXA_ID=id,out))   
}


#all submitted taxa accounted for! 
all(nars2324_Taxa %in% Nativeness_RangeMaps$TAXA_ID)

Nativeness_RangeMaps <- sv
#These taxa do not have a record in any of the databases 
Nativeness_RangeMaps[Nativeness_RangeMaps$HUC8 == "-9999999",]
#these sites do not have coordinates or are outside CONUS
Nativeness_RangeMaps[Nativeness_RangeMaps$TAXA_ID=="99999", "NON_NATIVE"]<-""
#add HUCS for sites that did not detect fish
NoFish <- data.frame(unique(fish_col[
  fish_col$TAXA_ID==99999 & fish_col$HUC8 != "", 
  c("TAXA_ID","HUC8")]), SpeciesName = "", 
  NON_NATIVE = "", Source = "2023-24" )

NRHUC8 <- Nativeness_RangeMaps[Nativeness_RangeMaps$TAXA_ID==99999,"HUC8"]
NoFish <- NoFish[!NoFish$HUC8%in%NRHUC8,]

Nativeness_RangeMaps <- rbind(Nativeness_RangeMaps, NoFish)

##### add missing HUCs to table
# sv <- Nativeness_RangeMaps


MissingTaxa_HUC <- merge(fish_col, 
                         Nativeness_RangeMaps, 
                         by = c("HUC8", "TAXA_ID"), 
                         all.x = T)
n=7
#these taxa do not have native/nonnative designation
#the need to be added to the Nativeness maps
Nativeness_RangeMaps[Nativeness_RangeMaps$HUC8 == "-9999999",]
#while state ment here=== keep reunning while this is >0
while(n>=5){
  #sum(is.na(MissingTaxa_HUC$NON_NATIVE) & MissingTaxa_HUC$HUC8!="")
  
  #add HUC6
  MissingTaxa_HUC <- MissingTaxa_HUC%>%
    filter(is.na(NON_NATIVE) & HUC8!="")%>%
    select(c("HUC8","TAXA_ID"))%>%
    mutate(HUC_NEW = substring(HUC8, 1, n)) %>% 
    distinct() 
  
  # This calculates percentage of HUC8s with with native/nonnative
  # record within the larger HUC
  LargerHUC <- Nativeness_RangeMaps %>%
    filter(HUC8!="-9999999") %>%
    mutate(HUC_NEW = substring(HUC8, 1, n)) %>%
    group_by(HUC_NEW, TAXA_ID) %>% 
    reframe(TAXA_ID = unique(TAXA_ID),
            NON_NATIVE = mean(NON_NATIVE=="Y"), 
            Source = paste0(unique(Source), collapse = "/"))
  
  # Use this to update nativeness maps
  LargerHUC <- merge(MissingTaxa_HUC, LargerHUC, by = c("HUC_NEW", "TAXA_ID"))%>%
    filter(NON_NATIVE > 0.8|NON_NATIVE <0.2)%>%
    mutate(NON_NATIVE = ifelse(NON_NATIVE > 0.8, "Y", 
                               ifelse(NON_NATIVE < 0.2, "N", "")),
           Source = paste0(Source, " [HUC", n-1,"]"),
           SpeciesName = "" , ) %>%
    select(c("TAXA_ID", "SpeciesName", "HUC8", "NON_NATIVE", "Source"))
  
  Nativeness_RangeMaps <- rbind(Nativeness_RangeMaps, LargerHUC)
  
  MissingTaxa_HUC <- merge(fish_col, 
                           Nativeness_RangeMaps, 
                           by = c("HUC8","TAXA_ID"), 
                           all.x = T)
  n = n-2
}

sum(b$HUC8!=""&is.na(b$NON_NATIVE))

b <- merge(fish_col, 
           Nativeness_RangeMaps, 
           by = c("HUC8","TAXA_ID"), 
           all.x = T)
unique(b$Source)
b[!is.na(b$Source)&b$Source == "2008-09/NatureServe [HUC6]",]

Nativeness_RangeMaps[Nativeness_RangeMaps$TAXA_ID==630,]
Nativeness_RangeMaps[Nativeness_RangeMaps$HUC8 == "H03160204" & Nativeness_RangeMaps$TAXA_ID==552,]
b[b$HUC8!="" & is.na(b$NON_NATIVE), ]


#these would be manually checked
CHECK <- b[b$HUC8!="" & is.na(b$NON_NATIVE),
           c("HUC8","TAXA_ID","UID","NAME_COM_CORRECTED","SITE_ID")]
view(b)
names(Nativeness_RangeMaps)

nrow(fish_col)
Nativeness_RangeMaps[Nativeness_RangeMaps$TAXA_ID==478&
                       Nativeness_RangeMaps$HUC8=="H01010002",]

#these sites do not have coordinates or are outside CONUS
b[b$HUC8=="","NON_NATIVE"]<-""
b[b$TAXA_ID=="99999","NON_NATIVE"]<-""

#these taxa do not have native/nonnative designation
#the need to be added to the Nativeness maps
sum(is.na(b$NON_NATIVE))

#add HUC6
b <- b[is.na(b$NON_NATIVE), c("HUC8","TAXA_ID")]%>%
  mutate(HUC4 = substring(HUC8, 1, 5)) %>% 
  distinct() 

HUC4 <- Nativeness_RangeMaps %>%
  filter(HUC8!="-9999999")%>%
  mutate(HUC4 = substring(HUC8, 1, 5)) %>%
  group_by(HUC4,TAXA_ID) %>% 
  reframe(TAXA_ID = unique(TAXA_ID),
          NON_NATIVE = mean(NON_NATIVE=="Y"), 
          Source = paste0(unique(Source), collapse = "/"))


#use this to update nativeness maps
HUC4 <- merge(b, HUC4, by = c("HUC4", "TAXA_ID"))

HUC4Matches <- HUC6%>%
  filter(NON_NATIVE == 1|NON_NATIVE == 0)%>%
  mutate(NON_NATIVE = ifelse(NON_NATIVE == 1, "Y", 
                             ifelse(NON_NATIVE == 0, 
                                    "N", "")),
         Source = paste(Source, "[HUC4]"),
         SpeciesName = "" , )%>%
  select(c("TAXA_ID","SpeciesName","HUC8","NON_NATIVE","Source"))

HUC4Matches[duplicated(HUC4Matches),]

Nativeness_RangeMaps <- rbind(Nativeness_RangeMaps, HUC4Matches)

##########



ifelse(HUC6$NON_NATIVE == 1 , "Y", ifelse(HUC6$NON_NATIVE == 0,"N",""))

#count proportion of sites... 
criteria <- is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL)& Nativeness_RangeMaps$HUC_LEVEL== "HUC6"

## count the numer of occurrences within each huc 6
split()
Prop <- lapply(strsplit(as.character(HUC6[,"NON_NATIVE"]),"/"), 
               function(x) data.frame(PropY = sum(x=="Y")/length(x), Count=length(x)))
Prop <- do.call(rbind,Prop)

Prop$NON_NATIVE <- ifelse(Prop$Count >= 0 & Prop$PropY > 0.5, "Y",
                          ifelse(Prop$Count >= 2 & Prop$PropY < 0.5, "N", "CHECK"))

Nativeness_RangeMaps[criteria,"NON_NATIVE_FINAL"] <- Prop$NON_NATIVE

merge(HUC6,nars_taxa_list[,c("FINAL_NAME","TAXA_ID")])


HUC4 <- Nativeness_RangeMaps %>%
  filter(HUC8!="-9999999")%>%
  mutate(HUC4 = substring(HUC8, 1, 5)) %>%
  group_by(HUC4) %>% 
  reframe(SpeciesName = unique(SpeciesName),
          NON_NATIVE = paste0(NON_NATIVE, collapse = "/"), 
          Source = paste0(unique(Source), collapse = "/"))

HUC2 <- Nativeness_RangeMaps %>%
  filter(HUC8!="-9999999")%>%
  mutate(HUC2 = substring(HUC8, 1, 3)) %>%
  group_by(HUC2) %>% 
  reframe(SpeciesName = unique(SpeciesName),
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

NONNATIVE_HUC[NONNATIVE_HUC$HUC8=="H11030017" & NONNATIVE_HUC$SpeciesName=="Gambusia affinis",]
NONNATIVE_HUC[NONNATIVE_HUC$HUC8=="H11030017" & NONNATIVE_HUC$SpeciesName=="Gambusia affinis",]

################################################################################
# Explain/interpret table
# generate document to share with partners
# taxa that have no-native status should be checked/flagged 
# taxa that have ambiguous designations at either HUC8 or HUC6 levels should 
# also be checked
################################################################################
Nativeness_RangeMaps$NON_NATIVE_FINAL <- NA
view(Nativeness_RangeMaps)
#these taxa were not matched in the databases
#non-native came from the database query, if it is NA 
#then there was no matching species record. 
Nativeness_RangeMaps[is.na(Nativeness_RangeMaps$NON_NATIVE), "HUC_LEVEL"] <- "RANGE"

#not worried about sites with NO fish or GU, assigned to blank
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$HUC8) &  
                       Nativeness_RangeMaps$TAXA_ID == "99999" | 
                       !is.na(Nativeness_RangeMaps$HUC8) &
                       c("HUC_LEVEL", "NON_NATIVE_FINAL")] <- c("", "")
Nativeness_RangeMaps[grep("GU", Nativeness_RangeMaps$SITE_ID),
                     c("HUC_LEVEL", "NON_NATIVE_FINAL")] <- c("", "")

unique(Nativeness_RangeMaps[Nativeness_RangeMaps$HUC_LEVEL == "RANGE",])

Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$HUC8) & 
                       Nativeness_RangeMaps$HUC8 == "", 
                     "NON_NATIVE_FINAL"] <- ""
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$HUC8) & 
                       Nativeness_RangeMaps$TAXA_ID == "99999", "NON_NATIVE_FINAL"] <- ""


table(Nativeness_RangeMaps$HUC_LEVEL)

# HUC8 matches are good
######
criteria <- is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & Nativeness_RangeMaps$HUC_LEVEL== "HUC8"
Nativeness_RangeMaps$NON_NATIVE_FINAL[criteria] <- Nativeness_RangeMaps$NON_NATIVE[criteria]
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & 
                       Nativeness_RangeMaps$NON_NATIVE_FINAL == "Y/N/N","NON_NATIVE_FINAL"]<-"N"
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & 
                       Nativeness_RangeMaps$NON_NATIVE_FINAL == "N/N/Y","NON_NATIVE_FINAL"]<-"N"
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & 
                       Nativeness_RangeMaps$NON_NATIVE_FINAL == "N/N","NON_NATIVE_FINAL"]<-"N"
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & 
                       Nativeness_RangeMaps$NON_NATIVE_FINAL == "Y/N","NON_NATIVE_FINAL"]<-"N"
Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & 
                       Nativeness_RangeMaps$NON_NATIVE_FINAL == "N/Y","NON_NATIVE_FINAL"]<-"N"
##################################

# HUC6 so-so
#########
criteria <- is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL)& Nativeness_RangeMaps$HUC_LEVEL== "HUC6"

## count the numer of occurrences within each huc 6
Prop <- lapply(strsplit(Nativeness_RangeMaps[criteria,"NON_NATIVE"],"/"), 
               function(x) data.frame(PropY = sum(x=="Y")/length(x), Count=length(x)))
Prop <- do.call(rbind,Prop)

Prop$NON_NATIVE <- ifelse(Prop$Count >= 0 & Prop$PropY > 0.5, "Y",
                          ifelse(Prop$Count >= 2 & Prop$PropY < 0.5, "N", "CHECK"))

Nativeness_RangeMaps[criteria,"NON_NATIVE_FINAL"] <- Prop$NON_NATIVE
###################################

# HUC4 so-so
#########
criteria <- is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL)& Nativeness_RangeMaps$HUC_LEVEL== "HUC4"

## count the numer of occurrences within each huc 4
Prop <- lapply(strsplit(Nativeness_RangeMaps[criteria,"NON_NATIVE"],"/"), 
               function(x) data.frame(PropY = sum(x=="Y")/length(x), Count=length(x)))
Prop <- do.call(rbind,Prop)

Prop$NON_NATIVE <- ifelse(Prop$Count >= 0 & Prop$PropY > 0.5, "Y",
                          ifelse(Prop$Count >= 5 & Prop$PropY < 0.5, "N", "CHECK"))
Nativeness_RangeMaps[criteria,"NON_NATIVE_FINAL"] <- Prop$NON_NATIVE
###################################

# HUC2 so-so
##########
criteria <- is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL)& Nativeness_RangeMaps$HUC_LEVEL== "HUC2"

## count the numer of occurrences 
Prop <- lapply(strsplit(Nativeness_RangeMaps[criteria,"NON_NATIVE"],"/"), 
               function(x) data.frame(PropY = sum(x=="Y")/length(x), Count=length(x)))
Prop <- do.call(rbind, Prop)

Prop$NON_NATIVE <- ifelse(Prop$Count >= 10 & Prop$PropY > 0.8, "Y",
                          ifelse(Prop$Count >= 10 & Prop$PropY < 0.8, "N", "CHECK"))
Nativeness_RangeMaps[criteria,"NON_NATIVE_FINAL"] <- Prop$NON_NATIVE
###################################

#ranges need to be checked
########
criteria <- is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL)& Nativeness_RangeMaps$HUC_LEVEL== "RANGE"
Nativeness_RangeMaps[criteria,"NON_NATIVE_FINAL"] <- "CHECK"
###################################


addmargins(table(Nativeness_RangeMaps$NON_NATIVE_FINAL,
                 Nativeness_RangeMaps$HUC_LEVEL))

Nativeness_RangeMaps[!is.na(Nativeness_RangeMaps$NON_NATIVE_FINAL) & 
                       Nativeness_RangeMaps$NON_NATIVE_FINAL=="CHECK",]

view(Nativeness_RangeMaps)
Nativeness_RangeMaps[Nativeness_RangeMaps$HUC_LEVE == "RANGE" & 
                       Nativeness_RangeMaps$TAXA_ID != "99999", 
                     "HUC8"]
Nativeness_RangeMaps
NATIVE_HUC
NONNATIVE_HUC[toupper(NONNATIVE_HUC$common_name)=="COMMON CARP",]

WBD <- st_read(dsn = "Supporting Information/WBD_National_GDB/WBD_National_GDB.gdb", layer = "WBDHU8")
Nativeness_RangeMaps

# name could not be found
"Gambusia affinis"
library(tmap)
for (id in nars2324_Taxa){
  id <- 184
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
            Source = paste0(Source, collapse = "/"))%>%
    mutate(HUC8 = substring(HUC8, 2, nchar(HUC8)))
  
  Nativeness_RangeMaps[Nativeness_RangeMaps$NON_NATIVE_FINAL=="CHECK", c("TAXA_ID","SpeciesName")]
  taxon <- Nativeness_RangeMaps[Nativeness_RangeMaps$SpeciesName == species & 
                                  !is.na(Nativeness_RangeMaps$SpeciesName),]
  taxon <- st_transform(
    st_as_sf(taxon,
             coords = c("LON_DD83","LAT_DD83"), 
             crs = st_crs(4269)),
    crs = 5070)
  
  
  Check_Taxon <- taxon[taxon$NON_NATIVE_FINAL == "CHECK",]
  taxon <- taxon[taxon$NON_NATIVE_FINAL != "CHECK",]
  
  #tmap_mode("view")
  #library(tmap)
  
  SUB <- merge(WBD, out, by.x = "huc8", by.y = "HUC8")
  SUB_dis <- sf_dissolve(SUB, y="NON_NATIVE")
  
  tm_shape(tes)+
    tm_polygons("NON_NATIVE")+
    tm_shape(Check_Taxon)+
    tm_dots("UID", size = 0.5, color = "black")+
    tm_shape(taxon) +
    tm_dots("NON_NATIVE_FINAL", size = 0.5)
  
  
  
  # update final designation... 
  Nativeness_RangeMaps[Nativeness_RangeMaps$SpeciesName=="Gambusia affinis" & 
                         Nativeness_RangeMaps$UID == "2023565", 
                       "NON_NATIVE_FINAL"]<-"Y"
  Nativeness_RangeMaps[Nativeness_RangeMaps$SpeciesName=="Gambusia affinis" & 
                         Nativeness_RangeMaps$UID == "2024175", 
                       "NON_NATIVE_FINAL"] <- "Y"
  st
  #NA Values
  Nativeness_RangeMaps[Nativeness_RangeMaps$SpeciesName==species & 
                         Nativeness_RangeMaps$UID == "2022656", 
                       "NON_NATIVE_FINAL"]<-"Y"
  Notropis stramineus
  
  Nativeness_RangeMaps$SpeciesName==
    install.packages("spatialEco")
  library(spatialEco)
  names(SUB)
  tm_shape(a) +
    # tm_polygons() +
    # a<-st_union(st_make_valid(SUB))
    sf_use_s2(FALSE)
  
  
  
  
  
  
  #RANGE need to be checked-- it indicates that the taxa was not observed within a 
  #HUC2 and could possibly be a mistaken entry
  
  
  Prop <- lapply(strsplit(Nativeness_RangeMaps[,"NON_NATIVE"],"/"), 
                 function(x) data.frame(PropY = sum(x=="Y")/length(x), Count=length(x)))
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
               Nativeness$HUC_LEVEL == "HUC2",
             c("UNIQUE_ID", "SITE_ID", "PSTL_CODE", 
               "VISIT_NO", "DATE_COL", "FINAL_NAME",
               "NAME_COM_CORRECTED", "TAXA_ID", 
               "NON_NATIVE", "Source","HUC_LEVEL", "HUC2")]
  
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
  
  
  