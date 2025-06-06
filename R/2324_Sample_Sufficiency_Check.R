#Sampling sampling Sufficency 

# 4 sampleing protocals exist for fish assemblages. 
# For all streams, 40 channel widths is necessary to characterize assemblage
# the transect lengths must be 40CW | min(150) | max(4000)
# Flag transect length if <= 35CW | <145 (accommodates rounding)


# Small wadeable streams have a mean CW <13m; 
# -Sample 100% reach (reach is 40CW or 150m)


# Large Wadeable streams have mean CW >=13m; 
# - Minimum 50% of reach or 500 fish collected
 
# Small non-wadeable rivers have <13m;
# -Sample entire reach(100%)
 
# Sufficent >= 90% of reach sampled (accomodates rounding)
# Large non-wadeable rivers have mean CW >=13

# Site specific modifications can be made based on comments provided by field
# crews

library(tidyverse)

# Data
############
# NRSA IM directory
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)


# Fish Collection File -- provides number of individuals
INDIV <- read.table("Data/nrsa2324_fishcollectionWide_fish_Corrected.tab",
                      sep = "\t") %>%
  filter(LINE_CORRECTED != "DELETE") %>%
  rowwise() %>%
  mutate(TOTAL = sum(c(COUNT_6, COUNT_12, COUNT_18, COUNT_19),na.rm=T))%>%
  group_by(UID) %>%
  reframe(TOTALINDV = sum(TOTAL, na.rm = T)) %>%
  data.frame()

# Fish Comments for Sampling file
SAMPLING_COMMENT <- dir_NRSA_2324 %>%
  paste0("/raw/tabfiles/nrsa2324_comments.tab") %>%
  read.table(sep = "\t", header = T) %>%
  filter(SAMPLE_TYPE=="FISH")%>%
  filter(grepl("SAMPLING_COMMENT", FLAG))

FISH_SAMPLING_SUFFICIENT_COMMENT <- dir_NRSA_2324 %>%
  paste0("/raw/tabfiles/nrsa2324_comments.tab") %>%
  read.table(sep = "\t", header = T) %>%
  filter(SAMPLE_TYPE=="FISH")%>%
  filter(grepl("FISH_SAMPLING_SUFFICIENT", FLAG))

CrewLead <- dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_verificationWide.tab") %>%
  read.table(sep = "\t", header = T) %>%
  select(UID, CREW)

# site verification file
# calculate whether the Total Reach Length is appropriate 
TRCHLEN <- dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_verificationWide.tab") %>%
  read.table(sep = "\t", header = T) %>%
  rowwise() %>%
  mutate(TRCHLEN_CWIDTH = TRCHLEN/round(RCHWIDTH)) %>%
  select(c("UID", "TRCHLEN", "RCHWIDTH", "TRCHLEN_CWIDTH")) %>%
  # total reach length should never be less than 150m and could only be
  # <40CW if the reach length >4000m
  mutate(TRCHLEN_FLAG = ifelse(!is.na(TRCHLEN) & 
                                 TRCHLEN_CWIDTH < 35 &
                                 TRCHLEN < 4000 |
                                 TRCHLEN < 145 & 
                                 !is.na(TRCHLEN)
                                 ,"Y", "N"))

# fishinfo file
# contains values on reach length fished
SAMPLE <- dir_NRSA_2324 %>%
  paste0("/raw/tabfiles/nrsa2324_fishinfoWide.tab") %>%
  read.table(sep = "\t", header = T) 


# merge files together and calculate the 
# percent of total reach length fished. If the total reach length was inappropriate 
# or there was reach length fished was not reported, set the value to NA
SAMPLE_SUF <- Reduce(function(x, y) 
  merge(x, y, by = "UID", all.x = T), 
  list(SAMPLE, TRCHLEN, INDIV, CrewLead)) %>%
  rowwise() %>%
  # if the total reach was not ~40CW or RCH_LENGTH is NA, set to zero
  # to indicate that the reach was not sufficently sampled
  mutate(CWIDTH_Sampled = ifelse(!is.na(RCH_LENGTH) & TRCHLEN_FLAG=="N", RCH_LENGTH/TRCHLEN, NA)) %>%
  merge(., SAMPLING_COMMENT[,c("UID", "COMMENT")], 
        by = "UID", all.x = T)%>%
  merge(., FISH_SAMPLING_SUFFICIENT_COMMENT[,c("UID", "COMMENT")], 
        by = "UID", all.x = T) %>%
  # some records with NO_FISH had NA values for counts
  mutate(TOTALINDV = ifelse(is.na(TOTALINDV),0, TOTALINDV))%>%
  # FISH_SAMPLING cannot have NO_FISH or NO_FISH_OBSERVED but a count
  # suggests there is a mistake with the field data
  mutate(FISH_SAMPLING = ifelse(FISH_SAMPLING == "NO_FISH" & TOTALINDV > 0|
                                FISH_SAMPLING == "NO_FISH_OBSERVED" & TOTALINDV > 0, 
                                "", FISH_SAMPLING))

####################################################

# Logic statements for identifying sampling sufficency
###########
SAMPLE_SUF <- SAMPLE_SUF %>%
  mutate(FISH_SAMPLING_SUFFICIENT_CORRECTED = 
           ifelse(
             # YES-SUFFICIENT: sampled >90% of reach or >50% of the reach for
             # large systems and >500 individuals or report NO_FISH or 
             # NO_FISH_OBSERVED with TOTAL == 0 
             FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) & 
               CWIDTH_Sampled >= 0.90 |
             FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) & 
               CWIDTH_Sampled < 0.90 & CWIDTH_Sampled >= 0.50 &
               FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
               TOTALINDV > 500 | 
             FISH_SAMPLING %in% c("NO_FISH", "NO_FISH_OBSERVED"), 
             'YES-SUFFICIENT',
             
             
             # SMALL WADEABLE and SMALL NON_WADEABLE 
             
             
             # YES-50-90% OF REACH: Crew sampled 50-90% of the reach and indicated
             # that the site was sufficiently sampled. This was set
             # to give crews the benefit of the doubt
             ifelse(
               FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) & 
                 CWIDTH_Sampled < 0.90 & CWIDTH_Sampled >= 0.50 &
                 FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                 FISH_SAMPLING_SUFFICIENT == "Y",
               "YES-50-90% OF REACH",
              
               # YES-<50% OF REACH,>=25 INDIV: Sampled <50% of reach but recovered 
               # > 25 individuals, crew indicated that they sampled sufficiently 
              ifelse(
                 FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) &
                   CWIDTH_Sampled < 0.90 & CWIDTH_Sampled >= 0.5 &
                   FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                   FISH_SAMPLING_SUFFICIENT != "Y" &
                   TOTALINDV >= 25,
                 "YES->50% OF REACH,>=25 INDIV",
                 
                 ifelse(
                   FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) &
                     CWIDTH_Sampled < 0.90 & CWIDTH_Sampled >= 0.5 &
                     FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                     FISH_SAMPLING_SUFFICIENT != "Y" &
                     TOTALINDV < 25,
                   "NO->50% OF REACH,<25 INDIV",
                 
                 # NO-<40 CW SAMPLED: <50% of reach sampled
                 ifelse(
                   FISH_SAMPLING == "" & FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                     is.na(CWIDTH_Sampled) | 
                     FISH_SAMPLING == "" & FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                     CWIDTH_Sampled < 0.50, 
                   "NO-<40 CW SAMPLED",
                   
                  
                   # LARGE WADEABLE AND NONWADEABLE
                   
                   
                   # 'YES->20 CW SAMPLED,<500 INDIV': sampled >50% of reach and < 500 individuals
                   # but crew had indicated that they sampled sufficiently to characterize 
                   # the assemblage at the site
                   ifelse(
                     FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) &
                       FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
                       CWIDTH_Sampled < 0.90 & CWIDTH_Sampled >= 0.50 &
                       TOTALINDV <= 500 & FISH_SAMPLING_SUFFICIENT == "Y",
                     'YES->20 CW SAMPLED,<500 INDIV',
                     
                     # NO->20 CW SAMPLED, <500 INDIV: sampled >50% of reach and < 500 individuals
                     # but crew did not indicate that the sample sufficiently characterized 
                     # the assemblage at the site
                     ifelse(
                       FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) &
                         FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
                         CWIDTH_Sampled < 0.90 & CWIDTH_Sampled >= 0.50 &
                         TOTALINDV <= 500 & FISH_SAMPLING_SUFFICIENT != "Y"&
                         FISH_SAMPLING == "",
                       'NO->20 CW SAMPLED, <500 INDIV',
                       
                       # 'YES-<20 CW SAMPLED, >500 INDIV': Sampled <50% of the reach, recovered 
                       # >500 individuals and crew indicated that the site was sufficiently 
                       # sampled
                       ifelse(
                         FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) & 
                           FISH_PROTOCOL %in% c("LG_NONWADEABLE","LG_WADEABLE")&
                           CWIDTH_Sampled < 0.50 &
                           TOTALINDV > 500 &
                           FISH_SAMPLING_SUFFICIENT == "Y", 
                         'YES-<20 CW SAMPLED, >500 INDIV',
                         
                         # 'NO-<20 CW SAMPLED,>500 INDIV': Sampled <50% of the reach, recovered 
                         # >500 individuals and crew did not indicate that the site was sufficiently 
                         # sampled  
                         ifelse(
                           FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) &
                             FISH_PROTOCOL %in% c("LG_NONWADEABLE","LG_WADEABLE")&
                             CWIDTH_Sampled < 0.50 &
                             TOTALINDV > 500 & 
                             FISH_SAMPLING_SUFFICIENT != "Y",
                           'NO-<20 CW SAMPLED,>500 INDIV',
                           
                           # 'NO-<20 CW SAMPLED,<500 INDIV': Sampled <50% of the reach and recovered 
                           # <500 individuals. Violates protocol 
                           ifelse(
                             FISH_SAMPLING == "" & !is.na(CWIDTH_Sampled) &
                               FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
                               CWIDTH_Sampled < 0.50 &
                               TOTALINDV <= 500, 
                             'NO-<20 CW SAMPLED,<500 INDIV',
                             
                             ifelse(
                               FISH_SAMPLING == "" & 
                                 FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE") &
                                 is.na(CWIDTH_Sampled), 
                               "NO-<40 CW SAMPLED",
                             
                             
                             # FISH_SAMPLING
                           
                             
                             # aggregate all to site conditions
                             ifelse(
                               FISH_SAMPLING %in% c("NO_PERMIT", "PERMIT_RESTRICT", 
                                   "EQUIPMENT_FAILURE", "NO_FISH_OTHER",
                                   "NOT_FISHED_REACH_MIN","SITE_CONDITIONS"),
                               "NO-SITE CONDITIONS", ""
                               ))))))))))))) %>%
  data.frame()
####################################################

#Seining Only Sites
############
SAMPLE_SUF[SAMPLE_SUF$PRIM_GEAR == "" & !is.na(SAMPLE_SUF$PRIM_HAULS),
           "FISH_SAMPLING_SUFFICIENT_CORRECTED"] <- "SEINING ONLY"
####################################################

fewerFields<- c("UID","SITE_ID","FISH_SAMPLING_SUFFICIENT", 
                "FISH_SAMPLING", "FISH_PROTOCOL","TRCHLEN", 
                "RCHWIDTH", "TRCHLEN_CWIDTH", "TRCHLEN_FLAG",
                "RCH_LENGTH", "CWIDTH_Sampled", "TOTALINDV", 
                "FISH_SAMPLING_SUFFICIENT_CORRECTED","PRIM_GEAR",
                "PRIM_LENGTH_FISHED", "SEC_GEAR","SEC_LENGTH_FISHED", 
                "CREW", "COMMENT.x","COMMENT.y")

# check to confirm all records have been assigned
########
if(all(!is.na(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED)) & 
   all(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED != "")){
  print ("All entries assigned sufficency class")
} else {
  stop("unassigned records, either NA or blank")
  view(SAMPLE_SUF[SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED == "",])
  b <- SAMPLE_SUF[SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED == "","UID"]
  view(SAMPLE_SUF[SAMPLE_SUF$UID %in% b, c(fewerFields)] )
}
########################################

# check UIDs from site info
########
# GU and Selawik did not have record in site.info. 
# This is ok because they did not sample fish
SAMPLE_SUF[(!SAMPLE_SUF$UID%in%site.info$UID), "SITE_ID"]
#sites in site info that did not have fishing record
site.info[(!site.info$UID%in%SAMPLE_SUF$UID), "UID"]
########################################

# compare results entered by field Crew to criteria above 
############

# instances where crew reported that they sampled sufficiently but 
# we designated them as violated protocol,
# typically not setting up the proper transect length,
# and therefore were considered NO using objective criteria 

view(SAMPLE_SUF %>%
  filter(FISH_SAMPLING_SUFFICIENT == "Y" &
    substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 1, 1) == "N")%>%      
  select(fewerFields))


# checked output and manually corrected sites where crew indicated that
# they did not sample sufficiently, but still reported fish length. 
# Most were because of site conditions constraining efforts listed in 
# sufficiency comments

view(SAMPLE_SUF %>%
       filter(FISH_SAMPLING_SUFFICIENT == "N" &
                substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 1, 1) == "Y")%>%      
       select(fewerFields))


# Comments by crew indicated that site conditions prevented sufficient sampling
SAMPLE_SUF[SAMPLE_SUF$UID%in%c(2022901,2022910,2022700,2022804,2022901,2022910,
                               2023708,2021888,2021967,2022169,2022176,2022314,
                               2022431,2022815,2023150,2023211,2023241,2023250,
                               2023294,2023300,2023356,2023360,2023389,2023394,
                               2023412,2023433,2023506,2023526,2023626,2023681,
                               2023728,2023820,2023909,2024006,2024031,2024100,
                               2024129,2023170,2022234,2022844,2023819,2022660),
           "FISH_SAMPLING_SUFFICIENT_CORRECTED"] <- "NO-SITE CONDITIONS"

# Few disagreements did not have comments and were sent to field crews 
# Check_Sampling <- SAMPLE_SUF[
#   SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT == "N" & 
#     substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 1, 1) == "Y"|
#     SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT == "Y" & 
#     substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 1, 1) == "N", 
#   fewerFields]

#write.csv(Check_Sampling, "Check_Sampling.csv")
############################

# results of assignment
table <- addmargins(table(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 
                          SAMPLE_SUF$FISH_PROTOCOL))

#write.csv(table, "Data/Sampling_Suffiency_Summary.csv")
#write.csv(SAMPLE_SUF[,fewerFields], "Data/2324_Fish_Sampling_Sufficent.csv")
