#Sampling sampling Sufficency 

# 4 sampleing protocals exist for fish assemblages. 
# For all streams, 40 channel widths is necessary to characterize assemblage
# the transect lengths must be 40CW | min(150) | max(4000)
# Flag transect length if <= 35CW | <145 (accomodates rounding)


# Small wadeable streams have a mean CW <13m; 
# -Sample 100% reach (reach is 40CW or 150m)


# Large Wadeable streams have mean CW >=13m; 
# - Minimum 50% of reach or 500 fish collected
 
# Small non-wadeable rivers have <13m;
# -Sample entire reach(100%)
 
# Sufficent >= 90% of reach sampled (accomodates rounding)
# Large non-wadeable rivers have mean CW >=13
# -Sufficent = >500 individuals collected or >=95% of reach sampled

# Site specific modifications can be made based on comments provided by field
# crews

library(tidyverse)

# NRSA IM directory
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)


# Fish Collection File -- provides number of individuals
# this will change when database is updated
INDIV <-read.table("nrsa2324_fishcollectionWide_fish_Corrected.tab",
                      sep = "\t") %>%
  filter(LINE_CORRECTED != "DELETE") %>%
  rowwise() %>%
  mutate(TOTAL = sum(c(COUNT_6, COUNT_12, COUNT_18, COUNT_19)))%>%
  group_by(UID) %>%
  summarize(TOTALINDV = sum(TOTAL, na.rm = T)) %>%
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
  paste0("/raw/tabfiles/nrsa2324_crew.tab") %>%
  read.table(sep = "\t", header = T) %>%
  filter(PARAMETER == "CREW_LEADER") %>%
  mutate(CREW_LEADER = RESULT) %>%
  select(UID,CREW_LEADER)

# site verification file 
TRCHLEN <- dir_NRSA_2324%>%
  paste0("/raw/tabfiles/nrsa2324_verificationWide.tab") %>%
  read.table(sep = "\t", header = T) %>%
  rowwise() %>%
  mutate(TRCHLEN_CWIDTH = TRCHLEN/round(RCHWIDTH)) %>%
  select(c("UID", "TRCHLEN", "RCHWIDTH", "TRCHLEN_CWIDTH")) %>%
  # transect should never be less than 150m and could only be
  # <40CW if the reach length >4000m
  mutate(TRCHLEN_FLAG = ifelse(!is.na(TRCHLEN) & 
                                 TRCHLEN_CWIDTH < 35 &
                                 TRCHLEN < 4000 |
                                 TRCHLEN < 145 & 
                                 !is.na(TRCHLEN)
                                 ,"Y", "N"))

# fishinfo file
SAMPLE <- dir_NRSA_2324 %>%
  paste0("/raw/tabfiles/nrsa2324_fishinfoWide.tab") %>%
  read.table(sep = "\t", header = T) 

# merge files together and calculate the 
# percent of transect fished
SAMPLE_SUF <- Reduce(function(x,y) 
  merge(x, y, by = "UID", all.x=T), 
  list(SAMPLE, 
       TRCHLEN, 
       INDIV,
       CrewLead)) %>%
  rowwise() %>%
  mutate(CWIDTH_Sampled = RCH_LENGTH/TRCHLEN) %>%
  merge(., SAMPLING_COMMENT[,c("UID", "COMMENT")], 
        by = "UID", all.x = T)%>%
  merge(., FISH_SAMPLING_SUFFICIENT_COMMENT[,c("UID", "COMMENT")], 
        by = "UID", all.x = T)


# Assign sufficiency category 
SAMPLE_SUF <- SAMPLE_SUF %>%
  mutate(FISH_SAMPLING_SUFFICIENT_CORRECTED = 
           ifelse(
             !is.na(CWIDTH_Sampled) & FISH_SAMPLING==""&
                    FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE",
                                         "LG_NONWADEABLE","LG_WADEABLE")&
                    CWIDTH_Sampled >= 0.90 &
                    TRCHLEN_FLAG == "N"|
               !is.na(CWIDTH_Sampled) & FISH_SAMPLING == "" &
               FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
               CWIDTH_Sampled < 0.90 &
               CWIDTH_Sampled >= 0.50 &
               TRCHLEN_FLAG == "N" &
               TOTALINDV > 500 | 
               #is.na(CWIDTH_Sampled)& 
               FISH_SAMPLING %in% c("NO_FISH", "NO_FISH_OBSERVED"), 
             'YES-SUFFICIENT',

            ifelse(
              !is.na(CWIDTH_Sampled) &
                FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                CWIDTH_Sampled < 0.90 & 
                CWIDTH_Sampled >= 0.50 &
                FISH_SAMPLING_SUFFICIENT != "N"&
                TRCHLEN_FLAG == "N",
                "YES-50-90% OF REACH",
              
              ifelse(
                !is.na(CWIDTH_Sampled) &
                  FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                  CWIDTH_Sampled < 0.90 & 
                  CWIDTH_Sampled >= 0.50 &
                  TRCHLEN_FLAG == "N",
                "NO-<40 CW SAMPLED",
            
            ifelse(
              !is.na(CWIDTH_Sampled) &
                FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                CWIDTH_Sampled < 0.50 &
                CWIDTH_Sampled > 0 &
                TRCHLEN_FLAG == "N" &
                TOTALINDV >= 25,
              "YES-<50% OF REACH,>=25 INDIV",
                         
            ifelse(
              !is.na(CWIDTH_Sampled) &
                FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE")&
                CWIDTH_Sampled < 0.50 &
                CWIDTH_Sampled > 0 &
                TRCHLEN_FLAG == "N"&
                TOTALINDV < 25 &
                FISH_SAMPLING == "",
              "NO-<50% OF REACH,<25 INDIV",
            
            # Large streams
            ifelse(
              !is.na(CWIDTH_Sampled) &
                    FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
                    CWIDTH_Sampled < 0.90 &
                    CWIDTH_Sampled >= 0.50 &
                    TRCHLEN_FLAG == "N" &
                    FISH_SAMPLING_SUFFICIENT != "Y"&
                    TOTALINDV < 500 & 
                    FISH_SAMPLING == "",
                  'NO->20 CW SAMPLED, <500 INDIV',
              
              ifelse(
                !is.na(CWIDTH_Sampled) &
                  FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
                  CWIDTH_Sampled < 0.90 &
                  CWIDTH_Sampled >= 0.50 &
                  TRCHLEN_FLAG == "N" &
                  FISH_SAMPLING_SUFFICIENT == "Y"&
                  TOTALINDV < 500,
                'YES->20 CW SAMPLED,<500 INDIV',
                
              ifelse(
                  !is.na(CWIDTH_Sampled) &
                    FISH_PROTOCOL %in% c("LG_NONWADEABLE","LG_WADEABLE")&
                    CWIDTH_Sampled < 0.50 &
                    CWIDTH_Sampled > 0 &
                    TRCHLEN_FLAG == "N" &
                    FISH_SAMPLING_SUFFICIENT != "Y"&
                    TOTALINDV > 500,
                  'NO-<20 CW SAMPLED,>500 INDIV',
                  
                  ifelse(
                    !is.na(CWIDTH_Sampled) &
                      FISH_PROTOCOL %in% c("LG_NONWADEABLE","LG_WADEABLE")&
                      CWIDTH_Sampled < 0.50 &
                      CWIDTH_Sampled > 0 &
                      TRCHLEN_FLAG == "N" &
                      FISH_SAMPLING_SUFFICIENT == "Y"&
                      TOTALINDV > 500,
                    'YES-<20 CW SAMPLED, >500 INDIV',

               ifelse(
                 !is.na(CWIDTH_Sampled) &
                   FISH_PROTOCOL %in% c("LG_NONWADEABLE","LG_WADEABLE")&
                   CWIDTH_Sampled < 0.50 &
                   CWIDTH_Sampled > 0 &
                   TRCHLEN_FLAG == "N" &
                   TOTALINDV < 500 &
                   FISH_SAMPLING == "",
                   'NO-<20 CW SAMPLED,<500 INDIV',

                 ifelse(is.na(CWIDTH_Sampled)|
                                !is.na(CWIDTH_Sampled) & 
                          FISH_SAMPLING %in% 
                          c("NO_PERMIT",	"PERMIT_RESTRICT", 
                            "EQUIPMENT_FAILURE", "NO_FISH_OTHER",
                            "NOT_FISHED_REACH_MIN","SITE_CONDITIONS"),
                         "NO-SITE CONDITIONS", 
                        
                 ifelse(TRCHLEN_FLAG == "Y" &
                          FISH_SAMPLING == "", 
                        paste0("NO-<40 CW SAMPLED"),""
                         
                       ))))))))))))) %>%
  data.frame()

#check to confirm all records have been assigned
if(all(!is.na(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED)) & 
   all(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED != "")){
  print ("All entries assigned sufficency class")
} else {
  stop("unassigned records, either NA or blank")
  SAMPLE_SUF[SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED == "",]
}

#results of assignments
addmargins(table(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 
                 SAMPLE_SUF$FISH_PROTOCOL))


# compare results entered by field Crew to decision 
fewerFields<- c("UID","SITE_ID","FISH_SAMPLING_SUFFICIENT", 
                "FISH_SAMPLING", "FISH_PROTOCOL","TRCHLEN", 
                "RCHWIDTH", "TRCHLEN_CWIDTH", "TRCHLEN_FLAG",
                "RCH_LENGTH", "CWIDTH_Sampled", "TOTALINDV", 
                "FISH_SAMPLING_SUFFICIENT_CORRECTED","PRIM_GEAR",
                "PRIM_LENGTH_FISHED", "SEC_GEAR","SEC_LENGTH_FISHED", 
                "CREW_LEADER", "COMMENT.x","COMMENT.y")

# view disagreements
Check_Sampling <- SAMPLE_SUF[
  SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT == "N" & 
  substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 1, 1) == "Y"|
    SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT == "Y" & 
    substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED,1,1) == "N", 
  fewerFields]

#write.csv(Check_Sampling, "Check_Sampling.csv")

# checked output and manually corrected sites where crew indicated that
# they did not sample sufficiently, but still reported fish length. 
# Most were because of site conditions constraining efforts listed in 
# sufficency comements
SAMPLE_SUF[SAMPLE_SUF$UID%in%c(2022901,2022910,2022700,2022804,2022901,2022910,
                               2023708,2021888,2021967,2022169,2022176,2022314,
                               2022431,2022815,2023150,2023211,2023241,2023250,
                               2023294,2023300,2023356,2023360,2023389,2023394,
                               2023412,2023433,2023506,2023526,2023626,2023681,
                               2023728,2023820,2023909,2024006,2024031,2024100,
                               2024129,2023170),
           "FISH_SAMPLING_SUFFICIENT_CORRECTED"] <- "NO-SITE CONDITIONS"

# instances where crew reported that they sampled sufficiently violated protocal 
# and therefore were considered NO using objective criteria 

# Few disagreements did not have comments and were sent to 
# to field crews by Michelle. 