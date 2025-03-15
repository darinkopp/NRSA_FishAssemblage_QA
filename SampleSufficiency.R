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
FISH_COMMENTS <- dir_NRSA_2324 %>%
  paste0("/raw/tabfiles/nrsa2324_comments.tab") %>%
  read.table(sep = "\t", header = T) %>%
  filter(SAMPLE_TYPE=="FISH")%>%
  filter(grepl("SAMPLING_COMMENT", FLAG))

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
       INDIV)) %>%
  rowwise() %>%
  mutate(CWIDTH_Sampled = RCH_LENGTH/TRCHLEN) %>%
  merge(., FISH_COMMENTS[,c("UID", "COMMENT")], 
        by = "UID", all.x = T)

#SAMPLE[SAMPLE$FISH_PROTOCOL == "" & SAMPLE$RCHWIDTH >= 13, "FISH_PROTOCOL"] <- "LG"
#SAMPLE[SAMPLE$FISH_PROTOCOL == "" & SAMPLE$RCHWIDTH<13,"FISH_PROTOCOL"] <- "SM"
#SAMPLE[!is.na(SAMPLE$RCH_LENGTH)&SAMPLE$RCH_LENGTH==15,
#c("SITE_ID", "FISH_SAMPLING_SUFFICIENT", "RCH_LENGTH")]

#query
SAMPLE_SUF <- SAMPLE_SUF %>%
  mutate(FISH_SAMPLING_SUFFICIENT_CORRECTED = 
           ifelse(
             !is.na(CWIDTH_Sampled) & FISH_SAMPLING==""&
                    FISH_PROTOCOL %in% c("SM_WADEABLE", "SM_NONWADEABLE",
                                         "LG_NONWADEABLE","LG_WADEABLE")&
                    CWIDTH_Sampled >= 0.90 &
                    TRCHLEN_FLAG == "N"|
               !is.na(CWIDTH_Sampled) & FISH_SAMPLING=="" &
               FISH_PROTOCOL %in% c("LG_NONWADEABLE","LG_WADEABLE")&
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
                TOTALINDV < 25,
              "NO-<50% OF REACH,<25 INDIV",
            
            # Large streams
            ifelse(
              !is.na(CWIDTH_Sampled) &
                    FISH_PROTOCOL %in% c("LG_NONWADEABLE", "LG_WADEABLE")&
                    CWIDTH_Sampled < 0.90 &
                    CWIDTH_Sampled >= 0.50 &
                    TRCHLEN_FLAG == "N" &
                    FISH_SAMPLING_SUFFICIENT != "Y"&
                    TOTALINDV < 500,
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
                   TOTALINDV < 500 ,
                   'NO-<20 CW SAMPLED,<500 INDIV',
                 
                ifelse(TRCHLEN_FLAG == "Y", 
                       paste0("NO-<40 CW TRCHLEN"),
                
                ifelse(is.na(CWIDTH_Sampled)|
                         !is.na(CWIDTH_Sampled) &
                                  FISH_SAMPLING %in% 
                         c("NO_PERMIT",	"PERMIT_RESTRICT", 
                           "EQUIPMENT_FAILURE", "NO_FISH_OTHER",
                           "NOT_FISHED_REACH_MIN"), "NOT FISHED", 
                
                 ifelse(is.na(CWIDTH_Sampled)|
                                !is.na(CWIDTH_Sampled) &
                                FISH_SAMPLING=="SITE_CONDITIONS",
                         "NO-SITE CONDITIONS", ""
                         
                       )))))))))))))) %>%
  data.frame()

#check to confirm all records have been assigned
if(all(!is.na(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED)) & 
   all(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED!="")){
  print ("All entries assigned sufficency class")
} else {
  stop("unassigned records, either NA or blank")
}

#results of assignments
addmargins(table(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED, 
                 SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED))


# compare results entered by field Crew to decision 
fewerFields<- c("UID","SITE_ID","FISH_SAMPLING_SUFFICIENT", 
                "FISH_SAMPLING", "FISH_PROTOCOL","TRCHLEN", 
                "RCHWIDTH", "TRCHLEN_CWIDTH", "TRCHLEN_FLAG",
                "RCH_LENGTH", "CWIDTH_Sampled", "TOTALINDV", 
                "SAMPLED_FISH","PRIM_GEAR","PRIM_LENGTH_FISHED",
                "SEC_GEAR","SEC_LENGTH_FISHED","COMMENT")


# check for agreements and dissagreements, send corrections directly 
# to Michelle integrate into database

# dissagreements 
view(SAMPLE_SUF[
  SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT != 
    substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED,1,1),
  fewerFields])

# agreements
view(SAMPLE_SUF[SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT == 
                  substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED,1,1),
                fewerFields])


# When crew said no but Screening said yes, most crews indicated that they did NO_FISH or NO_FISH_OBSERVED, or
# reported zero individuals without a comment. I am inclined to think they missunderstood the SAMPLING_SUFFICIENCY
view(SAMPLEwCOM[SAMPLEwCOM$FISH_SAMPLING_SUFFICIENT=="N" & substring(SAMPLEwCOM$SAMPLED_FISH,1,1)=="Y", fewerFields])
view(SAMPLEwCOM[SAMPLEwCOM$FISH_SAMPLING_SUFFICIENT=="Y" & substring(SAMPLEwCOM$SAMPLED_FISH,1,1)=="N", fewerFields])

SAMPLE_SUF[substring(SAMPLE_SUF$FISH_SAMPLING_SUFFICIENT_CORRECTED,1,2)=="NO", fewerFields]
