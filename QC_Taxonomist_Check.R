# Name Reconciliation -- QC taxonomist Comparision

rm(list=ls())
library(tidyverse)
library(sf)
library(tmap)
library(stringdist)
library(readxl)

################################################################################
# Data Files 
################################################################################
library(lubridate)

#fish collection file
#######
LAB_QA <- read_xlsx("QC Taxonomist Files/68HERC22F0307FinalDatabase_MBI_2025_03_26_v2.xlsx")%>%
  data.frame() %>%
  mutate(TAG = as.numeric(TAG.NUMBER))%>%
  mutate(DATE_COL = format(as.Date(as.character(DATE.COLLECTED), 
                                         "%Y-%m-%d"), "%m/%d/%Y"))%>%
  mutate(YEAR=substring(DATE_COL,nchar(DATE_COL)-3,nchar(DATE_COL)))%>%
  distinct(SITE.ID, VISIT.NUMBER, TAG.NUMBER, TAG, .keep_all = TRUE)

# Need to update TAG in fish collection when field crews does not 
# properly record
fish_col <- read.table("nrsa2324_fishcollectionWide_fish_Corrected.tab",
                       sep = "\t") %>%
  filter(LINE_CORRECTED != "DELETE")%>%
  mutate(YEAR = substring(DATE_COL, nchar(DATE_COL)-3, nchar(DATE_COL)))

# Unique index to reference fish_col 
if(anyDuplicated(fish_col[,c("SITE_ID","VISIT_NO","YEAR","LINE")])){
  stop("theses shouldn't be duplicated")
}

# Identify records in LAB_QA that do not have a matching record in the fish 
# collection table. These will need to be investigated.
a <- merge(fish_col, LAB_QA,  
           by.y = c("SITE.ID", "VISIT.NUMBER", "YEAR", "TAG"), 
           by.x = c("SITE_ID", "VISIT_NO", "YEAR", "TAG"),
           all.y = T)

# a list of sites that had a voucher identified by QC taxonomist 
# but not a corresponding tag
sid <- unique(a[is.na(a$NAME_COM_CORRECTED),c("SITE_ID", "VISIT_NO", "YEAR")])
tagID <- list()
for (i in sid$SITE_ID){
  #i <- "NRS23_ID_10192"
  #i <- "NRS23_CO_10094"
  #i <- "NRS23_GA_10041"
  
  #lab tags for a site 
  labtags <- a[a$SITE_ID == i, c("TAG",  "COMMON.NAME", "VISIT_NO", "YEAR")]%>%
    mutate(COMMON.NAME = toupper(COMMON.NAME))
  
  # fish from a site that need tags 
  field <- fish_col[fish_col$SITE_ID == i & 
                      fish_col$YEAR == unique(labtags$YEAR) & 
                      is.na(fish_col$TAG)|
                      fish_col$SITE_ID == i & !is.na(fish_col$TAG)&
                      !fish_col$TAG%in%labtags$TAG,
                    c("SITE_ID", "YEAR", "TAG", "LINE","VISIT_NO", "NAME_COM_CORRECTED")]
  
  # the idea is to merge the two tables by their matching names, 
  # gives an idea of where the dissagreements are and how to fix them
  tmp <- merge(field, labtags, 
               by.x = c("NAME_COM_CORRECTED","YEAR", "VISIT_NO"), 
               by.y = c("COMMON.NAME", "YEAR", "VISIT_NO"))
  
  tagID[[i]]<-tmp
}

# Visually inspect tagID, identify which sites
# used line numbers and didn't record tag, which submitted wrong tags 
# but has a matching identification or those that need further investigation
# Copy the names of the list elements into the 3 categories below
tagID

#Field crews used lines as tags
#########
for (i in c("NRS23_GA_10041","NRS23_KS_10026","NRS23_KS_10088",
            "NRS23_MA_10016","NRS23_MN_HP002","NRS23_MO_10056",
            "NRS23_MT_10013","NRS23_TX_10306","NRS23_MT_10069")){
  #i<-"NRS23_GA_10041"
  id<-unique(tagID[[i]][c("VISIT_NO","SITE_ID","YEAR")])  
  
  fish_col[fish_col$YEAR==id$YEAR&
             fish_col$VISIT_NO==id$VISIT_NO&
             fish_col$SITE_ID==id$SITE_ID,"TAG"]<-
    fish_col[fish_col$YEAR==id$YEAR&
               fish_col$VISIT_NO==id$VISIT_NO&
               fish_col$SITE_ID==id$SITE_ID,"LINE"]
}
#########################

# transfer tags when id's match
#######
for (i in c("NRS23_AR_10101","NRS23_ID_10192","NRS23_MO_10016",
           "NRS23_MT_10081","NRS23_NE_10167",
           "NRS23_NY_HP002","NRS23_SD_10017",
           "NRS23_VT_10004")){
    
  #i<-"NRS23_ID_10192"
  id<-tagID[[i]]  
  for (j in 1:nrow(id)){
    #j<-1
    q<-id[j,]
    fish_col[fish_col$YEAR == q$YEAR &
               fish_col$VISIT_NO == q$VISIT_NO &
               fish_col$SITE_ID == q$SITE_ID&
               fish_col$NAME_COM_CORRECTED==q$NAME_COM_CORRECTED,
             "TAG"] <- q$TAG.y 
  }
}
#################################

#?
#"NRS23_MT_10005"
#############
fish_col[fish_col$SITE_ID=="NRS23_MT_10005",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10005", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10005" &
           !is.na(fish_col$LINE)&fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(2), c("TAG")] <- c(1)
###################################################
#"NRS23_MT_10032"
##############
fish_col[fish_col$SITE_ID=="NRS23_MT_10032",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10032", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10032" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(4), c("TAG")] <- c(8)
#################################################
#"NRS23_MT_10169"
#############
fish_col[fish_col$SITE_ID=="NRS23_MT_10169",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10169", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10169" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(1), c("TAG")] <- c(1)
########################################
#NRS23_MT_10081
#########
fish_col[fish_col$SITE_ID=="NRS23_MT_10081",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10081", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10081" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(3), c("TAG")] <- c(4)
########################################
#NRS23_MT_10013
####################
fish_col[fish_col$SITE_ID=="NRS23_MT_10013",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10013", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10013" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(1), c("TAG")] <- c(41)
##########################################
#"NRS23_NV_10019"
############
fish_col[fish_col$SITE_ID=="NRS23_NV_10019",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_NV_10019", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_NV_10019" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(5), c("TAG")] <- c(1)
################################################
#"NRS23_CO_10094"
#############
fish_col[fish_col$SITE_ID=="NRS23_CO_10094",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_CO_10094", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_CO_10094" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(5), c("TAG")] <- c(15)
########################################
#"NRS23_MO_10050"
#############
#fis col does not ahve records for these specimens at the site 
# could these taxa been removed? 
fish_col[fish_col$SITE_ID=="NRS23_MO_10050",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MO_10050", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]

#####################################
#NRS23_MO_10056
#############
fish_col[fish_col$SITE_ID=="NRS23_MO_10056",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MO_10056", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
###################################
#NRS23_AR_10101
########
fish_col[fish_col$SITE_ID=="NRS23_AR_10101",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_AR_10101", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_AR_10101" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(25), c("TAG")] <- c(25)
################################################
#NRS23_KS_10026
#########
fish_col[fish_col$SITE_ID=="NRS23_KS_10026",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_KS_10026", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
#########################################

# Check results updates
#####
a <- merge(fish_col, LAB_QA,  
           by.y = c("SITE.ID", "VISIT.NUMBER", "YEAR", "TAG"), 
           by.x = c("SITE_ID", "VISIT_NO", "YEAR", "TAG"),
           all.y = T)

# a list of sites that had a voucher identified by QC taxonomist 
# but not a corresponding tag
sid <- unique(a[is.na(a$NAME_COM_CORRECTED),c("SITE_ID", "VISIT_NO", "YEAR")])
print(sid)
#####################


LAB_QA <- read_xlsx("QC Taxonomist Files/68HERC22F0307FinalDatabase_MBI_2025_03_26_v2.xlsx")%>%
  data.frame() %>%
  mutate(TAG=as.numeric(TAG.NUMBER))%>%
  mutate(DATE_COL = format(as.Date(as.character(DATE.COLLECTED), 
                                   "%Y-%m-%d"), "%m/%d/%Y"))%>%
  mutate(YEAR=substring(DATE_COL,nchar(DATE_COL)-3,nchar(DATE_COL)))

# correct line/tag numbers in fish_col
FINAL_QC_Eval <- merge(fish_col, LAB_QA,  
           by.y = c("SITE.ID", "VISIT.NUMBER", "YEAR", "TAG"), 
           by.x = c("SITE_ID", "VISIT_NO", "YEAR", "TAG"),
           all.y = T)

FINAL_QC_Eval$QC_Agree <- toupper(FINAL_QC_Eval$COMMON.NAME)==FINAL_QC_Eval$NAME_COM_CORRECTED

FINAL_QC_Eval
mean(FINAL_QC_Eval$QC_Agree,na.rm = T)
# check for agreements.
#############
# Tags are sometimes unreliable,
# so compare species lists. 
# What proportion did field taxonomist get correct? 
nrow(LAB_QA)
id <- unique(LAB_QA[,c("SITE.ID", "VISIT.NUMBER")])
out <- setNames(data.frame(matrix(NA, nrow(id),4)), 
                c("SITE_ID","Vouch.Complete", "Fieldtaxonomist", "Agreement"))
for (i in 1:nrow(id)){
  # i <- 149
  out$SITE_ID[i] <- id$SITE.ID[i]
  LABID <- toupper(LAB_QA[LAB_QA$SITE.ID==id$SITE.ID[i] & 
                            LAB_QA$VISIT.NUMBER==id$VISIT.NUMBER[i],
                          "COMMON.NAME"])
  
  FIELDID <- unique(fish_col[fish_col$SITE_ID == id$SITE.ID[i]&
                               fish_col$VISIT_NO==id$VISIT.NUMBER[i],
                             "NAME_COM_CORRECTED"])
  
  taxonomist <- unique(fish_col[fish_col$SITE_ID == id$SITE.ID[i]&
                                  fish_col$VISIT_NO==id$VISIT.NUMBER[i],
                                "FISH_TAXONOMIST"])
  
  # one taxonomist entered abbreviated name 
  out$Fieldtaxonomist[i] <- taxonomist[1]
  
  if(length(FIELDID)>=length(LABID)){
    out$Vouch.Complete[i]<-"Y"
  } else {
    out$Vouch.Complete[i]<-"N"
  }
  
  Fieldtaxa = length(FIELDID)
  Labtaxa = length(LABID)
  out$Agreement[i] <- sum(FIELDID %in% LABID)/length(unique(c(LABID,FIELDID)))
}

mean(out$Vouch.Complete=="Y")

a <- out%>%
  filter (Vouch.Complete=="Y")

median(a$Agreement)

P1 <- ggplot(a, aes(x = Agreement))+
  geom_histogram(binwidth = 0.125)+
  theme_bw()+
  geom_vline(aes(xintercept=median(Agreement)), 
             lwd = 2)
##############################################
ggsave("FieldLabAgree.jpeg", P1, height = 5, width = 5)


