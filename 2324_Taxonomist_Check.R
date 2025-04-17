# Name Reconciliation -- QC taxonomist Comparision

rm(list=ls())
library(tidyverse)
library(sf)
library(tmap)
library(stringdist)
library(readxl)
library(lubridate)

################################################################################
# Data Files 
################################################################################

#fish collection file and results of laboratory QC
#######
# lab QC results
LAB_QA <- read_xlsx("QC Taxonomist Files/68HERC22F0307FinalDatabase_MBI_2025_03_26_v2.xlsx")%>%
  data.frame() %>%
  mutate(TAG = as.numeric(TAG.NUMBER))%>%
  mutate(DATE_COL = format(as.Date(as.character(DATE.COLLECTED), 
                                         "%Y-%m-%d"), "%m/%d/%Y"))%>%
  mutate(YEAR=substring(DATE_COL,nchar(DATE_COL)-3,nchar(DATE_COL)))%>%
  distinct(SITE.ID, VISIT.NUMBER, TAG.NUMBER, TAG, .keep_all = TRUE)

# Need to update TAG in fish collection file when field crews did not 
# properly record
fish_col <- read.table("nrsa2324_fishcollectionWide_fish_Corrected.tab",
                       sep = "\t") %>%
  mutate(YEAR = substring(DATE_COL, nchar(DATE_COL)-3, nchar(DATE_COL)))

# Unique index to reference fish_col 
if(anyDuplicated(fish_col[,c("SITE_ID","VISIT_NO","YEAR","LINE")])){
  stop("these shouldn't be duplicated")
} else {"Good, unique record identifer is SITE_ID,VISIT_NO,YEAR,LINE"}
#########################################

# identify records in fish collection file with missing or potentially 
# mislabeled tags
########
# Identify records in LAB_QA that do not have a matching record in fish 
# collection table. These will need to be investigated.
a <- merge(fish_col, LAB_QA,  
           by.y = c("SITE.ID", "VISIT.NUMBER", "YEAR", "TAG"), 
           by.x = c("SITE_ID", "VISIT_NO", "YEAR", "TAG"),
           all.y = T)

# a list of sites that submitted voucher identified by QC taxonomist 
# but missing a corresponding tag
sid <- unique(a[is.na(a$NAME_COM_CORRECTED),c("SITE_ID", "VISIT_NO", "YEAR")])
tagID <- list()
for (i in sid$SITE_ID){
  #i <- "NRS23_ID_10192"

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
#########################################

# Visually inspect tagID, identify which sites 1) used line numbers but 
# didn't record tag, 2) which submitted wrong tags but has a matching 
# identification or 3) those that need further investigation
# TAG.x is tag listed in fish collection file, TAG.Y is tag provided by 
# QC lab. 
tagID

# Field crews used lines as tags
#######
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

# Field crews recorded tags were different from lab
# lab tag is used as correction, based on matching 
# field and lab identifications
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
#########################

# these records were manually checked and corrected
#######

#"NRS23_MT_10005"
fish_col[fish_col$SITE_ID=="NRS23_MT_10005",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10005", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10005" &
           !is.na(fish_col$LINE)&fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(2), c("TAG")] <- c(1)

#"NRS23_MT_10032"
fish_col[fish_col$SITE_ID=="NRS23_MT_10032",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10032", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10032" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(4), c("TAG")] <- c(8)

#"NRS23_MT_10169"
fish_col[fish_col$SITE_ID=="NRS23_MT_10169",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10169", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10169" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(1), c("TAG")] <- c(1)

#NRS23_MT_10081
fish_col[fish_col$SITE_ID=="NRS23_MT_10081",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10081", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10081" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(3), c("TAG")] <- c(4)

#NRS23_MT_10013
fish_col[fish_col$SITE_ID=="NRS23_MT_10013",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MT_10013", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_MT_10013" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(1), c("TAG")] <- c(41)

#"NRS23_NV_10019"
fish_col[fish_col$SITE_ID=="NRS23_NV_10019",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_NV_10019", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_NV_10019" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(5), c("TAG")] <- c(1)

#"NRS23_CO_10094"
fish_col[fish_col$SITE_ID=="NRS23_CO_10094",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_CO_10094", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_CO_10094" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(5), c("TAG")] <- c(15)

#"NRS23_MO_10050"
# fish col does not ahve records for these specimens at the site 
# could these taxa been removed? 
fish_col[fish_col$SITE_ID=="NRS23_MO_10050",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MO_10050", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]

#NRS23_MO_10056
fish_col[fish_col$SITE_ID=="NRS23_MO_10056",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_MO_10056", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]

#NRS23_AR_10101
fish_col[fish_col$SITE_ID=="NRS23_AR_10101",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_AR_10101", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]
fish_col[fish_col$SITE_ID == "NRS23_AR_10101" &
           !is.na(fish_col$LINE) & fish_col$VISIT_NO==1&
           fish_col$LINE %in% c(25), c("TAG")] <- c(25)

#NRS23_KS_10026
fish_col[fish_col$SITE_ID=="NRS23_KS_10026",
         c("SITE_ID", "VISIT_NO", "TAG","LINE","VOUCH_PHOTO", 
           "VOUCH_UNK","VOUCH_QA", "NAME_COM_CORRECTED", "DATE_COL")]
a[a$SITE_ID=="NRS23_KS_10026", c("TAG","LINE","VISIT_NO","COMMON.NAME"),]

#######################################################

# Check results updates
#####
a <- merge(fish_col, LAB_QA,  
           by.y = c("SITE.ID", "VISIT.NUMBER", "YEAR", "TAG"), 
           by.x = c("SITE_ID", "VISIT_NO", "YEAR", "TAG"),
           all.y = T)

# sites that had a voucher identified by QC taxonomist 
# but not report in fish collection file...
sid <- unique(a[is.na(a$NAME_COM_CORRECTED),c("SITE_ID", "VISIT_NO", "YEAR")])
print(sid)
#####################

#fish_col$TAG_CORRECTED <- fish_col$TAG 

# write comparison between field and QC identifications
#####
LAB_QA <- read_xlsx("QC Taxonomist Files/68HERC22F0307FinalDatabase_MBI_2025_03_26_v2.xlsx")%>%
  data.frame() %>%
  mutate(TAG=as.numeric(TAG.NUMBER))%>%
  mutate(DATE_COL = format(as.Date(as.character(DATE.COLLECTED), 
                                   "%Y-%m-%d"), "%m/%d/%Y"))%>%
  mutate(YEAR=substring(DATE_COL, nchar(DATE_COL)-3, nchar(DATE_COL)))

# update common names to unknown when Lab ID was not possible, 
# See Comments provided by taxonomist
LAB_QA[is.na(LAB_QA$COMMON.NAME),"COMMON.NAME"] <- 
  paste("UNKNOWN", LAB_QA[is.na(LAB_QA$COMMON.NAME), "SCIENTIFIC.NAME"])

# this should be the most recent taxa list
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"
nars_taxa_list <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)

# need to confirm that the names match the TaxaList
NewFish <- LAB_QA %>%
  filter(!toupper(COMMON.NAME) %in% nars_taxa_list$FINAL_NAME) %>%
  select(COMMON.NAME) %>%
  distinct()%>%
  mutate(COMMON.NAME=toupper(COMMON.NAME))


#NRSA taxa list as vector
NARS_NAME_COM_CORRECTED <- nars_taxa_list$FINAL_NAME

Fuzzy_result <- sapply(NewFish$COMMON.NAME, 
                       function(x){stringdist(x, 
                                              NARS_NAME_COM_CORRECTED, 
                                              method = 'cosine', 
                                              q = 1)})
rownames(Fuzzy_result) <- NARS_NAME_COM_CORRECTED
#selects the 15 NRSA taxa with closest match
Fuzzy_result <- apply(Fuzzy_result, 2, 
                      function(x) list(names(x)[order(x)[1:15]]))

Fuzzy_result 
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "TESSELATED DARTER", "COMMON.NAME"] <- "TESSELLATED DARTER"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "CUTTHROAT TROUT X RAINBOW TROUT", "COMMON.NAME"] <- "CUTBOW"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "FATHEAD MINNNOW", "COMMON.NAME"] <- "FATHEAD MINNOW"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "WARMOUTH X BLUEGILL", "COMMON.NAME"] <- "BLUEGILL X WARMOUTH"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "SKIPKJACK HERRING", "COMMON.NAME"] <- "SKIPJACK HERRING"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "SACREMENTO SUCKER", "COMMON.NAME"] <- "SACRAMENTO SUCKER"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "SACREMENTO PIKEMINNOW", "COMMON.NAME"] <- "SACRAMENTO PIKEMINNOW"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "RED X BLACKTAIL SHINER", "COMMON.NAME"] <- "BLACKTAIL SHINER X RED SHINER"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "PUMPKINSEED X BLUEGILL", "COMMON.NAME"] <- "BLUEGILL X PUMPKINSEED"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "PEARL DACE", "COMMON.NAME"] <- "NORTHERN PEARL DACE"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "UNKNOWN PETROMYZONTIDAE", "COMMON.NAME"] <- "UNKNOWN LAMPREY"
LAB_QA[toupper(LAB_QA$COMMON.NAME) == "REDSPOTTED X SPOTTED SUNFISH", "COMMON.NAME"] <- "REDSPOTTED SUNFISH X SPOTTED SUNFISH"

# correct line/tag numbers in fish_col
FINAL_QC_Eval <- merge(fish_col[,c("SITE_ID", "VISIT_NO", "YEAR", "TAG", 
                                   "LINE","NAME_COM_CORRECTED",
                                   "VOUCH_PHOTO", "VOUCH_UNK","CREW", 
                                   "VOUCH_QA", "VOUCH_NUM")], LAB_QA,  
           by.y = c("SITE.ID", "VISIT.NUMBER", "YEAR", "TAG"), 
           by.x = c("SITE_ID", "VISIT_NO", "YEAR", "TAG"),
           all.y = T)

FINAL_QC_Eval$QC_Agree <- toupper(FINAL_QC_Eval$COMMON.NAME)==FINAL_QC_Eval$NAME_COM_CORRECTED
#the taxa from Missouri were not recoded in the field
FINAL_QC_Eval[is.na(FINAL_QC_Eval$QC_Agree),"QC_Agree"]<-FALSE
#############################################

# check names that are different, Confirm that the mismatches are not due to 
# wrong labels. It is very easy for field crews to write wrong label. In general,
# give the preference to QC taxonomist - They double checked disagreements. 
# did not in general did not adjust update hybrids, nor specimens that were 
# in the greatest shape. Also, check QC recommendation Name against ranges. 
# Sometimes the new neame is beyond the range and was not updated 
# DO NOT OVERWRITE. 
#write.csv(FINAL_QC_Eval, "QC Taxonomist Files/QC_Check_DK_XXXXXXXX.csv")


# duplicate records are new taxa that need to be added to fish_col file
# counts are proportionally allocated
########
New_Taxa <- FINAL_QC_Eval[duplicated(FINAL_QC_Eval[,c("SITE_ID", "VISIT_NO", "YEAR", "TAG")]),
                          c("SITE_ID", "VISIT_NO", "YEAR", "TAG")]
New_Taxa <- unique(New_Taxa)
NewRecords <- data.frame()
for (i in 1:nrow(New_Taxa)){
  #i <- 1
  new_taxa <- FINAL_QC_Eval[FINAL_QC_Eval$SITE_ID==New_Taxa$SITE_ID[i]& 
                  FINAL_QC_Eval$TAG==New_Taxa$TAG[i] & 
                  FINAL_QC_Eval$YEAR==New_Taxa$YEAR[i],
                  c("LINE","COMMON.NAME","NUMBER.OF.INDIVIDUALS")]%>%
    mutate(prop = NUMBER.OF.INDIVIDUALS/sum(NUMBER.OF.INDIVIDUALS))
  
  tmp <- fish_col[fish_col$SITE_ID==New_Taxa$SITE_ID[i] & 
                    !is.na(fish_col$TAG) & 
                    fish_col$TAG==New_Taxa$TAG[i] & 
                    fish_col$YEAR==New_Taxa$YEAR[i],]
    
  tmp <- tmp[rep(1,nrow(new_taxa)),]
  #update names
  ind <- tmp$NAME_COM_CORRECTED != toupper(new_taxa$COMMON.NAME)
  ind.2 <- toupper(new_taxa$COMMON.NAME) != tmp$NAME_COM_CORRECTED
  
  tmp[ind,c("NAME_COM_CORRECTED")] <- toupper(new_taxa$COMMON.NAME)[ind.2]
  
  tmp$LINE <- paste0(tmp$LINE, ".", seq(1:nrow(tmp)))
  
  vals <- round(new_taxa$prop[order(new_taxa$COMMON.NAME)]*
    tmp[order(tmp$NAME_COM_CORRECTED),
        c("COUNT_6", "COUNT_12", "COUNT_18","COUNT_19")])
  
  #avoids adding in zero taxa from rounding
  ind.zero <- apply(vals,1,function(x) sum(x,na.rm=T)>0)
  
  print(ind.zero)
  
  vals <- vals[ind.zero,]
  
  tmp[order(tmp$NAME_COM_CORRECTED),
      c("COUNT_6", "COUNT_12",
        "COUNT_18","COUNT_19")] <- vals
  NewRecords <- rbind(NewRecords,tmp)
}

#check total counts for new taxa
apply(NewRecords[,c("COUNT_6", "COUNT_12", "COUNT_18","COUNT_19")],1,sum, na.rm=T)

# inital rows
init<-nrow(fish_col)
rows2rm <- grep("\\.",rownames(NewRecords), value = T, invert = T)

# remove record from fish_collection table
fish_col <- fish_col[!rownames(fish_col) %in% rows2rm,]
fish_col <- rbind(fish_col,NewRecords)
###################################
#additional records added
dim(fish_col)

# add line corrections
fish_col[grep("\\.",fish_col$LINE),"LINE_CORRECTED"] <- fish_col[grep("\\.",fish_col$LINE),"LINE"]

# update disagreements to side with QC taxonomist.  
#######
QC_Eval <- read.csv("QC Taxonomist Files/QC_Check_DK_4092025.csv")
correctNames <- QC_Eval[!QC_Eval$QC_Agree&QC_Eval$Correct!="N",]
for (q in 1:nrow(correctNames)){
  #q<-1
  fish_col[fish_col$SITE_ID == correctNames$SITE_ID[q]&
  fish_col$VISIT_NO==correctNames$VISIT_NO[q]&
  fish_col$YEAR==correctNames$YEAR[q]&
  fish_col$LINE==correctNames$LINE[q]&
  fish_col$TAG==correctNames$TAG[q], "NAME_COM_CORRECTED"] <- 
    toupper(correctNames$COMMON.NAME[q])
}
###########################
#write.table(fish_col, "nrsa2324_fishcollectionWide_fish_Corrected.tab", sep="\t")


# Quick analysis of field crew proficiency, does not account for number of 
# specimens submitted 
########
#total agreement
mean(FINAL_QC_Eval$QC_Agree, na.rm = T)

# results by crews that submitted specimens
crewResults <- FINAL_QC_Eval %>%
  group_by(CREW) %>%
  summarise(a = mean(QC_Agree,na.rm=T))

p1 <- ggplot(crewResults[complete.cases(crewResults),])+
  geom_dotplot(aes(x = a, y = CREW), dotsize = 0.5,
               stackdir = "center")+
  theme_bw()+ 
  geom_vline(aes(xintercept=median(a)), 
             lty = 2, lwd = 1.25, col = "red")+
  labs(x = "Mean Agreement (%)", 
       title = "Average Agreement for Crew")


# #average agreement for sites 
# a <- FINAL_QC_Eval %>%
#   group_by(SITE_ID) %>%
#   summarise(a = mean(QC_Agree, na.rm = T))
# 
# p2 <- ggplot(a, aes(x = a))+
#   geom_histogram(binwidth = 0.125)+
#   theme_bw()+
#   geom_vline(aes(xintercept=median(a)), 
#              lty = 2, lwd = 1.25, col = "red")+
#   labs(x = "Mean Agreement(%)",y = "SITES (#)",
#        title = "Mean agreement for sites")

##############################################
print(p1)
#ggsave("FieldLabAgree.jpeg", p1, height = 5, width = 5)
