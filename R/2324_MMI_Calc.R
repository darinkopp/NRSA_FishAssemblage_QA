# Calculate MMI
# needed epa token to install... see email
# devtools::install_github("USEPA/aquametBio")
rm(list = ls())
library(aquametBio)
library(tidyverse)

# vignette/workflow 
# https://github.com/USEPA/aquametBio/blob/main/vignettes/CalculateFishMetrics.Rmd

# NRSA DATA
########
# NRSA data directory
dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

# need site info for aggregated ecoregions
site.info <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_siteinfo.tab") %>%
  read.table(sep = "\t", header = T)%>%
  select("UID", "SITE_ID", "AG_ECO9","WSAREASQKM")%>%
  filter(!is.na(WSAREASQKM))


#fish Count file -- Prepared by KB/NARS IM team-- merged with Ecoregions
FishCnts <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fishcount_data.tab") %>%
  read.table(sep = "\t", header = T) %>%
  mutate(FINAL_CT = TOTAL) %>%
  select("UID", "TAXA_ID", "FINAL_CT", "ANOM_CT", "IS_DISTINCT", "NON_NATIVE","UNIQUE_ID")%>%
  inner_join(site.info, by = "UID")

#this should be the most recent taxa list
FishTaxa <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)
#########################################

# calculate fish MMI metrics
##########
# This function calculates metrics for each ecoregion's MMI.
outMets <- calcNRSA_FishMMImets(indata = FishCnts, 
                                inTaxa = FishTaxa, 
                                sampID = "UID",
                                ecoreg =  "AG_ECO9",
                                dist = "IS_DISTINCT", 
                                ct = "FINAL_CT",
                                taxa_id = "TAXA_ID", 
                                tol = "TOLERANCE_NRSA",
                                vel = "VEL_NRSA", 
                                habitat = "HABITAT_NRSA",
                                trophic = "TROPHIC_NRSA", 
                                migr = "MIGR_NRSA", 
                                nonnat = "NON_NATIVE",
                                reprod = "REPROD_NRSA", 
                                family = "FAMILY", 
                                genus = "GENUS",
                                comname = "FINAL_NAME")

#########################################
dim(outMets)
outMets[outMets$UID=="2021889",]
FishCnts[FishCnts$UID=="2021889",]

# calculate fish MMI
############
# We have to supply the log10 of watershed area because some metrics are adjusted for watershed size.
outMets.1 <- merge(outMets, site.info[, c("UID", "SITE_ID", "WSAREASQKM")], by = "UID")%>%
  mutate(LWSAREA = log10(WSAREASQKM), ECO9 = AG_ECO9)%>%
  select(-c("AG_ECO9", "WSAREASQKM"))

outMMI <- calcFishMMI(inMets = outMets.1, sampID = "UID", ecoreg = "ECO9", lwsarea = "LWSAREA")
#########################################

# assign condition
##########
# Additional variables, including watershed area (km2) and total number of individuals 
# collected would be required to assign condition only if we had missing MMI values.
outCond <- assignFishCondition(outMMI, sampID = "UID", ecoreg = "ECO9", mmi = "MMI_FISH")
#########################################
Data/2324_Fish_Sampling_Sufficent.csv
dim(outCond)
dim(outMMI)
names(Condition)


# identify/remove sites that were not sufficiently sampled 
Condition <- read.csv("Data/2324_Fish_Sampling_Sufficent.csv") %>%
  select("UID","FISH_SAMPLING_SUFFICIENT_CORRECTED", ) %>%
  left_join(site.info[,c("UID","SITE_ID","AG_ECO9")], by="UID") %>%
  left_join(outCond, by = "UID") %>%
  mutate(COND_CHECK = case_when(
    str_detect(FISH_SAMPLING_SUFFICIENT_CORRECTED, pattern = "YES") ~ FISH_MMI_COND,
    str_detect(FISH_SAMPLING_SUFFICIENT_CORRECTED, pattern = "NO") ~ "Not Assessed",
    str_detect(FISH_SAMPLING_SUFFICIENT_CORRECTED, pattern = "SEINING ONLY") ~ "Not Assessed"))

#
Condition[is.na(Condition$COND_CHECK),"COND_CHECK"]<-"Poor"

# there are some UIDs that appear to be incorrect
sum(Condition$COND_CHECK=="Not Assessed")
Condition[is.na(Condition$COND_CHECK),"FISH_SAMPLING_SUFFICIENT_CORRECTED"]
Condition[is.na(Condition$COND_CHECK)&Condition$FISH_SAMPLING_SUFFICIENT_CORRECTED=="YES-<20 CW SAMPLED, >500 INDIV","UID"]
FishCnts[FishCnts$UID==2022660,]
Condition[Condition$UID==2022660,]
INDIV[INDIV$UID%in%c(2022660, 2023413, 2023413, 2023802, 2023845),]

table(Condition$FISH_SAMPLING_SUFFICIENT_CORRECTED)
table(Condition$COND_CHECK)

prop.table(table(Condition$COND_CHECK[Condition$AG_ECO9=="TPL"]))


library(ggplot2)
library(ggpattern)
a <- tapply(Condition$COND_CHECK, Condition$AG_ECO9, function(s) round(prop.table(table(s)),3))
b <- do.call(rbind, lapply(a, function(x) data.frame(x)))

b$ECO <- substring(rownames(b), 0, 3)
b <- rbind(b,setNames(data.frame(round(prop.table(table(Condition$COND_CHECK)),2), 
                    ECO = "ALL"),c("s","Freq", "ECO")))

b$s <- factor(b$s, levels = c("Not Assessed", "Poor", "Fair","Good"))
P1 <- ggplot(b, aes(x = ECO, 
                     y = Freq, 
                     fill = s))+
  geom_bar(stat="identity", 
           color='black') +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(
    values = c("orange","white", "grey", "black")) +
  
  labs(
    title = "Fish assemblage condition as proportion of sites (unweighted)", 
    fill = "") + 
  theme(
    legend.title = element_text(hjust = 0.1), 
    legend.position = "top")+
  ylab("Percentage of Sites") +
  xlab("Ecoregion") 

ggsave(
  filename = "Figures/FishAssemblageCondition.jpeg", 
  P1, 
  height = 5, 
  width = 6, 
  units = "in")


reshape2::dcast(s~ECO,data=b, value.var = "Freq")


sum(is.na(Condition$SITE_ID))
Condition <- Condition[!is.na(Condition$SITE_ID),]
Condition$STATE <- unlist(lapply(strsplit(Condition[,"SITE_ID"],"_"),"[[",2))

tapply(Condition$COND_CHECK, Condition$STATE, function(s) round(prop.table(table(s)),3))


a <- tapply(Condition$COND_CHECK, Condition$STATE, function(s) mean(s=="Not Assessed"))
a <- data.frame(a,state=names(a))
a$state<-factor(a$state,levels = a$state[order(a$a)])
mean(Condition$COND_CHECK=="Not Assessed")

P2 <- ggplot(a)+
  geom_dotplot(aes(x = a, y = state), dotsize = 0.75,
             stackdir = "center")+
  theme_bw()+
  labs(x = "Proportion of sites not assessed", 
       title = "Not Assessed by State")+
  theme(text = element_text(size = 20))

ggsave("Figures/NotAssessed_State.jpeg", P2, height = 11, width = 8)

