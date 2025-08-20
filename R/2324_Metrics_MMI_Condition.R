###########################
# NRSA 2324 Fish Assemblage Condition Assignments 

# These scripts were modified from Karen Blocksom and reflect the workflow used 
# to calculate metrics, MMI and assign condition for NRSA 2324.

# When Karen left, the watershed area and sampling sufficiency were not finalized.
# This meant new records needed to be added and others needed to up updated. 
# Below recalculates the metrics, MMI and condition assignments using the updated 
# data and then compares these results to the records in the database. Records are 
# updates or depricated accordingly. 

# 8/20/2025 Darin Kopp

###########################

source(sprintf('C:/Users/%s/local/SharedCode/fetchHeaderData.r', Sys.getenv('USERNAME')))
library(tidyverse)
library(aquametBio)

chan2324 <- odbcConnect('NRSA2324')
chanNRSA <- odbcConnect('ALL_THE_NRSA')

################################################################################
# Begin 
# Metric Calculation
################################################################################

# read in data from NARS IM
#############
sites <- dbGet('ALL_THE_NRSA', 'tblSITETRUTH', 
               where = "STUDY = 'NRSA' AND DSGN_CYCLE = '2023-24' AND IND_DOMAIN IN('HAND', 'CORE') AND PARAMETER IN('SITE_ID', 'HUC8', 'AG_ECO9')") |> 
  pivot_wider(id_cols = c('UNIQUE_ID', 'IND_DOMAIN'),
              names_from = 'PARAMETER', 
              values_from = 'RESULT')

visits <- dbGet('NRSA2324', 'tblVERIFICATION', 
                where = "PARAMETER IN('SITE_ID')") |> 
  pivot_wider(id_cols = 'UID',
              names_from = 'PARAMETER', 
              values_from = 'RESULT') |> 
  inner_join(sites, by = 'SITE_ID')


taxa <- dbGet('ALL_THE_NRSA', 'tblTAXA', 
              where = "ASSEMBLAGE_NAME = 'VERTEBRATES' AND GEOG_ID = 'ALL'") |> 
  pivot_wider(id_cols = 'TAXA_ID',
              names_from = 'PARAMETER', 
              values_from = 'RESULT')

native <- dbGet('ALL_THE_NRSA', 'tblTAXA', 
                where = "ASSEMBLAGE_NAME = 'VERTEBRATES' AND GEOG_TYPE = 'HUC8'") |> 
  pivot_wider(id_cols = c('TAXA_ID', 'GEOG_ID'),
              names_from = 'PARAMETER', 
              values_from = 'RESULT') |> 
  select(TAXA_ID, HUC8 = GEOG_ID, NON_NATIVE)

fishcnts <- dbGet('NRSA2324', 'tblFISHCNT') |> 
  pivot_wider(id_cols = c('UID', 'TAXA_ID'),
              names_from = 'PARAMETER', 
              values_from = 'RESULT')

# We do not calculate metrics for sites that were not sampled sufficiently. 
finfo <- dbGet('NRSA2324', 'tblFISHINFO', 
               where = "PARAMETER IN('FISH_SAMPLING', 'SAMPLED_FISH')") |> 
  pivot_wider(id_cols = 'UID',
              names_from = 'PARAMETER', 
              values_from = 'RESULT')|>
  filter(str_starts(SAMPLED_FISH, "Y"))
################################################### 

# calculate fish metrics using aquametbio function
##############

# select fish from sites that were sufficiently sampled to calculate metrics
fish_prep <- inner_join(visits, fishcnts, by = 'UID') |> 
  left_join(native, by = c('TAXA_ID', 'HUC8')) |>
  filter(UID %in% finfo$UID)


fish_mets <- calcAllFishMets(indf = fish_prep,
                             inTaxa = taxa,
                             sampID = 'UID',
                             dist = 'IS_DISTINCT', 
                             ct = 'TOTAL',
                             anomct = 'ANOM_CT',
                             taxa_id = 'TAXA_ID',
                             tol = 'TOLERANCE_NRSA',
                             tolval = 'TOL_VAL_EMAPW',
                             vel = 'VEL_NRSA',
                             habitat = 'HABITAT_NRSA', 
                             trophic = 'TROPHIC_NRSA',
                             migr = 'MIGR_NRSA',
                             nonnat = 'NON_NATIVE',
                             reprod = 'REPROD_NRSA', 
                             temp = 'TEMP_NRSA', 
                             family = 'FAMILY',
                             genus = 'GENUS',
                             comname = 'FINAL_NAME')

# UID 1701, reflects changes in sampling sufficiency
length(unique(fish_prep$UID)) 


# Identify UIDs where sampling sufficient but  detected no fish, the values in
# fish met were updated to 0 from NA
suff_nofish <- fish_prep |>
  group_by(UID) |>
  summarise(TOTAL = sum(as.numeric(TOTAL))) |>
  filter(TOTAL == 0 & UID %in% finfo$UID)

# Import into tblFISHMET
ins_fish_mets <- pivot_longer(fish_mets, 
                              cols = 2:length(fish_mets),
                              names_to = 'PARAMETER', 
                              values_to = 'RESULT') |> 
  mutate(RESULT = as.character(RESULT), 
         SAMPLE_TYPE = 'FISH',
         REASON = 'reCalculated from tblFISHCNT with updated SAMPLED_FISH') |>
  mutate(RESULT = ifelse(UID %in% suff_nofish$UID, '0', RESULT),
         REASON = ifelse(UID %in% suff_nofish$UID, 
                         'Set value to 0 when no fish but sufficiently sampled', 
                         REASON)) 
##################################################

# Compare the ins_fish_mets to values in database to identify records to insert,
# update or depracate. after initial run, there should be no records to change.  
##############
mets <- dbGet('NRSA2324', 'tblFISHMET') %>%
  filter(PARAMETER %in% unique(ins_fish_mets$PARAMETER))

# sites that are already in the database but may have a 
# new values because of changes in sampling sufficiency
updateMets <- merge(ins_fish_mets, 
                    mets, by = c("UID", "PARAMETER", "SAMPLE_TYPE"), 
                    all = T)
# check for deprecated entries - records that were excluded from new calculations 
# but appear in database 
updateMets[is.na(updateMets$RESULT.x) & !is.na(updateMets$RESULT.y),]

# check for new records -- records to be appended
nr <- updateMets |>
  filter(!is.na(RESULT.x) & is.na(RESULT.y)) |>
  select(-RESULT.y) |>
  rename(RESULT=RESULT.x)

# check for different records -- records to be updated (none identified)
updateMets |>
  filter(!is.na(RESULT.x) & 
           !is.na(RESULT.y) & 
           RESULT.x != RESULT.y) |>
  select(-RESULT.y) |>
  rename(RESULT=RESULT.x)

# check matching records -- no action needed
matching <- updateMets %>% 
  filter(!is.na(RESULT.x) & 
           !is.na(RESULT.y) & 
           RESULT.x == RESULT.y|
           is.na(RESULT.x) & 
           is.na(RESULT.y)) |>
  select(-RESULT.y) |>
  rename(RESULT=RESULT.x)

#check new records and matching records are the name as ins_fish_mets
nrow(rbind(nr, matching)) == nrow(ins_fish_mets)
##################################################
nrow(ins_fish_mets)
unique(nr$REASON)
# make updates in the database
# dbAdd('NRSA2324','tblFISHMET', nr)

# Update FISH_SAMPLING field to enable use of FISH_SAMPLING == 'NO_FISH' to select 
# sites that did not observe fish at a site
##############
# Karen used 
# suff_nofish <- filter(finfo, FISH_SAMPLING == 'NO_FISH' & str_starts(SAMPLED_FISH, 'YES'))
# this excludes sites where site was sufficiently sampled but crew did not record,
# NO_FISH.  Since this field had a purpose of selecting sites that did not detect fish,
# these records should be added to FISH info

update <- finfo %>%
  filter(UID %in% suff_nofish$UID & is.na(FISH_SAMPLING))|>
  select(-SAMPLED_FISH)|>
  mutate(FISH_SAMPLING = "NO_FISH")|>
  pivot_longer(FISH_SAMPLING, names_to = "PARAMETER", values_to = "RESULT")|>
  mutate(SAMPLE_TYPE = "FISH",
         REASON = "Set value to NO_FISH if SAMPLED_FISH = YES and TOTAL = 0",
         FLAG = "")
##################################################
# dbAdd("NRSA2324", 'tblFISHINFO', update)

################################################################################
# End 
# Metric Calculation. 
# If updates are needed they should occur at end of the MMI section below
################################################################################




################################################################################
# Begin
# MMI Calculation
################################################################################

# read data from NARS IM
###############
sites <- dbGet('ALL_THE_NRSA', 'tblSITETRUTH', 
               where = "STUDY = 'NRSA' AND DSGN_CYCLE = '2023-24' AND IND_DOMAIN IN('HAND', 'CORE') AND PARAMETER IN('SITE_ID', 'AG_ECO9')") |> 
  pivot_wider(id_cols = c('UNIQUE_ID', 'IND_DOMAIN'),
              names_from = 'PARAMETER', 
              values_from = 'RESULT')

wsarea <- dbGet('ALL_THE_NRSA', 'tblLANDSCAPE', where = "PARAMETER = 'WSAREASQKM'") |> 
  pivot_wider(id_cols = 'SITE_ID',
              names_from = 'PARAMETER', 
              values_from = 'RESULT')

visits <- dbGet('NRSA2324', 'tblVERIFICATION', where = "PARAMETER IN('SITE_ID')") |> 
  pivot_wider(id_cols = 'UID',
              names_from = 'PARAMETER', 
              values_from = 'RESULT') |> 
  inner_join(sites, by = 'SITE_ID') |> 
  left_join(wsarea, by = 'SITE_ID') |> 
  mutate(lwsarea = log10(as.numeric(WSAREASQKM)))

# We only calculate metrics and MMI for sites that were sufficiently sampled
finfo <- dbGet('NRSA2324', 'tblFISHINFO', 
               where = "PARAMETER IN('FISH_SAMPLING', 'SAMPLED_FISH')") |> 
  pivot_wider(id_cols = 'UID',
              names_from = 'PARAMETER', 
              values_from = 'RESULT')|>
  filter(str_starts(SAMPLED_FISH, "Y"))

fish_mets <- dbGet('NRSA2324', 'tblFISHMET') |>
  pivot_wider(id_cols = 'UID',
              names_from = 'PARAMETER',
              values_from = 'RESULT')
################################################

# calculate MMI
###############
fish_in <- inner_join(visits, fish_mets, by = 'UID') 

fish_mmi <- calcFishMMI(fish_in, 
                        sampID = 'UID', 
                        ecoreg = 'AG_ECO9', 
                        lwsarea = 'lwsarea')

# identify sites that were sampled sufficiently but collected no fish are either 
# not assessed if WSAREA <2km^2 (RESULT = NA) or have MMI =0 if WSAREA >2km^2 
suff_nofish <- filter(finfo, FISH_SAMPLING %in% c("NO_FISH_OBSERVED", 'NO_FISH') & 
                        str_starts(SAMPLED_FISH, 'YES'))

ins_mmi <- fish_mmi |>
  pivot_longer(cols = 3:length(fish_mmi), 
               names_to = "PARAMETER", values_to = "RESULT") |>
  inner_join(visits[,c("UID", "WSAREASQKM")], by = 'UID') |> 
  mutate(WSAREASQKM = as.numeric(WSAREASQKM))|>
  mutate(RESULT = ifelse(WSAREASQKM <= 2 & UID %in% suff_nofish$UID, NA, 
                         ifelse(WSAREASQKM > 2 & UID %in% suff_nofish$UID, '0', 
                                RESULT)),
         REASON = 'reCalculate Fish MMI w/ updated WSAREASQKM and SAMPLED_FISH') |> 
  select(-WSAREASQKM)|>
  mutate(SAMPLE_TYPE = 'FISH',
         RESULT = as.character(RESULT))
################################################

# check against existing records in database to identify which need to be updated
# or removed
###############
cur_mets <- dbGet('NRSA2324', 'tblFISHMET')%>%
  filter(PARAMETER %in% unique(ins_mmi$PARAMETER))

# merge to compare results
upd_mets <- merge(ins_mmi, cur_mets, 
                  by = c('UID', 'SAMPLE_TYPE', 'PARAMETER'), 
                  all = T)

# check for deprecated entries - records that were excluded from new calculations 
# but appear in database 
uids <- unique(upd_mets[is.na(upd_mets$RESULT.x) & !is.na(upd_mets$RESULT.y),"UID"])
visits[visits$UID%in%uids,]
finfo[finfo$UID%in%uids,]

# records to deprecate  
rm_records <- upd_mets|>
  filter(is.na(RESULT.x) & !is.na(RESULT.y)) |>
  mutate(REASON = 'SAMPLED_FISH == NO_FISH_OBSERVED & WSAREASQKM < 2') |>
  #note RESULT.y - to remove the records with dbCUT, they need to match the 
  # database exactly
  select(UID, PARAMETER, RESULT = RESULT.y, SAMPLE_TYPE, REASON)

# records to update/insert
nr <- upd_mets |>
  filter(RESULT.x != RESULT.y | !is.na(RESULT.x) & is.na(RESULT.y)) |>
  select(UID, PARAMETER, RESULT = RESULT.x, SAMPLE_TYPE, REASON)

# records that match
matching <- upd_mets |>
  filter(RESULT.x == RESULT.y|is.na(RESULT.x) & is.na(RESULT.y)) |>
  select(UID, PARAMETER, RESULT = RESULT.x, SAMPLE_TYPE, REASON)

#check to make sure all records are accounted for. 
nrow(nr) + nrow(matching) + nrow(rm_records) == nrow(ins_mmi)
################################################

# deprecate records with watershed area too small for fish
#dbCut('NRSA2324', 'tblFISHMET', rm_records)


# Insert and update records in database
# dbAdd("NRSA2324", 'tblFISHMET', nr) 

# run dbAdd in batches, the number used here is probablaly much lower 
# than required. Remember to turnoff zscalar!! 
for(i in 1:10){ 
  print(i)
  print(Sys.time())
  if(i==10){
    minRow <- (i-1)*600+1
    maxRow <- nrow(nr)
  }else{
    minRow <- (i-1)*600+1
    maxRow <- i*600
  }
  print(paste(minRow, maxRow))
  # rc <- dbAdd("NRSA2324", 'tblFISHMET', nr[minRow:maxRow,]) 
  print(rc)
}

################################################################################
# End 
# MMI Calculation
################################################################################


################################################################################
# Begin 
# Assign condition
################################################################################

# read data from NARS IM
########
prep_cond <- dbGet('NRSA2324', 'tblFISHMET', where = "PARAMETER IN('MMI_FISH', 'TOTLNIND')") |> 
  pivot_wider(id_cols = 'UID', 
              names_from = 'PARAMETER',
              values_from = 'RESULT') |> 
  inner_join(visits, by = 'UID') |> 
  select(UID, MMI_FISH, TOTLNIND, WSAREASQKM, AG_ECO9) |> 
  as.data.frame()
###########################################

# Assign Condition 
###########
fish_cond <- assignFishCondition(inMMI = prep_cond,
                                 sampID = 'UID',
                                 ecoreg = 'AG_ECO9',
                                 mmi = 'MMI_FISH',
                                 wsarea = 'WSAREASQKM',
                                 totlnind = 'TOTLNIND')


# initial check, sites that were not sufficiently sampled or that were seined 
# are not assessed. result should be NULL, indicating all records are equal 
check_cond <- full_join(fish_cond, finfo, by = 'UID') |> 
  mutate(COND_CHECK = case_when(
    str_starts(SAMPLED_FISH, 'YES') ~ FISH_MMI_COND,
    str_starts(SAMPLED_FISH, 'NO') ~ 'Not Assessed',
    SAMPLED_FISH == 'SEINING ONLY' ~ 'Not Assessed',
    is.na(SAMPLED_FISH) ~ 'Not Assessed',
    .default = '?'
  )) |> 
  filter(COND_CHECK != FISH_MMI_COND)

# Insert condition based on fish_cond because all values match
ins_cond <- fish_cond |> 
  select(UID, RESULT = FISH_MMI_COND) |> 
  mutate(PARAMETER = 'FISH_MMI_COND',
         REASON = 'reCalculated using aquametBio::assignFishCondition after updates to FISH_SAMPLED and WSAREA')

# if a site was sufficiently sampled but collected no fish and WS Area <2, 
# the MMI is NA, and the assignFishCondition() functions assigns these as 
# not assessed, update reason for why these records might need to be updated
ins_cond[ins_cond$UID%in%prep_cond[is.na(prep_cond$MMI_FISH),"UID"],
         "REASON"] <- 'Assigned not assessed because WSAREASQKM < 2 and NO_FISH'

# if a site was not sufficiently sampled then metrics were not calculated and 
# no condition assigned, UIDS in visits but not ins_cond were not assessed bc
# they were not sampled sufficiently
ins_na <- anti_join(visits, ins_cond, by = 'UID') |>
  select(UID) |>
  mutate(PARAMETER = 'FISH_MMI_COND',
         RESULT = 'Not Assessed',
         REASON = 'Assigned not assessed because not sufficiently sampled')

#rbind results
ins_cond <- rbind(ins_cond,ins_na)|>
  mutate(SAMPLE_TYPE = 'FISH')

#check all visits are assigned a condition or not assessed
all(visits$UID%in%ins_cond$UID)
unique(ins_cond$REASON)
###########################################


# compare condition results to what already exists in the database to 
# identify if any records need to be updated or removed. After run there all 
# tables will be null
###########

# existing data in database
cur_cond <- dbGet('NRSA2324', 'tblCONDITION') %>%
  filter(PARAMETER %in% "FISH_MMI_COND")

# merge to check for differences
upd_cond <- merge(ins_cond, cur_cond, 
                  by = c('UID', 'PARAMETER'), 
                  all = T)

# No records to deprecate  
upd_cond|>
  filter(is.na(RESULT.x) & !is.na(RESULT.y)) 

# records to update/insert
nr <- upd_cond |>
  filter(RESULT.x != RESULT.y| !is.na(RESULT.x) & is.na(RESULT.y)) |>
  select(UID, PARAMETER, RESULT = RESULT.x, REASON)

# records that match
matching <- upd_cond |>
  filter(RESULT.x == RESULT.y|is.na(RESULT.x) & is.na(RESULT.y)) |>
  select(UID, PARAMETER, RESULT = RESULT.x,REASON)

#check to make sure all records are accounted for. 
nrow(nr) + nrow(matching) == nrow(ins_cond)
# check reason for updates and number of rows to be updated
unique(nr$REASON)
nrow(nr)
###############################################
# dbAdd("NRSA2324", 'tblCONDITION', nr)

################################################################################
# Begin 
# Assign condition
################################################################################



################################################################################
# Begin 
# Any updates

################################################################################


