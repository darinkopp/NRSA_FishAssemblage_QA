# check list of taxa against the AFS accepted names. 
library(pdfsearch)
library(tidyverse)
library(readxl)

dir_NRSA_2324 <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/IM-TH007/data/im/nrsa2324"

# this should be the most recent taxa list
nars_taxa_list <- dir_NRSA_2324 %>%
  paste0("/data/tabfiles/nrsa2324_fish_taxa.tab") %>%
  read.table(sep = "\t", header = T)%>%
  mutate(deprecate = "", hybrid="") # maybe how to more efficiently query data

# nars_taxa_list[grep("YOY", nars_taxa_list$FINAL_NAME),"deprecate"] <- "2324"
# nars_taxa_list[grep("JUVENILE", nars_taxa_list$FINAL_NAME),"deprecate"] <- "2324"
# # this should be the most recent taxa list
# nars_taxa_list[grep(" X |HYBRID", nars_taxa_list$SPECIES), "hybrid"] <- "Y"
# nars_taxa_list[grep(" X |HYBRID", nars_taxa_list$GENUS), "hybrid"] <- "Y"
# view(nars_taxa_list[nars_taxa_list$hybrid=="Y",])

# Copied table name from AFS .pdf
AFSNames <- read_xlsx("Table_1.xlsx")
Taxa_2_search <- nars_taxa_list[nars_taxa_list$HERP!="A", c("TAXA_ID", "FINAL_NAME")]
Taxa_2_search$NRSA_SciName <- apply(nars_taxa_list[nars_taxa_list$HERP!="A",
                                                   c("GENUS", "SPECIES")], 
                                    1, paste0, collapse= " ")
Taxa_2_search$CommonNames <- str_to_title(unique(Taxa_2_search$FINAL_NAME))


# need to add Year of when the name was introduced
# we discussed also having a column of retired year 
AFSResult <- data.frame()
for (i in 1:nrow(Taxa_2_search)){
  # i<-17 
  # SearchTaxon <- "Trout-Perch"
  # AFSNames[c(927, 928, 929),]
  
  SearchTaxon <- Taxa_2_search$CommonName[i]
  
  x <- apply(AFSNames, 1, function(x) grep(SearchTaxon, x, ignore.case = T))
  x <- unlist(lapply(x, function(x) length(x)>0))
  if(SearchTaxon=="Livebearers"){x=0}
  if(sum(x) > 0){
    AFS_Index <- which(x)
    AFS_Line <- setNames(data.frame(AFSNames[AFS_Index,]), "AFS_Line")  
    
    
    #sometimes headers match
    rmheaders <- apply(AFS_Line, 1, function(x) nrow(data.frame(str_locate_all(x, "\\.")))>3)  
    AFS_Line <- setNames(data.frame(AFS_Line[rmheaders,]), "AFS_Line")
    
    AFS_SciName <- apply(AFS_Line, 1, function(x) str_extract(x, "[^.]+"))
    AFS_ComName <-
      apply(AFS_Line, 1, function(x){ #Locate All periods in a string
          #x<-AFS[1,]
          col <- data.frame(str_locate_all(x, "\\."))
          breaks <- diff(col[,1]) > 1
          # always want the second break
          if(sum(breaks)>1){
            breaks <- which(breaks)[2]
            newstr <- col[breaks, "start"]
            newstr <- substring(x, newstr+1, )
            #extract text before period
            newstr <- str_extract(newstr, "[^.]+")
          } else { 
            breaks <- which(breaks)
            newstr <- col[breaks, "start"]
            newstr <- substring(x, newstr+1, )
            #extract after last period
            newstr <- str_extract(newstr, "[^.]*$")
          }
          
          return(newstr)
    })
    
    # is there an exact match, only report 1  
    match <- SearchTaxon==trimws(AFS_ComName)
    
    if(sum(match)==1){
      AFS_SciName <- AFS_SciName[which(match)]
      AFS_ComName <- AFS_ComName[which(match)]
      AFS_Line <- AFS_Line[which(match),]
    }
    
    } else {
    AFS_Line <- NA
    AFS_SciName <- NA
    AFS_ComName <- NA
  }
  
  tmp <- data.frame(Taxa_2_search[i,], AFS_SciName, AFS_ComName, AFS_Line)
  AFSResult <- rbind(AFSResult, tmp)
}

# 235 taxa w/o direct match... 
view(AFSResult[is.na(AFSResult$AFS_SciName),])

#write.csv(AFSResult, "AFS_Autecology_Crosswalk.csv")




#OLD PDF Search
############
file <- "C:/Users/DKOPP/OneDrive - Environmental Protection Agency (EPA)/Projects/NRSA_FishQA/Names-of-Fishes-8-Table1.pdf"
Taxa_2_search <- str_to_title(unique(CommonNames))
Taxa_2_search[Taxa_2_search=="Spot"]
AFS_results <- keyword_search(file, keyword = Taxa_2_search, path = TRUE, surround_lines = 1)

data.frame(unique(AFS_results[duplicated(AFS_results$keyword), "keyword"]))
view(nars_taxa_list[nars_taxa_list$FINAL_NAME %in% "Spot", c("TAXA_ID","FINAL_NAME",
                                                            "FAMILY","GENUS","SPECIES", 
                                                            "HERP")])

names(AFS_results$line_text) <- AFS_results$keyword
head(AFS_results$line_text, 3)

# List of taxa without an AFS match. Most were not identified to species level, 
# hybrids, or had capitalized letters. No changes were made because they were included in prevous 
# taxa lists and surveys. 
# in the future, may be come argument to merging Common Carp with Mirror Carp 
# and changing Eastern Blacknose Dace to Blacknose Dace. Tennessee Darter could 
# be considered Snubnose Darter. Rocky mountain sculpin is also unrecognized but
# grandfathered in. 
noAFS <- toupper(Taxa_2_search[!Taxa_2_search %in% AFS_results$keyword])
view(nars_taxa_list[nars_taxa_list$FINAL_NAME %in% noAFS, c("TAXA_ID","FINAL_NAME",
                                                            "FAMILY","GENUS","SPECIES", 
                                                            "HERP")])

#nars_taxa_list[nars_taxa_list$FINAL_NAME %in% c("COMMON CARP", "BLACKNOSE DACE"),]
#nars_taxa_list[nars_taxa_list$FINAL_NAME %in% c("TENNESSEE DARTER", "SNUBNOSE DARTER"),]
#view(fish_Count_CONUS[fish_Count_CONUS$NAME_COM_CORRECTED %in% c("COMMON CARP","MIRROR CARP"),])
######################################################

