# This script loads, cleans, and tidies the survey data and the osts/deq data. It then joins these two dataframes into
# a single dataframe called `working_df`. This dataframe includes variables from the survey data that I 
# considered to be key variables, but there are many more that could be included quite easily. 

# load necessary packages  --------------------------------------------------------------------------------------------
library(tidyverse)
library(readxl)

# loading in and cleaning the survey dataframes -----------------------------------------------------------------------
epa <- read.csv("data/raw/STATE_SINGLE_OR.csv")
#                          sheet = "STATE_SINGLE_OR")
colnames(epa) <- epa[1,]
epa <- epa[-1,]

epa_select <- epa[, c(3, 19, 32, 33)]

colnames(epa_select) <- c("PRIMARY_NAME", "HUC_CODE", "LATITUDE83", "LONGITUDE83")

epa_huc <- epa_select %>%
  select(PRIMARY_NAME, HUC_CODE, LATITUDE83, LONGITUDE83) 

epa_huc$PRIMARY_NAME <- str_replace_all(epa_huc$PRIMARY_NAME, "1", "PRIMARY_NAME")
epa_huc$HUC_CODE <- str_replace_all(epa_huc$HUC_CODE, "1", "HUC_CODE")
epa_huc$LATITUDE83 <- str_replace_all(epa_huc$LATITUDE83, "1", "LATITUDE83")
epa_huc$LONGITUDE83 <- str_replace_all(epa_huc$LONGITUDE83, "1", "LONGITUDE83")

epa_huc <- epa_huc %>%
  unite(PRIMARY_NAME, HUC_CODE, LATITUDE83, LONGITUDE83,
        sep = ", ", na.rm = TRUE)

epa_select <- left_join(epa_select, epa_huc)



# overwrites data file with file called `epa_select` in your global environment
 saveRDS(epa_select, "data/cleaned-and-or-rds/epa_select.rds")

# imports permit data file 
perms <- read.csv("data/raw/Permits.csv")

colnames(perms) <- perms[1,]
perms <- perms[-1,]

perms_select <- perms[, c(3:5, 7:25, 30:31)]

colnames(perms_select) <- c("Legal_Name", "Common_Name", "City", "treatmentClass", "collectionClass",
                            "basin", "subbasin", "stream", "permYr", "EPAref", "type1", "type2", "recycledReuse",
                            "emergencyOverflow", "recycledBiosolids", "yearRoundDischarge", "BOD5_monthly_lbs_day",
                            "BOD5_max_lbs_day", "TSS_monthly_average_lbs_day", "TSS_max_lbs_day", "dryDesignFlowMGD",
                            "massLoadFlowMGD", "note1", "note2")

perms <- perms_select %>%
  select(Legal_Name, Common_Name, City, treatmentClass, collectionClass,
         basin, subbasin, stream, permYr, EPAref, type1, type2, recycledReuse,
         emergencyOverflow, recycledBiosolids, yearRoundDischarge, BOD5_monthly_lbs_day,
         BOD5_max_lbs_day, TSS_monthly_average_lbs_day, TSS_max_lbs_day, dryDesignFlowMGD,
         massLoadFlowMGD, note1, note2) 

perms$Legal_Name <- str_replace_all(perms$Legal_Name, "1", "Legal_Name")
perms$Common_Name <- str_replace_all(perms$Common_Name, "1", "Common_Name")
perms$City <- str_replace_all(perms$City, "1", "City")
perms$treatmentClass <- str_replace_all(perms$treatmentClass, "1", "treatmentClass")
perms$collectionClass <- str_replace_all(perms$collectionClass, "1", "collectionClass")
perms$basin <- str_replace_all(perms$basin, "1", "basin")
perms$subbasin <- str_replace_all(perms$subbasin, "1", "subbasin")
perms$stream <- str_replace_all(perms$stream, "1", "stream")
perms$permYr <- str_replace_all(perms$permYr, "1", "permYr")
perms$EPAref <- str_replace_all(perms$EPAref, "1", "EPAref")
perms$type1 <- str_replace_all(perms$type1, "1", "type1")
perms$type2 <- str_replace_all(perms$type2, "1", "type2")
perms$recycledReuse <- str_replace_all(perms$recycledReuse, "1", "recycledReuse")
perms$emergencyOverflow <- str_replace_all(perms$emergencyOverflow, "1", "emergencyOverflow")
perms$recycledBiosolids <- str_replace_all(perms$recycledBiosolids, "1", "recycledBiosolids")
perms$yearRoundDischarge <- str_replace_all(perms$yearRoundDischarge, "1", "yearRoundDischarge")
perms$BOD5_monthly_lbs_day <- str_replace_all(perms$BOD5_monthly_lbs_day, "1", "BOD5_monthly_lbs_day")
perms$BOD5_max_lbs_day <- str_replace_all(perms$BOD5_max_lbs_day, "1", "BOD5_max_lbs_day")
perms$TSS_monthly_average_lbs_day <- str_replace_all(perms$TSS_monthly_average_lbs_day, "1", "TSS_monthly_average_lbs_day")
perms$TSS_max_lbs_day <- str_replace_all(perms$TSS_max_lbs_day, "1", "TSS_max_lbs_day")
perms$dryDesignFlowMGD <- str_replace_all(perms$dryDesignFlowMGD, "1", "dryDesignFlowMGD")
perms$massLoadFlowMGD <- str_replace_all(perms$massLoadFlowMGD, "1", "massLoadFlowMGD")
perms$note1 <- str_replace_all(perms$note1, "1", "note1")
perms$note2 <- str_replace_all(perms$note2, "1", "note2")

perms <- perms %>%
  unite(Legal_Name, Common_Name, City, treatmentClass, collectionClass,
        basin, subbasin, stream, permYr, EPAref, type1, type2, recycledReuse,
        emergencyOverflow, recycledBiosolids, yearRoundDischarge, BOD5_monthly_lbs_day,
        BOD5_max_lbs_day, TSS_monthly_average_lbs_day, TSS_max_lbs_day, dryDesignFlowMGD,
        massLoadFlowMGD, note1, note2,
        sep = ", ", na.rm = TRUE)

perms_select <- left_join(perms_select, perms)


# overwrites data file with file called `perms` in your global environment
 saveRDS(perms_select, "data/cleaned-and-or-rds/perms.rds")

# end of loading and tidying survey data -------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# Heavily reducing number of columns to key vars, merging epa and perms, -----------------------------------------------


epa_key_vars <- epa_select %>%
  select(PRIMARY_NAME, HUC_CODE, LATITUDE83, LONGITUDE83) %>%
  rename(Common_Name = PRIMARY_NAME)


perms_key_vars <- perms_select %>%
  select(Legal_Name, Common_Name, City, treatmentClass, collectionClass, basin, subbasin, stream, permYr, 
         EPAref, type1, type2, recycledReuse, emergencyOverflow, recycledBiosolids, yearRoundDischarge, 
         `BOD5_monthly_lbs_day`, `BOD5_max_lbs_day`, `TSS_monthly_average_lbs_day`,
         `TSS_max_lbs_day`, dryDesignFlowMGD, massLoadFlowMGD, note1, note2) 


key_survey_vars <- full_join(epa_key_vars, perms_key_vars,
                             by = c("Common_Name" = "Common_Name"))


# writes over perm_HUCs file with `key_survey_vars` from global environment
 write.csv(key_survey_vars, file = "perm_HUCs.csv")

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
