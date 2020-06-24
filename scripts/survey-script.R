library(tidyverse)
library(readxl)

survey_2017 <- read_excel("data/raw/Water_Rates_Survey_2017.xlsx", 
                          sheet = "CLEANED")
colnames(survey_2017) <- survey_2017[1,]
survey_2017 <- survey_2017[-1,]

saveRDS(survey_2017, "data/cleaned-and-or-rds/survey_2017.rds")


survey_2019 <- read_excel("data/raw/Water Rates Survey 2019_November 19, 2019_08.19.xlsx", 
                          sheet = "Cleaned")

survey_2019_select <- survey_2019[, c(1:4, 27,71, 137:162)]

# overwrites data file with file called `survey_2019` in your global environment
# saveRDS(survey_2019, "data/cleaned-and-or-rds/survey_2019.rds")
