# Attempt to remove depulicated columns in `survey_2017`
library(tidyverse)

survey_2017_test <- survey_2017

survey_2017_test <- survey_2017_test[!duplicated(colnames(survey_2017_test))]
