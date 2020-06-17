library(tidyverse)
library(readxl)
osts_municipalities <- read_excel("data/christine-google-drive-data/Oregon Sewage Treatment Systems.xlsx", 
                                  sheet = "Municipalities")

osts_non_municipalities <- read_excel("data/christine-google-drive-data/Oregon Sewage Treatment Systems.xlsx", 
                                      sheet = "non municipalities")

osts_municipalities <- osts_municipalities %>%
  mutate(municipality = "yes")

osts_non_municipalities <- osts_non_municipalities %>%
  mutate(municipality = "no")

osts <- full_join(osts_municipalities, osts_non_municipalities)

saveRDS(osts, "data/cleaned-and-or-rds/osts.rds")

