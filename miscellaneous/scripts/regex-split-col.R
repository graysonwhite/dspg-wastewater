library(dplyr)
library(tidyr)

epa_huc8 <- STATE_SINGLE_OR %>% 
  select(PGM_SYS_ACRNMS, PRIMARY_NAME, LATITUDE83, LONGITUDE83, HUC_CODE, FRS_FACILITY_DETAIL_REPORT_URL) %>% 
  extract(col = PGM_SYS_ACRNMS, into = c("PERMIT_TYPE", "PERMID_ID"), 
          regex = "^([^:]+):([^:]+)$", remove = FALSE) 
write.csv(epa_huc8, file = "EPA_HUC8.csv")

