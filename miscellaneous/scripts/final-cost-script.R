# This script loads the dataset for modeling. We join the original usda_cost dataframe with the new dataframe from Chris.
# We also filter the observations that are upgrades here. We write the new dataframe as final_cost. 
# Finally, we join with our working_df to be able to get as many useful variables as possible. 

library(tidyverse)


# load old usda_cost
usda_cost <- read_csv("usda_cost.csv")

usda_cost <- usda_cost %>%
  select(-X1) %>%
  rename(Population = Population.x)

# load new data from chris
new_usda_cost <- read_excel("data/raw/usda-cost-2.xlsx")

new_usda_cost <- new_usda_cost %>%
  filter(`Project Type` %in% c("-", "Treatment Plant / Outfall"))
new_usda_cost$`Construction Cost` <- as.numeric(new_usda_cost$`Construction Cost`)

# join data
final_cost <- full_join(usda_cost, new_usda_cost)

final_cost <- final_cost %>%
  select(-`Project Type`)

final_cost$Entity <- toupper(final_cost$Entity)

# join with working_df and select relevent columns
working_df <- read_csv("working_df.csv")
final_cost <- left_join(final_cost, working_df, by = c("Entity" = "Legal_Name"))

final_cost <- final_cost[, -(80:90)]
final_cost <- final_cost[, -(13:15)]
final_cost <- final_cost %>%
  select(-Population.y, -note2, -done, -QCODE, -municipality, -`5000_gal_bill`)

# select very relevant cols

final_cost_small <- final_cost %>%
  select(1:12, Region.x, 58:72)

final_cost_small <- final_cost_small %>%
  rename(
    Population = Population.x,
    Latitude = Latitude.x,
    Longitude = Longitude.x,
    Region = Region.x
  )

# write csvs
# write.csv(final_cost, "final_cost.csv")
# write.csv(final_cost_small, "final_cost_small.csv")
