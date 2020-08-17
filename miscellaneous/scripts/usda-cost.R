library(readxl)
usda_cost <- read_excel("data/raw/wastewater-projects.xlsx")

usda_cost$Entity <- sapply(usda_cost$Entity, toupper)
usda_cost$Entity[15] <- "PACIFIC CITY JOINT WATER-SANITARY AUTHORITY"

usda_cost <- usda_cost %>%
  left_join(working_df,
            by = c("Entity" = "Legal_Name")) %>%
  select(1:9, Latitude, Longitude)

# These are lat/long of the *city* not the exact of the plant
usda_cost$Latitude[6] <- 45.4832
usda_cost$Longitude[6] <- -118.8300 

usda_cost$Latitude[7] <- 43.6704
usda_cost$Longitude[7] <- -121.5036

usda_cost$Latitude[8] <- 44.4632
usda_cost$Longitude[8] <- -118.7099

usda_cost$Latitude[9] <- 43.3401
usda_cost$Longitude[9] <- -124.3301

usda_cost$Latitude[11] <- 45.9932
usda_cost$Longitude[11] <- -123.9226

usda_cost$Latitude[14] <- 44.2998
usda_cost$Longitude[14] <- -120.8345

usda_cost$Latitude[16] <- 45.7068
usda_cost$Longitude[16] <- -121.5281

usda_cost$Latitude[17] <- 44.8193
usda_cost$Longitude[17] <- -119.4211

usda_cost$Latitude[19] <- 45.2965
usda_cost$Longitude[19] <- -117.8080

usda_cost <- usda_cost %>%
  mutate(
    basic_treatment = case_when(
      Treatment %in% c("Anaerobic Lagoons, Chlorination, Dechlorization",
                       "Aerated Lagoons",
                       "Anaerobic Lagoons, Aerated Lagoons",
                       "Anaerobic Lagoons, Aerated Lagoons, Chlorination",
                       "Stabilization Ponds, Chlorination",
                       "Stabilization Ponds") ~ "Lagoons",
      Treatment %in% c("Activated Sludge",
                       "Activated Sludge, Phosphorus Removal, Ultraviolet",
                       "Chlorination, Sedimentation, Activated Sludge",
                       "Activated Sludge, Chlorination, Dechlorization, Sedimentation, Anaerobic Lagoons, Aerated Lagoons",
                       "Activated Sludge, Disinfection with Ozone",
                       "Ultraviolet, Activated Sludge") ~ "Activated Sludge",
      Treatment %in% c("Aeration, Disinfection",
                       NA,
                       "Chlorination",
                       "Sedimentation, Ultraviolet") ~ "Other"
    )
  )

# for Canyonville, so that line should read $13,032,416 for the total cost and $10,361,000 for construction.

usda_cost$`Construction Cost`[2] <- 10361000
usda_cost$`Total Cost`[2] <- 13032416

# write.csv(usda_cost, file = "usda_cost.csv")
