library(tidyverse)
library(sf)
library(USAboundaries)
library(PNWColors)

osts <- readRDS("data/cleaned-and-or-rds/osts.rds")

#Format data for plotting
plot_data <- osts %>%
  select(Flow, Latitude, Longitude) %>%
  mutate(Lagoons = str_detect(Flow, "lagoons"),
         Flow = str_replace_all(Flow, c(" MGD" = "", 
                                        "MGD" = "",
                                        " with lagoons" = "", 
                                        " " = "")))

plot_data$Flow <- fct_relevel(plot_data$Flow, 
                              c("<1","1-2","2-5","5-10",
                                "10-25","25-50",">50"))

#-------------------------------MAP 1----------------------------------------
#This map uses lat long data from the osts file, which means we can map flow
#rate to different aesthetics for better visualization. However, there is an
#issue with the data and/or with the projection so the points are not plotted 
#correctly and some points end up in the Pacific... 

#Download Oregon polygon
OR <- map_data("state", "OR")

#Plot treatment plants with size based on flow rate 
#(some points end up in the ocean, unsure if it's an issue with 
#the projection or with the data themselves)
ggplot() +
  geom_polygon(data = OR, 
               aes(x = long, y = lat), 
               fill = "#009474") +
  geom_point(data = plot_data, 
             aes(x = Longitude, y = Latitude,
                 color = Flow, shape = Lagoons)) +
  scale_color_manual(values = pnw_palette("Sunset")) +
  coord_quickmap() +
  theme_minimal() +
  labs(title = "Wastewater Facilities in Oregon")
  

#-------------------------------MAP 2----------------------------------------
#This map uses a separate shapefile from the EPA, so it does not have Flow
#rate data associated, and I can't see a common column to join them by.
#Also looks like there are plants that show up in one or the other but not both

#Import plants shapefile as an sf object
plants <- st_read("data/raw/EPA-shapefile/Environmental_Protection_Agency__EPA__Facility_Registry_Service__FRS__Wastewater_Treatment_Plants.shp")

#Download oregon sf object
OR_sf <- us_boundaries(type = "state", states = "OR")

#Plot treatment plants, no info about flow rate
#Points are plotted correctly here
ggplot() +
  geom_sf(data = OR_sf, fill = "#009474") +
  geom_sf(data = plants, color = "#41476b") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Wastewater Facilities in Oregon")
