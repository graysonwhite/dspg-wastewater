library(tidyverse)
library(leaflet)
library(maps)
library(tools)

working_df <- read_csv("working_df.csv")

popup_content <- paste("<b>", working_df$Common_Name, "</b></br>",
                       "Location: ", toTitleCase(tolower(working_df$Location)), ", ", working_df$City, ", ", working_df$State, "</br>",
                       "Basin: ", working_df$basin, "</br>",
                       "Technology: ", working_df$type1,
                       sep = "")

leaflet(working_df, options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~Longitude,
             lat = ~Latitude, 
             color = "maroon",
             opacity = 0.5,
             popup = popup_content,
             radius = 4) 
