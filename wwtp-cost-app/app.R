# Load packages needed to run the application
library(shiny)
library(tidyverse)
library(shinythemes)
library(sf)
library(USAboundaries)
library(PNWColors)
library(plotly)

# Load working data frame
working_df <- read_csv("working_df.csv")

# UI
ui <- navbarPage(
    theme = shinytheme("flatly"),
    title = "Visualizing Data of WWTPs in rural Oregon",
    tabPanel(
        "tab 1: basic example tab",
        mainPanel(
            plotlyOutput("map")
        )
        ),
    tabPanel(
        "tab 2: blank tab",
        sidebarPanel()
    ),
    tabPanel(
        "tab 3: contact info (doesn't have to have a sidebar)",
        tags$p(
            "our names and such"
        )
    )
    )

# Server
server <- function(input, output) {
    df_sf <- st_as_sf(working_df, coords = c("Longitude", "Latitude"), crs = 4326)
    OR_sf <- us_boundaries(type = "state", states = "OR")
    p1 <- ggplot() +
        geom_sf(data = OR_sf, fill = "#009474") +
        geom_point(data = working_df, aes(label = Common_Name, x = Longitude, y = Latitude)) +
        coord_sf() +
        theme_minimal() +
        labs(title = "Wastewater Facilities in Oregon")

    output$map <- renderPlotly({
        ggplotly(p1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
