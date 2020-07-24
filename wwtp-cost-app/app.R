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
    theme = shinytheme("united"),
    title = "Visualizing Data of WWTPs in rural Oregon",
    tabPanel(
        "Overview",
        mainPanel(
            width = 12,
            h1("Rural Wastewater Facility Cost Modeling & Planning Tools"),
            h2("Overview"),
            p("This web-based application includes tools for cost modeling and planning your potential
              wastewater treatment plant. It also includes educational resources for funding your wastewater treatment
              plant and for learning about options in regards to a centralized wastewater treatment system.
              It was created during summer 2020 for the Data Science for the
              Public Good program at Oregon State University."),
            h2("Panels"),
            h3("Maps"),
            p("text"),
            h3("Education"),
            p("text"),
            h3("Funding Resources"),
            p("text"),
            h2("Creators"),
            p("This application was created by Jakob Oetinger, Amanda Reding, and Grayson White, under the supervision
              of Dr. Christine Kelly.")
        )
    ),
    tabPanel(
        "Data Visualizations & Maps",
        tabsetPanel(
            tabPanel("Wastewater Treatment Plants",
                     sidebarPanel(
                         sliderInput("capacityslider", label = "Dry Design Capacity", min = 0, max = 1, value = 1),
                         submitButton("Generate Plot")
                     ),
                     mainPanel(
                         plotlyOutput("map")
                     )
                     ),
            tabPanel("Septic"),
            tabPanel("Misc. Data Visualizations",
                     mainPanel(
                         width = 12,
                         div(plotOutput("plot1", width = 600, height = 500), align = "center")
                     )
                     )
        )
        ),
    tabPanel(
        "Education",
        sidebarPanel(),
        tabsetPanel(
            tabPanel("Treatment"),
            tabPanel("Collection")
        )
    ),
    tabPanel(
        "Funding Resources",
        sidebarPanel()
    ),
    tabPanel(
        "Costs Estimator",
        sidebarPanel()
    )
    )

# Server
server <- function(input, output) {
    df_sf <- st_as_sf(working_df, coords = c("Longitude", "Latitude"), crs = 4326)
    OR_sf <- us_boundaries(type = "state", states = "OR")
    
    reactive_df <- reactive({
        working_df %>%
            filter(dryDesignFlowMGD <= input$capacityslider)
    })

    
    p1 <- reactive({
        ggplot() +
        geom_sf(data = OR_sf, fill = "#009474") +
        geom_point(data = reactive_df(), aes(label = Common_Name, x = Longitude, y = Latitude)) +
        coord_sf() +
        theme_void() +
        labs(title = "Wastewater Facilities in Oregon")
    })

    output$map <- renderPlotly({
            ggplotly(p1())
    })
    
    output$plot1 <- renderPlot({
        working_df %>%
        mutate(
            type_plot = case_when(
                type1 %in% c("lagoons", "pre-aerated lagoons") ~ "lagoons",
                type1 %in% c("trickling filter", "trickling filter - high rate",
                             "trickling filter - low rate") ~ "trickling filter",
                type1 %in% c("activated sludge") ~ "activated sludge",
                type1 %in% c("extended aeration", "membrane bioreactor", "recirculating gravel filter", NA,
                             "STEP system", "oxidation ditch", "biological contactors") ~ "other/NA")) %>%
        filter(type_plot %in% c("lagoons", "trickling filter", "activated sludge")) %>%
        ggplot(aes(y = type_plot,
                   x = dryDesignFlowMGD,
                   color = type_plot,
                   fill = type_plot)) +
        scale_color_manual(values = c("steelblue", "goldenrod", "forestgreen")) +
        scale_fill_manual(values = c("steelblue", "goldenrod", "forestgreen")) +
        geom_violin(
            alpha = 0.4) +
        geom_point() +
        theme_bw() +
        labs(
            x = "Dry Design Flow (MGD)",
            y = "Technology",
            title = "Flow of WWTPs, Grouped by Technology"
        ) +
        theme(
            legend.position = "none"
        ) +
        xlim(0,1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
