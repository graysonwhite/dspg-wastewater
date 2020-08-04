# Load packages needed to run the application ---------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(shinythemes)
library(sf)
library(USAboundaries)
library(PNWColors)
library(plotly)
library(DT)
library(readxl)
library(scales)
library(markdown)
library(leaflet)
library(maps)
library(tools)
library(LaCroixColoR)
library(viridis)
library(shinyWidgets)

# Load data -------------------------------------------------------------------------------------------------------------
final_cost_small <- read_csv("final_cost_small.csv")
working_df <- read_csv("working_df.csv")

# UI --------------------------------------------------------------------------------------------------------------------
ui <- navbarPage(
    theme = shinytheme("united"),
    title = "Small Scale Wastewater Treatment in Oregon",
    tabPanel(
        "Overview",
        mainPanel(
            fluidPage(
                setBackgroundImage(
                    src = "bg25.png"
                ),
                fluidRow(
                    column(12,
                           includeMarkdown('overview.Rmd')
                           ),
                    column(12,
                           img(src='dspgosu.png', align = "center", height = '500px', width = '700px'))
                )
            )
            )
        ),
    tabPanel(
        "Data Visualizations & Maps",
        tabsetPanel(
            tabPanel("Capital Cost by Dry Design Capacity (MGD)",
                     sidebarPanel(
                         
                     )
                    ),
            tabPanel("Wastewater Treatment Plants",
                     sidebarPanel(
                         sliderInput("capacityslider", label = "Dry Design Capacity", min = 0, max = 1, value = 1),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         leafletOutput("map"),
                         includeMarkdown("wwtp_plot_text.Rmd")
                     )
                     ),
            tabPanel("Cost Map",
                     sidebarPanel(
                         sliderInput("totalcost", label = "Total Cost",
                                     min = 0,
                                     max = 22000000,
                                     value = 22000000),
                         checkboxGroupInput("cost_type", label = "Technology Type",
                                            choices = list("Lagoons", "Activated Sludge", "Other"),
                                            selected = list("Lagoons", "Activated Sludge", "Other")),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         leafletOutput("costmap"),
                         includeMarkdown("costmap_text.Rmd")
                     )
            ),
            tabPanel("Capital cost by year",
                     sidebarPanel(
                         sliderInput("pop_3", label = "Population",
                                     min = 0,
                                     max = 10000,
                                     value = 10000),
                         checkboxGroupInput("tech_3", label = "Technology Type",
                                            choices = list("Lagoons", "Activated Sludge", "Other"),
                                            selected = list("Lagoons", "Activated Sludge", "Other")),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         plotlyOutput("plot_3"),
                         includeMarkdown("capcost_year_text.Rmd")
                     )
                     ),
            tabPanel("Stacked Histogram",
                     setBackgroundColor("white"),
                     mainPanel(
                         div(plotlyOutput("stacked_hist"), align = "center")
                     )
                     ),
            tabPanel("Misc. Data Visualizations",
                     mainPanel(
                         width = 12,
                         div(plotOutput("plot1", width = 600, height = 500), align = "center"),
                         div(plotOutput("plot2", width = 600, height = 500), align = "center"),
                         div(plotOutput("plot3", width = 600, height = 500), align = "center")
                     )
                     ),
            tabPanel("Data",
                     mainPanel(
                         width = 12,
                         dataTableOutput("cost_data")
                     )
                     )
        )
        ),
    tabPanel(
        "Community Wastewater Treatment",
        tabsetPanel(
            tabPanel("Centralized vs. decentralized"),
            tabPanel("Centralized technologies"),
            tabPanel(
                "Collection",
                mainPanel(
                    fluidPage(
                        includeMarkdown('collection.Rmd')
                    )
                )
            ),
            tabPanel(
                "Permits",
                     mainPanel(
                         fluidPage(
                             includeMarkdown('permits.Rmd')
                         )
                     )
                     ),
            tabPanel(
                "Funding resources",
                mainPanel(
                    fluidPage(
                        includeMarkdown('funding.Rmd')
                    )
                )
            )
        )
    ),
    tabPanel(
        "Statistical Cost Model",
        sidebarPanel()
    )
    )

# Server ----------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
    df_sf <- st_as_sf(working_df, coords = c("Longitude", "Latitude"), crs = 4326)
    OR_sf <- us_boundaries(type = "state", states = "OR")
    
    reactive_df <- reactive({
        working_df %>%
            filter(dryDesignFlowMGD <= input$capacityslider)
    })
    
    cost_react <- reactive({ 
        cost 
    })
    
# Data Visualization & Maps ---------------------------------------------------------------------------------------------
    # Wastewater Treatment Plants ---------------------------------------------------------------------------------------
    popup_content <- paste0("<b>", working_df$Common_Name, "</b></br>",
                            "Location: ", toTitleCase(tolower(working_df$Location)), ", ", working_df$City, ", ", working_df$State, "</br>",
                            "Basin: ", working_df$basin, "</br>",
                            "Technology: ", working_df$type1)
    
    output$map <- renderLeaflet({
        leaflet(reactive_df(), options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude, 
                             color = "maroon",
                             opacity = 0.5,
                             popup = popup_content,
                             radius = 4) 
    })
    
    # Septic-------------------------------------------------------------------------------------------------------------
    
    # Misc. Data Visualizations -----------------------------------------------------------------------------------------
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
    
    
    output$plot2 <- renderPlot({
        working_df %>%
            filter(!is.na(basin)) %>%
            group_by(basin) %>%
            summarize(mgd = mean(dryDesignFlowMGD, na.rm = TRUE)) %>%
            ggplot(
                aes(x = reorder(basin, mgd), y = mgd)
            ) +
            geom_col(fill = "maroon") +
            coord_flip() +
            theme_bw() +
            labs(
                y = "Average Dry Design Flow (MGD)",
                x = "Basin",
                title = "Average Dry Design Flow, Grouped By Basin"
            )
    })
    
    point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
    output$plot3 <- renderPlot({
        cost %>%
            group_by(type1) %>%
            summarize(mean = mean(`Total Cost` / Population.x)) %>%
            ggplot(aes(x = reorder(type1, mean),
                       y = mean)) +
            geom_col(fill = "forest green") +
            annotate(geom = "text", x = 1, y = 2000, label = "n = 3") +
            annotate(geom = "text", x = 2, y = 3000, label = "n = 1") +
            annotate(geom = "text", x = 3, y = 4000, label = "n = 6") +
            labs(x = "Type of WWTP",
                 y = "Average Cost / Population ($/person)") +
            theme_bw() +
            scale_y_continuous(labels = point)
    })
    
    # 3. Cap cost by year -----------------------------------------------------------------------------------------------
    final_cost_small$Year <- as.numeric(substr(final_cost_small$Year, start = 1, stop = 4))
    final_cost_small <- final_cost_small %>%
        mutate(`Treatment Type` = basic_treatment)
    
    p_3_react_df <- reactive({
        final_cost_small %>%
            filter(Population <= input$pop_3) %>%
            filter(`Treatment Type` %in% input$tech_3)
    })
    
    p_3 <- reactive({
        ggplot(p_3_react_df(),
                  aes(text = paste0("Entity: ", toTitleCase(tolower(Entity))),
                      x = Year,
                      y = `Total Cost`,
                      size = Population,
                      color = `Treatment Type`)) +
        geom_point(alpha = 0.75, position = "jitter") +
        scale_color_viridis_d() +
        scale_y_continuous(labels = comma) +
        theme_bw() +
        theme(
            legend.position = "bottom",
            axis.text.y = element_text(angle = 90, vjust = 1, hjust=0)
        ) +
        labs(color = "Treatment Type",
             title = "Total Cost of WWTP by Year, Sized by Population",
             y = "Total Cost ($)",
             x = "Year Built")
    })
    
    output$plot_3 <- renderPlotly(
        ggplotly(p_3()) %>%
            layout(legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 y = -0.1)) %>%
            layout(autosize = F, width = 600, height = 600)
    )
    
    # Stacked histogram (#5) --------------------------------------------------------------------------------------------
    p_5 <- ggplot(final_cost_small,
                  aes(x = Population,
                      fill = `Treatment Type`)) +
        geom_histogram(bins = 10) +
        scale_fill_viridis_d() +
        theme_bw() +
        theme(
            legend.position = "bottom"
        ) +
        labs(title = "Population Served by Treatment Type")
    
    output$stacked_hist <- renderPlotly({
        ggplotly(p_5, tooltip = c("count", "fill")) %>%
            layout(legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 y = -0.1)) %>%
            layout(autosize = F, width = 600, height = 600)
    })
    
    # Data --------------------------------------------------------------------------------------------------------------
    output$cost_data <- renderDataTable({
        datatable(
            final_cost_small
        )
    })
    # Cost Map ----------------------------------------------------------------------------------------------------------
    costmap_react_final <- reactive({
        final_cost_small %>%
            dplyr::filter(`Total Cost` <= input$totalcost) %>%
            dplyr::filter(basic_treatment %in% input$cost_type)
    })
    
    pal <- colorFactor(c("red", "green", "blue"), unique(final_cost_small$basic_treatment))
    
    output$costmap <- renderLeaflet({
        leaflet(costmap_react_final(), options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude, 
                             color = ~pal(basic_treatment),
                             opacity = 0.5,
                             popup = paste0(
                                 "<b>", toTitleCase(tolower(costmap_react_final()$Entity)), "</b></br>",
                                 "Treatment: ", costmap_react_final()$Treatment, "</br>",
                                 "Collection: ", costmap_react_final()$Collection, "</br>",
                                 "Discharge: ", costmap_react_final()$Discharge, "</br>",
                                 "Total Cost: ", dollar(costmap_react_final()$`Total Cost`), "</br>",
                                 "Construction Cost: ", dollar(costmap_react_final()$`Construction Cost`), "</br>"
                                 ),
                             radius = 4) 
    })
    
# Education
# Funding Resources
# Cost Estimator
}

# Runs the application --------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
