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
library(tidymodels)

# Load data -------------------------------------------------------------------------------------------------------------
final_cost_small <- read_csv("final_cost_small.csv")
working_df <- read_csv("working_df.csv")

working_df <- working_df %>%
    mutate(
        `Technology Type` = case_when(
            type1 %in% c("lagoons", "pre-aerated lagoons") ~ "Lagoons",
            type1 %in% c("trickling filter", "trickling filter - high rate",
                         "trickling filter - low rate") ~ "Trickling Filter",
            type1 %in% c("activated sludge") ~ "Activated Sludge",
            type1 %in% c("extended aeration", "membrance bioreactor",
                         "recirculating gravel filter", NA, "STEP system",
                         "oxidation ditch", "biological contactors") ~ "Other"
        ))

knitr::knit("cost_modeling.Rmd", quiet = TRUE)

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
            tabPanel("Capital Cost by Design Capacity",
                     sidebarPanel(
                         sliderInput("costSlider", label = "Total Cost (Million Dollars)", min = 0, max = 30, value = 30, step = 0.1),
                         sliderInput("mgdSlider", label = "Dry Design Capacity (MGD)", min = 0, max = 1, value = 1),
                         sliderInput("popSlider", label = "Population", min = 0, max = 10000, value = 10000),
                         checkboxGroupInput("typeCheckBox", label = "Technology Type",
                                            choices = list("Lagoons", "Activated Sludge", "Other"),
                                            selected = list("Lagoons", "Activated Sludge", "Other")),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         plotlyOutput("cap_mgd")
                     )
                    ),
            tabPanel("Wastewater Treatment Plants",
                     sidebarPanel(
                         sliderInput("capacityslider", label = "Dry Design Capacity (MGD)", min = 0, max = 1, value = 1),
                         checkboxGroupInput("wwtp_type", label = "Technology Type",
                                            choices = list("Lagoons", "Activated Sludge", "Trickling Filter", "Other"),
                                            selected = list("Lagoons", "Activated Sludge", "Trickling Filter", "Other")),
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
                         sliderInput("pop_cost_map", label = "Population",
                                     min = 0,
                                     max = 10000,
                                     value = 10000),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         leafletOutput("costmap"),
                         includeMarkdown("costmap_text.Rmd")
                     )
            ),
            tabPanel("Capital Cost by Year",
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
                     sidebarPanel(
                         checkboxGroupInput("tech_hist", label = "Technology Type",
                                            choices = list("Lagoons", "Activated Sludge", "Other"),
                                            selected = list("Lagoons", "Activated Sludge", "Other")),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         plotlyOutput("stacked_hist")
                     )
                     ),
            tabPanel("Technology Prevalence",
                     sidebarPanel(
                         checkboxGroupInput("barchart_tech", label = "Technologies to Include",
                                            choices = unique(working_df$type1),
                                            selected = unique(working_df$type1)),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         plotlyOutput("barchart")
                     )
                     ),
            tabPanel("Design Capacity by Technology Type",
                     sidebarPanel(
                         checkboxGroupInput("violin_tech", label = "Technologies to Include",
                                            choices = c("Lagoons", "Trickling Filter", "Activated Sludge"),
                                            selected = c("Lagoons", "Trickling Filter", "Activated Sludge")),
                         submitButton("Regenerate Plot")
                     ),
                     mainPanel(
                         plotOutput("violin", width = 600, height = 600)
                     )
            )
        )
        ),
    tabPanel(
        "Community Wastewater Treatment",
        tabsetPanel(
            tabPanel("Water quality",
                     mainPanel(
                         fluidRow(
                             column(12,
                                    includeMarkdown('water_quality.Rmd')
                                    ),
                             column(12,
                                    img(src='BOD5-totals.png', align = "center", height = '500px', width = '700px')
                                    )
                         )
                     )
            ),
            tabPanel("Centralized vs. decentralized",
                     mainPanel(
                         fluidPage(
                             includeMarkdown('central_vs_decentral.Rmd')
                         )
                     )
                     ),
            tabPanel("Centralized technologies",
                     mainPanel(
                         fluidPage(
                             includeMarkdown('central.Rmd')
                         )
                     )),
            tabPanel(
                "Collection",
                mainPanel(
                    fluidRow(
                        column(12,
                               includeMarkdown('collection.Rmd')
                               ),
                        column(12,
                               img(src='land-use.png', align = "center", height = '500px', width = '700px')
                               )
                    )
                )
            ),
            tabPanel(
                "Permits",
                     mainPanel(
                         fluidRow(
                             column(12,
                                    includeMarkdown('permits.Rmd')
                                    ),
                             column(12,
                                    img(src='NPDES-perms.png', align = "center", height = '500px', width = '700px')
                                    )
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
        sidebarPanel(
            numericInput("pop", label = "Population of your town", value = 0),
            numericInput("pop_density", label = "Population density (people/square mile) of your town", value = 0),
            radioButtons("treatment_type", label = "Intended Treatment Type",
                         choices = c(
                             "Lagoons", "Activated Sludge", "Other"
                         ),
                         selected = c(
                             "Lagoons"
                         )
                         ),
            radioButtons("has_pumps", label = "This WWTP will have pump(s)",
                         choices = c(
                             TRUE, FALSE
                         ),
                         selected = c(
                             TRUE
                         )
            ),
            submitButton("Regenerate Estimate")
        ),
        mainPanel(
            htmlOutput(outputId = "prediction")
        )
    )
    )

# Server ----------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
    df_sf <- st_as_sf(working_df, coords = c("Longitude", "Latitude"), crs = 4326)
    OR_sf <- us_boundaries(type = "state", states = "OR")
    
    cost_react <- reactive({ 
        cost 
    })
    
# Data Visualization & Maps ---------------------------------------------------------------------------------------------
    # Capital Cost by Dry Design Capacity -------------------------------------------------------------------------------
    cap_mgd_react_df <- reactive({
        final_cost_small %>%
            mutate(`Total Cost (Million Dollars)` = `Total Cost`/1000000,
                   `Dry Design Capacity` = dryDesignFlowMGD) %>%
            filter(`Total Cost (Million Dollars)` <= input$costSlider) %>%
            filter(`Dry Design Capacity` <= input$mgdSlider) %>%
            filter(Population <= input$popSlider) %>%
            filter(basic_treatment %in% input$typeCheckBox)
    })
    cap_mgd_static <- reactive({
        ggplot(cap_mgd_react_df(),
                             aes(x = `Dry Design Capacity`,
                                 y = `Total Cost (Million Dollars)`,
                                 label1 = Population)) +
        geom_point(color = "steelblue", size = 2) +
        theme_bw() +
        xlim(0,1) +
        ylim(0,30) +
        labs(title = "Dry Design Flow (MGD) by Total Cost",
             y = "Total Cost (Million Dollars)",
             x = "Dry Design Flow (MGD)")
    })
    
    output$cap_mgd <- renderPlotly({
        ggplotly(cap_mgd_static()) %>%
            layout(legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 y = -0.1)) %>%
            layout(autosize = F, width = 600, height = 600) %>%
            config(displayModeBar = F)
    })
        
    # Wastewater Treatment Plants ---------------------------------------------------------------------------------------
    reactive_df <- reactive({
        working_df %>%
            filter(`Technology Type` %in% input$wwtp_type) %>%
            filter(dryDesignFlowMGD <= input$capacityslider)
    })
    
    output$map <- renderLeaflet({
        leaflet(reactive_df(), options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude, 
                             color = "maroon",
                             opacity = 0.5,
                             popup = paste0("<b>", reactive_df()$Common_Name, "</b></br>",
                                            "Location: ", toTitleCase(tolower(reactive_df()$Location)), ", ", reactive_df()$City, ", ", reactive_df()$State, "</br>",
                                            "Basin: ", reactive_df()$basin, "</br>",
                                            "Technology: ", reactive_df()$type1),
                             radius = 4) 
    })
    # Septic-------------------------------------------------------------------------------------------------------------
    # 3. Cap cost by year -----------------------------------------------------------------------------------------------
    final_cost_small$Year <- as.numeric(substr(final_cost_small$Year, start = 1, stop = 4))
    final_cost_small <- final_cost_small %>%
        mutate(`Treatment Type` = basic_treatment) %>%
        mutate(`Total Cost (Million Dollars)` = `Total Cost` / 1000000)
    
    p_3_react_df <- reactive({
        final_cost_small %>%
            filter(Population <= input$pop_3) %>%
            filter(`Treatment Type` %in% input$tech_3)
    })
    
    p_3 <- reactive({
        ggplot(p_3_react_df(),
                  aes(text = paste0("Entity: ", toTitleCase(tolower(str_replace_all(final_cost_small$Entity, ", CITY OF", "")))),
                      x = Year,
                      y = `Total Cost (Million Dollars)`,
                      size = Population,
                      color = `Treatment Type`)) +
        geom_point(alpha = 0.75, position = "jitter") +
        scale_color_viridis_d() +
        scale_y_continuous(labels = comma) +
        theme_bw() +
        theme(
            legend.position = "bottom"
        ) +
        labs(color = "Treatment Type",
             title = "Total Cost of WWTP by Year, Sized by Population",
             y = "Total Cost (Million Dollars)",
             x = "Year Funded")
    })
    
    output$plot_3 <- renderPlotly(
        ggplotly(p_3()) %>%
            layout(legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 y = -0.1)) %>%
            layout(autosize = F, width = 600, height = 600) %>%
            config(displayModeBar = F)
    )
    
    # Stacked histogram (#5) --------------------------------------------------------------------------------------------
    hist_df <- reactive({
        working_df %>%
            mutate(`Treatment Type` = case_when(
                type1 %in% c("lagoons", "pre-aerated lagoons") ~ "Lagoons",
                type1 %in% c("trickling filter", "trickling filter - high rate",
                             "trickling filter - low rate") ~ "trickling filter",
                type1 %in% c("activated sludge") ~ "Activated Sludge",
                type1 %in% c("extended aeration", "membrane bioreactor", "recirculating gravel filter", NA,
                             "STEP system", "oxidation ditch", "biological contactors"
                ) ~ "Other")
            ) %>%
            filter(`Treatment Type` %in% input$tech_hist) %>%
            filter(Population <= 20000)
    })
    
    p_5 <- reactive({
        ggplot(hist_df(),
                  aes(x = Population,
                      fill = `Treatment Type`)) +
        geom_histogram(bins = 10) +
        scale_fill_viridis_d() +
        theme_bw() +
        theme(
            legend.position = "bottom"
        ) +
        labs(title = "Population Served by Treatment Type")
    })
    
    output$stacked_hist <- renderPlotly({
        ggplotly(p_5(), tooltip = c("count", "fill")) %>%
            layout(legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 y = -0.1)) %>%
            layout(autosize = F, width = 600, height = 600) %>%
            config(displayModeBar = F)
    })

    # Cost Map ----------------------------------------------------------------------------------------------------------
    costmap_react_final <- reactive({
        final_cost_small %>%
            dplyr::filter(`Total Cost` <= input$totalcost) %>%
            dplyr::filter(basic_treatment %in% input$cost_type) %>%
            filter(Population <= input$pop_cost_map)
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
                                 "<b>", toTitleCase(tolower(str_replace_all(costmap_react_final()$Entity, ", CITY OF", ""))), "</b></br>",
                                 "Treatment: ", costmap_react_final()$Treatment, "</br>",
                                 "Collection: ", costmap_react_final()$Collection, "</br>",
                                 "Discharge: ", costmap_react_final()$Discharge, "</br>",
                                 "Total Cost: ", dollar(costmap_react_final()$`Total Cost`), "</br>",
                                 "Construction Cost: ", dollar(costmap_react_final()$`Construction Cost`), "</br>"
                                 ),
                             radius = 4) 
    })
    
    # Technology Prevalence ---------------------------------------------------------------------------------------------
    reactive_barplot <- reactive({
        working_df %>%
            mutate(`Technology Type` = type1) %>%
            filter(`Technology Type` %in% input$barchart_tech)
    })
    
    bar_plot <- reactive({
        reactive_barplot() %>%
        ggplot(aes(fill = `Technology Type`,
                   x = 1)) +
        geom_bar() +
        coord_flip() +
        theme_void() +
        theme(
            legend.position = "bottom"
        ) +
        scale_fill_viridis_d(na.value = "grey50")
    })
    
    output$barchart <- renderPlotly({
        ggplotly(bar_plot(), tooltip = c("fill", "count")) %>%
            layout(legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 y = -0.1)) %>%
            layout(autosize = F, width = 600, height = 600) %>%
            config(displayModeBar = F)
    })
    
    # Violin plot -------------------------------------------------------------------------------------------------------
    violin_df <- reactive({
        working_df %>%
            mutate(
                type_plot = case_when(
                    type1 %in% c("lagoons", "pre-aerated lagoons") ~ "Lagoons",
                    type1 %in% c("trickling filter", "trickling filter - high rate",
                                 "trickling filter - low rate") ~ "Trickling Filter",
                    type1 %in% c("activated sludge") ~ "Activated Sludge",
                    type1 %in% c("extended aeration", "membrane bioreactor", "recirculating gravel filter", NA,
                                 "STEP system", "oxidation ditch", "biological contactors") ~ "other/NA")) %>%
            filter(type_plot %in% c("Lagoons", "Trickling Filter", "Activated Sludge")) %>%
            filter(type_plot %in% input$violin_tech)
    })
    
    output$violin <- renderPlot({
        violin_df() %>%
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

    
# Education
# Funding Resources
# Cost Estimator
    ## Set seed for reproducibility :-)
    set.seed(3737)
    
    ## Specify priors for each explanatory variable
    prior_dist <- rstanarm::normal(location = c(0.5, -0.5, -0.5, 0, 1),
                                   scale = c(0.25, 0.5, 0.5, 1, 0.5))
    
    ## Create model
    bayes_mod <-
        linear_reg() %>% 
        set_engine("stan", # This is the Bayesian engine
                   prior_intercept = rstanarm::normal(), 
                   prior = prior_dist) %>%
        translate()
    
    ## Fit model to data
    dat <- read_csv("dat.csv")
    bayes_fit <-
        bayes_mod %>%
        fit(log(total_cost) ~ log(Population) + log(pop_density) + basic_treatment + has_pumps,
            data = dat)
    
    ## Predict based on user inputs
    pred_bayes <- reactive({
        data.frame(Population = input$pop,
                   pop_density = input$pop_density,
                   basic_treatment = input$treatment_type,
                   has_pumps = as.logical(input$has_pumps)) 
    }) 
    
    prediction <- reactive({
        exp(predict(bayes_fit, pred_bayes()))
    })
    
    output$prediction <- renderUI({
        HTML(paste0("The predicted cost of your wastewater treatment plant is ", prediction()," dollars."))
    })
}

# Runs the application --------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
