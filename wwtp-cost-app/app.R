# Load packages needed to run the application
library(shiny)
library(tidyverse)
library(shinythemes)

# UI

ui <- navbarPage(
    theme = shinytheme("flatly"),
    title = "Visualizing Data of WWTPs in rural Oregon",
    tabPanel(
        "tab 1: basic example tab",
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(
            plotOutput("distPlot")
        )
        ),
    tabPanel(
        "tab 2 (can have a different sidebar than tab 1 :-) )",
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

    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
