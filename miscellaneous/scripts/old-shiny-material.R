# This document contains peices of the shiny app no longer in use, but that we may want to use at some point.

#### UI ####
# tabPanel("Misc. Data Visualizations",
#          mainPanel(
#              width = 12,
#              div(plotOutput("plot1", width = 600, height = 500), align = "center"),
#              div(plotOutput("plot2", width = 600, height = 500), align = "center"),
#              div(plotOutput("plot3", width = 600, height = 500), align = "center")
#          )
#          ),
# tabPanel("Data",
         # mainPanel(
         #     width = 12,
         #     dataTableOutput("cost_data")
         # )
#          )

#### Server ####
# Misc. Data Visualizations -----------------------------------------------------------------------------------------
# output$plot1 <- renderPlot({ 
#     working_df %>%
#     mutate(
#         type_plot = case_when(
#             type1 %in% c("lagoons", "pre-aerated lagoons") ~ "lagoons",
#             type1 %in% c("trickling filter", "trickling filter - high rate",
#                          "trickling filter - low rate") ~ "trickling filter",
#             type1 %in% c("activated sludge") ~ "activated sludge",
#             type1 %in% c("extended aeration", "membrane bioreactor", "recirculating gravel filter", NA,
#                          "STEP system", "oxidation ditch", "biological contactors") ~ "other/NA")) %>%
#     filter(type_plot %in% c("lagoons", "trickling filter", "activated sludge")) %>%
#     ggplot(aes(y = type_plot,
#                x = dryDesignFlowMGD,
#                color = type_plot,
#                fill = type_plot)) +
#     scale_color_manual(values = c("steelblue", "goldenrod", "forestgreen")) +
#     scale_fill_manual(values = c("steelblue", "goldenrod", "forestgreen")) +
#     geom_violin(
#         alpha = 0.4) +
#     geom_point() +
#     theme_bw() +
#     labs(
#         x = "Dry Design Flow (MGD)",
#         y = "Technology",
#         title = "Flow of WWTPs, Grouped by Technology"
#     ) +
#     theme(
#         legend.position = "none"
#     ) +
#     xlim(0,1)
# })
# 
# 
# output$plot2 <- renderPlot({
#     working_df %>%
#         filter(!is.na(basin)) %>%
#         group_by(basin) %>%
#         summarize(mgd = mean(dryDesignFlowMGD, na.rm = TRUE)) %>%
#         ggplot(
#             aes(x = reorder(basin, mgd), y = mgd)
#         ) +
#         geom_col(fill = "maroon") +
#         coord_flip() +
#         theme_bw() +
#         labs(
#             y = "Average Dry Design Flow (MGD)",
#             x = "Basin",
#             title = "Average Dry Design Flow, Grouped By Basin"
#         )
# })
# 
# point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
# output$plot3 <- renderPlot({
#     cost %>%
#         group_by(type1) %>%
#         summarize(mean = mean(`Total Cost` / Population.x)) %>%
#         ggplot(aes(x = reorder(type1, mean),
#                    y = mean)) +
#         geom_col(fill = "forest green") +
#         annotate(geom = "text", x = 1, y = 2000, label = "n = 3") +
#         annotate(geom = "text", x = 2, y = 3000, label = "n = 1") +
#         annotate(geom = "text", x = 3, y = 4000, label = "n = 6") +
#         labs(x = "Type of WWTP",
#              y = "Average Cost / Population ($/person)") +
#         theme_bw() +
#         scale_y_continuous(labels = point)
# })

# Data --------------------------------------------------------------------------------------------------------------
# output$cost_data <- renderDataTable({
#     datatable(
#         final_cost_small
#     )
# })
