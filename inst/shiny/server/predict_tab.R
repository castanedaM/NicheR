# Title:
# Description:
# Date Last Updated: 6/26/26


# Outputs -----------------------------------------------------------------

output$ell_version_predict <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  selectInput(inputId = "pred_ell_version",
              label = tagList(tags$span("Elliposid Version", class = "text-widget-title"),
                              tags$span(icon("circle-info"),
                                        title = "Select ellipsoid version to predict over",
                                        class = "tooltip-icon")
              ),

              choices = c("all", names(session_data$ellipsoid_list)))

})


output$advanced_settings_predict <- renderUI({

  box(title = tags$span("Advanced Prediciton Settings",
                        class = "text-tab-title"),
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(width = 8,
               tagList(tags$span("Truncation Level Adjustment",
                                 class = "text-widget-title"),
                       tags$span(icon("circle-info"),
                                 title = "Adjust the level of truncation within the current elliposid, this will this will truncate prediction inwards",
                                 class = "tooltip-icon"))),
        column(width = 4,
               numericInput(inputId = "adjust_trunc",
                            label = NULL,
                            value = 0.95,
                            min = 0.0001, max = 0.99999, step = 0.05)
        )
      )
  )


})



