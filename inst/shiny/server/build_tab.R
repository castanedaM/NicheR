# Title: Build tab server logic
# Description: Handles range inputs and e-space plot
# Date last updated: 05/28/2026

# Render Outputs ----------------------------------------------------------

output$range_manual_ui <- renderUI({
  req(session_data$sel_raster)
  vars <- names(session_data$sel_raster)

  header <- fluidRow(
    column(width = 4, tags$b("Variable")),
    column(width = 4, tags$b("Min")),
    column(width = 4, tags$b("Max"))
  )

  rows <- lapply(vars, function(v) {
    fluidRow(
      column(width = 4, class = "var-label", tags$span(v)),
      column(width = 4,
             numericInput(paste0("min_", v), label = NULL, value = 0, step = 0.01)),
      column(width = 4,
             numericInput(paste0("max_", v), label = NULL, value = 1, step = 0.01))
    )
  })

  tagList(
    header,
    rows,
    fluidRow(column(12, class = "btn-spaced",
                    actionButton("save_ranges", "Save Ranges", class = "btn-primary")))
  )
})

output$build_espace_plot <- renderPlot({
  req(session_data$sel_df)

  df   <- session_data$sel_df
  vars <- names(session_data$sel_raster)
  vars <- vars[seq_len(min(length(vars), 3))]
  df   <- df[, vars, drop = FALSE]

  if(nrow(df) > 5000) df <- df[sample(nrow(df), 5000), , drop = FALSE]

  pairs(df,
        pch         = 16,
        cex         = 0.3,
        col         = adjustcolor("steelblue", alpha.f = 0.4),
        upper.panel = NULL)
})

# Observer Events ---------------------------------------------------------

observeEvent(input$save_ranges, {
  vars <- names(session_data$sel_raster)

  session_data$ranges <- data.frame(
    min = vapply(vars, function(v) input[[paste0("min_", v)]], numeric(1)),
    max = vapply(vars, function(v) input[[paste0("max_", v)]], numeric(1)),
    row.names = vars
  )

  showNotification(paste("Ranges saved for:", paste(vars, collapse = ", ")),
                   type = "message", duration = 4)
})
