# Build tab for shiny app
#
# Date last updated: 05/28/2026

output$range_manual_ui <- renderUI({
  req(session_data$sel_raster)
  vars <- names(session_data$sel_raster)

  # header row
  header <- fluidRow(
    column(width = 4, tags$b("Variable")),
    column(width = 4, tags$b("Min")),
    column(width = 4, tags$b("Max"))
  )

  # one row per variable
  rows <- lapply(vars, function(v) {
    fluidRow(
      column(width = 4,
             style = "padding-top: 7px;",
             tags$span(v)),
      column(width = 4,
             numericInput(inputId = paste0("min_", v),
                          label = NULL,
                          value = 0,
                          step = 0.01)),
      column(width = 4,
             numericInput(inputId = paste0("max_", v),
                          label = NULL,
                          value = 1,
                          step = 0.01))
    )
  })

  tagList(
    header,
    rows,
    fluidRow(
      column(width = 12,
             style = "margin-top: 16px;",
             actionButton("confirm_ell", "Confirm Ellipsoid", class = "btn-primary"))
    )
  )
})

output$build_espace_plot <- renderPlot({
  req(session_data$sel_df)

  df <- session_data$sel_df
  vars <- names(session_data$sel_raster)

  # cap at 3 variables for pairs plot
  vars <- vars[seq_len(min(length(vars), 6))]
  df <- df[, vars, drop = FALSE]

  # subsample to keep plotting fast
  max_rows <- 5000

  if(nrow(df) > max_rows){
    df <- df[sample(nrow(df), max_rows), , drop = FALSE]
  }

  pairs(df)

})
