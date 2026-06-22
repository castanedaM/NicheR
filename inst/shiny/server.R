# Title: Server for shiny nicheR
# Description: The server of the app
# Lats Updated: 6/22/2026


function(input, output, session){

  session_data <- reactiveValues(
    input_mode = NULL,
    bg_raster = NULL,
    bg_df = NULL,
    bias = NULL,
    vars = NULL,
    ellipsoid = NULL,
    ellipsoid_version = 0L
  )

  cov_counters <- reactiveVal(list())

  source("server/helpers.R", local = TRUE)
  source("server/data_tab.R", local = TRUE)
  source("server/plot_tab.R", local = TRUE)
  source("server/build_tab.R", local = TRUE)
  source("server/predict_tab.R", local = TRUE)
  source("server/generate_tab.R", local = TRUE)
}
