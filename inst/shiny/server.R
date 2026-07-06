# Title: Server for shiny nicheR
# Description: The server of the app
# Lats Updated: 6/30/2026


function(input, output, session){

  session_data <- reactiveValues(

    input_mode = NULL,
    session_loading = FALSE,

    bg_raster = NULL,
    bg_df = NULL,

    vars = NULL,

    range_df = NULL,
    range_stats = NULL,

    bias = NULL,

    ellipsoid_list = list(),
    current_ellipsoid = NULL,

    ellipsoid_prediction_list = list(),
  )

  source("server/helpers.R", local = TRUE)
  source("server/save_load_session.R", local = TRUE)
  source("server/data_tab.R", local = TRUE)
  source("server/build_tab_plot.R", local = TRUE)
  source("server/build_tab.R", local = TRUE)
  source("server/predict_tab.R", local = TRUE)
  source("server/bias_tab.R", local = TRUE)
  source("server/generate_tab.R", local = TRUE)
}
