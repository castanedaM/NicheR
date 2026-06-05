
function(input, output, session){

  session_data <- reactiveValues(
    bg_raster = NULL,
    bg_df = NULL,
    bias = NULL,
    vars = NULL,
    ranges = NULL,
    df_range = NULL,
    ellipsoid = NULL
  )

  source("server/helpers.R", local = TRUE)
  source("server/data_tab.R", local = TRUE)
  source("server/build_tab.R", local = TRUE)
  source("server/predict_tab.R", local = TRUE)
  source("server/generate_tab.R", local = TRUE)
}
