
function(input, output, session){

  session_data <- reactiveValues(
    raster     = NULL,
    df         = NULL,
    bias       = NULL,
    sel_raster = NULL,
    sel_df     = NULL,
    ranges     = NULL
  )

  source("server/helpers.R",      local = TRUE)
  source("server/data_tab.R",     local = TRUE)
  source("server/build_tab.R",    local = TRUE)
  source("server/predict_tab.R",  local = TRUE)
  source("server/generate_tab.R", local = TRUE)
}
