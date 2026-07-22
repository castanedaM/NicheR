# Title: Save and Load Session Logic
# Description: all the logic to load and save the session, separate for ease later
# Date last updated: 07/21/2026


# Load previous session
observeEvent(input$load_session, {
  req(input$session_file)
  req(is.data.frame(input$session_file))
  req(file.exists(input$session_file$datapath))

  session_list <- tryCatch(
    readRDS(input$session_file$datapath),
    error = function(e){
      showNotification(paste("Could not load session:", e$message),
                       type = "error", duration = 4)
      NULL
    }
  )

  req(session_list)

  if(!is.null(session_list$bg_raster)){
    session_list$bg_raster <- tryCatch(
      terra::unwrap(session_list$bg_raster),
      error = function(e){
        showNotification("Could not restore raster from session file.",
                         type = "warning", duration = 4)
        NULL
      }
    )
  }

  # Unwrap predictions
  if(length(session_list$ellipsoid_prediction_list) > 0){
    session_list$ellipsoid_prediction_list <- lapply(
      session_list$ellipsoid_prediction_list,
      function(p) tryCatch(terra::unwrap(p), error = function(e) p)
    )
  }

  # Unwrap prepared_bias
  if(!is.null(session_list$prepared_bias)){
    session_list$prepared_bias <- list(
      combination_formula = session_list$prepared_bias$combination_formula,
      composite_surface   = tryCatch(
        terra::unwrap(session_list$prepared_bias$composite_surface),
        error = function(e) NULL),
      processed_layers    = tryCatch(
        terra::unwrap(session_list$prepared_bias$processed_layers),
        error = function(e) NULL)
    )
  }

  # Unwrap ellipsoid_prediction_list_biased
  session_list$ellipsoid_prediction_list_biased <- lapply(
    session_list$ellipsoid_prediction_list_biased,
    function(p) tryCatch(terra::unwrap(p), error = function(e) p)
  )

  for(nm in names(session_list)){
    session_data[[nm]] <- session_list[[nm]]
  }

  if(!is.null(session_data$ellipsoid_list[["base"]])){
    session_data$current_ellipsoid <- session_data$ellipsoid_list[["base"]]
  }

  if(!is.null(session_data$current_ellipsoid)){
    session_data$vars <- session_data$current_ellipsoid$var_names
  }

  showNotification("Session loaded successfully.", type = "message", duration = 4)

  if(length(session_data$ellipsoid_prediction_list) > 0){
    updateTabItems(session, "sidebarMenu", selected = "bias_tab")
  } else {
    updateTabsetPanel(session, "tabpanel-build", selected = "range")
  }
})


output$save_session_btn <- downloadHandler(
  filename = function(){
    paste0("nicheR_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  },
  content = function(file){

    # Must convert to plain list before saving
    session_list <- reactiveValuesToList(session_data)
    session_list$input_mode <- "prev_session"

    if(!is.null(session_list$bg_raster)){
      session_list$bg_raster <- terra::wrap(session_list$bg_raster)
    }

    pred_list <- session_data$ellipsoid_prediction_list

    if(length(pred_list) > 0){
      pred_list <- lapply(pred_list, function(p){
        if(inherits(p, "SpatRaster")) terra::wrap(p) else p
      })
    }

    session_list$ellipsoid_prediction_list <- pred_list

    tryCatch({
      saveRDS(session_list, file)
    }, error = function(e){
      showNotification(paste("Failed to save session:", e$message),
                       type = "error", duration = 4)
    })

    # Wrap prepared_bias
    if(!is.null(session_list$prepared_bias)){
      session_list$prepared_bias <- list(
        combination_formula = session_list$prepared_bias$combination_formula,
        composite_surface = terra::wrap(session_list$prepared_bias$composite_surface),
        processed_layers = terra::wrap(session_list$prepared_bias$processed_layers)
      )
    }

    # Wrap ellipsoid_prediction_list_biased
    session_list$ellipsoid_prediction_list_biased <- lapply(
      session_list$ellipsoid_prediction_list_biased,
      function(rast) if(inherits(rast, "SpatRaster")) terra::wrap(rast) else rast
    )

  }
)
