# Title: Save and Load Session Logic
# Description: all the logic to load and save the session, separate for ease later
# Date last updated: 6/30/2026

# Load previous session
observeEvent(input$load_session, {

  req(input$load_session)
  session_data$input_mode <- "prev_session"

  session_list <- tryCatch(
    readRDS(input$load_session$datapath),
    error = function(e){
      showNotification(paste("Could not load session:", e$message),
                       type = "error", duration = 4)
      NULL
    }
  )

  req(session_list)

  # Unwrap SpatRaster if present
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

  # Restore all session values
  for(nm in names(session_list)){
    session_data[[nm]] <- session_list[[nm]]
  }

  showNotification("Session loaded successfully.", type = "message", duration = 4)

  # Navigate to build tab if ellipsoid was restored
  if(!is.null(session_data$current_ellipsoid)){
    updateTabsetPanel(session, "tabpanel-build", selected = "range")
  }
})


# Save session
output$save_session_btn <- downloadHandler(
  filename = function(){
    paste0("nicheR_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  },
  content = function(file){

    if(is.null(session_data$current_ellipsoid)){
      showNotification("Please build an ellipsoid before saving a session.",
                       type = "warning", duration = 4)
      # Write an empty file so the browser doesn't error on a failed download
      saveRDS(list(), file)
      return()
    }

    session_list <- reactiveValuesToList(session_data)

    if(!is.null(session_list$bg_raster)){
      session_list$bg_raster <- terra::wrap(session_list$bg_raster)
    }

    tryCatch({
      saveRDS(session_list, file)
    }, error = function(e){
      showNotification(paste("Failed to save session:", e$message),
                       type = "error", duration = 4)
    })
  }
)
