# Title: Save and Load Session Logic
# Description: all the logic to load and save the session, separate for ease later
# Date last updated: 6/30/2026

# Load previous session
observeEvent(input$load_session, {

  req(input$session_file)
  req(is.data.frame(input$session_file))
  req(file.exists(input$session_file$datapath))

  session_data$input_mode <- "prev_session"

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

  for(nm in names(session_list)){
    session_data[[nm]] <- session_list[[nm]]
  }

  covariance_set(FALSE)
  cov_counters(list())

  showNotification("Session loaded successfully.", type = "message", duration = 4)

  if(length(session_data$ellipsoid_list) > 0){
    updateTabItems(session, "sidebarMenu", selected = "build_tab")
    updateTabsetPanel(session, "tabpanel-build", selected = "range")
  }
})
output$save_session_btn <- downloadHandler(
  filename = function(){
    paste0("nicheR_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  },
  content = function(file){

    req(session_data$current_ellipsoid)

    # Must convert to plain list before saving
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
