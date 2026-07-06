# Title: Save and Load Session Logic
# Description: all the logic to load and save the session, separate for ease later
# Date last updated: 07/06/2026


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


  for(nm in names(session_list)){
    session_data[[nm]] <- session_list[[nm]]
  }



  if(!is.null(session_data$ellipsoid_list[["base"]])){
    raw <- session_data$ellipsoid_list[["base"]]
    working_ell <- tag_ellipsoid(raw,
                                 name = paste0("ellipsoid_",
                                               ell_id_counter() + 1L))
    session_data$current_ellipsoid <- working_ell
  }

  # Align vars after working copy is set, using the correct object
  if(!is.null(session_data$current_ellipsoid)){
    session_data$vars <- session_data$current_ellipsoid$var_names
    message("vars aligned to: ", paste(session_data$vars, collapse = ", "))
  }


  ell_id_counter(length(session_data$ellipsoid_list))

  covariance_set(FALSE)
  cov_counters(list())

  centroid_set(FALSE)


  showNotification("Session loaded successfully.", type = "message", duration = 4)
  updateTabsetPanel(session, "tabpanel-build", selected = "range")
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

    tryCatch({
      saveRDS(session_list, file)
    }, error = function(e){
      showNotification(paste("Failed to save session:", e$message),
                       type = "error", duration = 4)
    })
  }
)
