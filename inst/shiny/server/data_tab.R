# Title: Data tab server logic
# Description: Handles file upload, validation, and variable selection
# Date last updated: 05/28/2026

# Observer Events ---------------------------------------------------------

# user chooses to upload data selected:
observeEvent(input$data_upload, {
  uploaded <- FALSE

  if(!is.null(input$raster_file)){
    session_data$raster <- terra::rast(input$raster_file$datapath)
    uploaded <- TRUE
  }
  if(!is.null(input$df_file)){
    session_data$df <- read.csv(input$df_file$datapath)
    uploaded <- TRUE
  }

  if(!uploaded){
    showNotification(
      "No files selected. Please choose at least one file before uploading.",
      type = "warning", duration = 4)
    return()
  }

  # success message
  msg <- paste(c(if(!is.null(session_data$raster)) "Raster loaded successfully.",
                 if(!is.null(session_data$df)) "CSV loaded successfully."),
               collapse = " ")
  showNotification(msg, type = "message", duration = 4)

  # run checks only if both files are present
  if(!is.null(session_data$raster) && !is.null(session_data$df)){

    raster_names <- names(session_data$raster)
    df_names <- names(session_data$df)
    missing_in_df <- setdiff(raster_names, df_names)
    raster_cells <- sum(!is.na(terra::values(session_data$raster[[1]])))
    row_match <- nrow(session_data$df) == raster_cells

    # build warning message based on what failed
    warning_parts <- c(
      if(length(missing_in_df) > 0) paste("Raster layers missing from CSV:",
                                          paste(missing_in_df, collapse = ", ")),
      if(!row_match) paste0("Row count mismatch: CSV has ",
                            nrow(session_data$df), " rows but raster has ",
                            raster_cells, " non-NA cells."))

    if(length(warning_parts) > 0){
      showModal(modalDialog(
        title = "Data mismatch warning",
        p("The following issues were found:"),
        tags$ul(lapply(warning_parts, tags$li)),
        p("You can continue using raster layers only, or re-upload corrected files."),
        footer = tagList(
          actionButton("continue_raster_only", "Continue with raster only",
                       class = "btn-warning"),
          actionButton("reupload", "Re-upload",
                       class = "btn-default")
        ),
        easyClose = FALSE
      ))
      return()  # do not proceed to settings until user decides
    }
  }

  # no issues, go to settings
  updateTabsetPanel(session, "tabset1", selected = "setting")
})

# user chooses to continue with raster only
observeEvent(input$continue_raster_only, {
  session_data$df <- NULL  # discard mismatched CSV
  removeModal()
  updateTabsetPanel(session, "tabset1", selected = "setting")
})

# user chooses to re-upload
observeEvent(input$reupload, {
  session_data$raster <- NULL
  session_data$df     <- NULL
  removeModal()
  # stays on Data Inputs tab
})

# user selects the variables to use
observeEvent(input$confirm_variables, {

  vars <- {
    n_slots <- min(length(names(session_data$raster %||% session_data$df)), 6)
    active <- vapply(seq_len(n_slots), function(i) {
      isTRUE(input[[paste0("var_active_", i)]])
    }, logical(1))
    selected <- vapply(seq_len(n_slots), function(i) {
      val <- input[[paste0("var_select_", i)]]
      if(is.null(val)) names(session_data$raster)[i] else val
    }, character(1))
    selected[active]
  }

  if(!is.null(session_data$raster)){
    session_data$sel_raster <- session_data$raster[[vars]]
  }

  session_data$sel_df <- if(!is.null(session_data$df)){
    session_data$df[, vars, drop = FALSE]
  } else {
    terra::as.data.frame(session_data$raster[[vars]], xy = TRUE, na.rm = TRUE)
  }

  msg <- paste("Selected variables:", paste(vars, collapse = ", "))
  showNotification(msg, type = "message", duration = 4)
  updateTabItems(session, "sidebarMenu", selected = "build_tab")
})


# Render Outputs ----------------------------------------------------------

# Show user their data
output$raster_print <- renderPrint({

  req(input$raster_file)

  tryCatch({
    rast <- terra::rast(input$raster_file$datapath)
  }, error = function(e) {
    stop(safeError(e))
  })

  return(print(rast))
})

output$df_header <- renderTable({

  req(input$df_file)

  tryCatch({
    df <- read.csv(input$df_file$datapath)
  }, error = function(e) {
    stop(safeError(e))
  })

  return(head(df))

})


# Render variable selectors dynamically
output$variable_selectors_ui <- renderUI({
  vars <- if(!is.null(session_data$raster)){
    names(session_data$raster)
  }else if(!is.null(session_data$df)){
    names(session_data$df)
  }else{
    return(NULL)
  }

  n_slots   <- min(length(vars), 6)
  all_choices <- vars

  var_slots <- lapply(seq_len(n_slots), function(i) {
    checkbox_id <- paste0("var_active_", i)
    select_id   <- paste0("var_select_", i)

    fluidRow(
      column(
        width = 1,
        style = "padding-top: 28px;",
        checkboxInput(
          inputId = checkbox_id,
          label   = NULL,
          value   = TRUE
        )
      ),
      column(
        width = 11,
        # Gray out the selectInput when checkbox is unchecked
        conditionalPanel(
          condition = paste0("input.", checkbox_id, " == true"),
          selectInput(
            inputId  = select_id,
            label    = paste("Variable", i),
            choices  = all_choices,
            selected = vars[i]   # default: layer at this index
          )
        ),
        conditionalPanel(
          condition = paste0("input.", checkbox_id, " == false"),
          tags$div(
            style = "opacity: 0.4; pointer-events: none;",
            selectInput(
              inputId  = paste0(select_id, "_ghost"),
              label    = paste("Variable", i),
              choices  = all_choices,
              selected = vars[i]
            )
          )
        )
      )
    )
  })

  tagList(
    h4("Select Variables", style = "margin-bottom: 16px;"),
    p("Uncheck a variable to exclude it from analysis.",
      style = "font-size: 13px; color: #888; margin-bottom: 12px;"),
    var_slots,
    fluidRow(
      column(
        width = 12,
        style = "margin-top: 16px;",
        actionButton("confirm_variables", "Confirm", class = "btn-primary")
      )
    )
  )
})


