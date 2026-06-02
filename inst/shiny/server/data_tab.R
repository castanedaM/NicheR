# Title: Data tab server logic
# Description: Handles file upload, validation, and variable selection
# Date last updated: 05/28/2026

# Observer Events ---------------------------------------------------------

observeEvent(input$raster_file, {
  showNotification("Raster file selected, preview loading...",
                   id = "raster_preview_msg",
                   type = "message",
                   duration = NULL)  # NULL keeps it until manually removed
})

observeEvent(input$df_file, {
  showNotification("CSV file selected, preview loading...",
                   id = "df_preview_msg",
                   type = "message",
                   duration = NULL)
})

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

  msg <- paste(c(if(!is.null(session_data$raster)) "Raster loaded successfully.",
                 if(!is.null(session_data$df)) "CSV loaded successfully."),
               collapse = " ")
  showNotification(msg, type = "message", duration = 4)

  if(!is.null(session_data$raster) && !is.null(session_data$df)){
    raster_names  <- names(session_data$raster)
    df_names      <- names(session_data$df)
    missing_in_df <- setdiff(raster_names, df_names)
    raster_cells  <- sum(!is.na(terra::values(session_data$raster[[1]])))
    row_match     <- nrow(session_data$df) == raster_cells

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
      return()
    }
  }

  updateTabsetPanel(session, "tabset1", selected = "setting")
})

observeEvent(input$continue_raster_only, {
  session_data$df <- NULL
  removeModal()
  updateTabsetPanel(session, "tabset1", selected = "setting")
})

observeEvent(input$reupload, {
  session_data$raster <- NULL
  session_data$df     <- NULL
  removeModal()
})

observeEvent(input$confirm_variables, {
  all_vars <- get_var_names(session_data)
  n_slots  <- min(length(all_vars), MAX_DIMS)

  active <- vapply(seq_len(n_slots), function(i)
    isTRUE(input[[paste0("var_active_", i)]]), logical(1))

  vars <- vapply(seq_len(n_slots), function(i) {
    val <- input[[paste0("var_select_", i)]]
    if(is.null(val)) all_vars[i] else val
  }, character(1))[active]

  session_data$sel_raster <- if(!is.null(session_data$raster)) session_data$raster[[vars]]
  session_data$sel_df     <- if(!is.null(session_data$df)){
    session_data$df[, vars, drop = FALSE]
  } else {
    terra::as.data.frame(session_data$raster[[vars]], xy = TRUE, na.rm = TRUE)
  }

  showNotification(paste("Selected variables:", paste(vars, collapse = ", ")),
                   type = "message", duration = 4)
  updateTabItems(session, "sidebarMenu", selected = "build_tab")
})

observeEvent({
  lapply(seq_len(MAX_DIMS), function(i) input[[paste0("var_select_", i)]])
  lapply(seq_len(MAX_DIMS), function(i) input[[paste0("var_active_", i)]])
}, {
  vars <- get_var_names(session_data)
  req(vars)

  n_slots <- min(length(vars), MAX_DIMS)

  active <- vapply(seq_len(n_slots), function(i)
    isTRUE(input[[paste0("var_active_", i)]]), logical(1))

  current <- vapply(seq_len(n_slots), function(i){
    val <- input[[paste0("var_select_", i)]]
    if(is.null(val)) vars[i] else val
  }, character(1))

  # only active slots block choices from others
  active_selections <- current[active]

  lapply(seq_len(n_slots), function(i){
    others <- if(active[i]) setdiff(active_selections, current[i]) else active_selections
    available <- c(current[i], setdiff(vars, others))
    updateSelectInput(session,
                      inputId = paste0("var_select_", i),
                      choices = available,
                      selected = current[i])
  })
}, ignoreNULL = TRUE, ignoreInit = TRUE)


# Render Outputs ----------------------------------------------------------

output$raster_print <- renderPrint({
  req(input$raster_file)
  ext <- tolower(tools::file_ext(input$raster_file$name))
  tryCatch(
    print(load_raster_file(input$raster_file$datapath, ext)),
    error = function(e) stop(safeError(e))
  )
  removeNotification("raster_preview_msg")

})

output$df_header <- renderTable({
  req(input$df_file)
  ext <- tolower(tools::file_ext(input$df_file$name))
  tryCatch(
    head(load_df_file(input$df_file$datapath, ext)),
    error = function(e) stop(safeError(e))
  )
  removeNotification("df_preview_msg")

})

output$bias_raster_print <- renderPrint({
  req(input$bias_raster_file)
  ext <- tolower(tools::file_ext(input$bias_raster_file$name))
  tryCatch(
    print(load_raster_file(input$bias_raster_file$datapath, ext)),
    error = function(e) stop(safeError(e))
  )

  removeNotification("bias_raster_preview_msg")

})

output$variable_selectors_ui <- renderUI({
  vars <- get_var_names(session_data)
  req(vars)

  n_slots     <- min(length(vars), MAX_DIMS)
  all_choices <- vars

  var_slots <- lapply(seq_len(n_slots), function(i) {
    fluidRow(
      column(width = 1, class = "checkbox-align",
             checkboxInput(paste0("var_active_", i), label = NULL, value = TRUE)),
      column(width = 11,
             conditionalPanel(paste0("input.var_active_", i, " == true"),
                              selectInput(paste0("var_select_", i), paste("Variable", i),
                                          all_choices, selected = vars[i])),
             conditionalPanel(paste0("input.var_active_", i, " == false"),
                              tags$div(class = "selector-disabled",
                                       selectInput(paste0("var_select_", i, "_ghost"), paste("Variable", i),
                                                   all_choices, selected = vars[i])))
      )
    )
  })

  tagList(
    "Select Variables",
    p(instructions$variable_settings, class = "text-instruction"),
    var_slots,
    fluidRow(column(12, class = "btn-spaced",
                    actionButton("confirm_variables", "Confirm", class = "btn-primary")))
  )
})
