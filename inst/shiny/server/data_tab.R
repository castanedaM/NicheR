# Title: Data tab server logic
# Description: Handles file upload, validation, and variable selection
# Date last updated: 06/30/2026

# Observer Events ---------------------------------------------------------

observeEvent(input$raster_file, {
  showNotification("Raster file selected, preview loading...",
                   id = "raster_preview_msg",
                   type = "message",
                   duration = NULL)  # NULL keeps it until manually removed

  shinyjs::hide("df_file")
})

observeEvent(input$df_file, {
  showNotification("CSV file selected, preview loading...",
                   id = "df_preview_msg",
                   type = "message",
                   duration = NULL)

})

observeEvent(input$data_upload, {
  uploaded <- FALSE

  session_data$input_mode <- "bg_layers"

  if(!is.null(input$raster_file)){
    ext <- tolower(tools::file_ext(input$raster_file$name))
    session_data$bg_raster <- tryCatch(
      load_raster_file(input$raster_file$datapath, ext),
      error = function(e) stop(safeError(e))
    )
    uploaded <- TRUE
  }

  if(!is.null(input$df_file)){
    ext <- tolower(tools::file_ext(input$df_file$name))

    session_data$bg_df <- tryCatch(
      load_df_file(input$df_file$datapath, ext),
      error = function(e) stop(safeError(e))
    )
    uploaded <- TRUE
  }

  if(!uploaded){
    showNotification(
      "No files selected. Please choose at least one file before uploading.",
      type = "warning", duration = 4)
    return()
  }

  if(is.null(session_data$bg_df) && !is.null(session_data$bg_raster)){
    session_data$bg_df <- terra::as.data.frame(session_data$bg_raster, xy = TRUE, na.rm = TRUE)
  }

  msg <- paste(c(if(!is.null(session_data$bg_raster)) "Raster loaded successfully.",
                 if(!is.null(session_data$bg_df)) "CSV loaded successfully."),
               collapse = " ")
  showNotification(msg, type = "message", duration = 4)

  if(!is.null(session_data$bg_raster) && !is.null(session_data$bg_df)){
    raster_names <- names(session_data$bg_raster)
    df_names <- names(session_data$bg_df)
    missing_in_df <- setdiff(raster_names, df_names)
    raster_cells <- sum(!is.na(terra::values(session_data$bg_raster[[1]])))
    row_match <- nrow(session_data$bg_df) == raster_cells

    warning_parts <- c(
      if(length(missing_in_df) > 0){
        paste("Raster layers missing from CSV:",
              paste(missing_in_df, collapse = ", "))},
      if(!row_match){
        paste0("Row count mismatch: CSV has ",
               nrow(session_data$bg_df), " rows but raster has ",
               raster_cells, " non-NA cells.")}
    )

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

  updateTabsetPanel(session, "tabpanel-build", selected = "range")
})

observeEvent(input$continue_raster_only, {
  session_data$bg_df <- NULL
  removeModal()
  updateTabsetPanel(session, "tabpanel-build", selected = "range")
})

observeEvent(input$reupload, {
  session_data$bg_raster <- NULL
  session_data$bg_df <- NULL
  removeModal()
})

observeEvent(input$continue_virtual, {
  session_data$input_mode <- "virtual"
  updateTabsetPanel(session, "tabpanel-build", selected = "range")
})

observeEvent(input$continue_example, {
  session_data$input_mode <- "example"

  session_data$bg_raster <- terra::rast(system.file("extdata", "ma_bios.tif",
                                                    package = "nicheR"))
  session_data$bg_df <- terra::as.data.frame(session_data$bg_raster,
                                             xy = TRUE, na.rm = TRUE)

  updateTabsetPanel(session, "tabpanel-build", selected = "range")
})

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

observeEvent({
  lapply(seq_len(MAX_DIMS), function(i) input[[paste0("var_select_", i)]])
  lapply(seq_len(MAX_DIMS), function(i) input[[paste0("var_active_", i)]])
}, {

  vars <- if(!is.null(session_data$bg_raster)){
    names(session_data$bg_raster)
  } else if (!is.null(session_data$bg_df)){
    names(session_data$bg_df)
  } else {
    return()
  }

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

observeEvent(input$confirm_variables, {

  if(identical(session_data$input_mode, "virtual")){

    req(input$virtual_n_dims)
    n_dims <- input$virtual_n_dims

    vars <- vapply(seq_len(n_dims), function(i){
      val <- input[[paste0("virtual_var_name_", i)]]
      if(is.null(val) || !nzchar(val)) paste0("var", i) else val
    }, character(1))

    if(length(unique(vars)) != length(vars)){
      showNotification("Variable names must be unique.", type = "error", duration = 4)
      return()
    }

    session_data$vars <- vars

    showNotification(paste("Defined variables:", paste(vars, collapse = ", ")),
                     type = "message", duration = 4)

  } else {

    all_vars <- if(!is.null(session_data$bg_raster)){
      names(session_data$bg_raster)
    } else if (!is.null(session_data$bg_df)){
      colnames(session_data$bg_df)
    } else {
      return()
    }

    n_slots  <- min(length(all_vars), MAX_DIMS)

    active <- vapply(seq_len(n_slots), function(i)
      isTRUE(input[[paste0("var_active_", i)]]), logical(1))

    vars <- vapply(seq_len(n_slots),
                   function(i){
                     val <- input[[paste0("var_select_", i)]]
                     if(is.null(val)) all_vars[i] else val
                   }, character(1))[active]

    session_data$vars <- vars

    showNotification(paste("Selected variables:", paste(vars, collapse = ", ")),
                     type = "message", duration = 4)
  }

  # Clear everything downstream that depended on the old variable set
  session_data$ellipsoid_list <- list()
  session_data$current_ellipsoid <- NULL
  session_data$current_ellipsoid_id <- NULL

})

observeEvent(input$edit_variables, {

  if(length(session_data$ellipsoid_list) > 0 || !is.null(session_data$vars)){
    showModal(modalDialog(
      title = "Edit variables?",
      p("You have already selected variables or built an ellipsoid. Editing your variables will
         delete the current selection or ellipsoid and any covariance adjustments."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_variables",
                     "Yes, edit variables",
                     class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  }

})

observeEvent(input$confirm_edit_variables, {
  removeModal()

  session_data$vars <- NULL
  session_data$ellipsoid_list <- list()
  session_data$current_ellipsoid <- NULL
  session_data$current_ellipsoid_id <- NULL

})

# Reset logic for input data
observeEvent(input$data_input_type_choice, {

  session_data$input_mode <- NULL
  session_data$bg_raster <- NULL
  session_data$bg_df <- NULL
  session_data$vars <- NULL
  session_data$ellipsoid_list <- list()
  session_data$current_ellipsoid <- NULL
  session_data$current_ellipsoid_id <- NULL

  updateRadioButtons(session, "range_method_choice", selected = character(0))

}, ignoreInit = TRUE)

# Render Outputs ----------------------------------------------------------

output$data_input_type <- renderUI({
  req(input$data_input_type_choice)

  switch(input$data_input_type_choice,
         "bg_layers" =  {

           tagList(box(title = tags$span("Load Background Layers", class = "text-section-header"),
                       width = 12,
                       p(instructions$data_upload, class = "text-instruction"),
                       fluidRow(
                         column(
                           width = 12,
                           fileInput(inputId = "raster_file",
                                     label = tagList(
                                       tags$span("Background Layers (Raster)", class = "text-widget-title"),
                                       tags$span(icon("circle-info"),
                                                 title = "Environmental conditions of the study area in raster format.\nRequired if no CSV is provided. Accepted: .tif, .rds", class = "text-widget-title")),
                                     multiple = TRUE,
                                     accept = c("tif", "tiff", "rds")),

                           fileInput(inputId = "df_file",
                                     label = tagList(
                                       tags$span("Background Layers (CSV)", class = "text-widget-title"),
                                       tags$span(icon("circle-info"),
                                                 title = "Same data as the raster but in tabular form.\nOptional if raster is provided. Accepted: .csv, .rds", class = "text-widget-title"))
                                     ,
                                     multiple = FALSE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values",
                                                "text/plain",
                                                ".csv", "rds")),

                           verbatimTextOutput("raster_print"),
                           tableOutput("df_header")

                         )
                       ),

                       fluidRow(
                         column(
                           width = 12,
                           class = "btn-spaced",
                           actionButton(inputId = "data_upload",
                                        label = "Upload",
                                        class = "btn-default")
                         )
                       )
           ))
         },

         "prev_session" = {

           tagList(box(title = tags$span("Load Previous Session", class = "text-section-header"),
                       width = 12,
                       p(instructions$prev_session, class = "text-instruction"),
                       fileInput(inputId = "session_file",
                                 label = tags$span("Session File (.rds)", class = "text-widget-title"),
                                 multiple = FALSE,
                                 accept = c(".rds")),
                       fluidRow(
                         column(
                           width = 12,
                           class = "btn-spaced",
                           actionButton(inputId = "load_session",
                                        label = "Load Session",
                                        class = "btn-default")
                         )
                       )
           ))

         },
         "virtual_mode" = {

           tagList(box(title = tags$span("Virtual Mode", class = "text-section-header"),
                       width = 12,
                       p(instructions$virtual_mode, class = "text-instruction"),
                       fluidRow(
                         column(
                           width = 12,
                           class = "btn-spaced",
                           actionButton(inputId = "continue_virtual",
                                        label = "Continue",
                                        class = "btn-default")
                         )
                       )
           ))

         },

         "example_data" = {

           tagList(
             box(title = tags$span("Example Data", class = "text-section-header"),
                 width = 12,
                 p(instructions$example_data, class = "text-instruction"),
                 fluidRow(
                   column(
                     width = 12,
                     class = "btn-spaced",
                     actionButton(inputId = "continue_example",
                                  label = "Continue",
                                  class = "btn-default")
                   )
                 )
             )
           )

         }

  )
})

output$raster_print <- renderPrint({
  req(input$raster_file)
  ext <- tolower(tools::file_ext(input$raster_file$name))
  result <- tryCatch(
    load_raster_file(input$raster_file$datapath, ext),
    error = function(e) stop(safeError(e))
  )

  removeNotification("raster_preview_msg")
  print(result)
})

output$df_header <- renderTable({
  req(input$df_file)
  ext <- tolower(tools::file_ext(input$df_file$name))
  result <- tryCatch(
    load_df_file(input$df_file$datapath, ext),
    error = function(e) stop(safeError(e))
  )

  removeNotification("df_preview_msg")
  head(result)

})

output$variable_selectors_ui <- renderUI({

  # If variables are already confirmed, show a collapsed summary box
  if(!is.null(session_data$vars)){
    return(
      box(title = tags$span("Variables", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p(paste("Selected variables:", paste(session_data$vars, collapse = ", ")),
            class = "text-instruction"),
          fluidRow(
            column(12, class = "btn-spaced",
                   actionLink("edit_variables",
                              label = tagList(icon("pen"), "Edit variables")))
          )
      )
    )
  }

  # Virtual mode: ask for number of dimensions, then name each one
  if(identical(session_data$input_mode, "virtual")){

    n_dims <- if(!is.null(input$virtual_n_dims)) input$virtual_n_dims else 2

    name_rows <- lapply(seq_len(n_dims), function(i){
      fluidRow(
        column(width = 4, class = "var-label",
               tags$span(paste("Variable", i), class = "text-widget-inner")),
        column(width = 8,
               textInput(inputId = paste0("virtual_var_name_", i),
                         label = NULL,
                         value = paste0("var", i)))
      )
    })

    return(
      box(title = tags$span("Define Variables", class = "text-section-header"),
          width = 12,
          p(instructions$virtual_variables, class = "text-instruction"),
          fluidRow(
            column(width = 4, tags$span("Number of dimensions",
                                        class = "text-widget-title")),
            column(width = 4,
                   numericInput(inputId = "virtual_n_dims",
                                label = NULL,
                                value = n_dims,
                                min = 2, max = MAX_DIMS, step = 1))
          ),
          name_rows,
          fluidRow(column(12, class = "btn-spaced",
                          actionButton("confirm_variables", "Confirm", class = "btn-default")))
      )
    )
  }

  # Default: bg_layers / example_data / prev_session
  vars <- if(!is.null(session_data$bg_raster)){
    names(session_data$bg_raster)
  } else if (!is.null(session_data$bg_df)){
    colnames(session_data$bg_df)
  } else {
    return()
  }

  n_slots <- min(length(vars), MAX_DIMS)
  all_choices <- vars

  var_slots <- lapply(seq_len(n_slots), function(i){
    fluidRow(
      column(width = 1, class = "checkbox-align",
             checkboxInput(paste0("var_active_", i),
                           label = NULL,
                           value = TRUE)),
      column(width = 11,
             conditionalPanel(paste0("input.var_active_", i, " == true"),
                              selectInput(paste0("var_select_", i),
                                          paste("Variable", i),
                                          all_choices,
                                          selected = vars[i])),
             conditionalPanel(paste0("input.var_active_", i, " == false"),
                              tags$div(class = "selector-disabled",
                                       selectInput(paste0("var_select_", i, "_ghost"),
                                                   paste("Variable", i),
                                                   all_choices,
                                                   selected = vars[i])))
      )
    )
  })

  box(title = tags$span("Select Variables", class = "text-section-header"),
      width = 12,
      p(instructions$variable_settings, class = "text-instruction"),
      var_slots,
      fluidRow(column(12, class = "btn-spaced",
                      actionButton("confirm_variables", "Confirm", class = "btn-default")))
  )
})

