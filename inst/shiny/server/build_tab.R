# Title: Build Tab Server Script

# Description: This script includes the code for the range, covariance, centroid
# mover logic. Function from nicheR: build_ellipsoid(),
# update_ellipsoid_covariance(), rang_from_stats(), range_from_df()

# Author: Mariana Castaneda-Guzman

# Date last updated: 07/08/2026

# ELLIPSOID ---------------------------------------------------------------

# Session-level counter, increments every time a new ellipsoid is created
ell_id_counter <- reactiveVal(0L)

make_ell_id <- function(){
  n <- ell_id_counter() + 1L
  ell_id_counter(n)
  paste0("E", n, "_", format(Sys.time(), "%d%m%y"))
}

tag_ellipsoid <- function(ell, name){
  ell$ell_id <- make_ell_id()
  ell$ell_name <- name
  ell
}


# Button that builds the ellipsoid
observeEvent(input$build_ell, {

  session_data$session_range <- isolate(range_preview())
  build_ellipsoid_shiny()

})

observeEvent(input$save_ell_version, {

  req(session_data$current_ellipsoid)

  default_name <- paste0("ellipsoid_", ell_id_counter())

  showModal(modalDialog(
    title = "Save Ellipsoid Version",
    p("Give this ellipsoid a name. Use letters, numbers, and spaces only.
       Spaces will be replaced with underscores."),
    textInput("ell_save_name",
              label = NULL,
              value = default_name,
              placeholder = default_name),
    tags$small(paste0("Max 30 characters.")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save_ell_version",
                   "Save",
                   class = "btn-primary")
    ),
    easyClose = FALSE
  ))
})

observeEvent(input$confirm_save_ell_version, {

  req(session_data$current_ellipsoid)

  raw_name <- if(!is.null(input$ell_save_name) && nzchar(trimws(input$ell_save_name))){
    input$ell_save_name
  } else {
    paste0("ellipsoid_", ell_id_counter())
  }

  clean_name <- gsub("\\s+", "_", trimws(raw_name))
  clean_name <- substr(clean_name, 1, 30)

  ell_to_save <- session_data$current_ellipsoid
  ell_to_save$ell_name <- clean_name

  session_data$ellipsoid_list[[ell_to_save$ell_id]] <- ell_to_save
  session_data$current_ellipsoid <- ell_to_save

  removeModal()

  showModal(modalDialog(
    title = paste0(clean_name, " saved."),
    p("What would you like to do next?"),
    footer = tagList(
      actionButton("next_build_another",
                   tagList(icon("circle-plus"), "Build another ellipsoid"),
                   class = "btn-default"),
      actionButton("next_go_predict",
                   tagList(icon("arrow-right"), "Continue to Predict"),
                   class = "btn-primary")
    ),
    easyClose = FALSE
  ))

})

observeEvent(input$next_build_another, {
  removeModal()

  raw <- isolate(session_data$ellipsoid_list[["base"]])
  req(raw)

  working_ell <- tag_ellipsoid(raw, name = paste0("elliposid_",
                                                  ell_id_counter()))
  session_data$current_ellipsoid <- working_ell

  covariance_set(FALSE)
  centroid_set(FALSE)

  cov_counters(list())

  showNotification("Covariance reset. Adjust and save a new version.",
                   type = "message", duration = 3)
})

# Continue to predict: navigate to the predict tab
observeEvent(input$next_go_predict, {
  removeModal()
  updateTabItems(session, "sidebarMenu", selected = "predict_tab")
})

output$ellipsoid_library <- renderUI({

  req(length(session_data$ellipsoid_list) > 0)

  versions <- session_data$ellipsoid_list
  ids <- names(versions)
  cur_ell <- session_data$current_ellipsoid

  # Working ellipsoid slot
  working_row <- if(!is.null(cur_ell)){
    fluidRow(
      style = "background: #f0f7f0; border-radius: 4px; margin-bottom: 6px; padding: 4px 0;",
      column(width = 4,
             tags$span(icon("pen"),
                       tags$span(paste0(" ", cur_ell$ell_name),
                                 class  = "text-widget-inner",
                                 style  = "color: #097a21; font-weight: 500;"),
                       tags$span("(current)",
                                 style  = "font-size: 11px; color: #aaa;"))),
      column(width = 3,
             tags$span(cur_ell$ell_id,
                       style = "font-size: 11px; color: #aaa;")),

      column(width = 5, class = "btn-spaced",
               actionButton("save_ell_version",
                            "Save Ellipsoid",
                            class = "btn-primary"))
    )
  }

  rows <- lapply(ids, function(id){

    ell <- versions[[id]]
    is_base <- id == "base"

    fluidRow(
      style = "padding: 2px 0;",
      column(width = 5,
             tags$span(ell$ell_name, class = "text-widget-inner")),
      column(width = 4,
             tags$span(id, style = "font-size: 11px; color: #aaa;")),
      column(width = 3,
             tags$div(
               style = "display: flex; gap: 8px;",

               if(is_base){
                 tagList(
                   actionLink(
                     inputId = "ell_view_base",
                     label = tags$span(icon("eye"),
                                         title = "View base ellipsoid (read-only)",
                                         class = "tooltip-icon")
                   ),
                   actionLink(
                     inputId = "ell_new_from_base",
                     label = tags$span(icon("circle-plus"),
                                         title = "Create new working copy from base",
                                         class = "tooltip-icon")
                   )
                 )
               } else {
                 tagList(
                   actionLink(
                     inputId = paste0("ell_load_", id),
                     label = tags$span(icon("pen-to-square"),
                                         title = paste0("Edit ", ell$ell_name),
                                         class = "tooltip-icon")
                   ),
                   actionLink(
                     inputId = paste0("ell_delete_", id),
                     label = tags$span(icon("trash"),
                                         title = paste0("Delete ", ell$ell_name),
                                         class = "tooltip-icon",
                                         style = "color: #e74c3c;")
                   )
                 )
               }
             ))
    )
  })

  box(title = tags$span("Ellipsoid library", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,

      if(!is.null(cur_ell)){
        tagList(
          # tags$span("Working", class = "text-widget-title"),
          working_row,
          tags$hr(style = "margin: 8px 0;")
        )
      },

      fluidRow(
        column(width = 5, tags$span("Name", class = "text-widget-title")),
        column(width = 4, tags$span("ID", class = "text-widget-title")),
        column(width = 3, tags$span("Actions", class = "text-widget-title"))
      ),

      tagList(rows)
  )
})

# Load a saved ellipsoid for editing
observeEvent({
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(ids, function(id) input[[paste0("ell_load_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("ell_load_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  id <- clicked[1]
  ell <- session_data$ellipsoid_list[[id]]

  session_data$current_ellipsoid <- ell

  covariance_set(FALSE)
  centroid_set(FALSE)

  cov_counters(list())


  # Push correct covariance values to sliders explicitly,
  pair_names <- apply(t(combn(ell$var_names, 2)), 1,
                      function(p) paste(p, collapse = "-"))

  lapply(pair_names, function(pn){
    parts <- strsplit(pn, "-")[[1]]
    updateSliderInput(session,
                      inputId = paste0("cov_", ell$ell_id, "_", pn),
                      value = round(ell$cov_matrix[parts[1], parts[2]], 4))
  })

  lapply(ell$var_names, function(v){
    updateSliderInput(session,
                      inputId = paste0("centroid_", v, "_", ell$ell_id),
                      value = round(ell$centroid[v], 4))
  })

  showNotification(paste0(ell$ell_name, " loaded for editing."),
                   type = "message", duration = 3)

}, ignoreInit = TRUE)

# View base read-only
observeEvent(input$ell_view_base, {

  base_ell <- session_data$ellipsoid_list[["base"]]
  req(base_ell)

  session_data$current_ellipsoid <- base_ell

  covariance_set(TRUE)
  centroid_set(TRUE)

  showNotification("Viewing base ellipsoid. Covariance is locked.",
                   type = "message", duration = 3)

}, ignoreInit = TRUE)

# New working copy from base
observeEvent(input$ell_new_from_base, {

  raw <- isolate(session_data$ellipsoid_list[["base"]])
  req(raw)

  working_ell <- tag_ellipsoid(raw,
                               name = paste0("ellipsoid_", ell_id_counter()))

  session_data$current_ellipsoid <- working_ell

  covariance_set(FALSE)
  cov_counters(list())

  centroid_set(FALSE)

  showNotification("New working copy created from base.",
                   type = "message", duration = 3)

}, ignoreInit = TRUE)

# Delete a saved ellipsoid
observeEvent({
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(ids, function(id) input[[paste0("ell_delete_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("ell_delete_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  id <- clicked[1]
  nm <- session_data$ellipsoid_list[[id]]$ell_name

  showModal(modalDialog(
    title = paste0("Delete ", nm, "?"),
    p(paste0("This will permanently remove ", nm,
             " and any prediction results associated with it.")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0("confirm_ell_delete_", id),
                   "Yes, delete",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))

}, ignoreInit = TRUE)

# Confirmed delete
observeEvent({
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(ids, function(id) input[[paste0("confirm_ell_delete_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("confirm_ell_delete_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  id <- clicked[1]
  nm <- session_data$ellipsoid_list[[id]]$ell_name

  removeModal()

  session_data$ellipsoid_list[[id]] <- NULL

  if(!is.null(session_data$ellipsoid_prediction_list[[id]])){
    session_data$ellipsoid_prediction_list[[id]] <- NULL
  }

  # If the deleted version was the current working ellipsoid,
  # create a fresh working copy from base
  if(identical(session_data$current_ellipsoid$ell_id, id)){
    raw <- isolate(session_data$ellipsoid_list[["base"]])
    req(raw)
    working_ell <- tag_ellipsoid(raw,
                                 name = paste0("ellipsoid_",
                                               ell_id_counter()))
    session_data$current_ellipsoid <- working_ell

    covariance_set(FALSE)
    cov_counters(list())

    centroid_set(FALSE)

    showNotification(paste0(nm, " deleted. New working copy created from base."),
                     type = "message", duration = 3)
  } else {
    showNotification(paste0(nm, " deleted."),
                     type = "message", duration = 3)
  }

}, ignoreInit = TRUE)

# RANGES ------------------------------------------------------------------

output$range_method_choice_ui <- renderUI({

  req(session_data$vars)

  is_built <- isTRUE(length(session_data$ellipsoid_list) > 0)

  if(is_built){
    return(
      box(title = tags$span("Range", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p("Ranges have been set and the base ellipsoid has been built.",
            class = "text-instruction"),
          fluidRow(
            column(width = 12, class = "btn-spaced",
                   actionLink("edit_range",
                              label = tagList(icon("pen"), "Edit ranges")))
          )
      )
    )
  }

  box(title = tags$span("Range", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$range_choice, class = "text-instruction"),
      radioButtons("range_method_choice",
                   label = tags$span("Select how to define the ranges for your ellipsoid:",
                                     class = "text-widget-title"),
                   choiceNames = list(
                     tagList("Manual",
                             tags$span(icon("circle-info"),
                                       title = "Type minimum and maximum values directly for each variable.",
                                       class = "tooltip-icon")),
                     tagList("From Data",
                             tags$span(icon("circle-info"),
                                       title = "Upload a CSV to derive observed min/max ranges, with optional expansion.",
                                       class = "tooltip-icon")),
                     tagList("From Stats",
                             tags$span(icon("circle-info"),
                                       title = "Derive ranges from mean and standard deviation, either from your background data or entered manually.",
                                       class = "tooltip-icon"))
                   ),
                   choiceValues = c("man", "df", "stats"),
                   selected = character(0)),
      uiOutput("range_method_ui")
  )
})

output$range_method_ui <- renderUI({

  req(input$range_method_choice, session_data$vars)

  vars <- session_data$vars

  # No background data in virtual mode, default stats to a standard scale
  has_bg_df <- !is.null(session_data$bg_df)

  # Same button in all range method
  cl_row <- fluidRow(
    column(width = 5, tags$div(class = "tooltip-label-row",
                               tags$span("Confidence Level (%)", class = "text-widget-title text-center"),
                               tags$span(icon("circle-info"), title = instructions$cl_range_tooltip, class = "tooltip-icon"))),
    column(width = 4,
           numericInput(inputId = "cl_range",
                        label = NULL,
                        value = 0.95,
                        min = 0, max = 1,
                        step = 0.1)
    ))

  # Same button for all ranges once processed
  build_btn <- fluidRow(
    column(width = 12,
           class = "btn-spaced",
           actionButton("build_ell",
                        "Initialize Ellipsoid",
                        class = "btn-primary"))
  )

  # Same Reset Button for all
  reset_btn <- fluidRow(
    column(width = 12,
           class = "btn-spaced",
           actionLink("reset_ranges",
                      label = tagList(icon("rotate-left"), "Reset to defaults"))
    )
  )

  if(!is.null(session_data$session_range)){
    dft_values <- session_data$session_range
  } else if (has_bg_df){
    q <- round(apply(session_data$bg_df[, vars, drop = FALSE], 2, quantile, na.rm = TRUE), 2)
    dft_values <- list(min = as.list(q[2, ]), max = as.list(q[4, ]),
                       mean = as.list((q[2, ] + q[4, ])/2),
                       sd = as.list((q[4, ] - q[2, ])/4),
                       expand_min = setNames(as.list(rep(0, length(vars))), vars),
                       expand_max = setNames(as.list(rep(0, length(vars))), vars))
  } else {
    dft_values <- list(min = setNames(as.list(rep(0, length(vars))), vars),
                       max = setNames(as.list(rep(1, length(vars))), vars),
                       mean = setNames(as.list(rep(0, length(vars))), vars),
                       sd = setNames(as.list(rep(1, length(vars))), vars),
                       expand_min = setNames(as.list(rep(0, length(vars))), vars),
                       expand_max = setNames(as.list(rep(0, length(vars))), vars))
  }

  # Choose which UI to show for ranges
  switch(input$range_method_choice,
         "man" = {
           header <- fluidRow(
             column(width = 12, p(instructions$range_manual, class = "text-instruction")),
             column(width = 4, tags$span("Variable", class = "text-widget-title text-center")),
             column(width = 4, tags$span("Min", class = "text-widget-title text-center")),
             column(width = 4, tags$span("Max", class = "text-widget-title text-center"))
           )

           var_rows <- lapply(vars, function(v){

             fluidRow(
               column(width = 4, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      numericInput(inputId = paste0("min_", v),
                                   label = NULL,
                                   value = dft_values$min[[v]],
                                   step = 0.5)),
               column(width = 4,
                      numericInput(inputId = paste0("max_", v),
                                   label = NULL,
                                   value = dft_values$max[[v]],
                                   step = 0.5))
             )
           })

           # Organizing the UI
           column(width = 12, header, var_rows, cl_row, reset_btn, br(), build_btn)
         },

         "df" = {

           upload_row <- fluidRow(
             column(width = 12, p(instructions$range_data, class = "text-instruction")),
             column(width = 12,
                    fileInput(inputId = "df_range_file",
                              label = tags$span("Choose CSV file with range data",
                                                class = "text-widget-title"),
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values",
                                         "text/plain",
                                         ".csv", ".rds")))
           )

           obs_ranges <- NULL
           shared_vars <- character(0)
           df_range <- NULL

           if(!is.null(session_data$df_range)){
             shared_vars <- intersect(session_data$vars, colnames(session_data$df_range))
             obs_ranges  <- lapply(shared_vars, function(v){
               list(min = round(min(session_data$df_range[, v], na.rm = TRUE), 2),
                    max = round(max(session_data$df_range[, v], na.rm = TRUE), 2))
             })

             names(obs_ranges) <- shared_vars

           } else if(!is.null(input$df_range_file)){

             ext <- tolower(tools::file_ext(input$df_range_file$name))
             df_range <- tryCatch(
               load_df_file(input$df_range_file$datapath, ext),
               error = function(e) NULL
             )

             if(!is.null(df_range)){
               shared_vars <- intersect(vars, colnames(df_range))

               if(length(shared_vars) == length(vars)){
                 session_data$df_range <- df_range
                 obs_ranges <- lapply(shared_vars, function(v){
                   list(min = round(min(df_range[, v], na.rm = TRUE), 2),
                        max = round(max(df_range[, v], na.rm = TRUE), 2))
                 })
                 names(obs_ranges) <- shared_vars
               }
             }
           }

           header1 <- fluidRow(
             column(width = 4,
                    tags$span("Variable", class = "text-widget-title text-center")),
             column(width = 4,
                    tags$span("Observed Min", class = "text-widget-title text-center")),
             column(width = 4,
                    tags$span("Observed Max", class = "text-widget-title text-center"))
             )

           header2 <- fluidRow(
             column(width = 4,
                    tags$span("Variable", class = "text-widget-title text-center")),
             column(width = 4,
                    tags$div(class = "tooltip-label-row",
                             tags$span("Expand Min (%)", class = "text-widget-title text-center"),
                             tags$span(icon("circle-info"),
                                       title = instructions$expand_range_tooltip,
                                       class = "tooltip-icon"))),
             column(width = 4,
                    tags$div(class = "tooltip-label-row",
                             tags$span("Expand Max (%)", class = "text-widget-title text-center"),
                             tags$span(icon("circle-info"),
                                       title = instructions$expand_range_tooltip,
                                       class = "tooltip-icon")))
           )

           rows1 <- lapply(shared_vars, function(v){
             fluidRow(
               column(width = 4, class = "var-label",
                      tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      tags$span(format(round(obs_ranges[[v]]$min, 2), nsmall = 2),
                                class = "text-widget-inner text-center")),
               column(width = 4,
                      tags$span(format(round(obs_ranges[[v]]$max, 2), nsmall = 2),
                                class = "text-widget-inner text-center")))
           })

           rows2 <- lapply(shared_vars, function(v){
             fluidRow(
               column(width = 4, class = "var-label",
                      tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      numericInput(inputId = paste0("expand_min_df_", v),
                                   label = NULL,
                                   value = dft_values$expand_min[[v]],
                                   step = 5)),
               column(width = 4,
                      numericInput(inputId = paste0("expand_max_df_", v),
                                   label = NULL,
                                   value = dft_values$expand_max[[v]],
                                   step = 5))
             )
           })

           range_rows <- if(!is.null(obs_ranges) && length(shared_vars) > 0){
             tagList(header1, rows1, br(), header2, rows2)
           } else if(!is.null(input$df_range_file)){
             if(is.null(df_range)){
               p("Could not read the uploaded file. Please check the format.",
                 class = "text-warning-note")
             } else if(length(shared_vars) != length(vars)){
               p("No matching variables found between the uploaded file and the
         background layers. Check that column names match.",
                 class = "text-warning-note")
             }
           } else {
             NULL
           }

           column(width = 12, upload_row, range_rows, cl_row, reset_btn, br(), build_btn)
         },

         "stats" = {

           header1 <- fluidRow(
             column(width = 12,
                    p(instructions$range_stats, class = "text-instruction")
             ),
             column(width = 4,
                    tags$span("Variable", class = "text-widget-title text-center")
             ),
             column(width = 4,
                    tags$span("Mean", class = "text-widget-title text-center")
             ),
             column(width = 4,
                    tags$span("SD", class = "text-widget-title text-center")
             ))

           header2 <- fluidRow(
             column(width = 4,
                    tags$span("Variable", class = "text-widget-title text-center")
             ),
             column(width = 4,
                    tags$div(class = "tooltip-label-row",
                             tags$span("Expand Min (%)",
                                       class = "text-widget-title text-center"),
                             tags$span(icon("circle-info"),
                                       title = instructions$expand_range_tooltip,
                                       class = "tooltip-icon"))
             ),
             column(width = 4,
                    tags$div(class = "tooltip-label-row",
                             tags$span("Expand Max (%)",
                                       class = "text-widget-title text-center"),
                             tags$span(icon("circle-info"),
                                       title = instructions$expand_range_tooltip,
                                       class = "tooltip-icon"))
             )
           )

           var_rows1 <- lapply(vars, function(v){
             fluidRow(
               column(width = 4, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      numericInput(inputId = paste0("mean_", v),
                                   label = NULL,
                                   value = dft_values$mean[[v]],
                                   step = 0.5)
               ),

               column(width = 4,
                      numericInput(inputId = paste0("sd_", v),
                                   label = NULL,
                                   value = dft_values$sd[[v]],
                                   step = 0.5)
               ))
             })

           var_rows2 <- lapply(vars, function(v){
             fluidRow(
               column(width = 4, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      numericInput(inputId = paste0("expand_min_stats_", v),
                                   label = NULL,
                                   value = dft_values$expand_min[[v]],
                                   step = 5)),
               column(width = 4,
                      numericInput(inputId = paste0("expand_max_stats_", v),
                                   label = NULL,
                                   value =  dft_values$expand_max[[v]],
                                   step = 5))
             )
           })

           column(width = 12, header1, var_rows1, header2, var_rows2,
                  cl_row, reset_btn, br(), build_btn)
         }
  )
})

range_preview <- reactive({

  if(is.null(input$range_method_choice) || is.null(session_data$vars)){
    return(NULL)
  }

  vars <- session_data$vars

  switch(input$range_method_choice,

         "man" = {
           mins <- setNames(sapply(vars, function(v){
             input[[paste0("min_", v)]]}),
             vars)
           maxs <- setNames(sapply(vars, function(v){
             input[[paste0("max_", v)]]}),
             vars)

           # Guard: if any input hasn't rendered yet (e.g. right after editing
           # variables), mins/maxs may contain NULL. Bail out cleanly instead
           # of comparing incompatible types.
           if(!is.numeric(mins) || !is.numeric(maxs)) return(NULL)
           if(length(mins) != length(vars) || length(maxs) != length(vars)) return(NULL)

           if(any(is.na(mins)) || any(is.na(maxs))) return(NULL)
           if(any(maxs <= mins)) return(NULL)

           list(mins = mins, maxs = maxs)
         },

         "df" = {

           req(input$df_range_file)

           expand_min <- setNames(sapply(vars, function(v){
             if(!is.null(input[[paste0("expand_min_df_", v)]])){
               input[[paste0("expand_min_df_", v)]]
             }else{
               NULL
             }
           }),
           vars)

           expand_max <- setNames(sapply(vars, function(v){
             if(!is.null(input[[paste0("expand_max_df_", v)]])){
               input[[paste0("expand_max_df_", v)]]
             }else{
               NULL
             }
           }),
           vars)

           ext <- tolower(tools::file_ext(input$df_range_file$name))

           df_range <- tryCatch(
             load_df_file(input$df_range_file$datapath, ext),
             error = function(e) NULL)

           if(is.null(df_range)) return(NULL)

           shared_vars <- intersect(vars, colnames(df_range))
           if(length(shared_vars) != length(vars)) return(NULL)

           ranges_df <- ranges_from_data(data = df_range[, vars, drop = FALSE],
                                         expand_min = as.list(expand_min),
                                         expand_max = as.list(expand_max))

           list(mins = ranges_df["min", ],
                maxs = ranges_df["max", ],
                expand_min = expand_min,
                expand_max = expand_max)
         },

         "stats" = {
           means <- setNames(
             sapply(vars, function(v) input[[paste0("mean_", v)]]),
             vars)

           sds <- setNames(
             sapply(vars, function(v) input[[paste0("sd_", v)]]),
             vars)

           expand_min <- setNames(sapply(vars, function(v){
             if(!is.null(input[[paste0("expand_min_stats_", v)]])){
               input[[paste0("expand_min_stats_", v)]]
             }else{
               NULL
             }
           }),
           vars)

           expand_max <- setNames(sapply(vars, function(v){
             if(!is.null(input[[paste0("expand_max_stats_", v)]])){
               input[[paste0("expand_max_stats_", v)]]
             }else{
               NULL
             }
           }),
           vars)

           cl <- if(!is.null(input$cl_range)){
             input$cl_range
           }else{
             0.95
           }

           range_stats <- ranges_from_stats(mean = means, sd = sds, cl = cl,
                                            expand_min = as.list(expand_min),
                                            expand_max = as.list(expand_max))

           list(mins = range_stats["min", ],
                maxs = range_stats["max", ],
                mean = means, sd = sds,
                expand_min = expand_min,
                expand_max = expand_max)
         }
  )
})

observeEvent(input$reset_ranges, {

  req(input$range_method_choice, session_data$vars)
  vars <- session_data$vars
  has_bg_df <- !is.null(session_data$bg_df)

  if(!is.null(session_data$session_range)){
    dft_values <- session_data$session_range
  } else if(has_bg_df){
    q <- round(apply(session_data$bg_df[, vars, drop = FALSE], 2, quantile, na.rm = TRUE), 2)
    dft_values <- list(min = as.list(q[2, ]), max = as.list(q[4, ]),
                       mean = as.list((q[2, ] + q[4, ])/2),
                       sd = as.list((q[4, ] - q[2, ])/4),
                       expand_min = setNames(as.list(rep(0, length(vars))), vars),
                       expand_max = setNames(as.list(rep(0, length(vars))), vars))
  } else {
    dft_values <- list(min = setNames(as.list(rep(0, length(vars))), vars),
                       max = setNames(as.list(rep(1, length(vars))), vars),
                       mean = setNames(as.list(rep(0, length(vars))), vars),
                       sd = setNames(as.list(rep(1, length(vars))), vars),
                       expand_min = setNames(as.list(rep(0, length(vars))), vars),
                       expand_max = setNames(as.list(rep(0, length(vars))), vars))
  }

  switch(input$range_method_choice,

         "man" = {
           lapply(vars, function(v){
             updateNumericInput(session, paste0("min_", v),
                                value = dft_values$min[[v]])
             updateNumericInput(session, paste0("max_", v),
                                value = dft_values$max[[v]])
           })
         },

         "df" = {
           lapply(vars, function(v){
             updateNumericInput(session, paste0("expand_min_df_", v),
                                value = dft_values$expand_min[[v]])
             updateNumericInput(session, paste0("expand_max_df_", v),
                                value = dft_values$expand_max[[v]])
           })
         },

         "stats" = {
           lapply(vars, function(v){
             updateNumericInput(session, paste0("mean_", v),
                                value = dft_values$mean[[v]])
             updateNumericInput(session, paste0("sd_", v),
                                value = dft_values$sd[[v]])
             updateNumericInput(session, paste0("expand_min_stats_", v),
                                value = dft_values$expand_min[[v]])
             updateNumericInput(session, paste0("expand_max_stats_", v),
                                value = dft_values$expand_max[[v]])
           })

           updateNumericInput(session, "cl_range", value = 0.95)
         }
  )
})

observeEvent(input$edit_range, {

  showModal(modalDialog(
    title = "Edit ranges?",
    p("Editing ranges will delete all built ellipsoids,
    adjustments, and any downstream results."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_edit_range",
                   "Yes, edit ranges",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))

})

observeEvent(input$confirm_edit_range, {
  removeModal()

  session_data$current_ellipsoid <- NULL
  session_data$ellipsoid_list <- list()
  session_data$ellipsoid_prediction_list <- list()
  session_data$session_range <- NULL

  covariance_set(FALSE)
  cov_counters(list())

  centroid_set(FALSE)

  if(!is.null(session_data$df_range) &&
     identical(session_data$input_mode, "prev_session")){
    updateRadioButtons(session, "range_method_choice", selected = "df")
  } else {
    updateRadioButtons(session, "range_method_choice", selected = character(0))
  }

})


# COVARIANCE --------------------------------------------------------------

# cov - reactives
covariance_set <- reactiveVal(FALSE)
cov_counters <- reactiveVal(list())

output$covariance_ui <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  if(isTRUE(covariance_set())){
    return(
      box(title = tags$span("Covariance", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p("Covariance has been set for this ellipsoid.",
            class = "text-instruction"),
          fluidRow(
            column(12, class = "btn-spaced",
                   actionLink("edit_covariance",
                              label = tagList(icon("pen"), "Edit covariance")))
          )
      )
    )
  }

  box(title = tags$span("Covariance", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$covariance, class = "text-instruction"),
      uiOutput("covariance_sliders_ui"),
      br(),
      fluidRow(
        column(width = 6,
               class = "btn-spaced",
               actionButton("set_cov",
                            "Set Covariance",
                            class = "btn-primary"))
      )
  )
})

output$covariance_sliders_ui <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  ell <- session_data$current_ellipsoid
  id <- ell$ell_id

  vars <- ell$var_names
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  lims <- ell$cov_limits
  rownames(lims) <- pair_names

  sliders <- lapply(pair_names, function(pn){
    min_val <- lims[pn, "min"]
    max_val <- lims[pn, "max"]
    step <- round((max_val - min_val) / 100, 4)
    parts <- strsplit(pn, "-")[[1]]
    cur_cov <- ell$cov_matrix[parts[1], parts[2]]

    fluidRow(
      column(width = 10,
             sliderInput(inputId = paste0("cov_", id, "_", pn),
                         label = pn,
                         min = round(min_val, 2),
                         max = round(max_val, 2),
                         value = round(cur_cov, 4),
                         step = step)),
      column(width = 2,
             actionLink(inputId = paste0("cov_reset_", id, "_", pn),
                        label = tags$span(icon("rotate-left"),
                                          title = instructions$covariance_reset_tooltip))
      ))
  })

  reset_all_btn <- fluidRow(
    column(width = 12, class = "btn-spaced",
           actionLink(inputId = paste0("cov_reset_all_", id),
                      label = tagList(icon("rotate-left"), "Reset all to zero")))
  )

  cov_counters(setNames(lapply(pair_names, function(pn) 0), pair_names))

  tagList(sliders, reset_all_btn)
})

# Covariance update observer
observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))
  lapply(pair_names, function(pn) input[[paste0("cov_", id, "_", pn)]])
}, {
  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  # Collect current slider values as named numeric vector
  cov_vals <- setNames(sapply(pair_names,
                              function(pn){
                                val <- input[[paste0("cov_", id, "_", pn)]]
                                if(is.null(val)) 0 else val
                              }),
                       pair_names)

  # Only update if values differ from current covariance matrix
  current_cov <- setNames(sapply(seq_len(nrow(pairs)),
                                 function(i){
                                   ell$cov_matrix[pairs[i, 1], pairs[i, 2]]
                                 }),
                          pair_names)

  if(all(cov_vals == current_cov)) return()

  updated_ell <- tryCatch(
    update_ellipsoid_covariance(object = ell,
                                covariance = cov_vals,
                                verbose = FALSE),
    error = function(e){
      showNotification(paste("Covariance update failed:", e$message),
                       type = "error")
      NULL
    }
  )

  req(updated_ell)

  updated_ell$ell_id <- ell$ell_id
  updated_ell$ell_name <- ell$ell_name
  session_data$current_ellipsoid <- updated_ell

  # Silently update slider bounds for remaining pairs using
  # cov_limits_remaining, preserving current selected values
  remaining <- updated_ell$cov_limits_remaining

  if(!is.null(remaining)){
    remaining_names <- rownames(remaining)

    lapply(remaining_names,
           function(pn){
             slider_id <- paste0("cov_", id, "_", pn)
             cur_val <- input[[slider_id]]

             if (!is.null(cur_val)){
               new_min <- remaining[pn, "min"]
               new_max <- remaining[pn, "max"]

               # Clamp current value to new limits silently
               clamped <- max(new_min, min(new_max, cur_val))

               updateSliderInput(session,
                                 inputId = slider_id,
                                 min = round(new_min, 2),
                                 max = round(new_max, 2),
                                 value = round(clamped, 2))
             }
           })
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Rests covariances to zeros
observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))
  lapply(pair_names, function(pn) input[[paste0("cov_reset_", id, "_", pn)]])
}, {
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  req(cov_counters)

  old_cnts <- cov_counters()

  # Find which pair was clicked by comparing current vs previous counter
  reset_pn <- pair_names[sapply(pair_names,
                                function(pn){
                                  cur <- input[[paste0("cov_reset_", id, "_", pn)]]
                                  prev <- old_cnts[[pn]]
                                  return(!is.null(cur) && !is.null(prev) && cur > prev)
                                })]

  req(length(reset_pn) > 0)
  reset_pn <- reset_pn[1]

  updateSliderInput(session,
                    inputId = paste0("cov_", id, "_", reset_pn),
                    value = 0)

  # Update stored counters
  new_cnts <- setNames(lapply(pair_names,
                              function(pn){
                                input[[paste0("cov_reset_", id, "_", pn)]]
                              }),
                       pair_names)

  cov_counters(new_cnts)
})

# Rest Covariance all
observeEvent(input[[paste0("cov_reset_all_", session_data$current_ellipsoid$ell_id)]], {

  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  lapply(pair_names, function(pn){
    updateSliderInput(session,
                      inputId = paste0("cov_", id, "_", pn),
                      value = 0)
  })

  # Reset counters so the per-pair reset observer doesn't misfire
  cov_counters(setNames(lapply(pair_names, function(pn) 0), pair_names))

}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Set current ellipsoid covariance
observeEvent(input$set_cov, {
  covariance_set(TRUE)
})

# Edit current ellipsoid covariance
observeEvent(input$edit_covariance, {
  covariance_set(FALSE)
})

# CENTROID ----------------------------------------------------------------

centroid_set <- reactiveVal(FALSE)


# centroid_preview reactive
centroid_preview <- reactive({

  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  vars <- ell$var_names
  id <- ell$ell_id

  vals <- setNames(sapply(vars, function(v){
    input[[paste0("centroid_", v, "_", id)]]
  }), vars)

  if(!is.numeric(vals) || any(is.na(vals))) return(NULL)

  vals
})

# Centroid mover box
output$centroid_mover_ui <- renderUI({

  req(length(session_data$ellipsoid_list) > 0, isTRUE(covariance_set()))

  if(isTRUE(centroid_set())){

    ell <- session_data$current_ellipsoid
    centroid <- if(!is.null(ell)) ell$centroid else NULL

    centroid_rows <- if(!is.null(centroid)){
      lapply(names(centroid), function(v){
        fluidRow(
          column(width = 6, tags$span(v, class = "text-widget-inner")),
          column(width = 6, tags$span(round(centroid[v], 3),
                                      class = "text-widget-inner"))
        )
      })
    }

    return(
      box(title = tags$div("Centroid Mover", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,

          fluidRow(
            column(width = 6,
                   tags$span("Variable", class = "text-widget-title")),
            column(width = 6,
                   tags$span("Centroid", class = "text-widget-title"))
          ),

          tagList(centroid_rows),

          br(),

          fluidRow(
            column(width = 12, class = "btn-spaced",
                   actionLink("edit_centroid",
                              label = tagList(icon("pen"), "Edit centroid")))
          )
      )
    )
  }

  box(title = tags$div("Centroid Mover", class = "text-section-header"),
      width       = 12,
      collapsible = TRUE,
      collapsed   = FALSE,
      p(instructions$centroid_mover, class = "text-instruction"),
      uiOutput("centroid_sliders_ui"),
      fluidRow(
        column(width = 6, class = "btn-spaced",
               actionButton("set_centroid",
                            "Set Centroid",
                            class = "btn-primary"))
      )
  )
})

# Centroid sliders
output$centroid_sliders_ui <- renderUI({

  req(length(session_data$ellipsoid_list) > 0)

  id <- session_data$current_ellipsoid$ell_id
  ell <- isolate(session_data$current_ellipsoid)
  req(ell)

  vars <- ell$var_names
  centroid <- ell$centroid

  sliders <- lapply(vars, function(v){
    sd_val <- 3 * sd(session_data$bg_df[, v], na.rm = TRUE)
    min_val <- round(min(session_data$bg_df[, v], na.rm = TRUE) - sd_val, 2)
    max_val <- round(max(session_data$bg_df[, v], na.rm = TRUE) + sd_val, 2)
    step <- round((max_val - min_val) / 100, 2)

    fluidRow(
      column(width = 10,
             sliderInput(inputId = paste0("centroid_", v, "_", id),
                         label = v,
                         min = min_val,
                         max = max_val,
                         value = round(centroid[v], 2),
                         step = step))
      # column(width = 2,
      #        actionLink(inputId = paste0("centroid_reset_", v, "_", id),
      #                   label = tags$span(icon("rotate-left"),
      #                                     title = instructions$centroid_reset_tooltip,
      #                                     class = "tooltip-icon"))
      #
      )
  })

  reset_all_btn <- fluidRow(
    column(width = 12, class = "btn-spaced",
           actionLink(inputId = paste0("centroid_reset_all_", id),
                      label = tagList(icon("rotate-left"),
                                      "Reset all to base centroid")))
  )

  tagList(sliders, br(), reset_all_btn, br())
})

# Set centroid observer
observeEvent(input$set_centroid, {
  centroid_set(TRUE)
})

observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id
  lapply(vars, function(v) input[[paste0("centroid_", v, "_", id)]])
}, {

  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  id <- session_data$current_ellipsoid$ell_id

  ell_base <- session_data$ellipsoid_list[["base"]]
  base_c <- ell_base$centroid


  # Collect current slider values as named numeric vector
  centroid_vals <- setNames(sapply(vars,
                              function(v){
                                val <- input[[paste0("centroid_", v, "_", id)]]
                                if(is.null(val)) base_c[v] else val
                              }),
                       vars)

  # Only update if values differ from current covariance matrix
  current_centroid <- setNames(sapply(vars,
                                 function(v){
                                   ell$centroid[v]
                                 }),
                          vars)

  if(all(centroid_vals == current_centroid)) return()


  updated_ell <- tryCatch(
    update_ellipsoid_centroid(session_data$current_ellipsoid,
                              new_centroid = centroid_vals,
                              verbose = FALSE),
    error = function(e){
      showNotification(paste("Centroid update failed:", e$message),
                       type = "error")
      NULL
    }
  )

  req(updated_ell)

  updated_ell$ell_id <- ell$ell_id
  updated_ell$ell_name <- ell$ell_name
  session_data$current_ellipsoid <- updated_ell

}, ignoreNULL = TRUE, ignoreInit = TRUE)


# Edit centroid observer
observeEvent(input$edit_centroid, {
  centroid_set(FALSE)
}, ignoreInit = TRUE)

# Per-variable centroid reset observer
observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  id <- ell$ell_id
  lapply(ell$var_names, function(v) input[[paste0("centroid_reset_", v, "_", id)]])
}, {
  ell <- session_data$current_ellipsoid
  req(ell)
  id <- ell$ell_id
  ell_base <- session_data$ellipsoid_list[["base"]]
  base_c <- ell_base$centroid

  clicked <- ell$var_names[vapply(ell$var_names, function(v){
    val <- input[[paste0("centroid_reset_", v, "_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)

  lapply(clicked, function(v){
    updateSliderInput(session,
                      inputId = paste0("centroid_", v, "_", id),
                      value = round(base_c[v], 4))
  })

}, ignoreInit = TRUE)

# Reset all centroid observer
observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  input[[paste0("centroid_reset_all_", ell$ell_id)]]}, {

  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  id <- ell$ell_id
  ell_base <- session_data$ellipsoid_list[["base"]]
  base_c <- setNames(object = ell_base$centroid, nm = ell$var_names)

  lapply(ell$var_names, function(v){
    updateSliderInput(session,
                      inputId = paste0("centroid_", v, "_", id),
                      value = round(base_c[v], 2))
  })

}, ignoreNULL = TRUE, ignoreInit = TRUE)



