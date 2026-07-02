# Title: Build tab server logic
# Description: Handles range inputs
# Date last updated: 07/02/2026


# Functions ---------------------------------------------------------------

#' Build elliposid helper function
#'
#' @description
#' helper function is used to keep track of the ellipsoid version being ran,
#' so if user click on build ellipsoid again it can get prompted that the
#' other ellipsoid will be overwritten.
#'
#' @returns does not return, only calls to main function build_ellipsoid()
#' @keywords internal
#'
#' @noRd
build_ellipsoid_shiny <- function(){
  req(session_data$vars)

  ranges <- range_preview()

  if(is.null(ranges)){
    showNotification("Please check your range inputs before building.",
                     type = "error")
    return()
  }

  # Convert mins/maxs back to range_df format for build_ellipsoid
  vars <- session_data$vars
  range_df <- as.data.frame(rbind(unlist(ranges$mins),
                                  unlist(ranges$maxs)),
                            row.names = c("min", "max"))
  colnames(range_df) <- vars

  cl <- if(!is.null(input$cl_range)) input$cl_range else 0.95

  tryCatch({
    session_data$current_ellipsoid <- build_ellipsoid(range = range_df,
                                                      cl = cl,
                                                      verbose = FALSE)
    session_data$ellipsoid_list[["base"]] <- session_data$current_ellipsoid

    session_data$current_ellipsoid_id <- "base"

    showNotification("Ellipsoid built successfully.", type = "message")

  }, error = function(e){
    showNotification(paste("Error building ellipsoid:", e$message),
                     type = "error")
  })
}


# Observers ----------------------------------------------------------------

# Reset range values to defaults
observeEvent(input$reset_ranges, {

  req(input$range_method_choice, session_data$vars)
  vars <- session_data$vars
  has_bg_df <- !is.null(session_data$bg_df)

  switch(input$range_method_choice,

         "man" = {
           lapply(vars, function(v){
             if(has_bg_df){
               vals <- session_data$bg_df[[v]]
               m <- round(mean(vals, na.rm = TRUE), 2)
               s <- round(sd(vals, na.rm = TRUE), 2)
             } else {
               m <- 0
               s <- 1
             }
             updateNumericInput(session, paste0("min_", v), value = m)
             updateNumericInput(session, paste0("max_", v), value = m + s)
           })
         },

         "df" = {
           lapply(vars, function(v){
             updateNumericInput(session, paste0("expand_min_df_", v), value = 0)
             updateNumericInput(session, paste0("expand_max_df_", v), value = 0)
           })
         },

         "stats" = {
           lapply(vars, function(v){
             if(has_bg_df){
               vals <- session_data$bg_df[[v]]
               m <- round(mean(vals, na.rm = TRUE), 2)
               s <- round(sd(vals, na.rm = TRUE), 2)
             } else {
               m <- 0
               s <- 1
             }
             updateNumericInput(session, paste0("mean_", v), value = m)
             updateNumericInput(session, paste0("sd_", v), value = s)
             updateNumericInput(session, paste0("expand_min_stats_", v), value = 0)
             updateNumericInput(session, paste0("expand_max_stats_", v), value = 0)
           })

           updateNumericInput(session, "cl_range", value = 0.95)
         }
  )
})

# Rests covariances to zeros
observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  version <- session_data$current_ellipsoid_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))
  lapply(pair_names, function(pn) input[[paste0("cov_reset_", version, "_", pn)]])
}, {
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  version <- session_data$current_ellipsoid_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  req(cov_counters)

  old_cnts <- cov_counters()

  # Find which pair was clicked by comparing current vs previous counter
  reset_pn <- pair_names[sapply(pair_names,
                                function(pn){
                                  cur <- input[[paste0("cov_reset_", version, "_", pn)]]
                                  prev <- old_cnts[[pn]]
                                  return(!is.null(cur) && !is.null(prev) && cur > prev)
                                })]

  req(length(reset_pn) > 0)
  reset_pn <- reset_pn[1]

  updateSliderInput(session,
                    inputId = paste0("cov_", version, "_", reset_pn),
                    value = 0)

  # Update stored counters
  new_cnts <- setNames(lapply(pair_names,
                              function(pn){
                                input[[paste0("cov_reset_", version, "_", pn)]]
                              }),
                       pair_names)

  cov_counters(new_cnts)
})

# Button triggered, builds the ellipsoid
observeEvent(input$build_ell, {

  # If an ellipsoid already exists warn the user
  if (length(session_data$ellipsoid_list) > 0){
    showModal(modalDialog(
      title = "Overwrite ellipsoid?",
      p("One or more ellipsoids have already been built. Building a new one will
overwrite the all current and saved ellipsoids and any downstream results."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_build_ell",
                     "Yes, overwrite",
                     class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  } else {
    build_ellipsoid_shiny()
  }
})

# Overwrites the elliposoid
observeEvent(input$confirm_build_ell, {
  removeModal()
  build_ellipsoid_shiny()

})

# Covariance update observer
observeEvent({
  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  version <- session_data$current_ellipsoid_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))
  lapply(pair_names, function(pn) input[[paste0("cov_", version, "_", pn)]])
}, {
  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  vars <- ell$var_names
  version <- session_data$current_ellipsoid_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  # Collect current slider values as named numeric vector
  cov_vals <- setNames(sapply(pair_names,
                              function(pn){
                                val <- input[[paste0("cov_", version, "_", pn)]]
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

  session_data$current_ellipsoid <- updated_ell

  # Silently update slider bounds for remaining pairs using
  # cov_limits_remaining, preserving current selected values
  remaining <- updated_ell$cov_limits_remaining

  if(!is.null(remaining)){
    remaining_names <- rownames(remaining)

    lapply(remaining_names,
           function(pn){
             slider_id <- paste0("cov_", version, "_", pn)
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


# Rest Cov all
observeEvent(input[[paste0("cov_reset_all_", session_data$current_ellipsoid_id)]], {

  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  vars <- ell$var_names
  version <- session_data$current_ellipsoid_id
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  lapply(pair_names, function(pn){
    updateSliderInput(session,
                      inputId = paste0("cov_", version, "_", pn),
                      value = 0)
  })

  # Reset counters so the per-pair reset observer doesn't misfire
  cov_counters(setNames(lapply(pair_names, function(pn) 0), pair_names))

}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$set_cov, {
  covariance_set(TRUE)
})

observeEvent(input$edit_covariance, {
  covariance_set(FALSE)
})

observeEvent(input$next_build_another, {
  removeModal()

  base_ell <- session_data$ellipsoid_list[["base"]]
  req(base_ell)

  session_data$current_ellipsoid <- base_ell
  session_data$current_ellipsoid_id <- "base"
  covariance_set(FALSE)
  cov_counters(list())

  showNotification("Covariance reset. Adjust and save a new version.",
                   type = "message", duration = 3)
})

observeEvent(input$save_ell_version, {

  req(session_data$current_ellipsoid)

  n <- length(session_data$ellipsoid_list) + 1L
  deflt <- paste0("ellipsoid_", n)

  showModal(modalDialog(
    title = "Save Ellipsoid Version",
    p("Give this ellipsoid a name. Use letters, numbers, and spaces only.
       Spaces will be replaced with underscores."),
    textInput("ell_save_name",
              label = NULL,
              value = deflt,
              placeholder = deflt),
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
    paste0("Ellipsoid_", length(session_data$ellipsoid_list))
  }

  clean_name <- gsub("\\s+", "_", trimws(raw_name))
  clean_name <- substr(clean_name, 1, 30)

  # Ensure uniqueness with exact matching, not regex
  existing <- setdiff(names(session_data$ellipsoid_list), "base")
  base_name <- clean_name
  suffix <- 1L

  while(clean_name %in% existing){
    clean_name <- substr(paste0(base_name, "_", suffix), 1, 30)
    suffix <- suffix + 1L
  }

  new_list  <- session_data$ellipsoid_list
  new_list[[clean_name]] <- session_data$current_ellipsoid
  session_data$ellipsoid_list <- new_list

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

# Continue to predict: navigate to the predict tab
observeEvent(input$next_go_predict, {
  removeModal()
  updateTabItems(session, "sidebarMenu", selected = "predict_tab")
})

# Reactives ---------------------------------------------------------------

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
                maxs = ranges_df["max", ])
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
                maxs = range_stats["max", ])
         }
  )
})

covariance_set <- reactiveVal(FALSE)
cov_counters <- reactiveVal(list())

# Render Outputs ----------------------------------------------------------

observeEvent(input$edit_range, {

  showModal(modalDialog(
    title = "Edit ranges?",
    p("Editing ranges will delete all built ellipsoids,
    adjustments,
       and any prediction results."),
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
  session_data$current_ellipsoid_id <- NULL
  session_data$ellipsoid_list <- list()
  session_data$ellipsoid_prediction_list <- list()

  covariance_set(FALSE)
  cov_counters(list())

  updateRadioButtons(session, "range_method_choice", selected = character(0))
})

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
             default_min <- if(has_bg_df){
               round(mean(session_data$bg_df[, v], na.rm = TRUE), 2)
             } else {
               0
             }
             default_max <- if(has_bg_df){
               round(mean(session_data$bg_df[, v], na.rm = TRUE), 2) +
                 round(sd(session_data$bg_df[, v], na.rm = TRUE), 2)
             } else {
               1
             }

             fluidRow(
               column(width = 4, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      numericInput(inputId = paste0("min_", v),
                                   label = NULL,
                                   value = default_min,
                                   step = 0.5)),
               column(width = 4,
                      numericInput(inputId = paste0("max_", v),
                                   label = NULL,
                                   value = default_max,
                                   step = 0.5))
             )
           })

           # Organizing the UI
           column(width = 12, header, var_rows, cl_row, reset_btn, br(), build_btn)
         },

         "df" = {

           upload_row <- fluidRow(
             column(width = 12, p(instructions$range_data, class = "text-instruction")),
             column(width = 6,
                    fileInput(inputId = "df_range_file",
                              label = tags$span("Choose CSV file with range data",
                                                class = "text-widget-title"),
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values",
                                         "text/plain",
                                         ".csv", ".rds")))
           )

           range_rows <- if(!is.null(input$df_range_file)){

             ext <- tolower(tools::file_ext(input$df_range_file$name))

             df_range <- tryCatch(
               load_df_file(input$df_range_file$datapath, ext),
               error = function(e) NULL)

             if(is.null(df_range)){

               p("Could not read the uploaded file. Please check the format.",
                 class = "text-warning-note")

             }else{

               shared_vars <- intersect(vars, colnames(df_range))

               if(length(shared_vars) != length(vars)){

                 p("No matching variables found between the uploaded file and the
background layers. Check that column names match.",
                   class = "text-warning-note")

               }else{

                 session_data$range_df <- df_range

                 obs_ranges <- lapply(shared_vars, function(v){
                   list(min = round(min(df_range[, v], na.rm = TRUE), 2),
                        max = round(max(df_range[, v], na.rm = TRUE), 2))
                 })

                 names(obs_ranges) <- shared_vars

                 header <- fluidRow(
                   column(width = 3, tags$span("Variable", class = "text-widget-title text-center")),
                   column(width = 2, tags$span("Observed Min", class = "text-widget-title text-center")),
                   column(width = 2, tags$span("Observed Max", class = "text-widget-title text-center")),
                   column(width = 2, tags$div(class = "tooltip-label-row",
                                              tags$span("Expand Min (%)", class = "text-widget-title text-center"),
                                              tags$span(icon("circle-info"), title = instructions$expand_range_tooltip, class = "tooltip-icon"))),
                   column(width = 2, tags$div(class = "tooltip-label-row",
                                              tags$span("Expand Max (%)", class = "text-widget-title text-center"),
                                              tags$span(icon("circle-info"), title = instructions$expand_range_tooltip, class = "tooltip-icon")))
                 )

                 rows <- lapply(shared_vars, function(v){
                   fluidRow(
                     column(width = 3, class = "var-label", tags$span(v, class = "text-widget-inner")),
                     column(width = 2, tags$span(format(round(obs_ranges[[v]]$min, 2), nsmall = 2), class = "text-widget-inner text-center")),
                     column(width = 2, tags$span(format(round(obs_ranges[[v]]$max, 2), nsmall = 2), class = "text-widget-inner text-center")),
                     column(width = 2,
                            numericInput(inputId = paste0("expand_min_df_", v),
                                         label = NULL,
                                         value = 0,
                                         step = 5)),
                     column(width = 2,
                            numericInput(inputId = paste0("expand_max_df_", v),
                                         label = NULL,
                                         value = 0,
                                         step = 5))
                   )
                 })

                 tagList(header, rows)
               }
             }
           }

           column(width = 12, upload_row, range_rows, cl_row, reset_btn, br(), build_btn)
         },

         "stats" = {

           header <- fluidRow(
             column(width = 12, p(instructions$range_stats, class = "text-instruction")),
             column(width = 3, tags$span("Variable", class = "text-widget-title text-center")),
             column(width = 2, tags$span("Mean", class = "text-widget-title text-center")),
             column(width = 2, tags$span("SD", class = "text-widget-title text-center")),
             column(width = 2, tags$div(class = "tooltip-label-row",
                                        tags$span("Expand Min (%)", class = "text-widget-title text-center"),
                                        tags$span(icon("circle-info"), title = instructions$expand_range_tooltip, class = "tooltip-icon"))),
             column(width = 2, tags$div(class = "tooltip-label-row",
                                        tags$span("Expand Max (%)", class = "text-widget-title text-center"),
                                        tags$span(icon("circle-info"), title = instructions$expand_range_tooltip, class = "tooltip-icon")))
           )

           var_rows <- lapply(vars, function(v){
             default_mean <- if(has_bg_df){
               round(mean(session_data$bg_df[, v], na.rm = TRUE), 2)
             } else {
               0
             }
             default_sd <- if(has_bg_df){
               round(sd(session_data$bg_df[, v], na.rm = TRUE), 2)
             } else {
               1
             }

             fluidRow(
               column(width = 3, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 2,
                      numericInput(inputId = paste0("mean_", v),
                                   label = NULL,
                                   value = default_mean,
                                   step = 0.5)
               ),

               column(width = 2,
                      numericInput(inputId = paste0("sd_", v),
                                   label = NULL,
                                   value = default_sd,
                                   step = 0.5)
               ),
               column(width = 2,
                      numericInput(inputId = paste0("expand_min_stats_", v),
                                   label = NULL,
                                   value = 0,
                                   step = 5)),
               column(width = 2,
                      numericInput(inputId = paste0("expand_max_stats_", v),
                                   label = NULL,
                                   value = 0,
                                   step = 5))
             )
           })

           column(width = 12, header, var_rows, cl_row, reset_btn, br(), build_btn)
         }
  )
})

output$covariance_ui <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  if(isTRUE(covariance_set())){
    return(
      box(title = tags$span("Covariance", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p("Covariance has been set for this ellipsoid.", class = "text-instruction"),
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

  ell <- isolate(session_data$current_ellipsoid)
  vars <- isolate(ell$var_names)
  version <- session_data$current_ellipsoid_id

  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  lims <- ell$cov_limits
  rownames(lims) <- pair_names

  sliders <- lapply(pair_names, function(pn){
    min_val <- lims[pn, "min"]
    max_val <- lims[pn, "max"]
    step <- round((max_val - min_val) / 100, 4)

    fluidRow(
      column(width = 11,
             sliderInput(inputId = paste0("cov_", version, "_", pn),
                         label = pn,
                         min = round(min_val, 2),
                         max = round(max_val, 2),
                         value = 0,
                         step = step)),
      column(width = 1,
             actionLink(inputId = paste0("cov_reset_", version, "_", pn),
                        label = tags$span(icon("rotate-left"),
                                          title = instructions$covariance_reset_tooltip))
      ))
  })

  reset_all_btn <- fluidRow(
    column(width = 12, class = "btn-spaced",
           actionLink(inputId = paste0("cov_reset_all_", version),
                      label = tagList(icon("rotate-left"), "Reset all to zero")))
  )

  cov_counters(setNames(lapply(pair_names, function(pn) 0), pair_names))

  tagList(sliders, reset_all_btn)
})

output$centroid_mover_ui <- renderUI({

  req(length(session_data$ellipsoid_list) > 0, isTRUE(covariance_set()))

  box(title = tags$div("Centroid Mover", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$centroid_mover, class = "text-instruction"),
      uiOutput("centroid_sliders_ui"),
      fluidRow(
        column(width = 6,
               class = "btn-spaced",
               actionButton("save_ell_version",
                            "Save Elliposid Version",
                            class = "btn-primary"))
      )
  )

})

output$centroid_sliders_ui <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  ell <- isolate(session_data$current_ellipsoid)
  vars <- isolate(ell$var_names)
  version <- session_data$current_ellipsoid_id

  centroid <- ell$centroid

  sliders <- lapply(vars, function(v){
    sd_val <- 3 * round(sd(session_data$bg_df[, v], na.rm = TRUE))
    min_val <- round(max(session_data$bg_df[, v], na.rm = TRUE)) - sd_val
    max_val <- round(max(session_data$bg_df[, v], na.rm = TRUE)) + sd_val
    step <- round((max_val - min_val) / 100, 4)

    fluidRow(
      column(width = 11,
             sliderInput(inputId = paste0("centroid_", v, "_", version),
                         label = v,
                         min = round(min_val, 2),
                         max = round(max_val, 2),
                         value = centroid[v],
                         step = step)),
      column(width = 1,
             actionLink(inputId = paste0("centroid_reset_", v, "_", version),
                        label = tags$span(icon("rotate-left"),
                                          title = instructions$centroid_reset_tooltip))
      ))
  })

  reset_all_btn <- fluidRow(
    column(width = 12, class = "btn-spaced",
           actionLink(inputId = paste0("centroid_reset_all_", version),
                      label = tagList(icon("rotate-left"), "Reset all back to original values")))
  )

  tagList(sliders, br(), reset_all_btn)
})


output$ellipsoid_library <- renderUI({

  req(length(session_data$ellipsoid_list) > 0)

  versions <- session_data$ellipsoid_list
  nms <- names(versions)

  rows <- lapply(nms, function(nm){

    is_base <- nm == "base"
    is_current <- identical(session_data$current_ellipsoid_id, nm)

    fluidRow(
      style = if(is_current) "background: #f0f7f0; border-radius: 4px;" else "",

      column(width = 6,
             tags$span(
               if(is_current) icon("circle-check") else icon("circle"),
               nm,
               class = "text-widget-inner",
               style = if(is_current) "color: #2ecc71; font-weight: 500;" else ""
             )),

      column(width = 6,
             tags$div(
               style = "display: flex; gap: 8px;",

               # Load as current
               actionLink(
                 inputId = paste0("ell_load_", nm),
                 label   = tags$span(icon("pen-to-square"),
                                     title = paste0("Load ", nm, " as current ellipsoid"),
                                     class = "tooltip-icon")
               ),

               # Delete, not available for base
               if(!is_base){
                 actionLink(
                   inputId = paste0("ell_delete_", nm),
                   label   = tags$span(icon("trash"),
                                       title = paste0("Delete ", nm),
                                       class = "tooltip-icon",
                                       style = "color: #e74c3c;")
                 )
               } else {
                 tags$span(icon("lock"),
                           title = "Base ellipsoid cannot be deleted",
                           class = "tooltip-icon",
                           style = "color: #ccc;")
               }
             ))
    )
  })

  box(title = tags$span("Ellipsoid library", class = "text-section-header"),
      width       = 12,
      collapsible = TRUE,
      collapsed   = FALSE,

      fluidRow(
        column(width = 6, tags$span("Name",   class = "text-widget-title")),
        column(width = 6, tags$span("Actions", class = "text-widget-title"))
      ),

      tagList(rows)
  )
})


# Load observer
observeEvent({
  nms <- names(session_data$ellipsoid_list)
  lapply(nms, function(nm) input[[paste0("ell_load_", nm)]])
}, {
  nms <- names(session_data$ellipsoid_list)
  req(length(nms) > 0)

  clicked <- nms[vapply(nms, function(nm){
    val <- input[[paste0("ell_load_", nm)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  nm <- clicked[1]

  session_data$current_ellipsoid    <- session_data$ellipsoid_list[[nm]]
  session_data$current_ellipsoid_id <- nm
  covariance_set(FALSE)
  cov_counters(list())

  showNotification(paste0(nm, " loaded as current ellipsoid."),
                   type = "message", duration = 3)

}, ignoreInit = TRUE)


# Delete observer
observeEvent({
  nms <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(nms, function(nm) input[[paste0("ell_delete_", nm)]])
}, {
  nms <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(nms) > 0)

  clicked <- nms[vapply(nms, function(nm){
    val <- input[[paste0("ell_delete_", nm)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  nm <- clicked[1]

  showModal(modalDialog(
    title = paste0("Delete ", nm, "?"),
    p(paste0("This will permanently remove ", nm,
             " and any prediction results associated with it.")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0("confirm_ell_delete_", nm),
                   "Yes, delete",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))

}, ignoreInit = TRUE)


# Confirmed delete, dynamic per name
observeEvent({
  nms <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(nms, function(nm) input[[paste0("confirm_ell_delete_", nm)]])
}, {
  nms <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(nms) > 0)

  clicked <- nms[vapply(nms, function(nm){
    val <- input[[paste0("confirm_ell_delete_", nm)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  nm <- clicked[1]

  removeModal()

  session_data$ellipsoid_list[[nm]] <- NULL

  if(!is.null(session_data$ellipsoid_prediction_list[[nm]])){
    session_data$ellipsoid_prediction_list[[nm]] <- NULL
  }

  # If the deleted version was current, fall back to base
  if(identical(session_data$current_ellipsoid_id, nm)){
    session_data$current_ellipsoid    <- session_data$ellipsoid_list[["base"]]
    session_data$current_ellipsoid_id <- "base"
    covariance_set(FALSE)
    cov_counters(list())
    showNotification(paste0(nm, " deleted. Reverted to base ellipsoid."),
                     type = "message", duration = 3)
  } else {
    showNotification(paste0(nm, " deleted."),
                     type = "message", duration = 3)
  }

}, ignoreInit = TRUE)
