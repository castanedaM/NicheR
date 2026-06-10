# Title: Build tab server logic
# Description: Handles range inputs and e-space plot
# Date last updated: 06/10/2026



# Functions ---------------------------------------------------------------

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
    session_data$ellipsoid <- build_ellipsoid(range = range_df,
                                              cl = cl,
                                              verbose = FALSE)

    session_data$ellipsoid_version <- session_data$ellipsoid_version + 1L

    showNotification("Ellipsoid built successfully.", type = "message")

  }, error = function(e) {
    showNotification(paste("Error building ellipsoid:", e$message),
                     type = "error")
  })
}

# Observers ----------------------------------------------------------------

observeEvent(input$reset_ranges, {

  req(input$range_method_choice, session_data$vars, session_data$bg_df)
  vars <- session_data$vars

  switch(input$range_method_choice,

         "man" = {
           lapply(vars, function(v){
             vals <- session_data$bg_df[[v]]
             m <- round(mean(vals, na.rm = TRUE), 2)
             s <- round(sd(vals, na.rm = TRUE), 2)
             updateNumericInput(session, paste0("min_", v), value = m)
             updateNumericInput(session, paste0("max_", v), value = m + s)
           })
         },

         "df" = {
           req(session_data$df_range)
           lapply(vars, function(v){
             updateNumericInput(session, paste0("expand_min_df_", v), value = 0)
             updateNumericInput(session, paste0("expand_max_df_", v), value = 0)
           })
         },

         "stats" = {
           lapply(vars, function(v){
             vals <- session_data$bg_df[[v]]
             updateNumericInput(session, paste0("mean_", v),
                                value = round(mean(vals, na.rm = TRUE), 2))
             updateNumericInput(session, paste0("sd_", v),
                                value = round(sd(vals, na.rm = TRUE), 2))
             updateNumericInput(session, paste0("expand_min_stats_", v), value = 0)
             updateNumericInput(session, paste0("expand_max_stats_", v), value = 0)
           })

           updateNumericInput(session, "cl_range", value = 0.99)
         }
  )
})

observeEvent({
  input$plot_state
  input$plot_2d_x
  input$plot_2d_y
  session_data$vars
 }, {

    req(session_data$vars)

    vars <- session_data$vars

    req(input$plot_state == "plot_2d")

    x_sel <- input$plot_2d_x
    y_sel <- input$plot_2d_y

    # Set defaults if missing or invalid
    if (is.null(x_sel) || !x_sel %in% vars) {
      x_sel <- vars[1]
    }

    y_choices <- setdiff(vars, x_sel)

    if (is.null(y_sel) || !y_sel %in% y_choices) {
      y_sel <- y_choices[1]
    }

    x_choices <- setdiff(vars, y_sel)

    updateSelectInput(session, "plot_2d_x",
                      choices = x_choices,
                      selected = x_sel)

    updateSelectInput(session, "plot_2d_y",
                      choices = y_choices,
                      selected = y_sel)
  }, ignoreInit = FALSE)

observeEvent(input$build_ell, {

  # If an ellipsoid already exists warn the user
  if (!is.null(session_data$ellipsoid)) {
    showModal(modalDialog(
      title = "Overwrite ellipsoid?",
      p("An ellipsoid has already been built. Building a new one will
         overwrite the current ellipsoid and any downstream results."),
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

observeEvent(input$confirm_build_ell, {
  removeModal()
  build_ellipsoid_shiny()

})

# Covariance update observer
observeEvent({
  ell <- session_data$ellipsoid
  req(ell)
  vars <- ell$var_names
  version <- session_data$ellipsoid_version
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))
  lapply(pair_names, function(pn) input[[paste0("cov_", version, "_", pn)]])
}, {
  req(session_data$ellipsoid)

  ell <- session_data$ellipsoid
  vars <- ell$var_names
  version <- session_data$ellipsoid_version
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  # Collect current slider values as named numeric vector
  cov_vals <- setNames(
    sapply(pair_names, function(pn) {
      val <- input[[paste0("cov_", version, "_", pn)]]
      if(is.null(val)) 0 else val
    }),
    pair_names)

  # Only update if values differ from current covariance matrix
  current_cov <- setNames(
    sapply(seq_len(nrow(pairs)), function(i) {
      ell$cov_matrix[pairs[i, 1], pairs[i, 2]]
    }),
    pair_names)

  if(all(cov_vals == current_cov)) return()

  updated_ell <- tryCatch(
    update_ellipsoid_covariance(object = ell,
                                covariance = cov_vals,
                                verbose = FALSE),
    error = function(e) {
      showNotification(paste("Covariance update failed:", e$message),
                       type = "error")
      NULL
    }
  )

  req(updated_ell)

  session_data$ellipsoid <- updated_ell

  # Silently update slider bounds for remaining pairs using
  # cov_limits_remaining, preserving current selected values
  remaining <- updated_ell$cov_limits_remaining

  if(!is.null(remaining)) {
    remaining_names <- rownames(remaining)

    lapply(remaining_names, function(pn){
      slider_id <- paste0("cov_", version, "_", pn)
      cur_val <- input[[slider_id]]

      if (!is.null(cur_val)) {
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

# Reactives ---------------------------------------------------------------

range_preview <- reactive({

  req(input$range_method_choice, session_data$vars, session_data$bg_df)
  vars <- session_data$vars

  switch(input$range_method_choice,

         "man" = {
           mins <- setNames(sapply(vars, function(v){
             input[[paste0("min_", v)]]}),
             vars)
           maxs <- setNames(sapply(vars, function(v){
             input[[paste0("max_", v)]]}),
             vars)

           if(any(is.na(mins)) || any(is.na(maxs))) return(NULL)
           if(any(maxs <= mins)) return(NULL)

           list(mins = mins, maxs = maxs)
         },

         "df" = {
           req(session_data$df_range)

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

           ranges_df <- ranges_from_data(data = session_data$df_range[, vars, drop = FALSE],
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

# Render Outputs ----------------------------------------------------------

output$range_method_ui <- renderUI({

  req(input$range_method_choice, session_data$vars, session_data$bg_df)

  vars <- session_data$vars

  # Same button in all range method
  cl_row <- fluidRow(
    column(width = 5, tags$span("Confidence Level (%)", class = "text-widget-title text-center")),
    column(width = 4,
           numericInput(inputId = "cl_range",
                        label = NULL,
                        value = 0.99,
                        min = 0, max = 1,
                        step = 0.1)
    ))

  # Same button for all ranges once processed
  build_btn <- fluidRow(
    column(width = 12,
           class = "btn-spaced",
           actionButton("build_ell",
                        "Build Ellipsoid",
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
             column(width = 4, tags$span("Variable", class = "text-widget-title text-center")),
             column(width = 4, tags$span("Min", class = "text-widget-title text-center")),
             column(width = 4, tags$span("Max", class = "text-widget-title text-center"))
           )

           var_rows <- lapply(vars, function(v) {
             fluidRow(
               column(width = 4, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 4,
                      numericInput(inputId = paste0("min_", v),
                                   label = NULL,
                                   value = round(mean(session_data$bg_df[, v], na.rm = TRUE), 2),
                                   step = 0.5)),
               column(width = 4,
                      numericInput(inputId = paste0("max_", v),
                                   label = NULL,
                                   value = round(mean(session_data$bg_df[, v], na.rm = TRUE), 2) + round(sd(session_data$bg_df[, v], na.rm = TRUE), 2),
                                   step = 0.5))
             )
           })

           # Organizing the UI
           tagList(header, var_rows, cl_row, reset_btn, build_btn)
         },

         "df" = {

           upload_row <- fluidRow(
             p(instructions$range_data, class = "text-instruction"),
             column(width = 6,
                    fileInput(inputId = "df_range_file",
                              label = tags$span("Choose CSV file with range data", class = "text-widget-title"),
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

                 # TO DO: check later if i want to keep this as a session data or not
                 session_data$df_range <- df_range

                 obs_ranges <- lapply(shared_vars, function(v){
                   list(min = round(min(df_range[, v], na.rm = TRUE), 2),
                        max = round(max(df_range[, v], na.rm = TRUE), 2))
                 })

                 names(obs_ranges) <- shared_vars

                 header <- fluidRow(
                   column(width = 3, tags$span("Variable", class = "text-widget-title text-center")),
                   column(width = 2, tags$span("Observed Min", class = "text-widget-title text-center")),
                   column(width = 2, tags$span("Observed Max", class = "text-widget-title text-center")),
                   column(width = 2, tags$span("Expand Min (%)", class = "text-widget-title text-center")),
                   column(width = 2, tags$span("Expand Max (%)", class = "text-widget-title text-center"))
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

           tagList(upload_row, range_rows, cl_row, reset_btn, build_btn)
         },

         "stats" = {

           header <- fluidRow(
             p(instructions$range_stats, class = "text-instruction"),
             column(width = 3, tags$span("Variable", class = "text-widget-title text-center")),
             column(width = 2, tags$span("Mean", class = "text-widget-title text-center")),
             column(width = 2, tags$span("SD", class = "text-widget-title text-center")),
             column(width = 2, tags$span("Expand Min (%)", class = "text-widget-title text-center")),
             column(width = 2, tags$span("Expand Max (%)", class = "text-widget-title text-center"))
           )

           var_rows <- lapply(vars, function(v){
             fluidRow(
               column(width = 3, class = "var-label", tags$span(v, class = "text-widget-inner")),
               column(width = 2,
                      numericInput(inputId = paste0("mean_", v),
                                   label = NULL,
                                   value = round(mean(session_data$bg_df[, v], na.rm = TRUE), 2),
                                   step = 0.5)
               ),

               column(width = 2,
                      numericInput(inputId = paste0("sd_", v),
                                   label = NULL,
                                   value = round(sd(session_data$bg_df[, v], na.rm = TRUE), 2),
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

           tagList(header, var_rows, cl_row, reset_btn, build_btn)
         }
  )
})

output$build_espace_plot <- renderPlot({

  req(range_preview(), session_data$vars, session_data$bg_df, input$plot_state)

  # This is how to access the reactive method
  ranges <- range_preview()
  vars <- session_data$vars
  bg <- session_data$bg_df


  switch(input$plot_state,
         "plot_pairs" = {

           # Build all pairwise combinations
           pairs <- t(combn(seq_along(vars), 2))
           n_pairs <- nrow(pairs)
           n_cols <- ceiling(sqrt(n_pairs))
           n_rows <- ceiling(n_pairs / n_cols)

           old_par <- par(no.readonly = TRUE)
           on.exit(par(old_par))

           par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

           for (i in seq_len(n_pairs)) {
             v1 <- vars[pairs[i, 1]]
             v2 <- vars[pairs[i, 2]]

             # Background scatter
             plot(bg[[v1]], bg[[v2]],
                  col  = "grey70",
                  pch  = 20,
                  cex  = 0.3,
                  xlab = v1,
                  ylab = v2,
                  main = paste(v1, "vs.", v2))

             # Vertical lines for v1 range
             abline(v = ranges$mins[[v1]], col = "#e10000", lwd = 2, lty = 2)
             abline(v = ranges$maxs[[v1]], col = "#e10000", lwd = 2, lty = 2)

             # Horizontal lines for v2 range
             abline(h = ranges$mins[[v2]], col = "#0004d5", lwd = 2, lty = 2)
             abline(h = ranges$maxs[[v2]], col = "#0004d5", lwd = 2, lty = 2)
           }
         },

         "plot_2d" = {

           req(input$plot_2d_x, input$plot_2d_y)

           v1 <- input$plot_2d_x
           v2 <- input$plot_2d_y

           old_par <- par(no.readonly = TRUE)
           on.exit(par(old_par))

           par(mar = c(4, 4, 2, 1))

           plot(bg[[v1]], bg[[v2]],
                col  = "grey70",
                pch  = 20,
                cex  = 0.3,
                xlab = v1,
                ylab = v2,
                main = paste(v1, "vs.", v2))

           # Vertical lines for v1 range
           abline(v = ranges$mins[[v1]], col = "#e10000", lwd = 2, lty = 2)
           abline(v = ranges$maxs[[v1]], col = "#e10000", lwd = 2, lty = 2)

           # Horizontal lines for v2 range
           abline(h = ranges$mins[[v2]], col = "#0004d5", lwd = 2, lty = 2)
           abline(h = ranges$maxs[[v2]], col = "#0004d5", lwd = 2, lty = 2)

        }
  )
}, height = function() {

  if (input$plot_state == "plot_2d") {
    return(450)
  }

  if (input$plot_state == "plot_pairs") {
    return(500)
  }

  500
})

# Covariance sliders UI --------------------------------------------------

output$covariance_ui <- renderUI({
  req(session_data$ellipsoid)

  box(title = tags$span("Covariance", class = "text-tab-title"),
      width = 12,
      p(instructions$covariance, class = "text-instruction"),
      uiOutput("covariance_sliders_ui")
  )
})

output$covariance_sliders_ui <- renderUI({
  req(session_data$ellipsoid)

  ell <- session_data$ellipsoid
  vars <- ell$var_names
  version <- session_data$ellipsoid_version


  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = "-"))

  # Current off-diagonal values from cov_matrix
  current_cov <- setNames(
    sapply(seq_len(nrow(pairs)), function(i){
      ell$cov_matrix[pairs[i, 1], pairs[i, 2]]
    }),
    pair_names)

  # Limits come from cov_limits rownames
  lims <- ell$cov_limits
  rownames(lims) <- pair_names

  sliders <- lapply(pair_names, function(pn){
    min_val <- lims[pn, "min"]
    max_val <- lims[pn, "max"]
    cur_val <- current_cov[[pn]]

    # Clamp current value to limits in case of floating point drift
    cur_val <- max(min_val, min(max_val, cur_val))

    step <- round((max_val - min_val) / 100, 4)

    fluidRow(
      column(width = 11,
             sliderInput(inputId = paste0("cov_", version, "_", pn),
                         label = pn,
                         min = round(min_val, 2),
                         max = round(max_val, 2),
                         value = round(cur_val, 2),
                         step = step)),
      column(width = 1,
             class = "btn-spaced",
             actionLink(inputId = paste0("cov_reset_", version, '_', pn),
                        label = icon("rotate-left")
                        )
             )
    )
  })

  tagList(sliders)
})

output$ellipsoid_print <- renderPrint({
  req(session_data$ellipsoid)
  print(session_data$ellipsoid)
})


