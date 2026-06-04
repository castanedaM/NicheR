# Title: Build tab server logic
# Description: Handles range inputs and e-space plot
# Date last updated: 06/04/2026


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
               input[[paste0("expand_min_stats_", v)]]
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
    column(width = 5, tags$b("Confidence Level (%)")),
    column(width = 4,
           numericInput(inputId = "cl_range",
                        label = NULL,
                        value = 0.99,
                        min = 0, max = 1,
                        step = 0.1)
    ))

  # Same button for all ranges once processed
  build_btn <- fluidRow(
    column(width = 12, class = "btn-spaced",
           actionButton("build_ell",
                        "Build Ellipsoid",
                        class = "btn-primary"))
  )

  # Same Reset Button for all
  reset_btn <- fluidRow(
    actionLink("reset_ranges",
               label = tagList(icon("rotate-left"), "Reset to defaults"))
  )

  # Choose which UI to show for ranges
  switch(input$range_method_choice,
         "man" = {
           header <- fluidRow(
             column(width = 4, tags$b("Variable")),
             column(width = 4, tags$b("Min")),
             column(width = 4, tags$b("Max"))
           )

           var_rows <- lapply(vars, function(v) {
             fluidRow(
               column(width = 4, class = "var-label", tags$span(v)),
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
                              label = "Choose CSV file with range data",
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
                 class = "text-warning")

             }else{

               shared_vars <- intersect(vars, colnames(df_range))

               if(length(shared_vars) != length(vars)){

                 p("No matching variables found between the uploaded file and the
               background layers. Check that column names match.",
                   class = "text-warning")

               }else{

                 # TO DO: check later if i want to keep this as a session data or not
                 session_data$df_range <- df_range

                 obs_ranges <- lapply(shared_vars, function(v){
                   list(min = round(min(df_range[, v], na.rm = TRUE), 2),
                        max = round(max(df_range[, v], na.rm = TRUE), 2))
                 })

                 names(obs_ranges) <- shared_vars

                 header <- fluidRow(
                   column(width = 3, tags$b("Variable")),
                   column(width = 2, tags$b("Observed Min")),
                   column(width = 2, tags$b("Observed Max")),
                   column(width = 2, tags$b("Expand Min (%)")),
                   column(width = 2, tags$b("Expand Max (%)"))
                 )

                 rows <- lapply(shared_vars, function(v){
                   fluidRow(
                     column(width = 3, class = "var-label", tags$span(v)),
                     column(width = 2, tags$span(format(round(obs_ranges[[v]]$min, 2), nsmall = 2))),
                     column(width = 2, tags$span(format(round(obs_ranges[[v]]$max, 2), nsmall = 2))),
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
             column(width = 3, tags$b("Variable")),
             column(width = 2, tags$b("Mean")),
             column(width = 2, tags$b("SD")),
             column(width = 2, tags$b("Expand Min (%)")),
             column(width = 2, tags$b("Expand Max (%)"))
           )

           var_rows <- lapply(vars, function(v){
             fluidRow(
               column(width = 3, class = "var-label", tags$span(v)),
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
                                   value = 10,
                                   step = 5)),
               column(width = 2,
                      numericInput(inputId = paste0("expand_max_stats_", v),
                                   label = NULL,
                                   value = 10,
                                   step = 5))
             )
           })

           tagList(header, var_rows, cl_row, reset_btn, build_btn)
         }
  )
})

output$build_espace_plot <- renderPlot({

  req(range_preview(), session_data$vars, session_data$bg_df)

  # This is how to access the reactive method
  ranges <- range_preview()
  vars <- session_data$vars
  bg <- session_data$bg_df

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
})

