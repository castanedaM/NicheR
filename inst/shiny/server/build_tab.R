# Title: Build tab server logic
# Description: Handles range inputs and e-space plot
# Date last updated: 06/03/2026

# Render Outputs ----------------------------------------------------------

output$range_method_ui <- renderUI({

  req(input$range_method_choice, session_data$vars)

  vars <- session_data$vars

  # Same button in all range method
  cl_row <- fluidRow(
    column(width = 4, tags$b("Confidence Level")),
    column(width = 4,
           sliderInput(inputId = "cl_range",
                       label = NULL,
                       min = 0,
                       max = 1,
                       value = 0.99,
                       step = 0.01,
                       ticks = TRUE))
  )

  # Same button for all ranges once processed
  build_btn <- fluidRow(
    column(width = 12, class = "btn-spaced",
           actionButton("build_ell",
                        "Build Ellipsoid",
                        class = "btn-primary"))
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
                                   value = 0,
                                   step = 0.01)),
               column(width = 4,
                      numericInput(inputId = paste0("max_", v),
                                   label = NULL,
                                   value = 1,
                                   step = 0.01))
             )
           })

           # Organizing the UI
           tagList(header, var_rows, cl_row, build_btn)
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
                                         ".csv")))
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

                session_data$bg_raster <- df_range

                 obs_ranges <- lapply(shared_vars, function(v){
                   vals <- df_range[[v]]
                   list(min = min(vals, na.rm = TRUE),
                        max = max(vals, na.rm = TRUE))
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
                     column(width = 2, tags$span(round(obs_ranges[[v]]$min, 3))),
                     column(width = 2, tags$span(round(obs_ranges[[v]]$max, 3))),
                     column(width = 2,
                            sliderInput(inputId = paste0("expand_min_df_", v),
                                        label = NULL,
                                        min = 0, max = 100,
                                        value = 0, step = 5,
                                        post = "%")),
                     column(width = 2,
                            sliderInput(inputId = paste0("expand_max_df_", v),
                                        label = NULL,
                                        min = 0, max = 100,
                                        value = 0, step = 5,
                                        post = "%"))
                   )
                 })

                 tagList(header, rows)
               }
             }
           }

           tagList(upload_row, range_rows, cl_row, build_btn)
         },

         "stats" = {

           data_source <- if(!is.null(session_data$sel_df)){
             session_data$sel_df
           } else{
             as.data.frame(session_data$sel_raster, na.rm = TRUE)
           }

           obs_stats <- lapply(vars, function(v){
             vals <- data_source[[v]]
             list(mean = mean(vals, na.rm = TRUE),
                  sd   = sd(vals,   na.rm = TRUE))
           })
           names(obs_stats) <- vars

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
               column(width = 2, tags$span(round(obs_stats[[v]]$mean, 3))),
               column(width = 2, tags$span(round(obs_stats[[v]]$sd,   3))),
               column(width = 2,
                      sliderInput(inputId = paste0("expand_min_stats_", v),
                                  label = NULL,
                                  min = 0, max = 50,
                                  value = 0, step = 1,
                                  post = "%")),
               column(width = 2,
                      sliderInput(inputId = paste0("expand_max_stats_", v),
                                  label = NULL,
                                  min = 0, max = 50,
                                  value = 0, step = 1,
                                  post = "%"))
             )
           })

           tagList(
             header,
             var_rows,
             cl_row,
             build_btn
           )
         }
  )
})



observeEvent(input$build_ell, {

  req(input$range_method_choice, session_data$vars)

  vars <- session_data$vars

  cl   <- input$cl_range

  range_df <- switch(
    input$range_method_choice,
    "man" = {
      mins <- sapply(vars, function(v) input[[paste0("min_", v)]])
      maxs <- sapply(vars, function(v) input[[paste0("max_", v)]])

      if(any(is.na(mins)) || any(is.na(maxs))){
        showNotification("Please fill in all min and max values.",
                         type = "error")
        return(NULL)
      }

      if(any(maxs <= mins)){
        showNotification("Max must be greater than min for all variables.",
                         type = "error")
        return(NULL)
      }

      range_df <- as.data.frame(rbind(mins, maxs))
      colnames(range_df) <- vars
      range_df
    },

    "df" = {
      req(session_data$df_range)

      expand_min <- setNames(
        lapply(vars, function(v) input[[paste0("expand_min_df_", v)]]),
        shared_vars
      )

      expand_max <- setNames(
        lapply(vars, function(v) input[[paste0("expand_max_df_", v)]]),
        shared_vars
      )

      ranges_from_data(data = df_range[, vars, drop = FALSE],
                       expand_min = expand_min,
                       expand_max = expand_max)
    },

    "stats" = {
      data_source <- if(!is.null(session_data$sel_df)){
        session_data$sel_df
      }else{
        as.data.frame(session_data$sel_raster, na.rm = TRUE)
      }

      means <- setNames(
        sapply(vars, function(v) mean(data_source[[v]], na.rm = TRUE)),
        vars
      )

      sds <- setNames(
        sapply(vars, function(v) sd(data_source[[v]], na.rm = TRUE)),
        vars
      )

      expand_min <- setNames(
        lapply(vars, function(v) input[[paste0("expand_min_stats_", v)]]),
        vars
      )

      expand_max <- setNames(
        lapply(vars, function(v) input[[paste0("expand_max_stats_", v)]]),
        vars
      )

      ranges_from_stats(mean = means,
                        sd = sds,
                        cl = cl,
                        expand_min = expand_min,
                        expand_max = expand_max)
    }
  )

  req(range_df)

  tryCatch({
    session_data$ellipsoid <- build_ellipsoid(range = range_df,
                                              cl = cl,
                                              verbose = FALSE)

    showNotification("Ellipsoid built successfully.", type = "message")

  }, error = function(e){

    showNotification(paste("Error building ellipsoid:", e$message),
                     type = "error")
  })

})


output$build_espace_plot <- renderPlot({
  req(session_data$sel_df)

  df <- session_data$bg_df
  vars <- session_data$vars
  vars <- vars[seq_len(min(length(vars), 3))]
  df <- df[, vars, drop = FALSE]

  if(nrow(df) > 5000) df <- df[sample(nrow(df), 5000), , drop = FALSE]

  pairs(df)

})



