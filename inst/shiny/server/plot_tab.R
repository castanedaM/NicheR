# Title: Plot logic
# Description: Handle e-space plots
# Date last updated: 06/15/2026

output$plot_options_ui <- renderUI({
  req(session_data$vars)

  fluidRow(
    column(width = 12,

           radioButtons("plot_state",
                        label = tags$span("Select plot type:",
                                          class = "text-widget-title"),
                        choices = c("Pairs" = "plot_pairs",
                                    "2D" = "plot_2d"),
                        inline = TRUE),

           conditionalPanel("input.plot_state == 'plot_2d'",
                            column(width = 6,
                                   selectInput("plot_2d_x", label = NULL,
                                               choices = character(0))
                            ),
                            column(width = 6,
                                   selectInput("plot_2d_y", label = NULL,
                                               choices = character(0))
                            )
           )
    )
  )
})

# Updates the axis showing in 2d plot
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
  if (is.null(x_sel) || !x_sel %in% vars){
    x_sel <- vars[1]
  }

  y_choices <- setdiff(vars, x_sel)

  if (is.null(y_sel) || !y_sel %in% y_choices){
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


output$build_espace_plot <- renderPlot({

  req(range_preview(), session_data$vars, input$plot_state)

  # This is how to access the reactive method
  ranges <- range_preview()
  vars <- session_data$vars
  bg <- session_data$bg_df  # NULL in virtual mode

  # Helper: draw background scatter if available, otherwise an empty frame
  # padded around the current range so the lines have room to show
  draw_panel <- function(v1, v2){
    if(!is.null(bg)){
      plot(bg[[v1]], bg[[v2]],
           col  = "grey70",
           pch  = ".",
           cex  = 0.3,
           xlab = v1,
           ylab = v2,
           main = paste(v1, "vs.", v2))
    } else {
      x_range <- ranges$maxs[[v1]] - ranges$mins[[v1]]
      y_range <- ranges$maxs[[v2]] - ranges$mins[[v2]]

      plot(NA, NA,
           xlim = c(ranges$mins[[v1]] - 0.25 * x_range,
                    ranges$maxs[[v1]] + 0.25 * x_range),
           ylim = c(ranges$mins[[v2]] - 0.25 * y_range,
                    ranges$maxs[[v2]] + 0.25 * y_range),
           xlab = v1,
           ylab = v2,
           main = paste(v1, "vs.", v2))
    }
  }

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

           for (i in seq_len(n_pairs)){
             v1 <- vars[pairs[i, 1]]
             v2 <- vars[pairs[i, 2]]

             draw_panel(v1, v2)

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

           draw_panel(v1, v2)

           # Vertical lines for v1 range
           abline(v = ranges$mins[[v1]], col = "#e10000", lwd = 2, lty = 2)
           abline(v = ranges$maxs[[v1]], col = "#e10000", lwd = 2, lty = 2)

           # Horizontal lines for v2 range
           abline(h = ranges$mins[[v2]], col = "#0004d5", lwd = 2, lty = 2)
           abline(h = ranges$maxs[[v2]], col = "#0004d5", lwd = 2, lty = 2)

         }
  )
}, height = function(){

  if(input$plot_state == "plot_2d"){
    return(450)
  }

  if(input$plot_state == "plot_pairs"){
    return(500)
  }

  500
})
