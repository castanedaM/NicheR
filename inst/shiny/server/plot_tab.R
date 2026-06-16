# Title: Plot logic
# Description: Handle e-space plots
# Date last updated: 06/16/2026

output$plot_options_ui <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 12,

           radioButtons("plot_state",
                        label = tags$span("Select plot type:",
                                          class = "text-widget-title"),
                        choices = c("Pairs" = "plot_pairs",
                                    "2D" = "plot_2d"),
                        selected = "plot_pairs",
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

  vars <- plot_vars()
  req(vars)

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


# Helper: variables to plot, reactive to live selections before confirm
# Before vars are confirmed, show the first 6 non-spatial columns from
# whichever data source exists.
plot_vars <- reactive({

  if(isTRUE(session_data$vars_confirmed)){
    req(session_data$vars)
    return(session_data$vars)
  }

  # Virtual mode pre-confirm: nothing meaningful to plot yet
  if(identical(session_data$input_mode, "virtual")) return(NULL)

  all_vars <- if(!is.null(session_data$bg_raster)){
    names(session_data$bg_raster)
  } else if(!is.null(session_data$bg_df)){
    colnames(session_data$bg_df)
  } else {
    return(NULL)
  }

  all_vars <- all_vars[!grepl(SPATIAL_COL_PATTERN, all_vars, ignore.case = TRUE)]

  # Reflect live (unconfirmed) selections if the selector UI has rendered
  n_slots <- min(length(all_vars), MAX_DIMS)

  live_active <- vapply(seq_len(n_slots), function(i){
    val <- input[[paste0("var_active_", i)]]
    if(is.null(val)) TRUE else isTRUE(val)
  }, logical(1))

  live_select <- vapply(seq_len(n_slots), function(i){
    val <- input[[paste0("var_select_", i)]]
    if(is.null(val)) all_vars[i] else val
  }, character(1))

  selected <- live_select[live_active]

  if(length(selected) == 0) return(head(all_vars, 6))

  selected
})

# Advanced plot settings UI
output$plot_settings_ui <- renderUI({

  has_ell <- isTRUE(session_data$ellipsoid_version > 0)

  box(title = tags$span("Advanced Plot Settings", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,

      fluidRow(
        column(width = 4,
               tags$span("Point shape (pch)", class = "text-widget-title"),
               selectInput("plot_pch", label = NULL,
                           choices = c("Dot (.)" = ".", "Open circle" = "1",
                                       "Filled circle" = "16", "Square" = "15",
                                       "Triangle" = "17", "Cross" = "3"),
                           selected = ".")),
        column(width = 4,
               tags$span("Point size (cex)", class = "text-widget-title"),
               numericInput("plot_cex", label = NULL, value = 0.3,
                            min = 0.1, max = 5, step = 0.1)),
        column(width = 4,
               tags$span("Background color", class = "text-widget-title"),
               colourpicker::colourInput("plot_bg_col", label = NULL,
                                         value = "#B3B3B3"))
      ),

      conditionalPanel(
        condition = "input.range_method_choice != null && input.range_method_choice != ''",
        fluidRow(
          column(width = 3,
                 checkboxInput("show_range_lines", "Show range lines", value = TRUE)),
          column(width = 3,
                 tags$span("X-line color", class = "text-widget-title"),
                 colourpicker::colourInput("plot_xline_col", label = NULL,
                                           value = "#E10000")),
          column(width = 3,
                 tags$span("Y-line color", class = "text-widget-title"),
                 colourpicker::colourInput("plot_yline_col", label = NULL,
                                           value = "#0004D5")),
          column(width = 3,
                 tags$span("Line width", class = "text-widget-title"),
                 numericInput("plot_line_lwd", label = NULL, value = 2,
                              min = 0.5, max = 6, step = 0.5))
        )
      ),

      if(has_ell) tagList(
        fluidRow(
          column(width = 3,
                 checkboxInput("show_ellipsoid", "Show ellipsoid", value = TRUE)),
          column(width = 3,
                 tags$span("Ellipsoid color", class = "text-widget-title"),
                 colourpicker::colourInput("plot_ell_col", label = NULL,
                                           value = "#000000")),
          column(width = 3,
                 tags$span("Ellipsoid line width", class = "text-widget-title"),
                 numericInput("plot_ell_lwd", label = NULL, value = 2,
                              min = 0.5, max = 6, step = 0.5)),
          column(width = 3,
                 tags$span("Ellipsoid line type", class = "text-widget-title"),
                 selectInput("plot_ell_lty", label = NULL,
                             choices = c("Solid" = "1", "Dashed" = "2",
                                         "Dotted" = "3"),
                             selected = "1"))
        ),

        fluidRow(
          column(width = 3,
                 checkboxInput("show_centroid", "Show centroid", value = TRUE)),
          column(width = 3,
                 tags$span("Centroid shape (pch)", class = "text-widget-title"),
                 selectInput("plot_centroid_pch", label = NULL,
                             choices = c("Cross (X)" = "4", "Star" = "8",
                                         "Filled diamond" = "18",
                                         "Filled circle" = "16"),
                             selected = "8")),
          column(width = 3,
                 tags$span("Centroid color", class = "text-widget-title"),
                 colourpicker::colourInput("plot_centroid_col", label = NULL,
                                           value = "#000000")),
          column(width = 3,
                 tags$span("Centroid size (cex)", class = "text-widget-title"),
                 numericInput("plot_centroid_cex", label = NULL, value = 1.5,
                              min = 0.5, max = 5, step = 0.5))
        )
      ),

      fluidRow(
        column(width = 4,
               tags$span("Zoom", class = "text-widget-title"),
               radioButtons("plot_zoom_mode", label = NULL,
                            choices = c("Auto" = "auto",
                                        "Zoom to ellipsoid" = "ellipsoid",
                                        "Manual" = "manual"),
                            selected = "auto")),

        conditionalPanel(
          condition = "input.plot_zoom_mode == 'manual'",
          column(width = 4,
                 tags$span("X limits", class = "text-widget-title"),
                 fluidRow(
                   column(6, numericInput("plot_xlim_min", NULL, value = NA)),
                   column(6, numericInput("plot_xlim_max", NULL, value = NA))
                 )),
          column(width = 4,
                 tags$span("Y limits", class = "text-widget-title"),
                 fluidRow(
                   column(6, numericInput("plot_ylim_min", NULL, value = NA)),
                   column(6, numericInput("plot_ylim_max", NULL, value = NA))
                 ))
        )
      )
  )
})
# Plot ------------------------------------------------------------------

# Shared plotting routine so the on-screen render and the exported figure
# use exactly the same code path
draw_espace_plot <- function(){

  req(input$plot_state)

  vars <- plot_vars()
  req(vars)

  ranges <- range_preview()  # may be NULL before a range method is set
  bg <- session_data$bg_df   # NULL in virtual mode

  show_lines <- if(!is.null(input$show_range_lines)) input$show_range_lines else TRUE
  show_lines <- show_lines && !is.null(ranges)

  has_ell <- isTRUE(session_data$ellipsoid_version > 0)
  ell <- if(has_ell) session_data$ellipsoid else NULL
  show_ell <- has_ell && (if(!is.null(input$show_ellipsoid)) input$show_ellipsoid else TRUE)
  show_centroid <- has_ell && (if(!is.null(input$show_centroid)) input$show_centroid else TRUE)

  pch_val <- if(!is.null(input$plot_pch)) input$plot_pch else "."
  pch_val <- if(pch_val == ".") "." else as.numeric(pch_val)
  cex_val <- if(!is.null(input$plot_cex)) input$plot_cex else 0.3
  bg_col  <- if(!is.null(input$plot_bg_col)) input$plot_bg_col else "#B3B3B3"

  xline_col <- if(!is.null(input$plot_xline_col)) input$plot_xline_col else "#E10000"
  yline_col <- if(!is.null(input$plot_yline_col)) input$plot_yline_col else "#0004D5"
  line_lwd  <- if(!is.null(input$plot_line_lwd)) input$plot_line_lwd else 2

  ell_col <- if(!is.null(input$plot_ell_col)) input$plot_ell_col else "#000000"
  ell_lwd <- if(!is.null(input$plot_ell_lwd)) input$plot_ell_lwd else 2
  ell_lty <- if(!is.null(input$plot_ell_lty)) as.numeric(input$plot_ell_lty) else 1

  centroid_pch <- if(!is.null(input$plot_centroid_pch)) as.numeric(input$plot_centroid_pch) else 8
  centroid_col <- if(!is.null(input$plot_centroid_col)) input$plot_centroid_col else "#000000"
  centroid_cex <- if(!is.null(input$plot_centroid_cex)) input$plot_centroid_cex else 1.5

  zoom_mode <- if(!is.null(input$plot_zoom_mode)) input$plot_zoom_mode else "auto"

  # Compute shared xlim/ylim for a given variable pair, honoring zoom mode
  compute_lims <- function(v1, v2){

    if(zoom_mode == "manual" &&
       !is.null(input$plot_xlim_min) && !is.na(input$plot_xlim_min) &&
       !is.null(input$plot_xlim_max) && !is.na(input$plot_xlim_max) &&
       !is.null(input$plot_ylim_min) && !is.na(input$plot_ylim_min) &&
       !is.null(input$plot_ylim_max) && !is.na(input$plot_ylim_max)){

      return(list(xlim = c(input$plot_xlim_min, input$plot_xlim_max),
                  ylim = c(input$plot_ylim_min, input$plot_ylim_max)))
    }

    if(zoom_mode == "ellipsoid" && has_ell){
      idx <- match(c(v1, v2), ell$var_names)
      ell_pts <- ellipsoid_boundary_2d(ell, n_segments = 100, dim = idx)
      pad_x <- diff(range(ell_pts[, 1])) * 0.1
      pad_y <- diff(range(ell_pts[, 2])) * 0.1
      return(list(xlim = range(ell_pts[, 1]) + c(-pad_x, pad_x),
                  ylim = range(ell_pts[, 2]) + c(-pad_y, pad_y)))
    }

    # Auto: cover background (or range box) and ellipsoid if present
    pts_xy <- if(!is.null(bg)){
      bg[, c(v1, v2)]
    } else if(!is.null(ranges)){
      data.frame(x = c(ranges$mins[[v1]], ranges$maxs[[v1]]),
                 y = c(ranges$mins[[v2]], ranges$maxs[[v2]]))
    } else {
      data.frame(x = c(0, 1), y = c(0, 1))
    }

    if(has_ell){
      idx <- match(c(v1, v2), ell$var_names)
      ell_pts <- ellipsoid_boundary_2d(ell, n_segments = 100, dim = idx)
      return(safe_lims(pts_xy, ell_pts))
    }

    list(xlim = range(pts_xy[, 1], na.rm = TRUE),
         ylim = range(pts_xy[, 2], na.rm = TRUE))
  }

  draw_panel <- function(v1, v2){

    lims <- compute_lims(v1, v2)

    if(!is.null(bg)){
      plot(bg[[v1]], bg[[v2]],
           col  = bg_col,
           pch  = pch_val,
           cex  = cex_val,
           xlim = lims$xlim,
           ylim = lims$ylim,
           xlab = v1,
           ylab = v2,
           main = paste(v1, "vs.", v2))
    } else {
      plot(NA, NA,
           xlim = lims$xlim,
           ylim = lims$ylim,
           xlab = v1,
           ylab = v2,
           main = paste(v1, "vs.", v2))
    }

    if(show_lines){
      abline(v = ranges$mins[[v1]], col = xline_col, lwd = line_lwd, lty = 2)
      abline(v = ranges$maxs[[v1]], col = xline_col, lwd = line_lwd, lty = 2)
      abline(h = ranges$mins[[v2]], col = yline_col, lwd = line_lwd, lty = 2)
      abline(h = ranges$maxs[[v2]], col = yline_col, lwd = line_lwd, lty = 2)
    }

    if(show_ell){
      idx <- match(c(v1, v2), ell$var_names)
      add_ellipsoid(ell, dim = idx,
                    col_ell = ell_col, lwd = ell_lwd, lty = ell_lty)
    }

    if(show_centroid){
      idx <- match(c(v1, v2), ell$var_names)
      points(ell$centroid[idx[1]], ell$centroid[idx[2]],
             pch = centroid_pch, col = centroid_col, cex = centroid_cex)
    }
  }

  switch(input$plot_state,
         "plot_pairs" = {

           pairs <- t(combn(seq_along(vars), 2))
           n_pairs <- nrow(pairs)
           n_cols <- ceiling(sqrt(n_pairs))
           n_rows <- ceiling(n_pairs / n_cols)

           old_par <- par(no.readonly = TRUE)
           on.exit(par(old_par))

           par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

           for(i in seq_len(n_pairs)){
             draw_panel(vars[pairs[i, 1]], vars[pairs[i, 2]])
           }
         },

         "plot_2d" = {

           req(input$plot_2d_x, input$plot_2d_y)

           old_par <- par(no.readonly = TRUE)
           on.exit(par(old_par))

           par(mar = c(4, 4, 2, 1))

           draw_panel(input$plot_2d_x, input$plot_2d_y)
         }
  )
}

output$build_espace_plot <- renderPlot({
  draw_espace_plot()
}, height = function(){

  state <- if(!is.null(input$plot_state)) input$plot_state else "plot_pairs"

  if(state == "plot_2d"){
    return(450)
  }

  if(state == "plot_pairs"){
    return(500)
  }

  500
})

output$export_espace_plot <- downloadHandler(
  filename = function(){
    paste0("espace_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  },
  content = function(file){
    png(file, width = 1400, height = 1000, res = 150)
    draw_espace_plot()
    dev.off()
  }
)
