# Title: Plot logic
# Description: Handle e-space, g-space, and combined plots
# Date last updated: 06/23/2026


# Reactive: prediction raster -----------------------------------------------
# Computed once when ellipsoid is built and raster is available.
# Returns a SpatRaster with suitability_trunc layer, or NULL.

pred_raster <- reactive({

  req(session_data$ellipsoid_version > 0)
  req(session_data$bg_raster)

  ell <- session_data$ellipsoid
  vars <- ell$var_names

  tryCatch(
    predict(ell,
            newdata = session_data$bg_raster[[vars]],
            include_suitability = TRUE,
            include_mahalanobis = FALSE,
            suitability_truncated = TRUE),
    error = function(e) NULL
  )
})


# Helper: variables to plot, reactive to live selections before confirm ------
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


# E-space tab: radio for pairs vs 2D + 2D axis selectors -------------------

output$plot_espace_options_ui <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 12,
           radioButtons("plot_espace_state",
                        label = tags$span("Plot type:", class = "text-widget-title"),
                        choices= c("All pairs" = "plot_pairs",
                                   "2D"= "plot_2d"),
                        selected = "plot_pairs",
                        inline= TRUE),

           conditionalPanel(
             "input.plot_espace_state == 'plot_2d'",
             column(width = 6,
                    selectInput("plot_2d_x", label = NULL,
                                choices = character(0))),
             column(width = 6,
                    selectInput("plot_2d_y", label = NULL,
                                choices = character(0)))
           )
    )
  )
})


# Combined tab: axis selectors for the E-space panel -----------------------

output$plot_combined_options_ui <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 6,
           selectInput("plot_combined_x", label = NULL,
                       choices = character(0))),
    column(width = 6,
           selectInput("plot_combined_y", label = NULL,
                       choices = character(0)))
  )
})


# Update 2D axis selectors --------------------------------------------------

observeEvent({
  input$plot_espace_state
  input$plot_2d_x
  input$plot_2d_y
  session_data$vars
}, {

  vars <- plot_vars()
  req(vars)
  req(input$plot_espace_state == "plot_2d")

  x_sel <- input$plot_2d_x
  y_sel <- input$plot_2d_y

  if(is.null(x_sel) || !x_sel %in% vars){
    x_sel <- vars[1]
  }

  y_choices <- setdiff(vars, x_sel)

  if(is.null(y_sel) || !y_sel %in% y_choices){
    y_sel <- y_choices[1]
  }

  x_choices <- setdiff(vars, y_sel)

  updateSelectInput(session, "plot_2d_x", choices = x_choices, selected = x_sel)
  updateSelectInput(session, "plot_2d_y", choices = y_choices, selected = y_sel)

}, ignoreInit = FALSE)


# Update Combined axis selectors --------------------------------------------

observeEvent({
  input$plot_combined_x
  input$plot_combined_y
  session_data$vars
}, {

  vars <- plot_vars()
  req(vars)

  x_sel <- input$plot_combined_x
  y_sel <- input$plot_combined_y

  if(is.null(x_sel) || !x_sel %in% vars){
    x_sel <- vars[1]
  }

  y_choices <- setdiff(vars, x_sel)

  if(is.null(y_sel) || !y_sel %in% y_choices){
    y_sel <- y_choices[1]
  }

  x_choices <- setdiff(vars, y_sel)

  updateSelectInput(session, "plot_combined_x", choices = x_choices, selected = x_sel)
  updateSelectInput(session, "plot_combined_y", choices = y_choices, selected = y_sel)

}, ignoreInit = FALSE)


# Advanced plot settings UI -------------------------------------------------

output$plot_settings_ui <- renderUI({

  has_ell <- isTRUE(session_data$ellipsoid_version > 0)
  has_raster <- !is.null(session_data$bg_raster)

  box(title = tagList(
    tags$span("Advanced Plot Settings", class = "text-section-header"),
    tags$span(icon("circle-info"),
              title = instructions$plot_settings,
              class = "tooltip-icon")),
    width = 12,
    collapsible = TRUE,
    collapsed= TRUE,

    # E-space background point settings -----------------------------------
    fluidRow(
      column(width = 4,
             tags$span("Point shape (pch)", class = "text-widget-title"),
             selectInput("plot_pch", label = NULL,
                         choices= c("Dot (.)" = ".",
                                    "Open circle"= "1",
                                    "Filled circle" = "16",
                                    "Square"= "15",
                                    "Triangle"= "17",
                                    "Cross"= "3"),
                         selected = ".")),
      column(width = 4,
             tags$span("Point size (cex)", class = "text-widget-title"),
             numericInput("plot_cex", label = NULL, value = 0.3,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 4,
             tags$span("Background point color", class = "text-widget-title"),
             colourpicker::colourInput("plot_bg_col", label = NULL,
                                       value = "#B3B3B3"))
    ),

    # Range lines (only when range method is selected) --------------------
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

    # Ellipsoid, centroid, and suitable area (only when ellipsoid built) --
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
                           choices= c("Solid" = "1",
                                      "Dashed" = "2",
                                      "Dotted" = "3"),
                           selected = "1"))
      ),

      fluidRow(
        column(width = 3,
               checkboxInput("show_centroid", "Show centroid", value = TRUE)),
        column(width = 3,
               tags$span("Centroid shape (pch)", class = "text-widget-title"),
               selectInput("plot_centroid_pch", label = NULL,
                           choices= c("Cross (X)"= "4",
                                      "Star" = "8",
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
      ),

      fluidRow(
        column(width = 3,
               checkboxInput("show_suitable_espace",
                             "Show suitable area (E-space)", value = TRUE)),
        column(width = 3,
               tags$span("Suitable area color", class = "text-widget-title"),
               colourpicker::colourInput("plot_suitable_col", label = NULL,
                                         value = "#2ECC71")),
        column(width = 3,
               checkboxInput("show_suitable_gspace",
                             "Show suitable area (G-space)", value = TRUE)),
        column(width = 3,
               tags$span("Unsuitable area color", class = "text-widget-title"),
               colourpicker::colourInput("plot_unsuitable_col", label = NULL,
                                         value = "#D3D3D3"))
      )
    ),

    # Map background color (only when raster is available) ----------------
    if(has_raster) fluidRow(
      column(width = 4,
             tags$span("Map background color", class = "text-widget-title"),
             colourpicker::colourInput("plot_map_bg_col", label = NULL,
                                       value = "#F0F0F0"))
    ),

    # Zoom (E-space only) -------------------------------------------------
    fluidRow(
      column(width = 4,
             tags$span("Zoom", class = "text-widget-title"),
             radioButtons("plot_zoom_mode", label = NULL,
                          choices= c("Auto"= "auto",
                                     "Zoom to ellipsoid" = "ellipsoid",
                                     "Manual"= "manual"),
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


# Shared settings collection ------------------------------------------------
# Reads all plot input values with NULL-safe defaults.

collect_plot_settings <- function(){

  has_ell <- isTRUE(session_data$ellipsoid_version > 0)

  list(
    has_ell= has_ell,
    ell= if(has_ell) session_data$ellipsoid else NULL,

    show_ell = {
      v <- if(!is.null(input$show_ellipsoid)) input$show_ellipsoid else TRUE
      has_ell && v
    },
    show_centroid = {
      v <- if(!is.null(input$show_centroid)) input$show_centroid else TRUE
      has_ell && v
    },
    show_suitable_espace = {
      v <- if(!is.null(input$show_suitable_espace)) input$show_suitable_espace else TRUE
      has_ell && v
    },
    show_suitable_gspace = {
      v <- if(!is.null(input$show_suitable_gspace)) input$show_suitable_gspace else TRUE
      has_ell && v
    },

    pch_val = {
      v <- if(!is.null(input$plot_pch)) input$plot_pch else "."
      if(v == ".") "." else as.numeric(v)
    },
    cex_val= if(!is.null(input$plot_cex))input$plot_cex else 0.3,
    bg_col= if(!is.null(input$plot_bg_col))input$plot_bg_col else "#B3B3B3",
    suitable_col= if(!is.null(input$plot_suitable_col))input$plot_suitable_col else "#2ECC71",
    unsuitable_col = if(!is.null(input$plot_unsuitable_col))input$plot_unsuitable_col else "#D3D3D3",
    map_bg_col= if(!is.null(input$plot_map_bg_col)) input$plot_map_bg_col else "#F0F0F0",

    show_lines = {
      ranges <- tryCatch(range_preview(),
                         error              = function(e) NULL,
                         shiny.silent.error = function(e) NULL)

      # Fall back to ranges derived from the built ellipsoid
      if(is.null(ranges) && !is.null(session_data$ellipsoid)){
        ranges <- ranges_from_ellipsoid(session_data$ellipsoid)
      }

      show <- if(!is.null(input$show_range_lines)) input$show_range_lines else TRUE
      list(active = show && !is.null(ranges), ranges = ranges)

    },

    xline_col = if(!is.null(input$plot_xline_col)) input$plot_xline_col else "#E10000",
    yline_col = if(!is.null(input$plot_yline_col)) input$plot_yline_col else "#0004D5",
    line_lwd= if(!is.null(input$plot_line_lwd))input$plot_line_lwd else 2,

    ell_col= if(!is.null(input$plot_ell_col))input$plot_ell_col else "#000000",
    ell_lwd= if(!is.null(input$plot_ell_lwd))input$plot_ell_lwd else 2,
    ell_lty= if(!is.null(input$plot_ell_lty))as.numeric(input$plot_ell_lty) else 1,

    centroid_pch = if(!is.null(input$plot_centroid_pch)) as.numeric(input$plot_centroid_pch) else 8,
    centroid_col = if(!is.null(input$plot_centroid_col)) input$plot_centroid_col else "#000000",
    centroid_cex = if(!is.null(input$plot_centroid_cex)) input$plot_centroid_cex else 1.5,

    zoom_mode = if(!is.null(input$plot_zoom_mode)) input$plot_zoom_mode else "auto"
  )
}


# E-space helpers -----------------------------------------------------------

compute_lims <- function(v1, v2, s){

  if(s$zoom_mode == "manual" &&
     !is.null(input$plot_xlim_min) && !is.na(input$plot_xlim_min) &&
     !is.null(input$plot_xlim_max) && !is.na(input$plot_xlim_max) &&
     !is.null(input$plot_ylim_min) && !is.na(input$plot_ylim_min) &&
     !is.null(input$plot_ylim_max) && !is.na(input$plot_ylim_max)){

    return(list(xlim = c(input$plot_xlim_min, input$plot_xlim_max),
                ylim = c(input$plot_ylim_min, input$plot_ylim_max)))
  }

  if(s$zoom_mode == "ellipsoid" && s$has_ell){
    idx<- match(c(v1, v2), s$ell$var_names)
    ell_pts <- ellipsoid_boundary_2d(s$ell, n_segments = 100, dim = idx)
    pad_x<- diff(range(ell_pts[, 1])) * 0.1
    pad_y<- diff(range(ell_pts[, 2])) * 0.1
    return(list(xlim = range(ell_pts[, 1]) + c(-pad_x, pad_x),
                ylim = range(ell_pts[, 2]) + c(-pad_y, pad_y)))
  }

  bg<- session_data$bg_df
  ranges <- s$show_lines$ranges

  pts_xy <- if(!is.null(bg)){
    bg[, c(v1, v2)]
  } else if(!is.null(ranges)){
    data.frame(x = c(ranges$mins[[v1]], ranges$maxs[[v1]]),
               y = c(ranges$mins[[v2]], ranges$maxs[[v2]]))
  } else {
    data.frame(x = c(0, 1), y = c(0, 1))
  }

  if(s$has_ell){
    idx<- match(c(v1, v2), s$ell$var_names)
    ell_pts <- ellipsoid_boundary_2d(s$ell, n_segments = 100, dim = idx)
    return(safe_lims(pts_xy, ell_pts))
  }

  list(xlim = range(pts_xy[, 1], na.rm = TRUE),
       ylim = range(pts_xy[, 2], na.rm = TRUE))
}


draw_espace_panel <- function(v1, v2, s){

  lims <- compute_lims(v1, v2, s)
  bg<- session_data$bg_df
  pred <- if(s$show_suitable_espace) tryCatch(pred_raster(), error = function(e) NULL) else NULL

  if(!is.null(bg)){
    plot(bg[[v1]], bg[[v2]],
         col= s$bg_col,
         pch= s$pch_val,
         cex= s$cex_val,
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

  # Overlay suitable area in E-space as colored points
  if(!is.null(pred) && !is.null(bg)){
    pred_df<- terra::as.data.frame(pred, xy = TRUE, na.rm = FALSE)
    suitable <- pred_df[!is.na(pred_df$suitability_trunc) &
                          pred_df$suitability_trunc > 0, ]
    if(nrow(suitable) > 0){
      points(suitable[[v1]], suitable[[v2]],
             col = s$suitable_col, pch = s$pch_val, cex = s$cex_val)
    }
  }

  if(isTRUE(s$show_lines$active)){
    ranges <- s$show_lines$ranges
    abline(v = ranges$mins[[v1]], col = s$xline_col, lwd = s$line_lwd, lty = 2)
    abline(v = ranges$maxs[[v1]], col = s$xline_col, lwd = s$line_lwd, lty = 2)
    abline(h = ranges$mins[[v2]], col = s$yline_col, lwd = s$line_lwd, lty = 2)
    abline(h = ranges$maxs[[v2]], col = s$yline_col, lwd = s$line_lwd, lty = 2)
  }

  if(s$show_ell){
    idx <- match(c(v1, v2), s$ell$var_names)
    add_ellipsoid(s$ell, dim = idx,
                  col_ell = s$ell_col, lwd = s$ell_lwd, lty = s$ell_lty)
  }

  if(s$show_centroid){
    idx <- match(c(v1, v2), s$ell$var_names)
    points(s$ell$centroid[idx[1]], s$ell$centroid[idx[2]],
           pch = s$centroid_pch, col = s$centroid_col, cex = s$centroid_cex)
  }
}


# G-space helper ------------------------------------------------------------

draw_gspace_panel <- function(s, title = "G-space"){

  rast<- session_data$bg_raster
  pred<- if(s$show_suitable_gspace) tryCatch(pred_raster(), error = function(e) NULL) else NULL
  suitable_col<- s$suitable_col
  unsuitable_col <- s$unsuitable_col
  map_bg_col<- s$map_bg_col

  if(!is.null(pred)){

    binary <- terra::classify(pred[["suitability_trunc"]],
                              rcl = matrix(c(-Inf, 0,0,
                                             0,Inf, 1),
                                           ncol = 3, byrow = TRUE),
                              include.lowest = TRUE)

    terra::plot(binary,
                col = c(unsuitable_col, suitable_col),
                legend = FALSE,
                axes= TRUE,
                main= title,
                colNA= map_bg_col)

    terra::add_legend(x= "bottomright",
                      legend = c("Suitable", "Unsuitable"),
                      fill= c(suitable_col, unsuitable_col),
                      bty = "n",
                      cex = 0.8)

  } else if(!is.null(rast)){
    terra::plot(rast[[1]],
                main= title,
                colNA = map_bg_col,
                axes= TRUE)
  } else {
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Longitude", ylab = "Latitude", main = title)
    text(0.5, 0.5, "No raster data available.", cex = 1.2, col = "grey50")
  }
}


# Render outputs ------------------------------------------------------------

# Tab 1: E-space ------------------------------------------------------------

output$build_espace_plot <- renderPlot({

  vars <- plot_vars()
  req(vars)

  s <- collect_plot_settings()
  state <- if(!is.null(input$plot_espace_state)) input$plot_espace_state else "plot_pairs"

  switch(state,
         "plot_pairs" = {
           pairs<- t(combn(seq_along(vars), 2))
           n_pairs <- nrow(pairs)
           n_cols<- ceiling(sqrt(n_pairs))
           n_rows<- ceiling(n_pairs / n_cols)

           old_par <- par(no.readonly = TRUE)
           on.exit(par(old_par))
           par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

           for(i in seq_len(n_pairs)){
             draw_espace_panel(vars[pairs[i, 1]], vars[pairs[i, 2]], s)
           }
         },

         "plot_2d" = {

           req(input$plot_2d_x, input$plot_2d_y)

           old_par <- par(no.readonly = TRUE)
           on.exit(par(old_par))
           par(mar = c(4, 4, 2, 1))

           draw_espace_panel(input$plot_2d_x, input$plot_2d_y, s)
         }
  )

})

output$export_espace_plot <- downloadHandler(
  filename = function(){
    paste0("espace_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  },
  content = function(file){

    vars<- plot_vars()
    req(vars)

    s<- collect_plot_settings()
    state <- if(!is.null(input$plot_espace_state)) input$plot_espace_state else "plot_pairs"

    h <- if(state == "plot_2d") 800 else 1000

    png(file, width = 1400, height = h, res = 150)

    switch(state,
           "plot_pairs" = {
             pairs<- t(combn(seq_along(vars), 2))
             n_pairs <- nrow(pairs)
             n_cols<- ceiling(sqrt(n_pairs))
             n_rows<- ceiling(n_pairs / n_cols)
             par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
             for(i in seq_len(n_pairs)) draw_espace_panel(vars[pairs[i, 1]], vars[pairs[i, 2]], s)
           },
           "plot_2d" = {
             req(input$plot_2d_x, input$plot_2d_y)
             par(mar = c(4, 4, 2, 1))
             draw_espace_panel(input$plot_2d_x, input$plot_2d_y, s)
           }
    )

    dev.off()
  }
)


# Tab 2: G-space ------------------------------------------------------------

output$build_gspace_plot <- renderPlot({

  s <- collect_plot_settings()

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(4, 4, 2, 1))

  draw_gspace_panel(s)

})

output$export_gspace_plot <- downloadHandler(
  filename = function(){
    paste0("gspace_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  },
  content = function(file){
    s <- collect_plot_settings()
    png(file, width = 1000, height = 800, res = 150)
    par(mar = c(4, 4, 2, 1))
    draw_gspace_panel(s)
    dev.off()
  }
)


# Tab 3: Combined -----------------------------------------------------------

output$build_combined_plot <- renderPlot({

  req(input$plot_combined_x, input$plot_combined_y)

  s <- collect_plot_settings()

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

  draw_espace_panel(input$plot_combined_x, input$plot_combined_y, s)
  draw_gspace_panel(s, title = "G-space")

})

output$export_combined_plot <- downloadHandler(
  filename = function(){
    paste0("combined_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  },
  content = function(file){
    req(input$plot_combined_x, input$plot_combined_y)
    s <- collect_plot_settings()
    png(file, width = 1000, height = 1400, res = 150)
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
    draw_espace_panel(input$plot_combined_x, input$plot_combined_y, s)
    draw_gspace_panel(s, title = "G-space")
    dev.off()
  }
)


output$export_btn_ui <- renderUI({

  active_tab <- if(!is.null(input$plot_tabs)) input$plot_tabs else "tab_espace"

  btn_id <- switch(active_tab,
                   "tab_espace" = "export_espace_plot",
                   "tab_gspace" = "export_gspace_plot",
                   "tab_combined" = "export_combined_plot",
                   "export_espace_plot")

  downloadButton(btn_id,
                 tags$span("Export Figure",
                           class = "text-widget-title",
                           title = "Download the current plot as a PNG."),
                 class = "btn-default")
})
