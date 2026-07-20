# Title: Predict Tab Plot Logic
# Description: E-space, G-space, and Combined prediction plots.
# get_input(), update_axis_selectors(), compute_lims(),
# open_device() are defined in build_tab_plot.R which loads first.
# Date Last Updated: 07/17/2026


# Helpers -------------------------------------------------------------------

pred_viz_ell <- function(){
  session_data$current_ellipsoid
}

pred_viz_result <- function(){
  ell <- session_data$current_ellipsoid
  if(is.null(ell) || is.null(ell$ell_id)) return(NULL)

  result
}

pred_layer_names <- function(){
  result <- pred_viz_result()
  if(is.null(result)) return(character(0))
  if(inherits(result, "SpatRaster")) return(names(result))
  setdiff(colnames(result), c(SPATIAL_COL_PATTERN, session_data$vars))
}

get_pred_pal <- function(s, n = 100){
  pal <- switch(s$palette,
                "viridis" = hcl.colors(n, palette = "viridis"),
                "heat" = heat.colors(n),
                "terrain" = terrain.colors(n),
                "topo" = topo.colors(n),
                "rainbow" = rainbow(n),
                "grayscale" = gray.colors(n, start = 0.9, end = 0.1),
                hcl.colors(n, palette = "viridis"))
  if(isTRUE(s$reverse_pal)) rev(pal) else pal
}

# Collect prediction plot settings.
collect_pred_settings <- function(){
  ell <- pred_viz_ell()
  has_ell <- !is.null(ell) &&
    !is.null(ell$cov_matrix) &&
    all(is.finite(ell$cov_matrix))

  list(
    has_ell = has_ell,
    ell = if(has_ell) ell else NULL,
    result = pred_viz_result(),

    pch_val = {
      v <- get_input("pred_pch", ".")
      if(v == ".") "." else as.numeric(v)
    },
    cex_val = get_input("pred_cex", 0.3),
    bg_col = get_input("pred_bg_col", "#B3B3B3"),
    show_ell = has_ell && isTRUE(get_input("pred_show_ell", TRUE)),
    ell_col = get_input("pred_ell_col", "#000000"),
    ell_lwd = get_input("pred_ell_lwd", 2),
    ell_lty = as.numeric(get_input("pred_ell_lty", "1")),
    suitable_col = get_input("pred_suitable_col", "#097a21"),
    unsuitable_col = get_input("pred_unsuitable_col", "#D3D3D3"),
    map_bg_col = get_input("pred_map_bg_col","#F0F0F0"),
    palette = get_input("pred_palette", "viridis"),
    reverse_pal = isTRUE(get_input("pred_reverse_pal", FALSE)),
    zoom_mode = get_input("pred_zoom_mode", "auto")
  )
}


# Draw functions ------------------------------------------------------------

draw_pred_espace_panel <- function(v1, v2, layer, s){

  lims <- compute_lims(v1, v2, s)
  bg <- session_data$bg_df
  result <- s$result

  if(!is.null(bg)){
    plot(bg[[v1]], bg[[v2]],
         col = s$bg_col,
         pch = s$pch_val,
         cex = s$cex_val,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = lims$asp,
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2, if(nzchar(layer)) paste0("(", layer, ")")))
  } else {
    plot(NA, NA,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = lims$asp,
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2, if(nzchar(layer)) paste0("(", layer, ")")))
  }

  if(!is.null(result) && nzchar(layer)){

    vals <- if(inherits(result, "SpatRaster")){
      # Extract values at background point locations
      coords <- bg[, c("x", "y")]
      extracted <- terra::extract(result[[layer]],
                                  as.matrix(coords))
      extracted[, 1]
    } else {
      result[[layer]]
    }

    if(!is.null(vals) && any(is.finite(vals))){
      finite_vals <- vals[is.finite(vals)]
      pal <- get_pred_pal(s)
      val_min <- min(finite_vals)
      val_max <- max(finite_vals)
      idx <- round((vals - val_min) / (val_max - val_min) * 99) + 1
      idx <- pmax(1L, pmin(100L, idx))
      cols <- pal[idx]
      cols[!is.finite(vals)] <- NA
      valid <- !is.na(cols)
      if(any(valid))
        points(bg[[v1]][valid], bg[[v2]][valid],
               col = cols[valid], pch = s$pch_val, cex = s$cex_val)
    }
  } else if(s$has_ell && !is.null(bg)){
    # No prediction stored yet: live binary fallback
    pred_fb <- tryCatch(
      predict(s$ell,
              newdata = bg,
              include_suitability = TRUE,
              include_mahalanobis = FALSE,
              suitability_truncated = TRUE,
              verbose = FALSE),
      error = function(e) NULL
    )
    if(!is.null(pred_fb)){
      inside <- !is.na(pred_fb$suitability_trunc) & pred_fb$suitability_trunc > 0
      if(any(inside))
        points(bg[[v1]][inside], bg[[v2]][inside],
               col = s$suitable_col, pch = s$pch_val, cex = s$cex_val)
    }
  }

  # Ellipsoid boundary
  if(s$show_ell && s$has_ell){
    idx <- match(c(v1, v2), s$ell$var_names)
    if(!any(is.na(idx))){
      add_ellipsoid(s$ell, dim = idx,
                    col_ell = s$ell_col,
                    lwd = s$ell_lwd,
                    lty = s$ell_lty)
    }
  }
}

draw_pred_espace_pairs <- function(vars, layer, s){
  pairs <- t(combn(seq_along(vars), 2))
  n_pairs <- nrow(pairs)
  n_cols <- ceiling(sqrt(n_pairs))
  n_rows <- ceiling(n_pairs / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
  for(i in seq_len(n_pairs))
    draw_pred_espace_panel(vars[pairs[i, 1]], vars[pairs[i, 2]], layer, s)
}

draw_pred_espace_2d_all <- function(v1, v2, s){

  bg     <- session_data$bg_df
  result <- s$result
  layers <- if(!is.null(result) && inherits(result, "SpatRaster")) names(result) else character(0)

  if(length(layers) > 0){
    n_cols <- min(length(layers), 2L)
    n_rows <- ceiling(length(layers) / n_cols)
    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
    for(layer in layers)
      draw_pred_espace_panel(v1, v2, layer, s)
  } else {
    par(mar = c(4, 4, 2, 1))
    draw_pred_espace_panel(v1, v2, "", s)
  }
}

draw_pred_gspace_all <- function(s){

  rast <- session_data$bg_raster
  result <- s$result
  map_bg_col <- s$map_bg_col

  has_result <- !is.null(result) && inherits(result, "SpatRaster")
  layers <- if(has_result) names(result) else character(0)
  n_layers <- length(layers)

  if(has_result && n_layers > 0){

    n_cols <- min(n_layers, 2L)
    n_rows <- ceiling(n_layers / n_cols)
    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

    for(layer in layers){
      pal <- get_pred_pal(s)
      terra::plot(result[[layer]],
                  col = pal,
                  axes = TRUE,
                  main = layer,
                  xlab = "Longitude", ylab = "Latitude",
                  colNA = map_bg_col)

    }

  } else if(s$has_ell && !is.null(rast)){
    # Live fallback: binary prediction from ellipsoid
    par(mar = c(4, 4, 2, 1))
    pred_fb <- tryCatch(
      predict(s$ell,
              newdata = terra::subset(rast, s$ell$var_names),
              include_suitability = TRUE,
              include_mahalanobis = FALSE,
              suitability_truncated = TRUE,
              verbose = FALSE),
      error = function(e) NULL
    )
    if(!is.null(pred_fb) && inherits(pred_fb, "SpatRaster")){
      binary <- terra::classify(pred_fb[["suitability_trunc"]],
                                rcl = matrix(c(-Inf, 0, 0,
                                               0, Inf, 1),
                                             ncol = 3, byrow = TRUE),
                                include.lowest = TRUE)
      terra::plot(binary,
                  col = c(s$unsuitable_col, s$suitable_col),
                  legend = FALSE,
                  axes = TRUE,
                  main = "G-space (live)",
                  xlab = "Longitude", ylab = "Latitude",
                  colNA = map_bg_col)
      terra::add_legend(x = "topright",
                        legend = c("Suitable", "Unsuitable"),
                        fill = c(s$suitable_col, s$unsuitable_col),
                        bty = "n",
                        cex = 0.8)
    } else if(!is.null(rast)){
      terra::plot(rast[[1]],
                  main = "G-space",
                  colNA = map_bg_col,
                  xlab = "Longitude", ylab = "Latitude",
                  axes = TRUE)
    }
  } else if(!is.null(rast)){
    par(mar = c(4, 4, 2, 1))
    terra::plot(rast[[1]],
                main = "G-space",
                colNA = map_bg_col,
                xlab = "Longitude", ylab = "Latitude",
                axes = TRUE)
  } else {
    par(mar = c(4, 4, 2, 1))
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Longitude", ylab = "Latitude", main = "G-space")
    text(0.5, 0.5, "No raster data available.", cex = 1.2, col = "grey50")
  }
}

draw_pred_gspace_layer <- function(layer, s, title = "G-space"){

  rast <- session_data$bg_raster
  result<- s$result
  map_bg_col <- s$map_bg_col

  if(!is.null(result) && inherits(result, "SpatRaster") && layer %in% names(result)){

    pal <- pal <- get_pred_pal(s)
    terra::plot(result[[layer]],
                col = pal,
                axes = TRUE,
                main = title,
                xlab = "Longitude", ylab = "Latitude",
                colNA = map_bg_col)

  } else if(!is.null(rast)){
    terra::plot(rast[[1]],
                main = title,
                colNA = map_bg_col,
                xlab = "Longitude", ylab = "Latitude",
                axes = TRUE)
  } else {
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Longitude", ylab = "Latitude", main = title)
    text(0.5, 0.5, "No raster data available.", cex = 1.2, col = "grey50")
  }
}


# Axis selector observers ---------------------------------------------------

observeEvent({
  input$pred_espace_state
  input$pred_2d_x
  input$pred_2d_y
  session_data$vars
}, {
  vars <- session_data$vars
  req(vars)
  req(input$pred_espace_state == "plot_2d")
  update_axis_selectors("pred_2d_x", "pred_2d_y", vars)
}, ignoreInit = FALSE)

observeEvent({
  input$pred_combined_x
  input$pred_combined_y
  session_data$vars
}, {
  vars <- session_data$vars
  req(vars)
  update_axis_selectors("pred_combined_x", "pred_combined_y", vars)
}, ignoreInit = FALSE)


# Outputs -------------------------------------------------------------------

output$pred_espace_options_ui <- renderUI({

  vars   <- session_data$vars
  req(vars)
  layers <- pred_layer_names()

  fluidRow(
    column(width = 12,
           radioButtons("pred_espace_state",
                        label    = tags$span("Plot type:", class = "text-widget-title"),
                        choices  = c("All pairs" = "plot_pairs", "2D" = "plot_2d"),
                        selected = "plot_pairs",
                        inline   = TRUE),

           # Layer selector only for pairs mode
           conditionalPanel(
             "input.pred_espace_state == 'plot_pairs'",
             if(length(layers) > 0){
               column(width = 4,
                      selectInput("pred_pairs_layer", label = NULL,
                                  choices  = layers,
                                  selected = layers[1]))
             }
           ),

           # 2D mode: just axis selectors, no layer selector
           conditionalPanel(
             "input.pred_espace_state == 'plot_2d'",
             column(width = 6,
                    selectInput("pred_2d_x", label = NULL, choices = character(0))),
             column(width = 6,
                    selectInput("pred_2d_y", label = NULL, choices = character(0)))
           )
    )
  )
})

output$pred_combined_options_ui <- renderUI({

  vars <- session_data$vars
  req(vars)
  layers <- pred_layer_names()

  fluidRow(
    column(width = 3,
           radioButtons("pred_combined_layout",
                        label = tags$span("Layout:", class = "text-widget-title"),
                        choices = c("Stacked" = "col", "Side by side" = "row"),
                        selected = "col",
                        inline = TRUE)),
    column(width = 3,
           selectInput("pred_combined_x", label = NULL, choices = character(0))),
    column(width = 3,
           selectInput("pred_combined_y", label = NULL, choices = character(0))),
    column(width = 3,
           if(length(layers) > 0){
             selectInput("pred_combined_layer", label = NULL,
                         choices = layers,
                         selected = layers[1])
           })
  )
})

output$pred_plot_settings_ui <- renderUI({

  has_ell <- !is.null(session_data$current_ellipsoid)
  has_raster <- !is.null(session_data$bg_raster)

  box(title = tagList(
    tags$span("Advanced Plot Settings", class = "text-section-header"),
    tags$span(icon("circle-info"),
              title = "Adjust how predictions are displayed.",
              class = "tooltip-icon")),
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,

    # Background points
    fluidRow(
      column(width = 4,
             tags$span("Point shape (pch)", class = "text-widget-title"),
             selectInput("pred_pch", label = NULL,
                         choices = c("Dot (.)" = ".",
                                     "Open circle" = "1",
                                     "Filled circle" = "16",
                                     "Square" = "15",
                                     "Triangle" = "17",
                                     "Cross" = "3"),
                         selected = ".")),
      column(width = 4,
             tags$span("Point size (cex)", class = "text-widget-title"),
             numericInput("pred_cex", label = NULL, value = 0.3,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 4,
             tags$span("Background color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type= "color",
                          value = "#B3B3B3",
                          oninput = "Shiny.setInputValue('pred_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
border: 1px solid #ccc; border-radius: 4px;
cursor: pointer;")))
    ),

    # Suitable / unsuitable colors
    fluidRow(
      column(width = 4,
             tags$span("Suitable color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type= "color",
                          value = "#097a21",
                          oninput = "Shiny.setInputValue('pred_suitable_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
border: 1px solid #ccc; border-radius: 4px;
cursor: pointer;"))),
      column(width = 4,
             tags$span("Unsuitable color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type= "color",
                          value = "#D3D3D3",
                          oninput = "Shiny.setInputValue('pred_unsuitable_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
border: 1px solid #ccc; border-radius: 4px;
cursor: pointer;")))
    ),

    # Continuous palette (base R only)
    fluidRow(
      column(width = 6,
             tags$span("Continuous palette", class = "text-widget-title"),
             selectInput("pred_palette", label = NULL,
                         choices  = c("Viridis" = "viridis",
                                      "Heat" = "heat",
                                      "Terrain" = "terrain",
                                      "Topo" = "topo",
                                      "Rainbow" = "rainbow",
                                      "Grayscale" = "grayscale"),
                         selected = "viridis")),
      column(width = 6,
             br(),
             checkboxInput("pred_reverse_pal", "Reverse palette", value = FALSE))
    ),

    # Ellipsoid overlay
    if(has_ell) fluidRow(
      column(width = 3,
             checkboxInput("pred_show_ell", "Show ellipsoid", value = TRUE)),
      column(width = 3,
             tags$span("Ellipsoid color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type= "color",
                          value = "#000000",
                          oninput = "Shiny.setInputValue('pred_ell_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
border: 1px solid #ccc; border-radius: 4px;
cursor: pointer;"))),
      column(width = 3,
             tags$span("Line width", class = "text-widget-title"),
             numericInput("pred_ell_lwd", label = NULL, value = 2,
                          min = 0.5, max = 6, step = 0.5)),
      column(width = 3,
             tags$span("Line type", class = "text-widget-title"),
             selectInput("pred_ell_lty", label = NULL,
                         choices = c("Solid" = "1",
                                     "Dashed" = "2",
                                     "Dotted" = "3"),
                         selected = "1"))
    ),

    # Map background
    if(has_raster) fluidRow(
      column(width = 4,
             tags$span("Map background color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type= "color",
                          value = "#F0F0F0",
                          oninput = "Shiny.setInputValue('pred_map_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
border: 1px solid #ccc; border-radius: 4px;
cursor: pointer;")))
    ),

    # Zoom
    fluidRow(
      column(width = 4,
             tags$span("Zoom", class = "text-widget-title"),
             radioButtons("pred_zoom_mode", label = NULL,
                          choices = c("Auto" = "auto",
                                      "Zoom to ellipsoid" = "ellipsoid"),
                          selected = "auto"))
    ),

    # Export
    fluidRow(
      column(width = 4,
             br(),
             actionButton("open_pred_export_modal",
                          tagList(icon("download"), "Export Figure"),
                          class = "btn-default"))
    )
  )
})

output$pred_espace_plot <- renderPlot({

  req(session_data$vars)
  req(session_data$current_ellipsoid)

  vars  <- session_data$vars
  s     <- collect_pred_settings()
  state <- if(!is.null(input$pred_espace_state)) input$pred_espace_state else "plot_pairs"

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  switch(state,
         "plot_pairs" = {
           layer <- if(!is.null(input$pred_pairs_layer)) input$pred_pairs_layer else ""
           draw_pred_espace_pairs(vars, layer, s)
         },
         "plot_2d" = {
           req(input$pred_2d_x, input$pred_2d_y)
           draw_pred_espace_2d_all(input$pred_2d_x, input$pred_2d_y, s)
         }
  )

})

output$pred_gspace_plot <- renderPlot({

  req(session_data$current_ellipsoid)

  s <- collect_pred_settings()

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  draw_pred_gspace_all(s)
})

output$pred_combined_plot <- renderPlot({
  req(session_data$vars)
  req(session_data$current_ellipsoid)
  req(input$pred_combined_x, input$pred_combined_y)

  s <- collect_pred_settings()
  layer <- if(!is.null(input$pred_combined_layer)) input$pred_combined_layer else ""
  layout <- if(!is.null(input$pred_combined_layout)) input$pred_combined_layout else "col"
  mfrow <- if(layout == "col") c(2, 1) else c(1, 2)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = mfrow, mar = c(4, 4, 2, 1))

  draw_pred_espace_panel(input$pred_combined_x, input$pred_combined_y, layer, s)
  draw_pred_gspace_layer(layer, s, title = "G-space")
})


# Export --------------------------------------------------------------------

output$pred_export_settings_ui <- renderUI({

  active_tab <- if(!is.null(input$pred_tabs)) input$pred_tabs else "pred_espace"
  ext <- if(!is.null(input$pred_export_filetype)) input$pred_export_filetype else "png"

  default_h <- switch(active_tab,
                      "pred_espace" = if(!is.null(input$pred_espace_state) &&
                                         input$pred_espace_state == "plot_2d") 800 else 1000,
                      "pred_gspace" = 800,
                      "pred_combined" = if(!is.null(input$pred_combined_layout) &&
                                           input$pred_combined_layout == "row") 800 else 1400,
                      1000
  )

  if(ext == "png"){
    fluidRow(
      column(width = 4,
             tags$span("Width (px)", class = "text-widget-title"),
             numericInput("pred_export_width_px", label = NULL,
                          value = 1400, min = 400, max = 4000, step = 100)),
      column(width = 4,
             tags$span("Height (px)", class = "text-widget-title"),
             numericInput("pred_export_height_px", label = NULL,
                          value = default_h, min = 400, max = 4000, step = 100)),
      column(width = 4,
             tags$span("Resolution (dpi)", class = "text-widget-title"),
             numericInput("pred_export_res", label = NULL,
                          value = 150, min = 72, max = 600, step = 50))
    )
  } else {
    tagList(
      fluidRow(
        column(width = 6,
               tags$span("Width (inches)", class = "text-widget-title"),
               numericInput("pred_export_width_in", label = NULL,
                            value = 10, min = 1, max = 30, step = 0.5)),
        column(width = 6,
               tags$span("Height (inches)", class = "text-widget-title"),
               numericInput("pred_export_height_in", label = NULL,
                            value = round(default_h / 150, 1),
                            min = 1, max = 30, step = 0.5))
      ),
      p("PDF and SVG use vector graphics and do not require a resolution setting.",
        class = "text-instruction")
    )
  }
})

open_pred_device <- function(file, ext){
  if(ext == "png"){
    w <- if(!is.null(input$pred_export_width_px)) input$pred_export_width_px else 1400
    h <- if(!is.null(input$pred_export_height_px)) input$pred_export_height_px else 1000
    res <- if(!is.null(input$pred_export_res)) input$pred_export_res else 150
    png(file, width = w, height = h, res = res)
  } else if(ext == "pdf"){
    w <- if(!is.null(input$pred_export_width_in)) input$pred_export_width_in else 10
    h <- if(!is.null(input$pred_export_height_in)) input$pred_export_height_in else 7
    pdf(file, width = w, height = h)
  } else if(ext == "svg"){
    w <- if(!is.null(input$pred_export_width_in)) input$pred_export_width_in else 10
    h <- if(!is.null(input$pred_export_height_in)) input$pred_export_height_in else 7
    svg(file, width = w, height = h)
  }
}

observeEvent(input$open_pred_export_modal, {
  showModal(modalDialog(
    title = "Export Prediction Figure",
    size = "m",
    fluidRow(
      column(width = 12,
             tags$span("File type", class = "text-widget-title"),
             radioButtons("pred_export_filetype", label = NULL,
                          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                          selected = if(!is.null(input$pred_export_filetype))
                            input$pred_export_filetype else "png",
                          inline = TRUE))
    ),
    uiOutput("pred_export_settings_ui"),
    footer = tagList(
      modalButton("Cancel"),
      downloadButton("confirm_pred_export", "Export", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

output$confirm_pred_export <- downloadHandler(
  filename = function(){
    ext <- if(!is.null(input$pred_export_filetype)) input$pred_export_filetype else "png"
    tab <- if(!is.null(input$pred_tabs)) input$pred_tabs else "pred_espace"
    prefix <- switch(tab,
                     "pred_espace" = "pred_espace",
                     "pred_gspace" = "pred_gspace",
                     "pred_combined" = "pred_combined",
                     "pred_espace")
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  },
  content = function(file){
    ext <- if(!is.null(input$pred_export_filetype)) input$pred_export_filetype else "png"
    tab <- if(!is.null(input$pred_tabs)) input$pred_tabs else "pred_espace"
    s<- collect_pred_settings()
    layer <- if(!is.null(input$pred_combined_layer)) input$pred_combined_layer else ""

    open_pred_device(file, ext)

    switch(tab,
           "pred_espace" = {
             vars  <- session_data$vars
             req(vars)
             state <- if(!is.null(input$pred_espace_state)) input$pred_espace_state else "plot_pairs"
             switch(state,
                    "plot_pairs" = {
                      layer <- if(!is.null(input$pred_pairs_layer)) input$pred_pairs_layer else ""
                      draw_pred_espace_pairs(vars, layer, s)
                    },
                    "plot_2d" = {
                      req(input$pred_2d_x, input$pred_2d_y)
                      draw_pred_espace_2d_all(input$pred_2d_x, input$pred_2d_y, s)
                    }
             )
           },
           "pred_gspace" = {
             draw_pred_gspace_all(s)
           },
           "pred_combined" = {
             req(input$pred_combined_x, input$pred_combined_y)
             layout <- if(!is.null(input$pred_combined_layout)) input$pred_combined_layout else "col"
             mfrow <- if(layout == "col") c(2, 1) else c(1, 2)
             par(mfrow = mfrow, mar = c(4, 4, 2, 1))
             draw_pred_espace_panel(input$pred_combined_x, input$pred_combined_y, layer, s)
             draw_pred_gspace_layer(layer, s, title = "G-space")
           }
    )

    dev.off()
  }
)
