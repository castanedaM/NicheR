# Title: Generate Tab Plot Logic
# Description: E-space, G-space, and Combined plots for generated occurrences.
# get_input(), update_axis_selectors(), compute_lims(),
# draw_gspace_panel(), open_device() defined in build_tab_plot.R.
# Date Last Updated: 07/29/2026


# Collect generate plot settings --------------------------------------------

collect_gen_settings <- function(){

  ell <- session_data$current_ellipsoid
  has_ell <- !is.null(ell) &&
    !is.null(ell$cov_matrix) &&
    all(is.finite(ell$cov_matrix))

  list(
    has_ell = has_ell,
    ell = if(has_ell) ell else NULL,

    show_ell = has_ell && isTRUE(get_input("gen_show_ell", TRUE)),
    ell_col = get_input("gen_ell_col", "#000000"),
    ell_lwd = get_input("gen_ell_lwd", 2),
    ell_lty = as.numeric(get_input("gen_ell_lty", "1")),

    pch_val = {
      v <- get_input("gen_pch", ".")
      if(v == ".") "." else as.numeric(v)
    },
    cex_val = get_input("gen_cex", 0.3),
    bg_col = get_input("gen_bg_col", "#B3B3B3"),

    occ_col = get_input("gen_occ_col", "#097a21"),
    occ_pch = as.numeric(get_input("gen_occ_pch", "16")),
    occ_cex = get_input("gen_occ_cex", 0.8),

    pred_palette = get_input("gen_pred_palette", "heat"),
    map_bg_col = get_input("gen_map_bg_col", "#F0F0F0"),

    zoom_mode = get_input("plot_zoom_mode", "auto"),
    asp_espace = get_input("plot_asp_espace", "auto"),
    export_cex = get_input("export_cex", 1)
  )
}


# Current ellipsoid occurrence lookup ---------------------------------------

gen_occ_for_current <- function(layer = NULL){
  ell <- session_data$current_ellipsoid
  if(is.null(ell)) return(NULL)

  occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_id]]
  if(is.null(occ_ell) && !is.null(ell$ell_name)){
    occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_name]]
  }
  if(is.null(occ_ell) || length(occ_ell) == 0) return(NULL)

  if(!is.null(layer) && layer %in% names(occ_ell)){
    return(occ_ell[[layer]])
  }
  occ_ell[[1]]
}


# Draw functions ------------------------------------------------------------

# E-space panel colored by prediction values
draw_gen_espace_pred <- function(v1, v2, layer, s){

  lims <- compute_lims(v1, v2, s)
  bg <- session_data$bg_df
  rast <- session_data$bg_raster

  # Get prediction result for this layer
  ell <- s$ell
  result <- if(!is.null(ell)){
    r <- session_data$ellipsoid_prediction_list[[ell$ell_id]]
    if(is.null(r) && !is.null(ell$ell_name))
      r <- session_data$ellipsoid_prediction_list[[ell$ell_name]]
    r
  } else NULL

  if(!is.null(bg)){
    plot(bg[[v1]], bg[[v2]],
         col = s$bg_col,
         pch = s$pch_val,
         cex = s$cex_val,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste0(v1, " vs. ", v2, " | ", layer))
  } else {
    plot(NA, NA,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste0(v1, " vs. ", v2, " | ", layer))
  }

  # Prediction overlay
  if(!is.null(result) && inherits(result, "SpatRaster") &&
     layer %in% names(result) && !is.null(bg)){
    coords <- bg[, c("x", "y")]
    extracted <- terra::extract(result[[layer]], as.matrix(coords))
    vals <- extracted[, 1]
    finite_v <- vals[is.finite(vals)]
    if(length(finite_v) > 0){
      pal <- switch(s$pred_palette,
                    "heat" = heat.colors(100),
                    "terrain" = terrain.colors(100),
                    "topo" = topo.colors(100),
                    "rainbow" = rainbow(100),
                    "grayscale" = gray.colors(100, start = 0.9, end = 0.1),
                    heat.colors(100))
      val_min <- min(finite_v)
      val_max <- max(finite_v)
      idx <- round((vals - val_min) / (val_max - val_min) * 99) + 1
      idx <- pmax(1L, pmin(100L, idx))
      cols <- pal[idx]
      cols[!is.finite(vals)] <- NA
      valid <- !is.na(cols)
      if(any(valid))
        points(bg[[v1]][valid], bg[[v2]][valid],
               col = cols[valid], pch = s$pch_val, cex = s$cex_val)
    }
  }

  # Ellipsoid boundary
  if(s$show_ell && s$has_ell){
    idx <- match(c(v1, v2), s$ell$var_names)
    if(!any(is.na(idx)))
      add_ellipsoid(s$ell, dim = idx,
                    col_ell = s$ell_col, lwd = s$ell_lwd, lty = s$ell_lty)
  }
}

# E-space panel with occurrences overlaid
draw_gen_espace_occ <- function(v1, v2, layer, s){

  lims <- compute_lims(v1, v2, s)
  bg <- session_data$bg_df
  rast <- session_data$bg_raster
  occ <- gen_occ_for_current(layer)

  if(!is.null(bg)){
    plot(bg[[v1]], bg[[v2]],
         col = s$bg_col,
         pch = s$pch_val,
         cex = s$cex_val,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste0(v1, " vs. ", v2, " | occurrences"))
  } else {
    plot(NA, NA,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste0(v1, " vs. ", v2, " | occurrences"))
  }

  # Ellipsoid boundary
  if(s$show_ell && s$has_ell){
    idx <- match(c(v1, v2), s$ell$var_names)
    if(!any(is.na(idx)))
      add_ellipsoid(s$ell, dim = idx,
                    col_ell = s$ell_col, lwd = s$ell_lwd, lty = s$ell_lty)
  }

  # Occurrences in E-space via extract
  if(!is.null(occ) && nrow(occ) > 0 && !is.null(rast)){
    env_vals <- tryCatch(
      terra::extract(rast[[c(v1, v2)]], as.matrix(occ[, c("x", "y")])),
      error = function(e) NULL
    )
    if(!is.null(env_vals)){
      valid <- is.finite(env_vals[[v1]]) & is.finite(env_vals[[v2]])
      if(any(valid))
        points(env_vals[[v1]][valid], env_vals[[v2]][valid],
               col = s$occ_col,
               pch = s$occ_pch,
               cex = s$occ_cex)
    }
  } else if(!is.null(occ) && nrow(occ) > 0){
    # No raster: use add_data from nicheR if env columns exist
    tryCatch(
      add_data(occ, v1 = v1, v2 = v2,
               col = s$occ_col, pch = s$occ_pch, cex = s$occ_cex),
      error = function(e) NULL
    )
  }
}

# G-space panel with prediction raster
draw_gen_gspace_pred <- function(layer, s, title = "G-space prediction"){

  rast <- session_data$bg_raster
  map_bg_col <- s$map_bg_col
  cex_val <- s$export_cex

  ell <- s$ell
  result <- if(!is.null(ell)){
    r <- session_data$ellipsoid_prediction_list[[ell$ell_id]]
    if(is.null(r) && !is.null(ell$ell_name))
      r <- session_data$ellipsoid_prediction_list[[ell$ell_name]]
    r
  } else NULL

  plot_rast <- function(r, ttl){
    par(cex.axis = cex_val, cex.lab = cex_val, cex.main = cex_val * 1.1)
    terra::plot(r, main = ttl, colNA = map_bg_col,
                axes = TRUE, xlab = "Longitude", ylab = "Latitude")
  }

  if(!is.null(result) && inherits(result, "SpatRaster") &&
     layer %in% names(result)){
    plot_rast(result[[layer]], title)
  } else if(!is.null(rast)){
    plot_rast(rast[[1]], title)
  } else {
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "", main = title)
    text(0.5, 0.5, "No raster available.", cex = cex_val, col = "grey50")
  }
}

# G-space panel with occurrences overlaid on raster
draw_gen_gspace_occ <- function(layer, s, title = "G-space occurrences"){

  rast <- session_data$bg_raster
  map_bg_col <- s$map_bg_col
  cex_val <- s$export_cex
  occ <- gen_occ_for_current(layer)

  plot_rast <- function(r, ttl){
    par(cex.axis = cex_val, cex.lab = cex_val, cex.main = cex_val * 1.1)
    terra::plot(r, main = ttl, colNA = map_bg_col,
                axes = TRUE, xlab = "Longitude", ylab = "Latitude")
  }

  if(!is.null(rast)){
    plot_rast(rast[[1]], title)
  } else {
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "", main = title)
    text(0.5, 0.5, "No raster available.", cex = cex_val, col = "grey50")
    return(invisible(NULL))
  }

  if(!is.null(occ) && nrow(occ) > 0){
    points(occ$x, occ$y,
           col = s$occ_col,
           pch = s$occ_pch,
           cex = s$occ_cex)
  }
}


# Axis selector observers ---------------------------------------------------

observeEvent({
  input$gen_2d_x
  input$gen_2d_y
  session_data$vars
}, {
  vars <- session_data$vars
  req(vars, length(vars) >= 2)
  update_axis_selectors("gen_2d_x", "gen_2d_y", vars)
}, ignoreInit = FALSE)


# Top UI: which ellipsoid is being viewed -----------------------------------

output$gen_current_ell_ui <- renderUI({

  ell <- session_data$current_ellipsoid
  req(ell)

  occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_id]]
  if(is.null(occ_ell) && !is.null(ell$ell_name))
    occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_name]]

  n_sets <- if(!is.null(occ_ell)) length(occ_ell) else 0L

  fluidRow(
    column(width = 12,
           tags$div(
             style = "display: flex; align-items: center; gap: 10px; margin-bottom: 6px;",
             icon("eye"),
             tags$span(ell$ell_name,
                       style = "font-weight: 500; color: #097a21; font-size: 13px;"),
             tags$span(paste0("(", n_sets, " occurrence set",
                              if(n_sets != 1) "s" else "", ")"),
                       style = "font-size: 11px; color: #aaa;")
           ))
  )
})


# Layer selector for 2D and combined ----------------------------------------

output$gen_layer_select_ui <- renderUI({

  ell <- session_data$current_ellipsoid
  req(ell)

  occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_id]]
  if(is.null(occ_ell) && !is.null(ell$ell_name))
    occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_name]]

  req(!is.null(occ_ell) && length(occ_ell) > 0)

  layers <- names(occ_ell)
  default <- if("suitability_trunc" %in% layers) "suitability_trunc" else layers[1]

  selectInput("gen_layer_selected",
              label = tags$span("Occurrence layer", class = "text-widget-title"),
              choices = layers,
              selected = default)
})


# E-space options UI --------------------------------------------------------

output$generate_espace_options_ui <- renderUI({

  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names
  req(length(vars) >= 2)

  fluidRow(
    column(width = 5,
           selectInput("gen_2d_x", label = NULL,
                       choices = vars,
                       selected = vars[1])),
    column(width = 5,
           selectInput("gen_2d_y", label = NULL,
                       choices = setdiff(vars, vars[1]),
                       selected = if(length(vars) > 1) vars[2] else vars[1])),
    column(width = 2,
           uiOutput("gen_layer_select_ui"))
  )
})


# Advanced settings UI ------------------------------------------------------

output$gen_plot_settings_ui <- renderUI({

  has_ell <- !is.null(session_data$current_ellipsoid)
  has_raster <- !is.null(session_data$bg_raster)

  box(title = tagList(
    tags$span("Advanced Plot Settings", class = "text-section-header"),
    tags$span(icon("circle-info"),
              title = "Adjust how occurrences and predictions are displayed.",
              class = "tooltip-icon")),
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,

    # Background points
    fluidRow(
      column(width = 4,
             tags$span("Point shape (pch)", class = "text-widget-title"),
             selectInput("gen_pch", label = NULL,
                         choices = c("Dot (.)" = ".",
                                     "Open circle" = "1",
                                     "Filled circle" = "16",
                                     "Square" = "15",
                                     "Triangle" = "17",
                                     "Cross" = "3"),
                         selected = ".")),
      column(width = 4,
             tags$span("Point size (cex)", class = "text-widget-title"),
             numericInput("gen_cex", label = NULL, value = 0.3,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 4,
             tags$span("Background color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#B3B3B3",
                          oninput = "Shiny.setInputValue('gen_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")))
    ),

    # Occurrence points
    fluidRow(
      column(width = 4,
             tags$span("Occurrence shape (pch)", class = "text-widget-title"),
             selectInput("gen_occ_pch", label = NULL,
                         choices = c("Filled circle" = "16",
                                     "Open circle" = "1",
                                     "Square" = "15",
                                     "Triangle" = "17",
                                     "Cross" = "3"),
                         selected = "16")),
      column(width = 4,
             tags$span("Occurrence size (cex)", class = "text-widget-title"),
             numericInput("gen_occ_cex", label = NULL, value = 0.8,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 4,
             tags$span("Occurrence color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#097a21",
                          oninput = "Shiny.setInputValue('gen_occ_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")))
    ),

    # Prediction palette
    fluidRow(
      column(width = 6,
             tags$span("Prediction palette", class = "text-widget-title"),
             selectInput("gen_pred_palette", label = NULL,
                         choices = c("Heat" = "heat",
                                     "Terrain" = "terrain",
                                     "Topo" = "topo",
                                     "Rainbow" = "rainbow",
                                     "Grayscale" = "grayscale"),
                         selected = "heat"))
    ),

    # Ellipsoid overlay
    if(has_ell) fluidRow(
      column(width = 3,
             checkboxInput("gen_show_ell", "Show ellipsoid", value = TRUE)),
      column(width = 3,
             tags$span("Ellipsoid color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#000000",
                          oninput = "Shiny.setInputValue('gen_ell_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;"))),
      column(width = 3,
             tags$span("Line width", class = "text-widget-title"),
             numericInput("gen_ell_lwd", label = NULL, value = 2,
                          min = 0.5, max = 6, step = 0.5)),
      column(width = 3,
             tags$span("Line type", class = "text-widget-title"),
             selectInput("gen_ell_lty", label = NULL,
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
               tags$input(type = "color",
                          value = "#F0F0F0",
                          oninput = "Shiny.setInputValue('gen_map_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")))
    ),

    # Export button
    fluidRow(
      column(width = 4,
             br(),
             actionButton("open_gen_export_modal",
                          tagList(icon("download"), "Export Figure"),
                          class = "btn-default"))
    )
  )
})


# Render outputs ------------------------------------------------------------

output$generate_espace_plot <- renderPlot({

  req(session_data$current_ellipsoid)
  req(session_data$bg_raster)
  req(input$gen_2d_x, input$gen_2d_y)

  s <- collect_gen_settings()
  s$cex_val <- s$cex_espace
  layer <- if(!is.null(input$gen_layer_selected)) input$gen_layer_selected else ""

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

  draw_gen_espace_pred(input$gen_2d_x, input$gen_2d_y, layer, s)
  draw_gen_espace_occ(input$gen_2d_x, input$gen_2d_y, layer, s)

}, height = 450)

output$generate_gspace_plot <- renderPlot({

  req(session_data$current_ellipsoid)
  req(session_data$bg_raster)

  s <- collect_gen_settings()
  layer <- if(!is.null(input$gen_layer_selected)) input$gen_layer_selected else ""

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 2), mar = c(3, 3, 2, 4))

  draw_gen_gspace_pred(layer, s, title = "Prediction")
  draw_gen_gspace_occ(layer, s, title = "Occurrences")

}, height = 450)

output$generate_combined_plot <- renderPlot({

  req(session_data$current_ellipsoid)
  req(session_data$bg_raster)
  req(input$gen_2d_x, input$gen_2d_y)

  s <- collect_gen_settings()
  s$cex_val <- s$cex_espace
  layer <- if(!is.null(input$gen_layer_selected)) input$gen_layer_selected else ""

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

  # Row 1: E-space prediction | E-space occurrences
  draw_gen_espace_pred(input$gen_2d_x, input$gen_2d_y, layer, s)
  draw_gen_espace_occ(input$gen_2d_x, input$gen_2d_y, layer, s)

  # Row 2: G-space prediction | G-space occurrences
  par(mar = c(3, 3, 2, 4))
  draw_gen_gspace_pred(layer, s, title = "Prediction")
  draw_gen_gspace_occ(layer, s, title = "Occurrences")

}, height = 800)


# Export --------------------------------------------------------------------

observeEvent(input$open_gen_export_modal, {
  showModal(modalDialog(
    title = "Export Generate Figure",
    size = "m",
    fluidRow(
      column(width = 6,
             tags$span("File type", class = "text-widget-title"),
             radioButtons("export_filetype", label = NULL,
                          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                          selected = if(!is.null(input$export_filetype))
                            input$export_filetype else "png",
                          inline = TRUE)),
      column(width = 6,
             tags$span("Unit", class = "text-widget-title"),
             radioButtons("export_unit", label = NULL,
                          choiceNames = list(
                            tags$span("mm", class = "text-widget-inner"),
                            tags$span("inches", class = "text-widget-inner"),
                            tags$span("px", class = "text-widget-inner")
                          ),
                          choiceValues = c("mm", "in", "px"),
                          selected = if(!is.null(input$export_unit))
                            input$export_unit else "mm",
                          inline = TRUE))
    ),
    uiOutput("export_settings_ui"),
    footer = tagList(
      modalButton("Cancel"),
      downloadButton("confirm_gen_export", "Export", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

output$confirm_gen_export <- downloadHandler(
  filename = function(){
    ext <- if(!is.null(input$export_filetype)) input$export_filetype else "png"
    tab <- if(!is.null(input$generate_tabs)) input$generate_tabs else "generate_espace"
    prefix <- switch(tab,
                     "generate_espace" = "gen_espace",
                     "generate_gspace" = "gen_gspace",
                     "generate_combined" = "gen_combined",
                     "gen_espace")
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  },
  content = function(file){
    req(session_data$current_ellipsoid)
    req(session_data$bg_raster)

    ext <- if(!is.null(input$export_filetype)) input$export_filetype else "png"
    tab <- if(!is.null(input$generate_tabs)) input$generate_tabs else "generate_espace"
    s <- collect_gen_settings()
    s$cex_val <- s$cex_espace
    layer <- if(!is.null(input$gen_layer_selected)) input$gen_layer_selected else ""

    open_device(file, ext)

    switch(tab,
           "generate_espace" = {
             req(input$gen_2d_x, input$gen_2d_y)
             par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
             draw_gen_espace_pred(input$gen_2d_x, input$gen_2d_y, layer, s)
             draw_gen_espace_occ(input$gen_2d_x, input$gen_2d_y, layer, s)
           },
           "generate_gspace" = {
             par(mfrow = c(1, 2), mar = c(3, 3, 2, 4))
             draw_gen_gspace_pred(layer, s, title = "Prediction")
             draw_gen_gspace_occ(layer, s, title = "Occurrences")
           },
           "generate_combined" = {
             req(input$gen_2d_x, input$gen_2d_y)
             par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
             draw_gen_espace_pred(input$gen_2d_x, input$gen_2d_y, layer, s)
             draw_gen_espace_occ(input$gen_2d_x, input$gen_2d_y, layer, s)
             par(mar = c(3, 3, 2, 4))
             draw_gen_gspace_pred(layer, s, title = "Prediction")
             draw_gen_gspace_occ(layer, s, title = "Occurrences")
           }
    )

    dev.off()
  }
)
