# Title: Plot logic
# Description: Handle e-space, g-space, and combined plots
# Date last updated: 07/30/2026

# Functions -----------------------------------------------------------------

draw_espace_panel_generate <- function(v1, v2, s){

  lims <- compute_lims(v1, v2, s)
  bg <- session_data$bg_df
  layer <- s$layer
  ell <- s$ell

  # Determine source raster for this layer
  pred_result <- if(!is.null(ell)){
    r <- session_data$ellipsoid_prediction_list[[ell$ell_id]]
    if(is.null(r)) session_data$ellipsoid_prediction_list[[ell$ell_name]] else r
  } else NULL

  bias_result <- if(!is.null(ell)){
    r <- session_data$ellipsoid_prediction_list_biased[[ell$ell_id]]
    if(is.null(r)) session_data$ellipsoid_prediction_list_biased[[ell$ell_name]] else r
  } else NULL

  source_rast <- if(nzchar(layer)){
    if(!is.null(pred_result) && inherits(pred_result, "SpatRaster") &&
       layer %in% names(pred_result)){
      pred_result
    } else if(!is.null(bias_result) && inherits(bias_result, "SpatRaster") &&
              layer %in% names(bias_result)){
      bias_result
    } else {
      NULL
    }
  } else {
    NULL
  }

  # Background scatter
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
         main = paste0(v1, " vs. ", v2,
                       if(nzchar(layer)) paste0(" (", layer, ")") else ""))
  } else {
    plot(NA, NA,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste0(v1, " vs. ", v2,
                       if(nzchar(layer)) paste0(" (", layer, ")") else ""))
  }

  if(!is.null(source_rast) && nzchar(layer) && !is.null(bg)){
    coords <- bg[, c("x", "y")]
    extracted <- terra::extract(source_rast[[layer]], as.matrix(coords))
    vals <- extracted[, 1]
    finite_v <- vals[is.finite(vals)]
    if(length(finite_v) > 0){
      pal <- switch(s$palette,
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

  # Occurrences overlaid on same panel
  if(isTRUE(s$show_occ) && s$has_ell){
    layer <- if(!is.null(s$layer) && length(s$layer) > 0) s$layer else ""
    ell <- s$ell
    occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_id]]
    if(is.null(occ_ell) && !is.null(ell$ell_name))
      occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_name]]
    occ <- if(nzchar(layer) && !is.null(occ_ell)) occ_ell[[layer]] else NULL
    if(!is.null(occ) && nrow(occ) > 0 && !is.null(session_data$bg_raster)){
      env_vals <- terra::extract(session_data$bg_raster[[c(v1, v2)]],
                                 as.matrix(occ[, c("x", "y")]))
      valid <- is.finite(env_vals[[v1]]) & is.finite(env_vals[[v2]])
      if(any(valid))
        points(env_vals[[v1]][valid], env_vals[[v2]][valid],
               col = s$occ_col, pch = s$occ_pch, cex = s$occ_cex)
    }
  }
}

draw_espace_pairs_generate <- function(vars, s){
  pairs <- t(combn(seq_along(vars), 2))
  n_pairs <- nrow(pairs)
  n_cols <- ceiling(sqrt(n_pairs))
  n_rows <- ceiling(n_pairs / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
  for(i in seq_len(n_pairs)) draw_espace_panel_generate(vars[pairs[i, 1]], vars[pairs[i, 2]], s)
}

draw_gspace_panel_generate <- function(rast, s, title = NULL, col = NULL){
  map_bg_col <- s$map_bg_col
  cex_val <- if(!is.null(s$export_cex)) s$export_cex else 1
  ttl <- if(!is.null(title)) title else names(rast)[1]
  par(cex.axis = cex_val, cex.lab = cex_val, cex.main = cex_val * 1.1)
  if(!is.null(col)){
    terra::plot(rast,
                col = col,
                legend = FALSE,
                main = ttl,
                colNA = map_bg_col,
                axes = TRUE,
                xlab = "Longitude",
                ylab = "Latitude")
  } else {
    terra::plot(rast,
                main = ttl,
                colNA = map_bg_col,
                axes = TRUE,
                xlab = "Longitude",
                ylab = "Latitude")
  }
}

draw_gspace_all_generate <- function(vars, s){
  n_cols <- 2
  n_rows <- ceiling(length(vars) / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
  for(i in seq_len(length(vars)))
    draw_gspace_panel_generate(session_data$bg_raster[[vars[i]]], s)
}

# Called at the top of every draw function.
# Returns a plain list so drawing functions are pure and testable.
collect_plot_settings_generate <- function(){

  ell <- session_data$current_ellipsoid
  has_ell <- isTRUE(!is.null(ell) &&
                      !is.null(ell$cov_matrix) &&
                      all(is.finite(ell$cov_matrix)))
  list(
    has_ell = has_ell,
    layer = {
      val <- get_input("gen_viz_layer", "none")
      if(is.null(val) || length(val) == 0 || val == "none") "" else val
    },
    ell = if(has_ell) ell else NULL,

    pch_val = {
      v <- get_input("gen_plot_pch", ".")
      if(v == ".") "." else as.numeric(v)
    },
    cex_val = get_input("gen_plot_cex", 0.3),
    bg_col = get_input("gen_plot_bg_col", "#B3B3B3"),

    show_occ = isTRUE(get_input("gen_show_occ", TRUE)),
    occ_col = get_input("gen_plot_occ_col", "#097a21"),
    occ_pch = as.numeric(get_input("gen_plot_occ_pch", "16")),
    occ_cex = get_input("gen_plot_occ_cex", 0.8),

    palette = get_input("gen_plot_palette", "heat"),

    show_ell = has_ell && isTRUE(get_input("gen_show_ell", TRUE)),
    ell_col = get_input("gen_plot_ell_col", "#000000"),
    ell_lwd = get_input("gen_plot_ell_lwd", 2),
    ell_lty = as.numeric(get_input("gen_plot_ell_lty", "1")),

    map_bg_col = get_input("gen_plot_map_bg_col", "#F0F0F0"),
    zoom_mode = get_input("gen_plot_zoom_mode", "auto"),
    asp_espace = get_input("gen_plot_asp_espace", "auto"),
    export_cex = get_input("gen_export_cex", 1)
  )
}

# Selector observers ------------------------------------------------------

# Selects the x and y based on the available variables, prevent form selection a
# 1:1
observeEvent({
  input$gen_plot_espace_state
  input$gen_plot_2d_x
  input$gen_plot_2d_y
  session_data$vars
}, {
  vars <- plot_vars()
  req(vars)
  req(input$gen_plot_espace_state == "gen_plot_2d")
  update_axis_selectors("gen_plot_2d_x", "gen_plot_2d_y", vars)
}, ignoreInit = FALSE)

observeEvent({
  input$gen_plot_combined_x
  input$gen_plot_combined_y
  session_data$vars
}, {
  vars <- plot_vars()
  req(vars)
  update_axis_selectors("gen_plot_combined_x", "gen_plot_combined_y", vars)
}, ignoreInit = FALSE)

# Outputs -----------------------------------------------------------------

# Checked
output$espace_top_options_ui_generate <- renderUI({

  vars <- session_data$vars
  req(vars)

  ell <- session_data$current_ellipsoid
  req(ell)

  # Get layer names from both prediction lists
  pred_result <- session_data$ellipsoid_prediction_list[[ell$ell_id]]
  if(is.null(pred_result) && !is.null(ell$ell_name))
    pred_result <- session_data$ellipsoid_prediction_list[[ell$ell_name]]

  bias_result <- session_data$ellipsoid_prediction_list_biased[[ell$ell_id]]
  if(is.null(bias_result) && !is.null(ell$ell_name))
    bias_result <- session_data$ellipsoid_prediction_list_biased[[ell$ell_name]]

  pred_lyrs <- if(!is.null(pred_result) && inherits(pred_result, "SpatRaster"))
    names(pred_result) else character(0)
  bias_lyrs <- if(!is.null(bias_result) && inherits(bias_result, "SpatRaster"))
    names(bias_result) else character(0)

  all_layers <- unique(c("none", pred_lyrs, bias_lyrs))

  fluidRow(
    column(width = 4,
           radioButtons("gen_plot_espace_state",
                        label = tags$span("Plot type:", class = "text-widget-title"),
                        choices = c("All pairs" = "gen_plot_pairs",
                                    "2D" = "gen_plot_2d"),
                        selected = "gen_plot_pairs",
                        inline = TRUE)),

    conditionalPanel(
      "input.gen_plot_espace_state == 'gen_plot_2d'",
      column(width = 4,
             selectInput("gen_plot_2d_x", label = NULL, choices = character(0))),
      column(width = 4,
             selectInput("gen_plot_2d_y", label = NULL, choices = character(0)))
    ),

    column(width = 4,
           selectInput("gen_viz_layer",
                       label = tags$span("Layer", class = "text-widget-title"),
                       choices = all_layers,
                       selected = "none"))
  )
})

# Checked
output$espace_generate <- renderPlot({

  vars <- session_data$vars
  req(vars)

  s <- collect_plot_settings_generate()

  req(s)

  state <- if(!is.null(input$gen_plot_espace_state)) input$gen_plot_espace_state else "gen_plot_pairs"

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  switch(state,
         "gen_plot_pairs" = draw_espace_pairs_generate(vars, s),
         "gen_plot_2d"= {
           req(input$gen_plot_2d_x, input$gen_plot_2d_y)
           par(mar = c(4, 4, 2, 1))
           draw_espace_panel_generate(input$gen_plot_2d_x, input$gen_plot_2d_y, s)
         }
  )

})

output$gspace_top_options_ui_generate <- renderUI({

  req(session_data$bg_raster)
  req(session_data$vars)

  vars <- session_data$vars
  has_ell <- !is.null(session_data$current_ellipsoid) &&
    length(session_data$ellipsoid_list) > 0

  if(has_ell){
    fluidRow(
      column(width = 4,
             radioButtons("gen_plot_gspace_state",
                          label = tags$span("Show:", class = "text-widget-title"),
                          choices = c("All variables" = "gen_plot_all",
                                      "One variable" = "gen_plot_one"),
                          selected = "gen_plot_all",
                          inline = TRUE)),
      conditionalPanel(
        "input.plot_gspace_state == 'plot_one'",
        column(width = 4,
               selectInput("gen_plot_gspace_lyr",
                           label = NULL,
                           choices = vars))
      )
    )
  } else {
    fluidRow(
      column(width = 4,
             radioButtons("gen_plot_gspace_state",
                          label = tags$span("Show:", class = "text-widget-title"),
                          choices = c("All layers" = "gen_plot_all",
                                      "One layer" = "gen_plot_one"),
                          selected = "gen_plot_one",
                          inline = TRUE)),
      conditionalPanel(
        "input.plot_gspace_state == 'plot_one'",
        column(width = 4,
               selectInput("gen_plot_gspace_lyr",
                           label = NULL,
                           choices = vars))
      )
    )
  }
})

output$gspace_generate <- renderPlot({

  vars <- plot_vars()
  req(vars)

  is_virtual <- identical(session_data$input_mode, "virtual_mode")
  if(is_virtual || is.null(session_data$bg_raster)){
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "", main = "G-space")
    text(0.5, 0.5, "Virtual mode on or no raster provided.\nG-space unavailable.",
         cex = 1, col = "grey50")
    return(invisible(NULL))
  }

  s <- collect_plot_settings_generate()
  rast <- session_data$bg_raster
  lyr <- if(!is.null(input$gen_plot_gspace_lyr)) input$gen_plot_gspace_lyr else vars[1]
  state <- if(!is.null(input$gen_plot_gspace_state)) input$gen_plot_gspace_state else "gen_plot_all"

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  has_ell <- s$has_ell && !is.null(s$ell$ranges)

  binarize_by_range <- function(v){
    r_min <- s$ell$ranges["min", v]
    r_max <- s$ell$ranges["max", v]
    binary <- terra::classify(rast[[v]],
                              rcl = matrix(c(-Inf, r_min, NA,
                                             r_min, r_max, 1,
                                             r_max, Inf, NA),
                                           ncol = 3, byrow = TRUE),
                              include.lowest = TRUE, right = FALSE)
    names(binary) <- v
    binary
  }

  if(has_ell){

    within_col <- c("#D3D3D3", "#E07B39")
    outside_col <- s$map_bg_col

    if(state == "gen_plot_all"){
      n_cols <- 2L
      n_rows <- ceiling(length(vars) / n_cols)
      par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 3))
      for(v in vars){
        draw_gspace_panel_generate(binarize_by_range(v), s,
                                   title = paste0(v, " (within range)"),
                                   col = c(outside_col, within_col))
      }
      if(length(vars) %% 2 != 0) plot.new()
    } else {
      par(mar = c(4, 4, 2, 4))
      draw_gspace_panel_generate(binarize_by_range(lyr), s,
                                 title = paste0(lyr, " (within range)"),
                                 col = c(outside_col, within_col))
    }

  } else {

    if(state == "gen_plot_all"){
      draw_gspace_all_generate(vars, s)
    } else {
      par(mar = c(4, 4, 2, 4))
      draw_gspace_panel_generate(rast[[lyr]], s, title = lyr)
    }

  }
})

output$combined_generate <- renderPlot({

  is_virtual <- identical(session_data$input_mode, "virtual_mode")
  if(is_virtual || is.null(session_data$bg_raster)){
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "", main = "Combined")
    text(0.5, 0.5, "Virtual mode on or raster not provided.\nG-space unavailable.",
         cex = 1, col = "grey50")
    return(invisible(NULL))
  }

  req(input$gen_plot_combined_x, input$gen_plot_combined_y)

  vars <- plot_vars()
  req(vars)

  s <- collect_plot_settings_generate()
  # s$cex_val <- s$cex_espace
  s$asp_espace <- s$asp_combined

  layout <- if(!is.null(input$gen_plot_combined_layout)) input$gen_plot_combined_layout else "col"
  mfrow <- if(layout == "col") c(2, 1) else c(1, 2)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = mfrow, mar = c(4, 4, 2, 1))

  draw_espace_panel_generate(input$gen_plot_combined_x, input$gen_plot_combined_y, s)

  # G-space: binary suitability if ellipsoid exists, else first raster layer
  gspace_rast <- if(s$show_suitable_gspace && s$has_ell){
    pred <- tryCatch(pred_raster_vis(), error = function(e) NULL)
    if(!is.null(pred)){
      terra::classify(pred[["suitability_trunc"]],
                      rcl = matrix(c(-Inf, 0, 0,
                                     0, Inf, 1),
                                   ncol = 3, byrow = TRUE),
                      include.lowest = TRUE)
    } else {
      session_data$bg_raster[[1]]
    }
  } else {
    session_data$bg_raster[[1]]
  }

  draw_gspace_panel_generate(gspace_rast, s,
                             title = "G-space",
                             col = if(s$show_suitable_gspace && s$has_ell)
                               c(s$unsuitable_col, s$suitable_col)
                             else NULL)
})

output$combined_options_ui_generate <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 4,
           radioButtons("gen_plot_combined_layout",
                        label = tags$span("Layout:", class = "text-widget-title"),
                        choices = c("Stacked" = "col", "Side by side" = "row"),
                        selected = "col",
                        inline = TRUE)),
    column(width = 4,
           selectInput("gen_plot_combined_x", label = NULL, choices = character(0))),
    column(width = 4,
           selectInput("gen_plot_combined_y", label = NULL, choices = character(0)))
  )
})

output$espace_bottom_options_ui_generate <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  fluidRow(
    column(width = 1,
           tags$span("Zoom:", class = "text-widget-title")),
    column(width = 4,
           radioButtons("gen_plot_zoom_mode", label = NULL,
                        choiceNames = list(
                          tags$span("Auto", class = "text-widget-inner"),
                          tags$span("Zoom in", class = "text-widget-inner")
                        ),
                        choiceValues = c("auto", "ellipsoid"),
                        selected = "auto",
                        inline = TRUE)),
    column(width = 3,
           tags$span("Aspect ratio:", class = "text-widget-title")),
    column(width = 4,
           radioButtons("gen_plot_asp_espace", label = NULL,
                        choiceNames = list(
                          tags$span("Auto", class = "text-widget-inner"),
                          tags$span("Fixed", class = "text-widget-inner")
                        ),
                        choiceValues = c("auto", "fixed"),
                        selected = "auto",
                        inline = TRUE))
  )
})

output$combined_bottom_options_ui_generate <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  fluidRow(
    column(width = 1,
           tags$span("Zoom:", class = "text-widget-title")),
    column(width = 5,
           radioButtons("gen_plot_zoom_mode", label = NULL,
                        choiceNames = list(
                          tags$span("Auto", class = "text-widget-inner"),
                          tags$span("Zoom in", class = "text-widget-inner")
                        ),
                        choiceValues = c("auto", "ellipsoid"),
                        selected = "auto",
                        inline = TRUE)),
    column(width = 2,
           tags$span("Aspect ratio:", class = "text-widget-title")),
    column(width = 4,
           radioButtons("gen_plot_asp_espace", label = NULL,
                        choiceNames = list(
                          tags$span("Auto", class = "text-widget-inner"),
                          tags$span("Fixed", class = "text-widget-inner")
                        ),
                        choiceValues = c("auto", "fixed"),
                        selected = "auto",
                        inline = TRUE))
  )
})

# Ellipsoid library, this shows all version of the base elliposid created
output$ellipsoid_info_generate <- renderUI({

  req(session_data$current_ellipsoid)

  ell <- session_data$current_ellipsoid
  base_ell <- session_data$ellipsoid_list[["base"]]
  vars <- ell$var_names
  n_vars <- length(vars)

  # Volume change relative to base
  vol_current <- ell$volume
  vol_base <- if(!is.null(base_ell) && !is.null(base_ell$volume)){
    base_ell$volume
  } else {
    vol_current
  }

  vol_pct <- if(!is.null(base_ell) && vol_base > 0){
    round((vol_current - vol_base) / vol_base * 100, 1)
  } else {
    0
  }
  vol_icon<- if(vol_pct > 0) icon("arrow-trend-up") else if(vol_pct < 0) icon("arrow-trend-down") else icon("minus")
  vol_color <- if(vol_pct > 0) "#097a21" else if(vol_pct < 0) "#e74c3c" else "#888"

  # Covariance pairs that differ from zero
  pairs <- t(combn(vars, 2))
  pair_names <- apply(pairs, 1, function(p) paste(p, collapse = " / "))
  cov_vals <- apply(pairs, 1, function(p){
    round(ell$cov_matrix[p[1], p[2]], 4)
  })

  nonzero <- cov_vals != 0

  # Covariance table rows
  cov_rows <- if(any(nonzero)){
    lapply(which(nonzero), function(i){
      val <- cov_vals[i]
      color <- if(val > 0) "#097a21" else "#e74c3c"
      icn <- if(val > 0) icon("arrow-up") else icon("arrow-down")
      tags$tr(
        tags$td(pair_names[i],
                style = "font-size: 12px; color: #666; padding: 3px 6px;"),
        tags$td(style = paste0("color:", color, "; padding: 3px 6px; font-size: 12px;"),
                icn, " ", format(val, nsmall = 4))
      )
    })
  } else {
    list(tags$tr(tags$td(
      colspan = "2",
      style = "font-size: 12px; color: #aaa; padding: 3px 6px;",
      "All covariances at zero (base ellipsoid)"
    )))
  }

  # Centroid rows
  centroid_rows <- lapply(vars, function(v){

    cur_val<- round(ell$centroid[v], 3)
    base_val <- if(!is.null(base_ell)) round(base_ell$centroid[v], 3) else cur_val
    delta <- round(cur_val - base_val, 3)
    color <- if(delta > 0) "#097a21" else if(delta < 0) "#e74c3c" else "#aaa"
    delta_str <- if(delta > 0) paste0("+", delta) else if(delta < 0) as.character(delta) else "no change"

    tags$tr(
      tags$td(v,
              style = "font-size: 12px; color: #666; padding: 3px 6px;"),
      tags$td(cur_val,
              style = "font-size: 12px; color: #555; padding: 3px 6px;"),
      tags$td(delta_str,
              style = paste0("color:", color, "; padding: 3px 6px; font-size: 12px;"))
    )
  })

  box(title = tagList(
    tags$span("Ellipsoid summary", class = "text-section-header"),
    tags$span(paste0(" — ", session_data$current_ellipsoid$ell_name),
              style = "font-size: 12px; color: #888; font-weight: 400; margin-left: 4px;")),
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,

    fluidRow(
      column(width = 4,
             tags$div(
               tags$span("Dimensions", class = "text-widget-title"),
               tags$p(paste0(n_vars, "D (", paste(vars, collapse = ", "), ")"),
                      style = "font-size: 12px; color: #555; margin: 2px 0 12px;"),

               tags$span("Confidence level", class = "text-widget-title"),
               tags$p(paste0(round(ell$cl * 100, 1), "%"),
                      style = "font-size: 12px; color: #555; margin: 2px 0 12px;"),

               tags$span("Volume", class = "text-widget-title"),
               tags$p(
                 tags$span(format(round(vol_current, 2), big.mark = ","),
                           style = "font-size: 12px; color: #555;"),
                 tags$span(
                   style = paste0("font-size: 11px; color:", vol_color,
                                  "; margin-left: 6px;"),
                   vol_icon, " ", abs(vol_pct), "% vs base"
                 ),
                 style = "margin: 2px 0 12px;"
               )
             )
      ),

      # Covariance summary
      column(width = 4,
             tags$span("Covariance adjustments", class = "text-widget-title"),
             tags$table(
               style = "width: 100%; margin-top: 4px;",
               tags$tbody(cov_rows)
             )
      ),

      # Centroid
      column(width = 4,
             tags$span("Centroid", class = "text-widget-title"),
             tags$table(
               style = "width: 100%; margin-top: 4px;",
               tags$tbody(centroid_rows)
             )
      )
    )
  )
})

# Advanced plot settings UI
output$plot_settings_ui_generate <- renderUI({

  has_ell <- !is.null(session_data$current_ellipsoid)
  has_raster <- !is.null(session_data$bg_raster)

  box(title = tagList(
    tags$span("Advanced Plot Settings", class = "text-section-header"),
    tags$span(icon("circle-info"),
              title = "Adjust how generated occurrences are displayed.",
              class = "tooltip-icon")),
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,

    # Background points
    fluidRow(
      column(width = 4,
             tags$span("Point shape (pch)", class = "text-widget-title"),
             selectInput("gen_plot_pch", label = NULL,
                         choices = c("Dot (.)" = ".",
                                     "Open circle" = "1",
                                     "Filled circle" = "16",
                                     "Square" = "15",
                                     "Triangle" = "17",
                                     "Cross" = "3"),
                         selected = ".")),
      column(width = 4,
             tags$span("Point size (cex)", class = "text-widget-title"),
             numericInput("gen_plot_cex", label = NULL, value = 0.3,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 4,
             tags$span("Background color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#B3B3B3",
                          oninput = "Shiny.setInputValue('gen_plot_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")))
    ),

    # Prediction palette
    fluidRow(
      column(width = 6,
             tags$span("Prediction palette", class = "text-widget-title"),
             selectInput("gen_plot_palette", label = NULL,
                         choices = c("Heat" = "heat",
                                     "Terrain" = "terrain",
                                     "Topo" = "topo",
                                     "Rainbow" = "rainbow",
                                     "Grayscale" = "grayscale"),
                         selected = "heat"))
    ),

    # Occurrence points
    fluidRow(
      column(width = 3,
             checkboxInput("gen_show_occ",
                           "Show occurrences", value = TRUE)),
      column(width = 3,
             tags$span("Occurrence shape (pch)", class = "text-widget-title"),
             selectInput("gen_plot_occ_pch", label = NULL,
                         choices = c("Filled circle" = "16",
                                     "Open circle" = "1",
                                     "Square" = "15",
                                     "Triangle" = "17",
                                     "Cross" = "3"),
                         selected = "16")),
      column(width = 3,
             tags$span("Occurrence size (cex)", class = "text-widget-title"),
             numericInput("gen_plot_occ_cex", label = NULL, value = 0.8,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 3,
             tags$span("Occurrence color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#097a21",
                          oninput = "Shiny.setInputValue('gen_plot_occ_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")))
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
                          oninput = "Shiny.setInputValue('gen_plot_ell_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;"))),
      column(width = 3,
             tags$span("Line width", class = "text-widget-title"),
             numericInput("gen_plot_ell_lwd", label = NULL, value = 2,
                          min = 0.5, max = 6, step = 0.5)),
      column(width = 3,
             tags$span("Line type", class = "text-widget-title"),
             selectInput("gen_plot_ell_lty", label = NULL,
                         choices = c("Solid" = "1",
                                     "Dashed" = "2",
                                     "Dotted" = "3"),
                         selected = "1"))
    ),

    if(has_raster) fluidRow(
      column(width = 4,
             tags$span("Map background color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#F0F0F0",
                          oninput = "Shiny.setInputValue('gen_plot_map_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")))
    ),

    fluidRow(
      column(width = 4,
             br(),
             actionButton("open_gen_export_modal",
                          tagList(icon("download"), "Export Figure"),
                          class = "btn-default"))
    )
  )
})

# Export Logic ------------------------------------------------------------

open_device_generate <- function(file, ext){

  unit <- if(!is.null( input$gen_export_unit)) input$gen_export_unit else "mm"
  w_val <- if(!is.null( input$gen_export_width_val)) input$gen_export_width_val else 166
  h_val <- if(!is.null( input$gen_export_height_val)) input$gen_export_height_val else 166
  res <- if(!is.null( input$gen_export_res)) input$gen_export_res else 300
  cex_val <- if(!is.null( input$gen_export_cex)) input$gen_export_cex else 1

  # Convert to px for png, inches for pdf/svg
  to_inches <- function(val){
    switch(unit,
           "mm" = val / 25.4,
           "in" = val,
           "px" = val / res)
  }

  to_px <- function(val){
    switch(unit,
           "mm" = round(val / 25.4 * res),
           "in" = round(val * res),
           "px" = round(val))
  }

  if(ext == "png"){
    png(file, width = to_px(w_val), height = to_px(h_val), res = res)
  } else if(ext == "pdf"){
    pdf(file, width = to_inches(w_val), height = to_inches(h_val))
  } else if(ext == "svg"){
    svg(file, width = to_inches(w_val), height = to_inches(h_val))
  }

  # Set all cex parameters so they are not overridden by individual plot calls
  par(cex = cex_val,
      cex.axis = cex_val,
      cex.lab = cex_val,
      cex.main = cex_val * 1.1,
      cex.sub = cex_val * 0.9)
}

output$export_settings_ui_generate <- renderUI({

  ext <- if(!is.null( input$gen_export_filetype)) input$gen_export_filetype else "png"

  unit <- if(!is.null( input$gen_export_unit)) input$gen_export_unit else "mm"

  defaults <- switch(unit,
                     "mm" = list(val = 166, min = 50, max = 500, step = 1),
                     "in" = list(val = 6.54, min = 1, max = 20, step = 0.1),
                     "px" = list(val = 1961, min = 400, max = 6000, step = 100)
  )

  if(ext == "png"){
    tagList(
      p(tagList(icon("circle-info"),
                " Default is a standard full-page publication figure (166 x 166 mm at 300 dpi)."),
        style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
      fluidRow(
        column(width = 4,
               tags$span(paste0("Width (", unit, ")"), class = "text-widget-title"),
               numericInput("gen_export_width_val", label = NULL,
                            value = defaults$val,
                            min = defaults$min,
                            max = defaults$max,
                            step = defaults$step)),

        column(width = 4,
               tags$span(paste0("Height (", unit, ")"), class = "text-widget-title"),
               numericInput("gen_export_height_val", label = NULL,
                            value = defaults$val,
                            min = defaults$min,
                            max = defaults$max,
                            step = defaults$step)),
        column(width = 4,
               tags$span("Resolution (dpi)", class = "text-widget-title"),
               numericInput("gen_export_res", label = NULL,
                            value = 300, min = 72, max = 600, step = 50))
      ),
      fluidRow(
        column(width = 4,
               tags$span("Text size (cex)", class = "text-widget-title"),
               numericInput("gen_export_cex", label = NULL,
                            value = 1, min = 0.5, max = 3, step = 0.1)),
        column(width = 8,
               br(),
               uiOutput("gen_export_cex_msg"))
      )
    )
  } else {
    tagList(
      p(tagList(icon("circle-info"),
                " Default is a standard full-page publication figure (166 x 166 mm)."),
        style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
      fluidRow(
        column(width = 4,
               tags$span("Width (mm)", class = "text-widget-title"),
               numericInput("gen_export_width_val", label = NULL,
                            value = 166, min = 50, max = 500, step = 1)),
        column(width = 4,
               tags$span("Height (mm)", class = "text-widget-title"),
               numericInput("gen_export_height_val", label = NULL,
                            value = 166, min = 50, max = 500, step = 1))
      ),
      fluidRow(
        column(width = 4,
               tags$span("Text size (cex)", class = "text-widget-title"),
               numericInput("gen_export_cex", label = NULL,
                            value = 1, min = 0.5, max = 3, step = 0.1)),
        column(width = 8,
               br(),
               uiOutput("gen_export_cex_msg"))
      ),
      p("PDF and SVG do not require a resolution setting.",
        class = "text-instruction")
    )
  }
})

observeEvent(input$open_gen_export_modal, {
  showModal(modalDialog(
    title = "Export Figure",
    size = "m",

    fluidRow(
      column(width = 6,
             tags$span("File type", class = "text-widget-title"),
             radioButtons("gen_export_filetype", label = NULL,
                          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                          selected = if(!is.null( input$gen_export_filetype))
                            input$gen_export_filetype else "png",
                          inline = TRUE)),
      column(width = 6,
             tags$span("Unit", class = "text-widget-title"),
             radioButtons("gen_export_unit", label = NULL,
                          choiceNames = list(
                            tags$span("mm", class = "text-widget-inner"),
                            tags$span("inches", class = "text-widget-inner"),
                            tags$span("px", class = "text-widget-inner")
                          ),
                          choiceValues = c("mm", "in", "px"),
                          selected = if(!is.null( input$gen_export_unit))
                            input$gen_export_unit else "mm",
                          inline = TRUE))
    ),

    uiOutput("export_settings_ui_generate"),

    footer = tagList(
      modalButton("Cancel"),
      downloadButton("confirm_export", "Export", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

output$confirm_export_generate <- downloadHandler(
  filename = function(){
    ext <- if(!is.null( input$gen_export_filetype)) input$gen_export_filetype else "png"
    tab <- if(!is.null(input$gen_plot_generate)) input$gen_plot_generate else "gen_tab_espace"
    prefix <- switch(tab,
                     "gen_tab_espace" = "espace_plot",
                     "gen_tab_gspace" = "gspace_plot",
                     "gen_tab_combined" = "combined_plot",
                     "espace_plot")
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)

  },
  content = function(file){
    ext <- if(!is.null( input$gen_export_filetype)) input$gen_export_filetype else "png"
    tab <- if(!is.null(input$gen_plot_generate)) input$gen_plot_generate else "gen_tab_espace"
    s <- collect_plot_settings_generate()
    req(s)

    open_device(file, ext)

    switch(tab,
           "gen_tab_espace" = {
             vars <- plot_vars()
             req(vars)
             state <- if(!is.null(input$gen_plot_espace_state)) input$gen_plot_espace_state else "gen_plot_pairs"
             switch(state,
                    "gen_plot_pairs" = draw_espace_pairs_generate(vars, s),
                    "gen_plot_2d" = {
                      req(input$gen_plot_2d_x, input$gen_plot_2d_y)
                      par(mar = c(4, 4, 2, 1))
                      draw_espace_panel_generate(input$gen_plot_2d_x, input$gen_plot_2d_y, s)
                    }
             )
           },
           "gen_tab_gspace" = {
             par(mar = c(4, 4, 2, 1))
             draw_gspace_panel_generate(s)
           },
           "gen_tab_combined" = {
             req(input$gen_plot_combined_x, input$gen_plot_combined_y)
             layout <- if(!is.null(input$gen_plot_combined_layout)) input$gen_plot_combined_layout else "col"
             mfrow <- if(layout == "col") c(2, 1) else c(1, 2)
             par(mfrow = mfrow, mar = c(4, 4, 2, 1))
             draw_espace_panel_generate(input$gen_plot_combined_x, input$gen_plot_combined_y, s)
             draw_gspace_panel_generate(s, title = "G-space")
           }
    )

    dev.off()
  }
)
