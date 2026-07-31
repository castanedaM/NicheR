# Title: Plot logic
# Description: Handle e-space, g-space, and combined plots
# Date last updated: 07/29/2026

# Functions -----------------------------------------------------------------

# Helper to avoid repeating the mutual-exclusion update logic in varibales.
update_axis_selectors_gen <- function(x_id, y_id, vars){
  x_sel <- input[[x_id]]
  y_sel <- input[[y_id]]

  if(is.null(x_sel) || !x_sel %in% vars) x_sel <- vars[1]

  y_choices <- setdiff(vars, x_sel)
  if(is.null(y_sel) || !y_sel %in% y_choices) y_sel <- y_choices[1]

  x_choices <- setdiff(vars, y_sel)

  updateSelectInput(session, x_id, choices = x_choices, selected = x_sel)
  updateSelectInput(session, y_id, choices = y_choices, selected = y_sel)
}

compute_lims_gen <- function(v1, v2, s){

  ell_valid <- s$has_ell &&
    !is.null(s$ell$cov_matrix) &&
    all(is.finite(s$ell$cov_matrix))

  if(s$zoom_mode == "ellipsoid" && ell_valid){
    idx <- match(c(v1, v2), s$ell$var_names)
    if(any(is.na(idx))) return(list(xlim = c(0, 1), ylim = c(0, 1), asp = NA))
    ell_pts <- ellipsoid_boundary_2d(s$ell, n_segments = 100, dim = idx)
    pad_x <- diff(range(ell_pts[, 1])) * 0.1
    pad_y <- diff(range(ell_pts[, 2])) * 0.1
    xlim <- range(ell_pts[, 1]) + c(-pad_x, pad_x)
    ylim <- range(ell_pts[, 2]) + c(-pad_y, pad_y)
    return(list(xlim = xlim, ylim = ylim, asp = diff(ylim) / diff(xlim)))
  }

  bg <- session_data$bg_df


  # All data frames keyed by v1/v2 so rbind works correctly
  pts_xy <- if(!is.null(bg)){
    bg[, c(v1, v2)]
  # }
  # else if(!is.null(ranges)){
  #   df <- data.frame(x = c(ranges$mins[[v1]], ranges$maxs[[v1]]),
  #                    y = c(ranges$mins[[v2]], ranges$maxs[[v2]]))
  #   names(df) <- c(v1, v2)
  #   df
  } else {
    df <- data.frame(x = c(0, 1), y = c(0, 1))
    names(df) <- c(v1, v2)
    df
  }


  if(ell_valid){
    idx <- match(c(v1, v2), s$ell$var_names)
    if(!any(is.na(idx))){
      ell_pts <- ellipsoid_boundary_2d(s$ell, n_segments = 100, dim = idx)
      all_pts <- if(!is.null(range_pts)) rbind(pts_xy, range_pts) else pts_xy
      lims <- safe_lims(all_pts, ell_pts)
      return(c(lims, list(asp = NA)))
    }
  }

  # No ellipsoid: expand bg limits to include range lines
  all_x <- c(pts_xy[, 1], if(!is.null(range_pts)) range_pts[[v1]])
  all_y <- c(pts_xy[, 2], if(!is.null(range_pts)) range_pts[[v2]])

  list(xlim = range(all_x, na.rm = TRUE),
       ylim = range(all_y, na.rm = TRUE),
       asp = NA)
}

draw_espace_panel_gen <- function(v1, v2, s){

  lims <- compute_lims(v1, v2, s)
  bg <- session_data$bg_df

  if(!is.null(bg)){
    plot(bg[[v1]], bg[[v2]],
         col = s$bg_col,
         pch = s$pch_val,
         cex = s$cex_val,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(!is.null(s$asp_espace) && s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2))
  } else {
    plot(NA, NA,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = if(!is.null(s$asp_espace) && s$asp_espace == "fixed") lims$asp else NA,
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2))
  }

  # Predicted area overlay
  # if(s$show_suitable_espace && !is.null(bg) && s$has_ell){
  #   pred_df <- tryCatch(
  #     predict(s$ell,
  #             newdata = bg,
  #             include_suitability = TRUE,
  #             include_mahalanobis = FALSE,
  #             suitability_truncated = TRUE,
  #             verbose = FALSE),
  #     error = function(e) NULL
  #   )
  #
  #   if(!is.null(pred_df)){
  #     suitable <- pred_df[!is.na(pred_df$suitability_trunc) &
  #                           pred_df$suitability_trunc > 0, ]
  #     if(nrow(suitable) > 0){
  #       points(suitable[[v1]], suitable[[v2]],
  #              col = s$suitable_col,
  #              pch = s$suitable_pch,
  #              cex = s$suitable_cex)
  #     }
  #   }
  # }


  if(s$show_ell){
    idx <- match(c(v1, v2), s$ell$var_names)
    add_ellipsoid(s$ell, dim = idx,
                  col_ell = s$ell_col,
                  lwd = s$ell_lwd,
                  lty = s$ell_lty)
  }

  if(s$show_centroid && !is.null(s$ell)){
    idx <- match(c(v1, v2), s$ell$var_names)
    c_pos <- if(!is.null(s$centroid_preview_val)){
      s$centroid_preview_val
    } else {
      s$ell$centroid
    }
    points(c_pos[idx[1]], c_pos[idx[2]],
           pch = s$centroid_pch, col = s$centroid_col, cex = s$centroid_cex)
  }
}

draw_gspace_panel_gen <- function(s, title = "G-space"){

  rast <- session_data$bg_raster

  map_bg_col <- s$map_bg_col
  cex_val <- if(!is.null(s$export_cex)) s$export_cex else 1
  is_virtual <- identical(session_data$input_mode, "virtual_mode")

  if(is_virtual){
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "", main = title)
    text(0.5, 0.5, "Virtual mode on\nG-space unavailable.",
         cex = cex_val, col = "grey50")
    return(invisible(NULL))
  }

  if(is.null(rast)){
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "", main = title)
    text(0.5, 0.5, "No raster data available.", cex = cex_val, col = "grey50")
    return(invisible(NULL))
  }

  plot_rast <- function(r, ttl){
    par(cex.axis = cex_val,
        cex.lab = cex_val,
        cex.main = cex_val * 1.1)
    terra::plot(r,
                main = ttl,
                colNA = map_bg_col,
                axes = TRUE,
                xlab = "Longitude",
                ylab = "Latitude")
  }

  if(!s$has_ell || !s$show_suitable_gspace){
    plot_rast(rast[[1]], title)
    return(invisible(NULL))
  }

  pred <- tryCatch(pred_raster_vis(), error = function(e) NULL)

  if(!is.null(pred)){
    binary <- terra::classify(pred[["suitability_trunc"]],
                              rcl = matrix(c(-Inf, 0, 0,
                                             0, Inf, 1),
                                           ncol = 3, byrow = TRUE),
                              include.lowest = TRUE)
    plot_rast(binary, title)
  } else {
    plot_rast(rast[[1]], title)
  }
}

draw_espace_pairs_gen <- function(vars, s){
  pairs <- t(combn(seq_along(vars), 2))
  n_pairs <- nrow(pairs)
  n_cols <- ceiling(sqrt(n_pairs))
  n_rows <- ceiling(n_pairs / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
  for(i in seq_len(n_pairs)) draw_espace_panel(vars[pairs[i, 1]], vars[pairs[i, 2]], s)
}

# Called at the top of every draw function.
# Returns a plain list so drawing functions are pure and testable.
collect_plot_settings_gen <- function(){

  has_ell <- !is.null(session_data$current_ellipsoid)

  list(
    has_ell = has_ell,
    ell = if(has_ell) session_data$current_ellipsoid else NULL,

    show_ell = has_ell && get_input("show_ellipsoid",TRUE),
    show_centroid = has_ell && get_input("show_centroid", TRUE),

    pch_val = {
      v <- get_input("plot_pch", ".")
      if(v == ".") "." else as.numeric(v)
    },

    cex_val = get_input("plot_cex", 0.3),
    bg_col = get_input("plot_bg_col", "#B3B3B3"),

    suitable_pch = as.numeric(get_input("plot_suitable_pch", "16")),
    suitable_cex = get_input("plot_suitable_cex", 0.3),
    suitable_col = get_input("plot_suitable_col", "#097a21"),
    unsuitable_col = get_input("plot_unsuitable_col", "#D3D3D3"),
    map_bg_col = get_input("plot_map_bg_col", "#F0F0F0"),

    show_lines = {
      show <- get_input("show_range_lines", TRUE)
      list(active = show && !is.null(ranges), ranges = ranges)
    },

    xline_col = get_input("plot_xline_col","#E10000"),
    yline_col = get_input("plot_yline_col","#0004D5"),
    line_lwd = get_input("plot_line_lwd", 2),

    ell_col = get_input("plot_ell_col", "#000000"),
    ell_lwd = get_input("plot_ell_lwd", 2),
    ell_lty = as.numeric(get_input("plot_ell_lty", "1")),

    centroid_pch = as.numeric(get_input("plot_centroid_pch", "8")),
    centroid_col = get_input("plot_centroid_col", "#000000"),
    centroid_cex = get_input("plot_centroid_cex", 1.5),

    zoom_mode = get_input("plot_zoom_mode","auto"),

    asp_espace = get_input("plot_asp_espace", "auto"),
    asp_combined = get_input("plot_asp_combined", "auto"),
    export_cex = get_input("export_cex", 1),

    centroid_preview_val = tryCatch(
      withCallingHandlers(
        centroid_preview(),
        shiny.silent.error = function(e) invokeRestart("muffleWarning")
      ),
      error = function(e) NULL
    )
  )
}

# Selector observers ------------------------------------------------------

# Selects the x and y based on the avialbel variables, prevent form selection a
# 1:1
observeEvent({
  input$plot_espace_state
  input$plot_2d_x
  input$plot_2d_y
  session_data$vars
}, {
  vars <- plot_vars()
  req(vars)
  req(input$plot_espace_state == "plot_2d")
  update_axis_selectors("plot_2d_x", "plot_2d_y", vars)
}, ignoreInit = FALSE)

observeEvent({
  input$plot_combined_x
  input$plot_combined_y
  session_data$vars
}, {
  vars <- plot_vars()
  req(vars)
  update_axis_selectors("plot_combined_x", "plot_combined_y", vars)
}, ignoreInit = FALSE)



# Outputs -----------------------------------------------------------------

output$espace_options_ui_generate <- renderUI({

  vars <- session_data$vars
  req(vars)
  ell <- session_data$current_elliposid

  layers <- c(names(session_data$ellipsoid_prediction_list[[ell$ell_id]]),
              names(session_data$ellipsoid_prediction_list_bias[[ell$ell_id]]))

  fluidRow(
    column(width = 12,
           column(width = 4,
                  radioButtons("plot_espace_state",
                               label = tags$span("Plot type:",
                                                 class = "text-widget-title"),
                               choices = c("All pairs" = "plot_pairs",
                                           "2D" = "plot_2d"),
                               selected = "plot_pairs",
                               inline  = TRUE)),

           conditionalPanel(
             "input.plot_espace_state == 'plot_2d'",
             column(width = 4,
                    selectInput("plot_2d_x",
                                label = NULL,
                                choices = character(0))),
             column(width = 4,
                    selectInput("plot_2d_y",
                                label = NULL,
                                choices = character(0)))
           ),

           column(width = 4,
                  if(length(layers) > 0){
                    selectInput("plot_viz_layer", label = NULL,
                                choices = layers,
                                selected = layers[1])
                  })
    )
  )
})

output$combined_options_ui_generate <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 3,
           radioButtons("plot_combined_layout",
                        label = tags$span("Layout:", class = "text-widget-title"),
                        choices = c("Stacked" = "col", "Side by side" = "row"),
                        selected = "col",
                        inline = TRUE)),
    column(width = 3,
           selectInput("plot_combined_x", label = NULL, choices = character(0))),
    column(width = 3,
           selectInput("plot_combined_y", label = NULL, choices = character(0)))
  )

})

output$build_espace_plot <- renderPlot({

  vars <- session_data$vars
  req(vars)

  s <- collect_plot_settings()

  req(s)
  s$cex_val <- s$cex_espace

  state <- if(!is.null(input$plot_espace_state)) input$plot_espace_state else "plot_pairs"

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  switch(state,
         "plot_pairs" = draw_espace_pairs(vars, s),
         "plot_2d"= {
           req(input$plot_2d_x, input$plot_2d_y)
           par(mar = c(4, 4, 2, 1))
           draw_espace_panel(input$plot_2d_x, input$plot_2d_y, s)
         }
  )

})

output$build_gspace_plot <- renderPlot({
  vars <- plot_vars()
  req(vars)

  s <- collect_plot_settings()
  req(s)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(4, 4, 2, 1))

  draw_gspace_panel(s)

})

output$gen_combined_plot <- renderPlot({

  req(input$plot_combined_x, input$plot_combined_y)

  vars <- plot_vars()
  req(vars)

  s <- collect_plot_settings()
  req(s)

  s$cex_val <- s$cex_espace
  s$asp_espace <- s$asp_combined

  layout <- if(!is.null(input$plot_combined_layout)) input$plot_combined_layout else "col"
  mfrow<- if(layout == "col") c(2, 1) else c(1, 2)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = mfrow, mar = c(4, 4, 2, 1))

  draw_espace_panel(input$plot_combined_x, input$plot_combined_y, s)
  draw_gspace_panel(s, title = "G-space")

})


output$plot_espace_bottom_options_ui <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  fluidRow(
    column(width = 1,
           tags$span("Zoom:", class = "text-widget-title")),
    column(width = 4,
           radioButtons("plot_zoom_mode", label = NULL,
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
           radioButtons("plot_asp_espace", label = NULL,
                        choiceNames = list(
                          tags$span("Auto", class = "text-widget-inner"),
                          tags$span("Fixed", class = "text-widget-inner")
                        ),
                        choiceValues = c("auto", "fixed"),
                        selected = "auto",
                        inline = TRUE))
  )
})

output$plot_espace_bottom_options_ui_combined <- renderUI({
  req(length(session_data$ellipsoid_list) > 0)

  fluidRow(
    column(width = 1,
           tags$span("Zoom:", class = "text-widget-title")),
    column(width = 5,
           radioButtons("plot_zoom_mode", label = NULL,
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
           radioButtons("plot_asp_espace", label = NULL,
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
output$ellipsoid_info <- renderUI({

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
output$plot_settings_ui <- renderUI({

  has_ell <- !is.null(session_data$current_ellipsoid)
  has_raster <- !is.null(session_data$bg_raster)

  box(title = tagList(
    tags$span("Advanced Plot Settings", class = "text-section-header"),
    tags$span(icon("circle-info"),
              title = instructions$plot_settings,
              class = "tooltip-icon")),
    width= 12,
    collapsible = TRUE,
    collapsed = TRUE,

    # Background points
    fluidRow(
      column(width = 4,
             tags$span("Point shape (pch)", class = "text-widget-title"),
             selectInput("plot_pch", label = NULL,
                         choices= c("Dot (.)"= ".",
                                    "Open circle" = "1",
                                    "Filled circle" = "16",
                                    "Square"= "15",
                                    "Triangle"= "17",
                                    "Cross" = "3"),
                         selected = ".")),
      column(width = 4,
             tags$span("Point size (cex)", class = "text-widget-title"),
             numericInput("plot_cex", label = NULL, value = 0.3,
                          min = 0.1, max = 5, step = 0.1)),
      column(width = 4,
             tags$span("Background point color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#B3B3B3",
                          oninput = "Shiny.setInputValue('plot_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
             )
      )
    ),

    # Range lines
    conditionalPanel(
      condition = "input.range_method_choice != null && input.range_method_choice != '' || output.ellipsoid_exists",
      fluidRow(
        column(width = 3,
               checkboxInput("show_range_lines", "Show range lines", value = TRUE)),
        column(width = 3,
               tags$span("X-line color", class = "text-widget-title"),
               tags$div(style = "display: flex; align-items: center; gap: 8px;",
                        tags$input(type = "color",
                                   value = "#E10000",
                                   oninput = "Shiny.setInputValue('plot_xline_col', this.value)",
                                   style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;"))
        ),
        column(width = 3,
               tags$span("Y-line color", class = "text-widget-title"),
               tags$div(
                 style = "display: flex; align-items: center; gap: 8px;",
                 tags$input(type = "color",
                            value = "#0004D5",
                            oninput = "Shiny.setInputValue('plot_yline_col', this.value)",
                            style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
               )
        ),
        column(width = 3,
               tags$span("Line width", class = "text-widget-title"),
               numericInput("plot_line_lwd", label = NULL, value = 2,
                            min = 0.5, max = 6, step = 0.5))
      )
    ),

    # Ellipsoid, centroid, suitable area (only when ellipsoid built)
    if(has_ell) tagList(
      fluidRow(
        column(width = 3,
               checkboxInput("show_ellipsoid", "Show ellipsoid", value = TRUE)),
        column(width = 3,
               tags$span("Ellipsoid color", class = "text-widget-title"),
               tags$div(
                 style = "display: flex; align-items: center; gap: 8px;",
                 tags$input(type = "color",
                            value = "#000000",
                            oninput = "Shiny.setInputValue('plot_ell_col', this.value)",
                            style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
               )
        ),
        column(width = 3,
               tags$span("Ellipsoid line width", class = "text-widget-title"),
               numericInput("plot_ell_lwd", label = NULL, value = 2,
                            min = 0.5, max = 6, step = 0.5)),
        column(width = 3,
               tags$span("Ellipsoid line type", class = "text-widget-title"),
               selectInput("plot_ell_lty", label = NULL,
                           choices= c("Solid"= "1",
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
                           choices = c("Cross (X)"= "4",
                                       "Star" = "8",
                                       "Filled diamond" = "18",
                                       "Filled circle"= "16"),
                           selected = "8")),
        column(width = 3,
               tags$span("Centroid color", class = "text-widget-title"),
               tags$div(
                 style = "display: flex; align-items: center; gap: 8px;",
                 tags$input(type = "color",
                            value = "#000000",
                            oninput = "Shiny.setInputValue('plot_centroid_col', this.value)",
                            style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
               )
        ),
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
               checkboxInput("show_suitable_gspace",
                             "Show suitable area (G-space)", value = TRUE)),
        column(width = 3,
               tags$span("Suitable area color", class = "text-widget-title"),
               tags$div(
                 style = "display: flex; align-items: center; gap: 8px;",
                 tags$input(type = "color",
                            value = "#097a21",
                            oninput = "Shiny.setInputValue('plot_suitable_col', this.value)",
                            style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
               )

        ),
        column(width = 3,
               tags$span("Unsuitable area color", class = "text-widget-title"),
               tags$div(
                 style = "display: flex; align-items: center; gap: 8px;",
                 tags$input(type = "color",
                            value = "#D3D3D3",
                            oninput = "Shiny.setInputValue('plot_unsuitable_col', this.value)",
                            style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
               )
        )
      ),

      fluidRow(
        column(width = 3,
               tags$span("Suitable point shape (pch)", class = "text-widget-title"),
               selectInput("plot_suitable_pch", label = NULL,
                           choices= c("Dot (.)"= ".",
                                      "Open circle" = "1",
                                      "Filled circle" = "16",
                                      "Square"= "15",
                                      "Triangle"= "17",
                                      "Cross" = "3"),
                           selected = "16")),
        column(width = 3,
               tags$span("Suitable point size (cex)", class = "text-widget-title"),
               numericInput("plot_suitable_cex", label = NULL, value = 0.3,
                            min = 0.1, max = 5, step = 0.1))
      )
    ),

    # Map background color
    if(has_raster) fluidRow(
      column(width = 4,
             tags$span("Map background color", class = "text-widget-title"),
             tags$div(
               style = "display: flex; align-items: center; gap: 8px;",
               tags$input(type = "color",
                          value = "#F0F0F0",
                          oninput = "Shiny.setInputValue('plot_map_bg_col', this.value)",
                          style = "width: 40px; height: 32px; padding: 2px;
 border: 1px solid #ccc; border-radius: 4px;
 cursor: pointer;")
             )
      )
    ),

    # Export button and settings sit below the tabBox, outside all panels
    fluidRow(
      column(width = 4,
             br(),
             actionButton("open_export_modal",
                          tagList(icon("download"), "Export Figure"),
                          class = "btn-default"))
    )
  )
})


# Export Logic ------------------------------------------------------------

open_device <- function(file, ext){

  unit <- if(!is.null(input$export_unit)) input$export_unit else "mm"
  w_val <- if(!is.null(input$export_width_val)) input$export_width_val else 166
  h_val <- if(!is.null(input$export_height_val)) input$export_height_val else 166
  res <- if(!is.null(input$export_res)) input$export_res else 300
  cex_val <- if(!is.null(input$export_cex)) input$export_cex else 1

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

output$export_settings_ui <- renderUI({

  ext <- if(!is.null(input$export_filetype)) input$export_filetype else "png"

  unit <- if(!is.null(input$export_unit)) input$export_unit else "mm"

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
               numericInput("export_width_val", label = NULL,
                            value = defaults$val,
                            min = defaults$min,
                            max = defaults$max,
                            step = defaults$step)),

        column(width = 4,
               tags$span(paste0("Height (", unit, ")"), class = "text-widget-title"),
               numericInput("export_height_val", label = NULL,
                            value = defaults$val,
                            min = defaults$min,
                            max = defaults$max,
                            step = defaults$step)),
        column(width = 4,
               tags$span("Resolution (dpi)", class = "text-widget-title"),
               numericInput("export_res", label = NULL,
                            value = 300, min = 72, max = 600, step = 50))
      ),
      fluidRow(
        column(width = 4,
               tags$span("Text size (cex)", class = "text-widget-title"),
               numericInput("export_cex", label = NULL,
                            value = 1, min = 0.5, max = 3, step = 0.1)),
        column(width = 8,
               br(),
               uiOutput("export_cex_msg"))
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
               numericInput("export_width_val", label = NULL,
                            value = 166, min = 50, max = 500, step = 1)),
        column(width = 4,
               tags$span("Height (mm)", class = "text-widget-title"),
               numericInput("export_height_val", label = NULL,
                            value = 166, min = 50, max = 500, step = 1))
      ),
      fluidRow(
        column(width = 4,
               tags$span("Text size (cex)", class = "text-widget-title"),
               numericInput("export_cex", label = NULL,
                            value = 1, min = 0.5, max = 3, step = 0.1)),
        column(width = 8,
               br(),
               uiOutput("export_cex_msg"))
      ),
      p("PDF and SVG do not require a resolution setting.",
        class = "text-instruction")
    )
  }
})

observeEvent(input$open_export_modal, {
  showModal(modalDialog(
    title = "Export Figure",
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
      downloadButton("confirm_export", "Export", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

output$confirm_export <- downloadHandler(

  filename = function(){
    ext <- if(!is.null(input$export_filetype)) input$export_filetype else "png"
    tab <- if(!is.null(input$plot_build)) input$plot_build else "tab_espace"
    prefix <- switch(tab,
                     "tab_espace" = "espace_plot",
                     "tab_gspace" = "gspace_plot",
                     "tab_combined" = "combined_plot",
                     "espace_plot")
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)

  },
  content = function(file){
    ext <- if(!is.null(input$export_filetype)) input$export_filetype else "png"
    tab <- if(!is.null(input$plot_build)) input$plot_build else "tab_espace"
    s <- collect_plot_settings()
    req(s)

    open_device(file, ext)

    switch(tab,
           "tab_espace" = {
             vars <- plot_vars()
             req(vars)
             state <- if(!is.null(input$plot_espace_state)) input$plot_espace_state else "plot_pairs"
             switch(state,
                    "plot_pairs" = draw_espace_pairs(vars, s),
                    "plot_2d" = {
                      req(input$plot_2d_x, input$plot_2d_y)
                      par(mar = c(4, 4, 2, 1))
                      draw_espace_panel(input$plot_2d_x, input$plot_2d_y, s)
                    }
             )
           },
           "tab_gspace" = {
             par(mar = c(4, 4, 2, 1))
             draw_gspace_panel(s)
           },
           "tab_combined" = {
             req(input$plot_combined_x, input$plot_combined_y)
             layout <- if(!is.null(input$plot_combined_layout)) input$plot_combined_layout else "col"
             mfrow <- if(layout == "col") c(2, 1) else c(1, 2)
             par(mfrow = mfrow, mar = c(4, 4, 2, 1))
             draw_espace_panel(input$plot_combined_x, input$plot_combined_y, s)
             draw_gspace_panel(s, title = "G-space")
           }
    )

    dev.off()
  }
)
