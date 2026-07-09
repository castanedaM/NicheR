# Title: Plot logic
# Description: Handle e-space, g-space, and combined plots
# Date last updated: 07/09/2026

# Functions -----------------------------------------------------------------

# Read a single plot input with a NULL-safe default.
# Keeps collect_plot_settings() concise without repeating the pattern.
get_input <- function(id, default){
  if(!is.null(input[[id]])){
    input[[id]]
  } else {
    default
  }
}

# Called at the top of every draw function.
# Returns a plain list so drawing functions are pure and testable.
collect_plot_settings <- function(){
  ranges <- tryCatch(
    withCallingHandlers(
      range_preview(),
      shiny.silent.error = function(e) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )

  if(is.null(ranges)){
    # Prefer base ellipsoid ranges for range lines since they represent
    # the original niche definition and should not move with the centroid
    base_ell <- session_data$ellipsoid_list[["base"]]
    ref_ell<- if(!is.null(base_ell)) base_ell else session_data$current_ellipsoid

    if(!is.null(ref_ell) &&
       !is.null(ref_ell$ranges) &&
       !is.null(colnames(ref_ell$ranges)) &&
       length(colnames(ref_ell$ranges)) > 0){
      ranges <- list(mins = as.list(ref_ell$ranges[1, ]),
                     maxs = as.list(ref_ell$ranges[2, ]))
    }
  }

  has_ell <- !is.null(session_data$current_ellipsoid)

  list(
    has_ell = has_ell,
    ell = if(has_ell) session_data$current_ellipsoid else NULL,

    show_ell = has_ell && get_input("show_ellipsoid",TRUE),
    show_centroid = has_ell && get_input("show_centroid", TRUE),
    show_suitable_espace = has_ell && get_input("show_suitable_espace", TRUE),
    show_suitable_gspace = has_ell && get_input("show_suitable_gspace", TRUE),

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

    centroid_preview_val = tryCatch(
      withCallingHandlers(
        centroid_preview(),
        shiny.silent.error = function(e) invokeRestart("muffleWarning")
      ),
      error = function(e) NULL
    )
  )
}

# Helper to avoid repeating the mutual-exclusion update logic in varibales.
update_axis_selectors <- function(x_id, y_id, vars){
  x_sel <- input[[x_id]]
  y_sel <- input[[y_id]]

  if(is.null(x_sel) || !x_sel %in% vars) x_sel <- vars[1]

  y_choices <- setdiff(vars, x_sel)
  if(is.null(y_sel) || !y_sel %in% y_choices) y_sel <- y_choices[1]

  x_choices <- setdiff(vars, y_sel)

  updateSelectInput(session, x_id, choices = x_choices, selected = x_sel)
  updateSelectInput(session, y_id, choices = y_choices, selected = y_sel)
}

compute_lims <- function(v1, v2, s){

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
  ranges <- s$show_lines$ranges

  pts_xy <- if(!is.null(bg)){
    bg[, c(v1, v2)]
  } else if(!is.null(ranges)){
    data.frame(x = c(ranges$mins[[v1]], ranges$maxs[[v1]]),
               y = c(ranges$mins[[v2]], ranges$maxs[[v2]]))
  } else {
    data.frame(x = c(0, 1), y = c(0, 1))
  }

  if(ell_valid){
    idx <- match(c(v1, v2), s$ell$var_names)
    if(!any(is.na(idx))){
      ell_pts <- ellipsoid_boundary_2d(s$ell, n_segments = 100, dim = idx)
      lims<- safe_lims(pts_xy, ell_pts)
      return(c(lims, list(asp = NA)))
    }
  }

  xlim <- range(pts_xy[, 1], na.rm = TRUE) + c(-sd(pts_xy[, 1], na.rm = TRUE),
                                               sd(pts_xy[, 1], na.rm = TRUE))

  ylim <- range(pts_xy[, 2], na.rm = TRUE) + c(-sd(pts_xy[, 2], na.rm = TRUE),
                                               sd(pts_xy[, 2], na.rm = TRUE))

  list(xlim = xlim, ylim = ylim, asp = NA)
}

draw_espace_panel <- function(v1, v2, s){

  lims <- compute_lims(v1, v2, s)
  bg <- session_data$bg_df

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
         main = paste(v1, "vs.", v2))
  } else {
    plot(NA, NA,
         xlim = lims$xlim,
         ylim = lims$ylim,
         asp = lims$asp,
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2))
  }

  # Suitable area overlay using predict() on bg_df
  if(s$show_suitable_espace && !is.null(bg) && s$has_ell){
    pred_df <- tryCatch(
      predict(s$ell,
              newdata = bg,
              include_suitability = TRUE,
              include_mahalanobis = FALSE,
              suitability_truncated = TRUE,
              verbose = FALSE),
      error = function(e) NULL
    )

    if(!is.null(pred_df)){
      suitable <- pred_df[!is.na(pred_df$suitability_trunc) &
                            pred_df$suitability_trunc > 0, ]
      if(nrow(suitable) > 0){
        points(suitable[[v1]], suitable[[v2]],
               col = s$suitable_col,
               pch = s$suitable_pch,
               cex = s$suitable_cex)
      }
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

draw_gspace_panel <- function(s, title = "G-space"){

  rast <- session_data$bg_raster
  pred <- if(s$show_suitable_gspace) tryCatch(pred_raster_vis(), error = function(e) NULL) else NULL
  suitable_col <- s$suitable_col
  unsuitable_col <- s$unsuitable_col
  map_bg_col <- s$map_bg_col

  if(!is.null(pred)){

    binary <- terra::classify(pred[["suitability_trunc"]],
                              rcl = matrix(c(-Inf, 0, 0,
                                             0, Inf, 1),
                                           ncol = 3, byrow = TRUE),
                              include.lowest = TRUE)

    terra::plot(binary,
                col = c(unsuitable_col, suitable_col),
                legend = FALSE,
                axes = TRUE,
                main = title,
                xlab = "Longitude", ylab = "Latitude",
                colNA = map_bg_col)

    terra::add_legend(x = "topright",
                      legend = c("Suitable", "Unsuitable"),
                      fill = c(suitable_col, unsuitable_col),
                      bty = "n",
                      cex = 0.8)

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

draw_espace_pairs <- function(vars, s){
  pairs <- t(combn(seq_along(vars), 2))
  n_pairs <- nrow(pairs)
  n_cols <- ceiling(sqrt(n_pairs))
  n_rows <- ceiling(n_pairs / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
  for(i in seq_len(n_pairs)) draw_espace_panel(vars[pairs[i, 1]], vars[pairs[i, 2]], s)
}

open_device <- function(file, ext){
  if(ext == "png"){
    w <- if(!is.null(input$export_width_px))input$export_width_px else 1400
    h <- if(!is.null(input$export_height_px)) input$export_height_px else 1000
    res <- if(!is.null(input$export_res))input$export_res else 150
    png(file, width = w, height = h, res = res)
  } else if(ext == "pdf"){
    w <- if(!is.null(input$export_width_in))input$export_width_in else 10
    h <- if(!is.null(input$export_height_in)) input$export_height_in else 7
    pdf(file, width = w, height = h)
  } else if(ext == "svg"){
    w <- if(!is.null(input$export_width_in))input$export_width_in else 10
    h <- if(!is.null(input$export_height_in)) input$export_height_in else 7
    svg(file, width = w, height = h)
  }
}

# Reactives ---------------------------------------------------------------

# Prediction raster for G-space and Combined tabs only.
# E-space uses predict() on bg_df directly inside draw_espace_panel().
pred_raster_vis <- reactive({

  req(session_data$current_ellipsoid)
  req(session_data$bg_raster)

  ell <- session_data$current_ellipsoid
  vars <- ell$var_names

  tryCatch(
    predict(ell,
            newdata = session_data$bg_raster[[vars]],
            include_suitability = TRUE,
            include_mahalanobis = FALSE,
            suitability_truncated = TRUE,
            verbose = FALSE),
    error = function(e) NULL
  )

})


# Variables to plot, reactive to live selections before confirm.
# Before vars are confirmed: first 6 non-spatial columns from bg data.
# After confirm: session_data$vars.
plot_vars <- reactive({

  if(!is.null(session_data$vars)){
    return(session_data$vars)
  }

  if(identical(session_data$input_mode, "virtual")) return(NULL)

  all_vars <- if(!is.null(session_data$bg_raster)){
    names(session_data$bg_raster)
  } else if(!is.null(session_data$bg_df)){
    colnames(session_data$bg_df)
  } else {
    return(NULL)
  }

  all_vars <- all_vars[!grepl(SPATIAL_COL_PATTERN, all_vars, ignore.case = TRUE)]

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


# Observers ---------------------------------------------------------------

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

output$plot_espace_options_ui <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 12,
           radioButtons("plot_espace_state",
                        label= tags$span("Plot type:", class = "text-widget-title"),
                        choices= c("All pairs" = "plot_pairs",
                                   "2D"= "plot_2d"),
                        selected = "plot_pairs",
                        inline = TRUE),

           conditionalPanel(
             "input.plot_espace_state == 'plot_2d'",
             column(width = 6,
                    selectInput("plot_2d_x", label = NULL, choices = character(0))),
             column(width = 6,
                    selectInput("plot_2d_y", label = NULL, choices = character(0)))
           )
    )
  )
})

output$plot_combined_options_ui <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 6,
           selectInput("plot_combined_x", label = NULL, choices = character(0))),
    column(width = 6,
           selectInput("plot_combined_y", label = NULL, choices = character(0)))
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
                           choices= c("Cross (X)"= "4",
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

    # Zoom
    fluidRow(
      column(width = 4,
             tags$span("Zoom", class = "text-widget-title"),
             radioButtons("plot_zoom_mode", label = NULL,
                          choices= c("Auto"= "auto",
                                     "Zoom to ellipsoid" = "ellipsoid"),
                          selected = "auto"))
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

output$build_espace_plot <- renderPlot({

  vars <- plot_vars()
  req(vars)

  s <- collect_plot_settings()

  req(s)

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

output$build_combined_plot <- renderPlot({

  req(input$plot_combined_x, input$plot_combined_y)

  vars <- plot_vars()
  req(vars)

  s <- collect_plot_settings()
  req(s)

  layout <- if(!is.null(input$plot_combined_layout)) input$plot_combined_layout else "col"
  mfrow<- if(layout == "col") c(2, 1) else c(1, 2)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = mfrow, mar = c(4, 4, 2, 1))

  draw_espace_panel(input$plot_combined_x, input$plot_combined_y, s)
  draw_gspace_panel(s, title = "G-space")

})

output$plot_combined_options_ui <- renderUI({

  vars <- plot_vars()
  req(vars)

  fluidRow(
    column(width = 4,
           radioButtons("plot_combined_layout",
                        label = tags$span("Layout:", class = "text-widget-title"),
                        choices = c("Stacked" = "col", "Side by side" = "row"),
                        selected = "col",
                        inline = TRUE)),
    column(width = 4,
           selectInput("plot_combined_x", label = NULL, choices = character(0))),
    column(width = 4,
           selectInput("plot_combined_y", label = NULL, choices = character(0)))
  )
})

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

output$export_settings_ui <- renderUI({

  active_tab <- if(!is.null(input$plot_tabs)) input$plot_tabs else "tab_espace"
  ext        <- if(!is.null(input$export_filetype)) input$export_filetype else "png"

  default_h <- switch(active_tab,
                      "tab_espace"   = if(!is.null(input$plot_espace_state) &&
                                          input$plot_espace_state == "plot_2d") 800 else 1000,
                      "tab_gspace"   = 800,
                      "tab_combined" = if(!is.null(input$plot_combined_layout) &&
                                          input$plot_combined_layout == "row") 800 else 1400,
                      1000
  )

  if(ext == "png"){
    fluidRow(
      column(width = 4,
             tags$span("Width (px)", class = "text-widget-title"),
             numericInput("export_width_px", label = NULL,
                          value = 1400, min = 400, max = 4000, step = 100)),
      column(width = 4,
             tags$span("Height (px)", class = "text-widget-title"),
             numericInput("export_height_px", label = NULL,
                          value = default_h, min = 400, max = 4000, step = 100)),
      column(width = 4,
             tags$span("Resolution (dpi)", class = "text-widget-title"),
             numericInput("export_res", label = NULL,
                          value = 150, min = 72, max = 600, step = 50))
    )
  } else {
    tagList(
      fluidRow(
        column(width = 6,
               tags$span("Width (inches)", class = "text-widget-title"),
               numericInput("export_width_in", label = NULL,
                            value = 10, min = 1, max = 30, step = 0.5)),
        column(width = 6,
               tags$span("Height (inches)", class = "text-widget-title"),
               numericInput("export_height_in", label = NULL,
                            value = round(default_h / 150, 1),
                            min = 1, max = 30, step = 0.5))
      ),
      p("PDF and SVG use vector graphics and do not require a resolution setting.",
        class = "text-instruction")
    )
  }
})

observeEvent(input$open_export_modal, {
  showModal(modalDialog(
    title = "Export Figure",
    size  = "m",
    fluidRow(
      column(width = 12,
             tags$span("File type", class = "text-widget-title"),
             radioButtons("export_filetype", label = NULL,
                          choices  = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                          selected = if(!is.null(input$export_filetype)) input$export_filetype else "png",
                          inline   = TRUE))
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
    ext      <- if(!is.null(input$export_filetype)) input$export_filetype else "png"
    tab      <- if(!is.null(input$plot_tabs)) input$plot_tabs else "tab_espace"
    prefix   <- switch(tab,
                       "tab_espace"   = "espace_plot",
                       "tab_gspace"   = "gspace_plot",
                       "tab_combined" = "combined_plot",
                       "espace_plot")
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  },
  content = function(file){
    ext    <- if(!is.null(input$export_filetype)) input$export_filetype else "png"
    tab    <- if(!is.null(input$plot_tabs)) input$plot_tabs else "tab_espace"
    s      <- collect_plot_settings()
    req(s)

    open_device(file, ext)

    switch(tab,
           "tab_espace" = {
             vars  <- plot_vars()
             req(vars)
             state <- if(!is.null(input$plot_espace_state)) input$plot_espace_state else "plot_pairs"
             switch(state,
                    "plot_pairs" = draw_espace_pairs(vars, s),
                    "plot_2d"    = {
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
             mfrow  <- if(layout == "col") c(2, 1) else c(1, 2)
             par(mfrow = mfrow, mar = c(4, 4, 2, 1))
             draw_espace_panel(input$plot_combined_x, input$plot_combined_y, s)
             draw_gspace_panel(s, title = "G-space")
           }
    )

    dev.off()
  }
)
