# Title: Bias Tab Plot server
# Description: Server for the bias tab plots
# Date Last Updated: 7/29/26


# Prepare bias plot outputs ---------------------------------------------------------

# Tab 1: Original and processed layers side by side
output$bias_layers_plot <- renderPlot({

  has_raw <- !is.null(session_data$bias_raster)
  has_prepared <- !is.null(session_data$prepared_bias) &&
    !is.null(session_data$prepared_bias$processed_layers)

  req(has_raw)

  rast <- session_data$bias_raster
  n_raw <- terra::nlyr(rast)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if(has_prepared){
    processed <- session_data$prepared_bias$processed_layers
    n_proc <- terra::nlyr(processed)
    n_rows <- max(n_raw, n_proc)
    par(mfrow = c(n_rows, 2), mar = c(3, 3, 2, 4))

    for(i in seq_len(n_rows)){
      # Left column: raw
      if(i <= n_raw){
        terra::plot(rast[[i]],
                    main = paste0("Raw: ", names(rast[[i]])),
                    axes = TRUE)
      } else {
        plot.new()
      }
      # Right column: processed
      if(i <= n_proc){
        terra::plot(processed[[i]],
                    main = names(processed[[i]]),
                    axes = TRUE)
      } else {
        plot.new()
      }
    }

  } else {
    # No prepare yet: single column
    par(mfrow = c(n_raw, 1), mar = c(3, 3, 2, 4))
    for(i in seq_len(n_raw)){
      terra::plot(rast[[i]],
                  main = paste0("Raw: ", names(rast[[i]])),
                  axes = TRUE)
    }
  }

})

# Tab 2: Composite surface only
output$bias_composite_plot <- renderPlot({

  req(session_data$prepared_bias)
  req(session_data$prepared_bias$composite_surface)

  composite <- session_data$prepared_bias$composite_surface

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(3, 3, 2, 4))

  terra::plot(composite,
              main = paste0("Composite: ",
                            session_data$prepared_bias$combination_formula),
              axes = TRUE)

})

# Apply bias --------------------------------------------------------------

output$bias_gspace_layer_select <- renderUI({

  ell <- session_data$current_ellipsoid
  req(ell)

  pred_result <- session_data$ellipsoid_prediction_list[[ell$ell_id]]
  req(!is.null(pred_result) && inherits(pred_result, "SpatRaster"))

  selectInput("bias_gspace_layer",
              label = tags$span("Prediction layer", class = "text-widget-title"),
              choices = names(pred_result),
              selected = if("suitability_trunc" %in% names(pred_result)) "suitability_trunc"
              else names(pred_result)[1])
})

output$bias_gspace_plot <- renderPlot({

  has_raster <- !is.null(session_data$bg_raster)
  req(has_raster)
  req(input$bias_gspace_layer)

  ell <- session_data$current_ellipsoid
  req(ell)

  id <- ell$ell_id
  pred_result <- session_data$ellipsoid_prediction_list[[id]]
  bias_result <- session_data$ellipsoid_prediction_list_biased[[id]]

  has_pred_ell <- !is.null(pred_result) && inherits(pred_result, "SpatRaster")
  has_bias_ell <- !is.null(bias_result) && inherits(bias_result, "SpatRaster")

  req(has_pred_ell)

  layer <- input$bias_gspace_layer
  map_bg_col <- "#F0F0F0"

  matched_bias <- if(has_bias_ell){
    names(bias_result)[startsWith(names(bias_result), paste0(layer, "_biased_"))]
  } else {
    character(0)
  }

  # All panels: original first then biased
  all_panels <- c(list(list(type = "pred", name = layer)),
                  lapply(matched_bias, function(nm) list(type = "bias", name = nm)))

  n_total <- length(all_panels)
  n_cols <- 2L
  n_rows <- ceiling(n_total / n_cols)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 4))

  for(panel in all_panels){
    if(panel$type == "pred"){
      terra::plot(pred_result[[panel$name]],
                  main = panel$name,
                  axes = TRUE,
                  colNA = map_bg_col)
    } else {
      terra::plot(bias_result[[panel$name]],
                  main = panel$name,
                  axes = TRUE,
                  colNA = map_bg_col)
    }
  }

  # # Fill last cell if odd number of panels
  # if(n_total %% 2 != 0){
  #   plot.new()
  # }

})


# Prediction and Biased G-space -------------------------------------------

output$bias_gspace_layer_select <- renderUI({

  ell <- session_data$current_ellipsoid
  req(ell)

  pred_result <- session_data$ellipsoid_prediction_list[[ell$ell_id]]
  if(is.null(pred_result) && !is.null(ell$ell_name)){
    pred_result <- session_data$ellipsoid_prediction_list[[ell$ell_name]]
  }

  req(!is.null(pred_result) && inherits(pred_result, "SpatRaster"))

  selectInput("bias_gspace_layer",
              label    = tags$span("Prediction layer", class = "text-widget-title"),
              choices  = names(pred_result),
              selected = if("suitability_trunc" %in% names(pred_result)) "suitability_trunc"
              else names(pred_result)[1])
})


output$bias_gspace_plot <- renderPlot({

  req(!is.null(session_data$bg_raster))
  req(input$bias_gspace_layer)

  ell <- session_data$current_ellipsoid
  req(ell)

  id          <- ell$ell_id
  pred_result <- session_data$ellipsoid_prediction_list[[id]]
  if(is.null(pred_result) && !is.null(ell$ell_name)){
    pred_result <- session_data$ellipsoid_prediction_list[[ell$ell_name]]
  }

  bias_result <- session_data$ellipsoid_prediction_list_biased[[id]]
  if(is.null(bias_result) && !is.null(ell$ell_name)){
    bias_result <- session_data$ellipsoid_prediction_list_biased[[ell$ell_name]]
  }

  has_pred_ell <- !is.null(pred_result) && inherits(pred_result, "SpatRaster")
  has_bias_ell <- !is.null(bias_result) && inherits(bias_result, "SpatRaster")

  req(has_pred_ell)

  layer      <- input$bias_gspace_layer
  map_bg_col <- "#F0F0F0"

  matched_bias <- if(has_bias_ell){
    names(bias_result)[startsWith(names(bias_result), paste0(layer, "_biased_"))]
  } else {
    character(0)
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # No biased layers for this prediction layer
  if(length(matched_bias) == 0){
    par(mfrow = c(1, 2))
    terra::plot(pred_result[[layer]],
                main  = layer,
                axes  = TRUE,
                colNA = map_bg_col)
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
         xlab = "", ylab = "")
    text(0.5, 0.5, paste0("No biased layer\ncomputed for\n'", layer, "'"),
         cex = 1.1, col = "grey50", adj = c(0.5, 0.5))
    return(invisible(NULL))
  }

  # Original + all matched biased layers in 2-column grid
  all_panels <- c(
    list(list(type = "pred", name = layer)),
    lapply(matched_bias, function(nm) list(type = "bias", name = nm))
  )

  n_total <- length(all_panels)
  n_rows  <- ceiling(n_total / 2L)

  par(mfrow = c(n_rows, 2L), mar = c(3, 3, 2, 4))

  for(panel in all_panels){
    if(panel$type == "pred"){
      terra::plot(pred_result[[panel$name]],
                  main  = panel$name,
                  axes  = TRUE,
                  colNA = map_bg_col)
    } else {
      terra::plot(bias_result[[panel$name]],
                  main  = panel$name,
                  axes  = TRUE,
                  colNA = map_bg_col)
    }
  }

  if(n_total %% 2 != 0) plot.new()

})
