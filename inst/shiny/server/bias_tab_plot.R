# Title: Bias Tab Plot server
# Description: Server for the bias tab plots
# Date Last Updated: 7/22/26


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

output$bias_gspace_plot <- renderPlot({

  has_raster <- !is.null(session_data$bg_raster)
  req(has_raster)

  ell <- session_data$current_ellipsoid
  req(ell)

  id <- ell$ell_id
  pred_result <- session_data$ellipsoid_prediction_list[[id]]
  bias_result <- session_data$ellipsoid_prediction_list_biased[[id]]

  has_pred_ell <- !is.null(pred_result) && inherits(pred_result, "SpatRaster")
  has_bias_ell <- !is.null(bias_result)

  req(has_pred_ell)

  pred_layers <- names(pred_result)

  bias_layers <- if(has_bias_ell){
    names(bias_result)
  } else {
    character(0)
  }

  # Build matched pairs by name
  pairs <- lapply(pred_layers, function(layer){
    matched_bias <- if(has_bias_ell){
      bias_layers[bias_layers == paste0(layer, "_biased") |
                    startsWith(bias_layers, paste0(layer, "_biased_"))]
    } else {
      character(0)
    }

    list(pred = layer,
         bias = if(length(matched_bias) > 0) matched_bias[1] else NULL)
  })

  n_rows <- length(pairs)
  n_cols <- if(has_bias_ell) 2L else 1L
  map_bg_col <- "#F0F0F0"

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 4))

  for(pair in pairs){

    # Left column: original prediction
    terra::plot(pred_result[[pair$pred]],
                main = pair$pred,
                axes = TRUE,
                colNA = map_bg_col)

    # Right column: matched biased layer
    if(has_bias_ell){
      if(!is.null(pair$bias)){
        terra::plot(bias_result[[pair$bias]],
                    main = pair$bias,
                    axes = TRUE,
                    colNA = map_bg_col)
      } else {
        plot.new()
        title(main = paste0(pair$pred, " (no bias applied)"),
              col.main = "#aaa")
      }
    }
  }

})
