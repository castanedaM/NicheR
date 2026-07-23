
output$generate_espace_plot <- renderPlot({

  req(session_data$ellipsoid_occurrence_list)
  req(session_data$current_ellipsoid)
  req(session_data$bg_raster)
  req(input$gen_2d_x, input$gen_2d_y)

  ell <- session_data$current_ellipsoid
  occ_ell <- session_data$ellipsoid_occurrence_list[[ell$ell_id]]
  req(length(occ_ell) > 0)

  v1 <- input$gen_2d_x
  v2 <- input$gen_2d_y
  bg <- session_data$bg_df
  rast <- session_data$bg_raster

  occ <- occ_ell[[1]]
  req(!is.null(occ) && nrow(occ) > 0)

  # Extract environmental values at occurrence locations
  env_vals <- terra::extract(rast[[c(v1, v2)]],
                             as.matrix(occ[, c("x", "y")]))

  req(!is.null(env_vals))

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(4, 4, 2, 1))

  # Background scatter
  if(!is.null(bg)){
    plot(bg[[v1]], bg[[v2]],
         col = "#B3B3B3",
         pch = ".",
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2))
  } else {
    plot(NA, NA,
         xlim = range(env_vals[[v1]], na.rm = TRUE),
         ylim = range(env_vals[[v2]], na.rm = TRUE),
         xlab = v1,
         ylab = v2,
         main = paste(v1, "vs.", v2))
  }

  # Ellipsoid boundary
  if(!is.null(ell$cov_matrix) && all(is.finite(ell$cov_matrix))){
    idx <- match(c(v1, v2), ell$var_names)
    if(!any(is.na(idx))){
      add_ellipsoid(ell, dim = idx,
                    col_ell = "#000000", lwd = 2, lty = 1)
    }
  }

  # Occurrences in environmental space
  valid <- is.finite(env_vals[[v1]]) & is.finite(env_vals[[v2]])
  if(any(valid)){
    points(env_vals[[v1]][valid], env_vals[[v2]][valid],
           col = "#097a21",
           pch = 16,
           cex = 1)
  }

})

output$generate_espace_options_ui <- renderUI({

  ell <- session_data$current_ellipsoid
  req(ell)
  vars <- ell$var_names

  fluidRow(
    column(width = 6,
           selectInput("gen_2d_x", label = NULL,
                       choices = vars,
                       selected = vars[1])),
    column(width = 6,
           selectInput("gen_2d_y", label = NULL,
                       choices = vars,
                       selected = if(length(vars) > 1) vars[2] else vars[1]))
  )
})
