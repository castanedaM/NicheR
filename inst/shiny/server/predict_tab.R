# Title: Title: Predict Tab server

# Description: Server for the predict tab, it take theelliposid of more
# elliposid to predict over

# Date Last Updated: 7/10/26


# Outputs -----------------------------------------------------------------

output$pred_ell_select <- renderUI({

  req(length(session_data$ellipsoid_list) > 0)

  versions <- session_data$ellipsoid_list

  ell_choices <- c("All versions" = "all",
                   setNames(names(versions),
                            vapply(versions,
                                   function(ell) ell$ell_name,
                                   character(1))))

  selectInput(inputId = "pred_ell_selected",
              label = tagList(
                tags$span("Ellipsoid Version", class = "text-widget-title"),
                tags$span(icon("circle-info"),
                          title = "Select ellipsoid version to predict over",
                          class = "tooltip-icon")
              ),
              choices  = ell_choices,
              selected = "all")
})

output$advanced_settings_predict <- renderUI({

  box(title = tags$span("Advanced Prediciton Settings",
                        class = "text-tab-title"),
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(width = 8,
               tagList(tags$span("Truncation Level Adjustment",
                                 class = "text-widget-title"),
                       tags$span(icon("circle-info"),
                                 title = "Adjust the level of truncation within the current elliposid, this will this will truncate prediction inwards",
                                 class = "tooltip-icon"))),
        column(width = 4,
               numericInput(inputId = "adjust_trunc",
                            label = NULL,
                            value = 0.95,
                            min = 0.0001, max = 0.99999, step = 0.05)
        )
      )
  )

})

# Main predict observer
observeEvent(input$ell_predict, {

  req(length(session_data$ellipsoid_list) > 0)
  req(input$pred_ell_selected)

  versions <- session_data$ellipsoid_list
  req(length(versions) > 0)

  newdata <- if(!is.null(session_data$bg_raster)){
    session_data$bg_raster
  } else if(!is.null(session_data$bg_df)){
    session_data$bg_df
  } else {
    showNotification("No background data available for prediction.",
                     type = "error", duration = 4)
    return()
  }

  is_all <- input$pred_ell_selected == "all"

  if(is_all){

    # Community prediction
    req(input$pred_community_type)

    ell_community <- lapply(saved_ids, function(id) versions[[id]])
    reference <- versions[["base"]]
    n_ell <- length(ell_community)

    community_obj <- tryCatch(
      new_nicheR_community(
        ellipse_community = ell_community,
        reference = reference,
        pattern = "virtual",
        n = n_ell,
        smallest_proportion = 1 / n_ell
      ),
      error = function(e){
        showNotification(paste("Failed to build community:", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )

    req(community_obj)

    result <- tryCatch(
      predict(community_obj,
              newdata = newdata,
              prediction = input$pred_community_type,
              verbose = FALSE),
      error = function(e){
        showNotification(paste("Community prediction failed:", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )

    req(result)

    session_data$ellipsoid_prediction_list[["community"]] <- result
    showNotification("Community prediction completed.", type = "message", duration = 4)

  } else {

    # Single ellipsoid prediction
    id  <- input$pred_ell_selected
    ell <- versions[[id]]
    req(ell)

    trunc_val <- input$adjust_trunc
    use_trunc <- !is.null(trunc_val) && is.finite(trunc_val) &&
      trunc_val != ell$cl

    result <- tryCatch(
      predict(ell,
              newdata                 = newdata,
              adjust_truncation_level = if(use_trunc) trunc_val else NULL,
              include_suitability     = isTRUE(input$pred_include_suitability),
              suitability_truncated   = isTRUE(input$pred_suitability_trunc),
              include_mahalanobis     = isTRUE(input$pred_include_mahalanobis),
              mahalanobis_truncated   = isTRUE(input$pred_mahalanobis_trunc),
              verbose                 = FALSE),
      error = function(e){
        showNotification(paste("Prediction failed:", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )

    req(result)

    session_data$ellipsoid_prediction_list[[id]] <- result
    showNotification(paste0(ell$ell_name, ": prediction completed."),
                     type = "message", duration = 4)
  }
})

