# Title: Generate Tab Server Logic
# Description: UI and observers for sampling occurrence data
# Date Last Updated: 07/23/2026


# Helpers -------------------------------------------------------------------

generate_occ_for_ell <- function(ell_id, pred_list, biased_list,
                                 layer, surface, n_occ, sampling_mask,
                                 sampling, strict, seed = 123L){

  results <- list()
  pred <- pred_list[[ell_id]]
  method <- if(grepl("mahalanobis", layer, ignore.case = TRUE)) "mahalanobis" else "suitability"

  # unbiased
  if(surface %in% c("unbiased", "both")){
    if(!is.null(pred) && layer %in% names(pred)){
      occ <- tryCatch(
        sample_data(n_occ = n_occ,
                    prediction = pred,
                    prediction_layer = layer,
                    sampling = sampling,
                    method = method,
                    seed = seed,
                    sampling_mask = sampling_mask,
                    strict = strict,
                    verbose = FALSE),
        error = function(e){
          message("unbiased generate failed for ", ell_id, ": ", e$message)
          NULL
        }
      )
      if(!is.null(occ)){
        results[[layer]] <- occ[, c("x", "y"), drop = FALSE]
      }
    } else {
      message("Layer '", layer, "' not found for ", ell_id, " (unbiased). Skipping.")
    }
  }

  # Biased
  if(surface %in% c("bias", "both")){
    bias_rast <- biased_list[[ell_id]]
    if(!is.null(bias_rast)){
      matched <- names(bias_rast)[startsWith(names(bias_rast),
                                             paste0(layer, "_biased_"))]
      if(length(matched) == 0){
        message("No biased layer matching '", layer, "' for ", ell_id, ". Skipping.")
      } else {
        for(bias_lyr in matched){
          occ <- tryCatch(
            sample_data(n_occ = n_occ,
                        prediction = bias_rast,
                        prediction_layer = bias_lyr,
                        sampling = sampling,
                        method = method,
                        seed = seed,
                        sampling_mask = sampling_mask,
                        strict = strict,
                        verbose = FALSE),
            error = function(e){
              message("Biased generate failed for ", ell_id,
                      " (", bias_lyr, "): ", e$message)
              NULL
            }
          )
          if(!is.null(occ)){
            results[[bias_lyr]] <- occ[, c("x", "y"), drop = FALSE]
          }
        }
      }
    } else {
      message("No biased prediction for ", ell_id, ". Skipping bias surface.")
    }
  }

  results
}
# UI ------------------------------------------------------------------------

output$generate_ui <- renderUI({

  has_pred <- length(session_data$ellipsoid_prediction_list) > 0
  has_biased <- length(session_data$ellipsoid_prediction_list_biased) > 0
  has_done <- !is.null(session_data$ellipsoid_occurrence_list) &&
    length(session_data$ellipsoid_occurrence_list) > 0

  if(!has_pred){
    return(
      box(title = tags$span("Generate Occurrences", class = "text-section-header"),
          width = 12,
          p("Run predictions in the Predict tab before generating occurrences.",
            class = "text-instruction"))
    )
  }

  if(has_done){
    n_ell <- length(session_data$ ellipsoid_occurrence_list)
    return(
      box(title = tags$span("Generate Occurrences", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p(paste0("Occurrences generated for ", n_ell, " ellipsoid(s)."),
            class = "text-instruction"),
          fluidRow(
            column(width = 12, class = "btn-spaced",
                   actionLink("edit_generate",
                              label = tagList(icon("pen"), "Generate again")))
          )
      )
    )
  }

  box(title = tags$span("Generate Occurrences", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p("Sample virtual occurrence points from a prediction surface.",
        class = "text-instruction"),

      # Ellipsoid version selector
      uiOutput("gen_ell_select"),

      # Prediction surface choice
      fluidRow(
        column(width = 12,
               tags$span("Prediction surface", class = "text-widget-title"),
               tags$span(icon("circle-info"),
                         title = paste0("Unbiased: use original prediction. ",
                                        "Bias: use bias-corrected prediction. ",
                                        "Unbiased + Bias: generate both for comparison."),
                         class = "tooltip-icon"),
               radioButtons("gen_surface",
                            label = NULL,
                            choiceNames = list(
                              tags$span("Unbiased", class = "text-widget-inner"),
                              tags$span("Bias", class = "text-widget-inner"),
                              tags$span("Unbiased + Bias", class = "text-widget-inner")
                            ),
                            choiceValues = c("unbiased", "bias", "both"),
                            selected = if(has_biased) "both" else "unbiased",
                            inline = TRUE))
      ),

      uiOutput("gen_pred_lyr_options"),

      # Auto-detected method message
      uiOutput("gen_method_msg"),

      # Number of occurrences
      fluidRow(
        column(width = 6,
               tags$span("Number of occurrences", class = "text-widget-title"),
               numericInput("gen_n_occ",
                            label = NULL,
                            value = 100,
                            min = 1,
                            max = 1000000,
                            step = 10))
      ),

      # Sampling strategy
      fluidRow(
        column(width = 12,
               tags$span("Sampling strategy", class = "text-widget-title"),
               radioButtons("gen_sampling",
                            label = NULL,
                            choiceNames = list(
                              tagList(tags$span("Centroid", class = "text-widget-inner"),
                                      tags$span(icon("circle-info"),
                                                title = "Higher probability near the niche center.",
                                                class = "tooltip-icon")),
                              tagList(tags$span("Edge", class = "text-widget-inner"),
                                      tags$span(icon("circle-info"),
                                                title = "Higher probability near the niche boundary.",
                                                class = "tooltip-icon")),
                              tagList("Random",
                                      tags$span(icon("circle-info"),
                                                title = "Equal probability across all suitable cells.",
                                                class = "tooltip-icon"))
                            ),
                            choiceValues = c("centroid", "edge", "random"),
                            selected = "centroid",
                            inline = TRUE))
      ),

      # Strict filtering
      fluidRow(
        column(width = 12,
               tags$span("Strict filtering", class = "text-widget-title"),
               tags$span(icon("circle-info"),
                         title = paste0("When TRUE, removes NA and zero-valued cells before ",
                                        "sampling. Recommended for truncated layers."),
                         class = "tooltip-icon"),
               radioButtons("gen_strict",
                            label = NULL,
                            choiceNames = c("True", "False"),
                            choiceValues = c("TRUE", "FALSE"),
                            selected = "TRUE",
                            inline = TRUE))
      ),

      # Advanced settings
      box(title = tagList(
        tags$span("Advanced Settings", class = "text-section-header"),
        tags$span(icon("circle-info"),
                  title = "Optional sampling mask to restrict occurrence generation to a specific area.",
                  class = "tooltip-icon")),
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,

        fluidRow(
          column(width = 12,
                 fileInput("gen_mask_file",
                           label = tags$span("Sampling mask (optional)",
                                             class = "text-widget-title"),
                           multiple = FALSE,
                           accept = c("tif", "tiff", "rds")))
        ),
        p("If no mask is provided, sampling covers the full prediction extent.",
          class = "text-instruction")
      ),

      fluidRow(
        column(width = 12,
               div(class = "action-btn-row",
                   actionButton("generate_occ",
                                tagList(icon("play"), "Generate"),
                                class = "btn-continue"))
        )
      )
  )
})

# Auto-detected method message
output$gen_method_msg <- renderUI({

  req(input$gen_pred_layer)
  layer <- input$gen_pred_layer
  method <- if(grepl("mahalanobis", layer, ignore.case = TRUE)) "mahalanobis" else "suitability"

  if(method == "mahalanobis"){
    method_label <- paste0("Mahalanobis distance detected in '", layer,
                           "' — method set to mahalanobis.")
  } else {
    method_label <- paste0("Suitability surface detected in '", layer,
                           "' — method set to suitability.")
  }

  fluidRow(
    column(width = 12,
           tags$p(icon("circle-info"), " ", method_label,
                  style = "font-size: 10px; color: #ccc; margin: 4px 0 8px;")
    )
  )
})

# Ellipsoid version selector
output$gen_ell_select <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  versions <- session_data$ellipsoid_list
  pred_ids <- names(session_data$ellipsoid_prediction_list)

  ell_choices <- c(
    "All versions" = "all",
    setNames(pred_ids,
             vapply(pred_ids, function(id){
               ell <- versions[[id]]
               if(!is.null(ell)) ell$ell_name else id
             }, character(1)))
  )

  selectInput(inputId = "gen_ell_selected",
              label = tagList(
                tags$span("Ellipsoid version", class = "text-widget-title"),
                tags$span(icon("circle-info"),
                          title = "Select which ellipsoid version to generate occurrences from.",
                          class = "tooltip-icon")
              ),
              choices = ell_choices,
              selected = "all")
})


# Observers -----------------------------------------------------------------

observeEvent(input$generate_occ, {

  req(input$gen_ell_selected)
  req(input$gen_pred_layer)
  req(input$gen_n_occ)
  req(input$gen_sampling)

  pred_list <- session_data$ellipsoid_prediction_list
  bias_list <- session_data$ellipsoid_prediction_list_biased
  is_all <- input$gen_ell_selected == "all"
  selected_ids <- if(is_all) names(pred_list) else input$gen_ell_selected

  surface <- input$gen_surface
  n_occ <- as.integer(input$gen_n_occ)
  sampling <- input$gen_sampling
  strict <- switch(input$gen_strict,
                   "TRUE" = TRUE,
                   "FALSE" = FALSE,
                   NULL)

  sampling_mask <- if(!is.null(input$gen_mask_file)){
    ext <- tolower(tools::file_ext(input$gen_mask_file$name))
    tryCatch(
      load_raster_file(input$gen_mask_file$datapath, ext),
      error = function(e){
        showNotification(paste("Could not load sampling mask:", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )
  } else {
    NULL
  }

  session_data$sampling_mask <- sampling_mask

  is_all_lyrs <- input$gen_pred_layer == "all_lyrs"

  if(is_all_lyrs){
    first_id <- selected_ids[1]
    pred <- pred_list[[first_id]]
    unbiased_lyrs <- if(!is.null(pred) && inherits(pred, "SpatRaster")) names(pred) else character(0)
    bias <- bias_list[[first_id]]
    bias_lyrs <- if(!is.null(bias) && inherits(bias, "SpatRaster")) names(bias) else character(0)
    target_layers <- switch(surface,
                            "unbiased" = unbiased_lyrs,
                            "bias" = bias_lyrs,
                            "both" = unique(c(unbiased_lyrs, bias_lyrs)))
  } else {
    target_layers <- input$gen_pred_layer
  }

  if(length(target_layers) == 0){
    showNotification("No prediction layers found.", type = "warning", duration = 4)
    return()
  }

  # Run generate for each ellipsoid, each layer
  occurrence_list <- setNames(
    lapply(selected_ids, function(id){
      # Merge results across all target layers into one named list
      Reduce(function(acc, layer){
        new_res <- generate_occ_for_ell(ell_id = id,
                                        pred_list = pred_list,
                                        biased_list = bias_list,
                                        layer = layer,
                                        surface = surface,
                                        n_occ = n_occ,
                                        sampling = sampling,
                                        strict = strict,
                                        sampling_mask = sampling_mask,
                                        seed = 123L)
        c(acc, new_res)
      }, target_layers, list())
    }),
    selected_ids
  )

  n_success <- sum(vapply(occurrence_list, function(ell_res){
    sum(vapply(ell_res, function(df) !is.null(df) && nrow(df) > 0, logical(1)))
  }, integer(1)))

  n_skipped <- length(selected_ids) * length(target_layers) - n_success

  if(n_success == 0){
    showNotification("Occurrence generation failed for all selected ellipsoids.",
                     type = "error", duration = 4)
    return()
  }

  session_data$ellipsoid_occurrence_list <- occurrence_list

  msg <- paste0(n_success, " occurrence set(s) generated.")
  if(n_skipped > 0) msg <- paste0(msg, " ", n_skipped, " skipped.")
  showNotification(msg, type = "message", duration = 4)
})

observeEvent(input$edit_generate, {


})

output$gen_pred_lyr_options <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  surface <- if(!is.null(input$gen_surface)) input$gen_surface else "unbiased"
  sel_id <- if(!is.null(input$gen_ell_selected) &&
               input$gen_ell_selected != "all"){
    input$gen_ell_selected
  } else {
    names(session_data$ellipsoid_prediction_list)[1]
  }

  # unbiased layers
  pred <- session_data$ellipsoid_prediction_list[[sel_id]]
  unbiased_lyrs <- if(!is.null(pred) && inherits(pred, "SpatRaster")) names(pred) else character(0)

  # Biased layers
  bias <- session_data$ellipsoid_prediction_list_biased[[sel_id]]
  bias_lyrs <- if(!is.null(bias) && inherits(bias, "SpatRaster")) names(bias) else character(0)

  layers <- switch(surface,
                   "unbiased" = unbiased_lyrs,
                   "bias" = bias_lyrs,
                   "both" = unique(c(unbiased_lyrs, bias_lyrs)))

  req(length(layers) > 0)

  all_choices <- c("All layers" = "all_lyrs", layers)

  default <- if("suitability_trunc" %in% layers) "suitability_trunc"
  else if("suitability" %in% layers) "suitability"
  else layers[1]

  fluidRow(
    column(width = 8,
           tags$span("Prediction layer", class = "text-widget-title"),
           tags$span(icon("circle-info"),
                     title = "Select the prediction layer to sample from, or all.",
                     class = "tooltip-icon"),
           selectInput("gen_pred_layer",
                       label = NULL,
                       choices = all_choices,
                       selected = default))
  )
})

