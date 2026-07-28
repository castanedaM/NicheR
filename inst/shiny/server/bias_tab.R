# Title: Bias Tab server
# Description: Server for the bias tab
# Date Last Updated: 7/27/26

output$save_ell_bias_ui <- renderUI({
  req(length(session_data$ellipsoid_prediction_list_biased) > 0)

  div(class = "action-btn-row",
      actionButton(inputId = "save_ell_bias_btn",
                   label = tags$span("Comfirm bias",
                                     class = "text-widget-title"),
                   class = "btn-save")
  )

})

observeEvent(input$save_ell_bias_btn, {

  updateTabItems(session, "sidebarMenu", selected = "generate_tab")

})

# Input Bias Layers -------------------------------------------------------

continue_bias <- reactiveVal(FALSE)

observeEvent(input$bias_upload, {

  req(input$bias_raster_file)

  files <- input$bias_raster_file

  if(nrow(files) > 10){
    showNotification("Maximum 10 raster files allowed.",
                     type = "warning", duration = 4)
    return()
  }

  rasters <- lapply(seq_len(nrow(files)), function(i){
    ext <- tolower(tools::file_ext(files$name[i]))
    tryCatch(
      load_raster_file(files$datapath[i], ext),
      error = function(e){
        showNotification(paste0("Could not load ", files$name[i],
                                ": ", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )
  })

  rasters <- Filter(Negate(is.null), rasters)

  if(length(rasters) == 0){
    showNotification("No raster files could be loaded.",
                     type = "error", duration = 4)
    return()
  }

  rast <- if(length(rasters) == 1){
    rasters[[1]]
  } else {
    tryCatch(
      do.call(c, rasters),
      error = function(e){
        showNotification(paste0("Could not stack bias rasters: ", e$message,
                                " Check that all files have matching resolution, ",
                                "extent, and CRS."),
                         type = "error",
                         duration = 6)
        NULL
      }
    )
  }

  req(rast)

  session_data$bias_raster <- rast
  session_data$prepared_bias <- NULL

  showNotification(paste0(terra::nlyr(rast), " bias layer(s) loaded successfully."),
                   type = "message", duration = 4)
})

observeEvent(input$continue_bias_example, {
  rast <- tryCatch(
    terra::rast(system.file("extdata", "ma_biases.tif", package = "nicheR")),
    error = function(e){
      showNotification(paste("Could not load example bias:", e$message),
                       type = "error", duration = 4)
      NULL
    }
  )
  req(rast)
  session_data$bias_raster <- rast
  session_data$prepared_bias <- NULL
  showNotification("Example bias raster loaded.", type = "message", duration = 4)
})

observeEvent(input$edit_bias_upload, {
  showModal(modalDialog(
    title = "Edit bias raster?",
    p("This will remove the current bias raster and all downstream preparation.
 You will need to re-upload and re-prepare."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_edit_bias_upload",
                   "Yes, edit",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))
})

observeEvent(input$confirm_edit_bias_upload, {
  removeModal()
  session_data$bias_raster <- NULL
  session_data$prepared_bias <- NULL
  showNotification("Bias raster cleared. Upload a new file.",
                   type = "message", duration = 3)
})

output$upload_bias_ui <- renderUI({

  has_pred <- length(session_data$ellipsoid_prediction_list) > 0
  has_bias <- !is.null(session_data$bias_raster)

  if(!has_pred){
    return(
      box(title = tags$span("Bias Inputs", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p("Run predictions in the Predict tab before preparing and applying bias.",
            class = "text-instruction"))
    )
  }

  if(has_bias){
    return(
      box(title = tags$span("Bias Inputs", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          verbatimTextOutput("bias_raster_print"),
          fluidRow(
            column(width = 12, class = "btn-spaced",
                   actionLink("edit_bias_upload",
                              label = tagList(icon("pen"), "Edit bias raster")))
          )
      )
    )
  }

  box(title = tags$span("Bias input layers", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$bias_input, class = "text-instruction"),

      fluidRow(
        column(width = 12,
               fileInput(inputId = "bias_raster_file",
                         label = tags$span("Sampling bias layer/s (raster)",
                                           class = "text-widget-title"),
                         multiple = TRUE,
                         accept = c("tif", "tiff", "rds")))
      ),

      conditionalPanel(
        condition = "output.bias_raster_print != ''",
        verbatimTextOutput("bias_raster_print")
      ),

      fluidRow(
        column(width = 12,
               div(class = "action-btn-row",
                   actionButton("bias_upload",
                                "Upload bias",
                                class = "btn-continue"),
                   actionButton("continue_bias_example",
                                "Example bias",
                                class = "btn-back")
               )
        )
      )
  )
})

output$bias_raster_print <- renderPrint({
  req(input$bias_raster_file)
  req(nrow(input$bias_raster_file) <= 10)

  files <- input$bias_raster_file

  rasters <- lapply(seq_len(nrow(files)), function(i){
    ext <- tolower(tools::file_ext(files$name[i]))
    tryCatch(
      load_bias_raster_file(files$datapath[i], ext),
      error = function(e){
        cat("Could not load", files$name[i], ":", e$message, "\n")
        NULL
      }
    )
  })

  rasters <- Filter(Negate(is.null), rasters)
  req(length(rasters) > 0)

  if(length(rasters) == 1){
    print(rasters[[1]])
  } else {
    stacked <- tryCatch(
      do.call(c, rasters),
      error = function(e){
        cat("Could not stack bias rasters:", e$message, "\n")
        NULL
      }
    )
    if(!is.null(stacked)){
      print(stacked)
    }
  }
})

# Prepare Bias ------------------------------------------------------------

observeEvent(input$prepare_bias, {
  req(session_data$bias_raster)

  lyrs <- names(session_data$bias_raster)

  effect_direction <- vapply(lyrs, function(nm){
    val <- input[[paste0("bias_dir_", nm)]]
    if(is.null(val)) "direct" else val
  }, character(1))

  result <- tryCatch(
    prepare_bias(bias_surface = session_data$bias_raster,
                 effect_direction = effect_direction,
                 include_composite = TRUE,
                 include_processed_layers = TRUE,
                 mask_na = isTRUE(input$bias_mask_na == "TRUE"),
                 verbose = FALSE),
    error = function(e){
      showNotification(paste("Bias preparation failed:", e$message),
                       type = "error", duration = 4)
      NULL
    }
  )

  req(result)
  session_data$prepared_bias <- result
  showNotification("Bias prepared successfully.", type = "message", duration = 4)
})

observeEvent(input$edit_bias_prepare, {
  showModal(modalDialog(
    title = "Edit bias preparation?",
    p("This will remove the current prepared bias surface and any applied bias.
 The uploaded raster will be kept."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_edit_bias_prepare",
                   "Yes, edit",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))
})

observeEvent(input$confirm_edit_bias_prepare, {
  removeModal()
  session_data$prepared_bias <- NULL
  session_data$ellipsoid_prediction_list_biased <- list()

  showNotification("Bias preparation cleared. Adjust settings and re-prepare.",
                   type = "message", duration = 3)
})

output$prepare_bias_ui <- renderUI({

  req(session_data$bias_raster)

  has_prepared <- !is.null(session_data$prepared_bias)

  if(has_prepared){
    result <- session_data$prepared_bias
    return(
      box(title = tags$span("Prepare bias raster", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p(paste0("Formula: ", result$combination_formula),
            class = "text-instruction"),
          fluidRow(
            column(width = 12, class = "btn-spaced",
                   actionLink("edit_bias_prepare",
                              label = tagList(icon("pen"), "Edit bias preparation")))
          )
      )
    )
  }

  box(title = tags$span("Prepare Bias Raster", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p("Assign direction of effect for each bias layer and configure how
 overlapping NAs are handled before preparing the composite surface.",
        class = "text-instruction"),

      uiOutput("bias_effect_directions"),

      fluidRow(
        column(width = 12,
               tags$span("NA overlap handling", class = "text-widget-title"),
               tags$span(icon("circle-info"),
                         title = "Union keeps any pixel with at least one valid value across layers. NAs in other layers are ignored. Intersection keeps only pixels with valid values in ALL layers. Any pixel with an NA in any layer becomes NA in the composite.",
                         class = "tooltip-icon"),  radioButtons(inputId = "bias_mask_na",
                                                                label = NULL,
                                                                choiceNames = list("Union", "Intersection"),
                                                                choiceValues = c("FALSE", "TRUE"),
                                                                selected= "FALSE",
                                                                inline = TRUE))
      ),

      fluidRow(
        column(width = 12, class = "action-btn-row",
               actionButton("prepare_bias",
                            "Prepare Bias",
                            class = "btn-continue"))
      )
  )
})

output$bias_effect_directions <- renderUI({

  req(session_data$bias_raster)

  rast <- session_data$bias_raster
  lyrs <- names(rast)

  stats <- lapply(lyrs, function(nm){
    vals <- terra::values(rast[[nm]], na.rm = TRUE)
    list(mean = round(mean(vals, na.rm = TRUE), 3),
         sd = round(sd(vals, na.rm = TRUE), 3))
  })

  header <- fluidRow(
    column(width = 4,
           tags$span("Layer", class = "text-widget-title")),
    column(width = 3,
           tags$div(class = "tooltip-label-row",
                    tags$span("Mean (SD)", class = "text-widget-title"),
                    tags$span(icon("circle-info"),
                              title = "Mean and standard deviation of non-NA raster values.",
                              class = "tooltip-icon"))),
    column(width = 5,
           tags$div(class = "tooltip-label-row",
                    tags$span("Effect direction", class = "text-widget-title"),
                    tags$span(icon("circle-info"),
                              title = "Direct: higher values increase sampling probability. Inverse: higher values decrease sampling probability.",
                              class = "tooltip-icon")))
  )

  rows <- lapply(seq_along(lyrs), function(i){
    nm <- lyrs[i]
    fluidRow(
      column(width = 4, class = "var-label",
             tags$span(nm, class = "text-widget-inner")),
      column(width = 3,
             tags$span(paste0(stats[[i]]$mean, " (", stats[[i]]$sd, ")"),
                       class = "text-widget-inner")),
      column(width = 5,
             radioButtons(inputId = paste0("bias_dir_", nm),
                          label = NULL,
                          choiceNames = list(
                            tagList("Direct",
                                    tags$span(icon("circle-info"),
                                              title = "Higher values increase sampling probability.",
                                              class = "tooltip-icon")),
                            tagList("Inverse",
                                    tags$span(icon("circle-info"),
                                              title = "Higher values decrease sampling probability.",
                                              class = "tooltip-icon"))
                          ),
                          choiceValues = c("direct", "inverse"),
                          selected = "direct",
                          inline = FALSE))
    )
  })

  tagList(header, tagList(rows))
})


# Apply bias --------------------------------------------------------------

show_apply_bias_form <- reactiveVal(FALSE)

# Helper: apply bias to one layer for one ellipsoid and merge into biased list.
# If ell_id exists, appends new layer to existing SpatRaster stack.
# If layer already exists in stack, skips silently.
apply_bias_to_list <- function(biased_list, ell_id, pred_rast,
                               layer, prepared_bias, direction = "direct"){

  result <- tryCatch(
    apply_bias(prepared_bias = prepared_bias,
               prediction = pred_rast,
               prediction_layer = layer,
               effect_direction = direction,
               verbose = FALSE),
    error = function(e) NULL
  )

  if(is.null(result)) return(biased_list)

  # Extract SpatRaster from result, drop combination_formula
  new_rast <- result[[paste0(layer, "_biased")]]
  if(is.null(new_rast) || !inherits(new_rast, "SpatRaster")) return(biased_list)

  if(!ell_id %in% names(biased_list)){
    biased_list[[ell_id]] <- new_rast
  } else {
    existing <- biased_list[[ell_id]]
    new_lyr_nm <- names(new_rast)

    if(new_lyr_nm %in% names(existing)){
      message("Layer '", new_lyr_nm, "' already exists for ", ell_id, ". Skipping.")
    } else {
      biased_list[[ell_id]] <- c(existing, new_rast)
    }
  }

  biased_list
}

output$apply_bias_ui <- renderUI({

  req(session_data$prepared_bias)
  req(length(session_data$ellipsoid_prediction_list) > 0)

  has_applied <- length(session_data$ellipsoid_prediction_list_biased) > 0
  show_form <- isTRUE(show_apply_bias_form())

  if(has_applied && !show_form){

    # Count total biased layers across all ellipsoids
    n_layers <- sum(vapply(session_data$ellipsoid_prediction_list_biased,
                           function(rast) terra::nlyr(rast),
                           numeric(1)))

    return(
      box(title = tags$span("Apply bias", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p(paste0(n_layers, " biased layer(s) across ",
                   length(session_data$ellipsoid_prediction_list_biased),
                   " ellipsoid(s)."),
            class = "text-instruction"),
          fluidRow(
            column(width = 12, class = "btn-spaced",
                   actionLink("edit_apply_bias",
                              label = tagList(icon("pen"), "Add or view bias layers")))
          )
      )
    )
  }

  pred_ids <- names(session_data$ellipsoid_prediction_list)
  first_pred <- session_data$ellipsoid_prediction_list[[pred_ids[1]]]
  layers <- if(inherits(first_pred, "SpatRaster")) names(first_pred) else character(0)

  box(title = tags$span("Apply bias", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p("Apply the composite bias surface to a prediction. Previously applied
 combinations are skipped automatically.",
        class = "text-instruction"),

      uiOutput("bias_ell_select"),

      if(length(layers) > 0) fluidRow(
        column(width = 12,
               tags$span("Prediction layer", class = "text-widget-title"),
               tags$span(icon("circle-info"),
                         title = "Layer must be a suitability surface with values in [0, 1].",
                         class = "tooltip-icon"),
               selectInput("bias_prediction_layer",
                           label = NULL,
                           choices = c("All prediction layers" = "all_pred", layers),
                           selected = if("suitability_trunc" %in% layers) "suitability_trunc"
                           else if("suitability" %in% layers) "suitability"
                           else layers[1]))
      ),

      fluidRow(
        column(width = 12,
               tags$span("Effect direction", class = "text-widget-title"),
               radioButtons("bias_effect_direction",
                            label = NULL,
                            choiceNames = list(
                              tagList("Direct",
                                      tags$span(icon("circle-info"),
                                                title = "prediction x bias.",
                                                class = "tooltip-icon")),
                              tagList("Inverse",
                                      tags$span(icon("circle-info"),
                                                title = "prediction x (1 - bias).",
                                                class = "tooltip-icon"))
                            ),
                            choiceValues = c("direct", "inverse"),
                            selected = "direct",
                            inline = TRUE))
      ),

      fluidRow(
        column(width = 12, class = "action-btn-row",
               actionButton("apply_bias_btn",
                            label = "Apply Bias",
                            class = "btn-continue"))
      )
  )
})

output$bias_ell_select <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  pred_ids <- names(session_data$ellipsoid_prediction_list)
  versions <- session_data$ellipsoid_list


  ell_choices <- c(
    "All versions" = "all",
    setNames(pred_ids,
             vapply(pred_ids, function(id){
               ell <- versions[[id]]
               if(!is.null(ell) && !is.null(ell$ell_name)) ell$ell_name else id
             }, character(1)))
  )

  selectInput(inputId = "bias_ell_selected",
              label = tagList(
                tags$span("Ellipsoid version", class = "text-widget-title"),
                tags$span(icon("circle-info"),
                          title = "Select which ellipsoid version to apply bias to.",
                          class = "tooltip-icon")
              ),
              choices = ell_choices,
              selected = "all")
})


observeEvent(input$apply_bias_btn, {

  req(session_data$prepared_bias)
  req(length(session_data$ellipsoid_prediction_list) > 0)
  req(input$bias_prediction_layer)
  req(input$bias_ell_selected)

  pred_list <- session_data$ellipsoid_prediction_list
  is_all <- input$bias_ell_selected == "all"
  is_all_pred <- input$bias_prediction_layer == "all_pred"
  selected_ids <- if(is_all) names(pred_list) else input$bias_ell_selected
  direction <- if(!is.null(input$bias_effect_direction)) input$bias_effect_direction else "direct"

  first_pred <- pred_list[[selected_ids[1]]]
  all_layers <- if(inherits(first_pred, "SpatRaster")) names(first_pred) else character(0)
  target_layers <- if(is_all_pred) all_layers else input$bias_prediction_layer

  if(length(target_layers) == 0){
    showNotification("No prediction layers found.", type = "warning", duration = 4)
    return()
  }

  current_biased <- session_data$ellipsoid_prediction_list_biased
  if(is.null(current_biased)) current_biased <- list()

  n_success <- 0L
  n_skipped <- 0L

  for(id in selected_ids){

    pred <- pred_list[[id]]

    if(is.null(pred)){
      showNotification(paste0("No prediction found for ", id, ". Skipping."),
                       type = "warning", duration = 4)
      next
    }

    for(layer in target_layers){

      if(!layer %in% names(pred)){
        showNotification(paste0("Layer '", layer, "' not found for ", id, ". Skipping."),
                         type = "warning", duration = 4)
        next
      }

      direction_suffix <- paste0(layer, "_biased_", direction)

      # Check by raster layer name inside the stacked SpatRaster
      already_exists <- id %in% names(current_biased) &&
        direction_suffix %in% names(current_biased[[id]])

      if(already_exists){
        message("'", direction_suffix, "' already exists for ", id, ". Skipping.")
        n_skipped <- n_skipped + 1L
        next
      }

      prev_n <- length(current_biased)
      prev_layers <- if(id %in% names(current_biased)) terra::nlyr(current_biased[[id]]) else 0L

      current_biased <- apply_bias_to_list(
        biased_list = current_biased,
        ell_id = id,
        pred_rast = pred,
        layer = layer,
        prepared_bias = session_data$prepared_bias,
        direction = direction
      )

      new_layers <- if(id %in% names(current_biased)) terra::nlyr(current_biased[[id]]) else 0L

      if(new_layers > prev_layers){
        n_success <- n_success + 1L
      } else {
        n_skipped <- n_skipped + 1L
      }
    }
  }

  if(n_success == 0 && n_skipped > 0){
    showNotification(paste0("All selected combinations already exist. No new layers added."),
                     type = "warning", duration = 4)
    return()
  }

  if(n_success == 0){
    showNotification("Bias application failed for all selected combinations.",
                     type = "error", duration = 4)
    return()
  }

  session_data$ellipsoid_prediction_list_biased <- current_biased
  show_apply_bias_form(FALSE)

  msg <- paste0(n_success, " new biased layer(s) added.")
  if(n_skipped > 0) msg <- paste0(msg, " ", n_skipped, " already existed and were skipped.")
  showNotification(msg, type = "message", duration = 4)
})

observeEvent(input$edit_apply_bias, {
  show_apply_bias_form(TRUE)
})

observeEvent(input$confirm_edit_apply_bias, {
  removeModal()
  session_data$ellipsoid_prediction_list_biased <- list()
  show_apply_bias_form(FALSE)
  showNotification("Biased surfaces cleared.", type = "message", duration = 3)
})


# Ellipsoid Library -------------------------------------------------------

output$ellipsoid_library_bias <- renderUI({
  req(session_data$bias_raster)
  req(length(session_data$ellipsoid_list) > 0)

  versions <- session_data$ellipsoid_list
  ids <- names(versions)
  cur_ell <- session_data$current_ellipsoid

  if(!cur_ell$ell_id %in% names(session_data$ellipsoid_list)){
    cur_ell <- session_data$ellipsoid_list[["base"]]
  }

  is_base <- !is.null(cur_ell$ell_id) &&
    identical(cur_ell$ell_name, "base")

  # Working ellipsoid slot
  working_row <- if(!is.null(cur_ell)){
    fluidRow(
      style = "background: #f0f7f0; border-radius: 4px; margin-bottom: 6px; padding: 4px 0;",
      column(width = 4,
             tags$span(icon("eye"),
                       tags$span(paste0(" ", cur_ell$ell_name),
                                 class = "text-widget-inner",
                                 style = "color: #097a21; font-weight: 500;"),
                       tags$span("(current)",
                                 style = "font-size: 11px; color: #aaa;"))),
      column(width = 3,
             tags$span(cur_ell$ell_id,
                       style = "font-size: 11px; color: #aaa;")),

      column(width = 5,
             p("view only")
      )
    )
  }

  rows <- lapply(ids, function(id){

    ell <- versions[[id]]
    is_base <- id == "base"

    fluidRow(
      style = "padding: 2px 0;",
      column(width = 5,
             tags$span(ell$ell_name, class = "text-widget-inner")),
      column(width = 4,
             tags$span(id, style = "font-size: 11px; color: #aaa;")),
      column(width = 3,
             tags$div(
               style = "display: flex; gap: 8px;",

               tagList(
                 actionLink(
                   inputId = paste0("ell_view_bias_", id),
                   label = tags$span(icon("eye"),
                                     title = paste0("View ", ell$ell_name),
                                     class = "tooltip-icon")
                 ),

                 if(isFALSE(is_base)){
                   actionLink(
                     inputId = paste0("ell_delete_bias_", id),
                     label = tags$span(icon("trash"),
                                       title = paste0("Delete ", ell$ell_name),
                                       class = "tooltip-icon",
                                       style = "color: #e74c3c;")
                   )
                 }
               )
             ))
    )
  })

  box(title = tags$span("Ellipsoid library", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,

      if(!is.null(cur_ell)){
        tagList(
          working_row,
          tags$hr(style = "margin: 8px 0;")
        )
      },

      fluidRow(
        column(width = 5, tags$span("Name", class = "text-widget-title")),
        column(width = 4, tags$span("ID", class = "text-widget-title")),
        column(width = 3, tags$span("Actions", class = "text-widget-title"))
      ),

      tagList(rows)
  )
})

# Load a saved ellipsoid for viewing bias
observeEvent({
  ids <- names(session_data$ellipsoid_list)
  lapply(ids, function(id) input[[paste0("ell_view_bias_", id)]])
}, {
  ids <- names(session_data$ellipsoid_list)
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("ell_view_bias_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  id <- clicked[1]
  ell <- session_data$ellipsoid_list[[id]]

  session_data$current_ellipsoid <- ell

  showNotification(paste0(ell$ell_name, " loaded for prediction viewing"),
                   type = "message", duration = 3)

}, ignoreInit = TRUE)

# Delete a saved ellipsoid
observeEvent({
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(ids, function(id) input[[paste0("ell_delete_bias_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("ell_delete_bias_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  id <- clicked[1]
  nm <- session_data$ellipsoid_list[[id]]$ell_name

  showModal(modalDialog(
    title = paste0("Delete ", nm, "?"),
    p(paste0("This will permanently remove ", nm,
             " and any prediction results associated with it.")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0("confirm_ell_delete_bias_", id),
                   "Yes, delete",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))

}, ignoreInit = TRUE)

# Confirmed delete
observeEvent({
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(ids, function(id) input[[paste0("confirm_ell_delete_bias_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("confirm_ell_delete_bias_", id)]]
    !is.null(val) && val > 0
  }, logical(1))]

  req(length(clicked) > 0)
  id <- clicked[1]
  nm <- session_data$ellipsoid_list[[id]]$ell_name

  removeModal()

  session_data$ellipsoid_list[[id]] <- NULL

  if(!is.null(session_data$ellipsoid_prediction_list[[id]])){
    session_data$ellipsoid_prediction_list[[id]] <- NULL
  }

  if(!is.null(session_data$ellipsoid_prediction_list_biased[[id]])){
    session_data$ellipsoid_prediction_list_biased[[id]] <- NULL
  }

  # If the deleted version was the current working ellipsoid, move back to base
  if(identical(session_data$current_ellipsoid$ell_id, id)){
    session_data$current_ellipsoid <- session_data$ellipsoid_list[["base"]]

    showNotification(paste0(nm, " deleted. Moved to viewing base"),
                     type = "message", duration = 3)
  } else {
    showNotification(paste0(nm, " deleted."),
                     type = "message", duration = 3)
  }

}, ignoreInit = TRUE)


