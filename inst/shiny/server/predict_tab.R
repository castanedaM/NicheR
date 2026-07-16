# Title: Title: Predict Tab server

# Description: Server for the predict tab, it take theelliposid of more
# ellipsoid to predict over

# Date Last Updated: 7/17/26


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

observeEvent(input$ell_predict, {

  req(length(session_data$ellipsoid_list) > 0)
  req(input$pred_ell_selected)

  versions <- session_data$ellipsoid_list
  req(length(versions) > 0)

  newdata <- if(!is.null(session_data$bg_raster)){
    terra::subset(session_data$bg_raster, session_data$vars)
  } else if(!is.null(session_data$bg_df)){
    session_data$bg_df
  } else {
    showNotification("No background data available for prediction.",
                     type = "error", duration = 4)
    return()
  }

  is_all <- input$pred_ell_selected == "all"

  trunc_val <- input$adjust_trunc
  use_trunc <- !is.null(trunc_val) && is.finite(trunc_val) && trunc_val != versions$base$cl

  if(is_all){

    result_stacked <- setNames(
      lapply(names(versions), function(id){
        ell  <- versions[[id]]
        pred <- tryCatch(
          predict(ell,
                  newdata = newdata,
                  adjust_truncation_level = if(use_trunc) trunc_val else NULL,
                  include_suitability = isTRUE(input$pred_suitability),
                  suitability_truncated = isTRUE(input$pred_suitability_trunc),
                  include_mahalanobis = isTRUE(input$pred_mahalanobis),
                  mahalanobis_truncated = isTRUE(input$pred_mahalanobis_trunc),
                  verbose = FALSE),
          error = function(e){
            showNotification(paste("Ellipsoid prediction failed:", e$message),
                             type = "error", duration = 4)
            return(NULL)
          }
        )
        if(is.null(pred)) return(NULL)
        Reduce(c, pred)
      }),
      vapply(names(versions), function(id) versions[[id]]$ell_id, character(1))
    )

    req(result_stacked)

    session_data$ellipsoid_prediction_list <- result_stacked
     showNotification("Batch prediction completed.", type = "message", duration = 4)

  } else {

    id <- input$pred_ell_selected
    ell <- versions[[id]]
    req(ell)

    result <- tryCatch(
      predict(ell,
              newdata = newdata,
              adjust_truncation_level = if(use_trunc) trunc_val else NULL,
              include_suitability = isTRUE(input$pred_suitability),
              suitability_truncated = isTRUE(input$pred_suitability_trunc),
              include_mahalanobis = isTRUE(input$pred_mahalanobis),
              mahalanobis_truncated = isTRUE(input$pred_mahalanobis_trunc),
              verbose = FALSE),
      error = function(e){
        showNotification(paste("Prediction failed:", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )

    req(result)


    result_stacked <- lapply(result, function(ell){
      Reduce(c, ell)
    })

    session_data$ellipsoid_prediction_list[[id]] <- result_stacked
    showNotification(paste0(ell$ell_name, ": prediction completed."),
                     type = "message", duration = 4)
  }

})

output$ellipsoid_library_pred <- renderUI({

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
                                 class  = "text-widget-inner",
                                 style  = "color: #097a21; font-weight: 500;"),
                       tags$span("(current)",
                                 style  = "font-size: 11px; color: #aaa;"))),
      column(width = 3,
             tags$span(cur_ell$ell_id,
                       style = "font-size: 11px; color: #aaa;")),

      column(width = 5,
              p("View Only")
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
                   inputId = paste0("ell_view_pred_", id),
                   label = tags$span(icon("eye"),
                                     title = paste0("View ", ell$ell_name),
                                     class = "tooltip-icon")
                 ),

                 if(isFALSE(is_base)){
                   actionLink(
                     inputId = paste0("ell_delete_pred_", id),
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

# Load a saved ellipsoid for viewing pred
observeEvent({
  ids <- names(session_data$ellipsoid_list)
  lapply(ids, function(id) input[[paste0("ell_view_pred_", id)]])
}, {
  ids <- names(session_data$ellipsoid_list)
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("ell_view_pred_", id)]]
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
  lapply(ids, function(id) input[[paste0("ell_delete_pred_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("ell_delete_pred_", id)]]
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
      actionButton(paste0("confirm_ell_delete_pred_", id),
                   "Yes, delete",
                   class = "btn-warning")
    ),
    easyClose = FALSE
  ))

}, ignoreInit = TRUE)

# Confirmed delete
observeEvent({
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  lapply(ids, function(id) input[[paste0("confirm_ell_delete_pred_", id)]])
}, {
  ids <- setdiff(names(session_data$ellipsoid_list), "base")
  req(length(ids) > 0)

  clicked <- ids[vapply(ids, function(id){
    val <- input[[paste0("confirm_ell_delete_pred_", id)]]
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
