# Title: Predict Tab server

# Description: Server for the predict tab. Projects one or more saved
# ellipsoids onto the background data, and lists the saved ellipsoids in a
# view-only library.

# Date Last Updated: 08/04/2026


output$predict_save_ellipsoid_ui <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  div(class = "action-btn-row",
      actionButton(inputId = "predict_next_step_btn",
                   label = "Continue",
                   class = "btn-save")
  )

})

observeEvent(input$predict_next_step_btn, {
  removeModal()
  updateTabItems(session, "sidebar_menu", selected = "bias_tab")
})

output$predict_ellipsoid_selector_ui <- renderUI({

  req(length(session_data$ellipsoid_list) > 0)

  versions <- session_data$ellipsoid_list

  ell_choices <- c("All versions" = "all",
                   setNames(names(versions),
                            vapply(versions,
                                   function(ell) ell$ell_name,
                                   character(1))))

  selectInput(inputId = "predict_ellipsoid_selected",
              label = tagList(
                tags$span("Ellipsoid Version", class = "text-widget-title"),
                tags$span(icon("circle-info"),
                          title = instructions$predict_ellipsoid_select_tooltip,
                          class = "tooltip-icon")
              ),
              choices  = ell_choices,
              selected = "all")
})

observeEvent(input$predict_run_btn, {

  req(length(session_data$ellipsoid_list) > 0)
  req(input$predict_ellipsoid_selected)

  versions <- session_data$ellipsoid_list

  newdata <- if(!is.null(session_data$bg_raster)){
    terra::subset(session_data$bg_raster, session_data$vars)
  } else if(!is.null(session_data$bg_df)){
    session_data$bg_df
  } else {
    showNotification("No background data available for prediction.",
                     type = "error", duration = 4)
    return()
  }

  is_all <- input$predict_ellipsoid_selected == "all"

  trunc_val <- input$predict_adjust_trunc

  if(is_all){

    n_ok <- 0L

    # Assigned per id rather than replacing the whole list, so one failed
    # ellipsoid does not discard predictions that already succeeded
    for(id in names(versions)){

      ell <- versions[[id]]

      use_trunc <- !is.null(trunc_val) && is.finite(trunc_val) &&
        !isTRUE(all.equal(trunc_val, ell$cl))

      pred <- tryCatch(
        predict(ell,
                newdata = newdata,
                adjust_truncation_level = if(use_trunc) trunc_val else NULL,
                include_suitability = isTRUE(input$predict_suitability),
                suitability_truncated = isTRUE(input$predict_suitability_trunc),
                include_mahalanobis = isTRUE(input$predict_mahalanobis),
                mahalanobis_truncated = isTRUE(input$predict_mahalanobis_trunc),
                verbose = FALSE),
        error = function(e){
          showNotification(paste0(ell$ell_name, " prediction failed: ", e$message),
                           type = "error", duration = 4)
          NULL
        }
      )

      if(is.null(pred)) next

      session_data$ellipsoid_prediction_list[[id]] <- Reduce(c, pred)
      n_ok <- n_ok + 1L
    }

    req(n_ok > 0)

    showNotification(paste0("Batch prediction completed for ", n_ok,
                            " of ", length(versions), " ellipsoids."),
                     type = "message", duration = 4)

  } else {

    id <- input$predict_ellipsoid_selected
    ell <- versions[[id]]
    req(ell)

    use_trunc <- !is.null(trunc_val) && is.finite(trunc_val) &&
      !isTRUE(all.equal(trunc_val, ell$cl))

    result <- tryCatch(
      predict(ell,
              newdata = newdata,
              adjust_truncation_level = if(use_trunc) trunc_val else NULL,
              include_suitability = isTRUE(input$predict_suitability),
              suitability_truncated = isTRUE(input$predict_suitability_trunc),
              include_mahalanobis = isTRUE(input$predict_mahalanobis),
              mahalanobis_truncated = isTRUE(input$predict_mahalanobis_trunc),
              verbose = FALSE),
      error = function(e){
        showNotification(paste("Prediction failed:", e$message),
                         type = "error", duration = 4)
        NULL
      }
    )

    req(result)

    # Same shape as the batch branch: a single stacked SpatRaster, which is
    # what every downstream consumer checks for
    session_data$ellipsoid_prediction_list[[id]] <- Reduce(c, result)

    showNotification(paste0(ell$ell_name, ": prediction completed."),
                     type = "message", duration = 4)
  }

})


# ELLIPSOID LIBRARY -------------------------------------------------------

output$predict_ellipsoid_library_ui <- renderUI({

  cur_ell <- session_data$current_ellipsoid
  versions <- session_data$ellipsoid_list
  ids <- names(versions)

  req(!is.null(cur_ell) || length(ids) > 0)

  # Working slot, the ellipsoid every plot on this tab uses. Read-only here,
  # editing happens on the Build tab.
  working_row <- if(!is.null(cur_ell)){
    fluidRow(
      style = "background: #f0f7f0; border-radius: 4px; margin-bottom: 6px; padding: 4px 0;",
      column(width = 5,
             tags$span(icon("eye"),
                       tags$span(paste0(" ", cur_ell$ell_name),
                                 class = "text-widget-inner",
                                 style = "color: #097a21; font-weight: 500;"))),
      column(width = 4,
             tags$span(cur_ell$ell_id,
                       style = "font-size: 11px; color: #aaa;")),
      column(width = 3,
             tags$span("View-only", style = "font-size: 11px; color: #aaa;"))
    )
  }

  rows <- lapply(ids, function(id){

    ell <- versions[[id]]

    fluidRow(
      style = "padding: 2px 0;",
      column(width = 5,
             tags$span(ell$ell_name, class = "text-widget-inner")),
      column(width = 4,
             tags$span(id, style = "font-size: 11px; color: #aaa;")),
      column(width = 3,
             tags$div(
               style = "display: flex; gap: 8px;",

               tags$a(href = "#",
                      onclick = sprintf("Shiny.setInputValue('predict_ell_view', '%s', {priority: 'event'}); return false;", id),
                      tags$span(icon("eye"),
                                title = paste0("View ", ell$ell_name, " (read-only)"),
                                class = "tooltip-icon")),

               tags$a(href = "#",
                      onclick = sprintf("Shiny.setInputValue('predict_ell_delete', '%s', {priority: 'event'}); return false;", id),
                      tags$span(icon("trash"),
                                title = paste0("Delete ", ell$ell_name),
                                class = "tooltip-icon",
                                style = "color: #e74c3c;"))
             ))
    )
  })

  box(title = tags$span("Ellipsoid library", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$predict_library, class = "text-instruction"),

      if(!is.null(cur_ell)){
        tagList(working_row, tags$hr(style = "margin: 8px 0;"))
      },

      if(length(ids) > 0){
        tagList(
          fluidRow(
            column(width = 5, tags$span("Name", class = "text-widget-title")),
            column(width = 4, tags$span("ID", class = "text-widget-title")),
            column(width = 3, tags$span("Actions", class = "text-widget-title"))
          ),
          tagList(rows)
        )
      } else {
        p(instructions$predict_library_empty, class = "text-muted-small")
      }
  )
})

# View, read-only
observeEvent(input$predict_ell_view, {

  ell <- session_data$ellipsoid_list[[input$predict_ell_view]]
  req(ell)

  set_working_ellipsoid(ell, mode = "view")

  showNotification(paste0("Viewing ", ell$ell_name, "."),
                   type = "message", duration = 3)
})

# Delete, asks first
observeEvent(input$predict_ell_delete, {

  ell <- session_data$ellipsoid_list[[input$predict_ell_delete]]
  req(ell)

  session_data$pending_ell_delete <- input$predict_ell_delete

  showModal(modalDialog(
    title = paste0("Delete ", ell$ell_name, "?"),
    p(instructions$predict_delete_ell, class = "text-instruction"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("predict_confirm_ell_delete_btn",
                   "Yes, delete",
                   class = "btn-cancel")
    ),
    easyClose = FALSE
  ))
})

observeEvent(input$predict_confirm_ell_delete_btn, {

  id <- session_data$pending_ell_delete
  req(id)

  nm <- session_data$ellipsoid_list[[id]]$ell_name

  removeModal()

  session_data$ellipsoid_list[[id]] <- NULL
  session_data$ellipsoid_prediction_list[[id]] <- NULL
  session_data$ellipsoid_prediction_list_biased[[id]] <- NULL
  session_data$pending_ell_delete <- NULL

  # If the deleted version was the comparison reference, fall back to origin
  if(identical(session_data$reference_ellipsoid$ell_id, id)){
    session_data$reference_ellipsoid <- session_data$origin_ellipsoid
  }

  # If the deleted version was in the working slot, clear the slot
  if(identical(session_data$current_ellipsoid$ell_id, id)){
    session_data$current_ellipsoid <- NULL
    session_data$origin_ellipsoid <- NULL
    session_data$reference_ellipsoid <- NULL
    ell_mode("edit")

    covariance_set(FALSE)
    centroid_set(FALSE)

    showNotification(paste0(nm, " deleted. Go back to Build to create a new ellipsoid."),
                     type = "message", duration = 4)
  } else {
    showNotification(paste0(nm, " deleted."),
                     type = "message", duration = 3)
  }
})
