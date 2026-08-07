# Title: Predict Tab server

# Description: Server for the predict tab. Projects one or more saved
# ellipsoids onto the background data, and lists the saved ellipsoids in a
# view-only library.

# Date Last Updated: 08/05/2026


output$predict_next_step_ui <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  div(class = "action-btn-row",
      actionButton(inputId = "predict_next_step_btn",
                   label = tagList(tags$span("Continue",
                                             class = "text-widget-title"),
                                   icon("arrow-right")),
                   class = "btn-save")
  )

})

observeEvent(input$predict_next_step_btn, {
  updateTabItems(session, "sidebar_menu", selected = "bias_tab")
})

output$predict_ellipsoid_selector_ui <- renderUI({

  req(length(session_data$ellipsoid_list) > 0)

  if(identical(session_data$input_mode, "virtual")){
    return(p(instructions$predict_virtual_unavailable, class = "text-instruction"))
  }

  versions <- session_data$ellipsoid_list

  ell_choices <- c("All versions" = "all",
                   setNames(names(versions),
                            vapply(versions,
                                   function(ell) ell$ell_name,
                                   character(1))))

  # Keep the current choice across re-renders, otherwise saving an
  # ellipsoid on the Build tab resets this back to All versions
  keep <- if(!is.null(input$predict_ellipsoid_selected) &&
             input$predict_ellipsoid_selected %in% ell_choices){
    input$predict_ellipsoid_selected
  } else {
    "all"
  }

  selectInput(inputId = "predict_ellipsoid_selected",
              label = tagList(
                tags$span("Ellipsoid Version", class = "text-widget-title"),
                tags$span(icon("circle-info"),
                          title = instructions$predict_ellipsoid_select_tooltip,
                          class = "tooltip-icon")
              ),
              choices  = ell_choices,
              selected = keep)
})

observeEvent(input$predict_run_btn, {

  req(length(session_data$ellipsoid_list) > 0)
  req(input$predict_ellipsoid_selected)

  versions <- session_data$ellipsoid_list

  has_raster <- !is.null(session_data$bg_raster)

  if(!has_raster && is.null(session_data$bg_df)){
    showNotification("No background data available for prediction.",
                     type = "error", duration = 4)
    return()
  }

  layers <- c(isTRUE(input$predict_suitability),
              isTRUE(input$predict_suitability_trunc),
              isTRUE(input$predict_mahalanobis),
              isTRUE(input$predict_mahalanobis_trunc))

  if(!any(layers)){
    showNotification(instructions$predict_no_layers,
                     type = "warning", duration = 5)
    return()
  }

  trunc_val <- input$predict_adjust_trunc

  # Each ellipsoid predicts on its own variables rather than on
  # session_data$vars, so a version built from a different set still works
  predict_one <- function(ell){

    newdata <- if(has_raster){
      terra::subset(session_data$bg_raster, ell$var_names)
    } else {
      session_data$bg_df[, ell$var_names, drop = FALSE]
    }

    use_trunc <- !is.null(trunc_val) && is.finite(trunc_val) &&
      !isTRUE(all.equal(trunc_val, ell$cl))

    tryCatch(
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
                         type = "error", duration = 5)
        NULL
      }
    )
  }

  ids <- if(input$predict_ellipsoid_selected == "all"){
    names(versions)
  } else {
    input$predict_ellipsoid_selected
  }

  n_ok <- 0L

  # Assigned per id rather than replacing the whole list, so one failed
  # ellipsoid does not discard predictions that already succeeded
  for(id in ids){

    ell <- versions[[id]]
    if(is.null(ell)) next

    pred <- predict_one(ell)
    if(is.null(pred)) next

    # Always a single stacked SpatRaster, which is the shape every
    # downstream tab checks for
    session_data$ellipsoid_prediction_list[[id]] <- if(has_raster){
      Reduce(c, pred)
    } else {
      pred
    }

    # A new prediction invalidates anything derived from the old one
    session_data$ellipsoid_prediction_list_biased[[id]] <- NULL
    session_data$ellipsoid_occurrence_list[[id]] <- NULL

    n_ok <- n_ok + 1L
  }

  if(n_ok == 0L){
    showNotification("No predictions were completed.",
                     type = "error", duration = 4)
    return()
  }

  msg <- if(length(ids) > 1){
    paste0("Prediction completed for ", n_ok, " of ", length(ids), " ellipsoids.")
  } else {
    paste0(versions[[ids]]$ell_name, ": prediction completed.")
  }

  showNotification(msg, type = "message", duration = 4)
})


# ELLIPSOID LIBRARY -------------------------------------------------------

output$predict_ellipsoid_library_ui <- renderUI({

  cur_ell <- session_data$current_ellipsoid
  versions <- session_data$ellipsoid_list
  ids <- names(versions)

  req(!is.null(cur_ell) || length(ids) > 0)

  predicted <- names(session_data$ellipsoid_prediction_list)

  # Working slot, the ellipsoid the plots on this tab use. Read-only here,
  # editing happens on the Build tab.
  working_row <- if(!is.null(cur_ell)){
    fluidRow(
      class = "ell-row",
      style = "background: #f0f7f0; border-radius: 4px; margin-bottom: 6px; padding: 4px 0;",
      column(width = 5,
             tags$span(icon("eye"),
                       tags$span(paste0(" ", cur_ell$ell_name),
                                 class = "text-widget-inner",
                                 style = "color: #097a21; font-weight: 500;"))),
      column(width = 4,
             tags$span(ell_lineage_label(cur_ell),
                       style = "font-size: 11px; color: #aaa;")),
      column(width = 3,
             tags$span("View-only", style = "font-size: 11px; color: #aaa;"))
    )
  }

  rows <- lapply(ids, function(id){

    ell <- versions[[id]]
    has_pred <- id %in% predicted

    fluidRow(
      class = "ell-row",
      style = "padding: 2px 0;",
      column(width = 5,
             tags$span(ell$ell_name, class = "text-widget-inner"),
             tags$br(),
             tags$span(id, style = "font-size: 10px; color: #bbb;")),
      column(width = 4,
             tags$span(ell_lineage_label(ell),
                       style = "font-size: 11px; color: #aaa;"),
             tags$br(),
             tags$span(if(has_pred) "predicted" else "not predicted",
                       style = paste0("font-size: 10px; color: ",
                                      if(has_pred) "#097a21;" else "#bbb;"))),
      column(width = 3,
             class = "ell-actions",
             tags$a(href = "#",
                    onclick = sprintf("Shiny.setInputValue('predict_ell_view', '%s', {priority: 'event'}); return false;", id),
                    title = paste0("View ", ell$ell_name, " (read-only)"),
                    icon("eye")),
             tags$a(href = "#",
                    class = "ell-action-danger",
                    onclick = sprintf("Shiny.setInputValue('predict_ell_delete', '%s', {priority: 'event'}); return false;", id),
                    title = paste0("Delete ", ell$ell_name),
                    icon("trash-can")))
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
            class = "ell-row",
            style = "padding: 2px 0;",
            column(width = 5, tags$span("Name", class = "text-widget-title")),
            column(width = 4, tags$span("Built from", class = "text-widget-title")),
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

  n_children <- sum(vapply(session_data$ellipsoid_list, function(e){
    identical(e$parent_id, ell$ell_id)
  }, logical(1)))

  showModal(modalDialog(
    title = paste0("Delete ", ell$ell_name, "?"),
    p(instructions$predict_delete_ell, class = "text-instruction"),
    if(n_children > 0){
      p(paste0(n_children, " ellipsoid(s) were copied from this one. ",
               "They will be kept, but will no longer have a parent."),
        class = "text-muted-small")
    },
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
  session_data$ellipsoid_occurrence_list[[id]] <- NULL
  session_data$pending_ell_delete <- NULL

  # Copies of the deleted ellipsoid, captured before reparenting so the
  # message reports only what this delete changed
  orphaned <- names(session_data$ellipsoid_list)[
    vapply(session_data$ellipsoid_list,
           function(e) identical(e$parent_id, id), logical(1))]

  session_data$ellipsoid_list <- lapply(session_data$ellipsoid_list, function(e){
    if(identical(e$parent_id, id)) e$parent_id <- NULL
    e
  })

  dbg("DELETE ", id, "  reparented to root: ",
      if(length(orphaned) == 0) "none" else paste(orphaned, collapse = ", "))

  cur <- session_data$current_ellipsoid

  if(identical(cur$ell_id, id)){
    clear_working_ellipsoid()
    showNotification(paste0(nm, " deleted. Go back to Build to create a new ellipsoid."),
                     type = "message", duration = 4)
    return()
  }

  if(identical(cur$parent_id, id)){
    cur$parent_id <- NULL
    session_data$current_ellipsoid <- cur
  }

  showNotification(paste0(nm, " deleted."),
                   type = "message", duration = 3)
})
