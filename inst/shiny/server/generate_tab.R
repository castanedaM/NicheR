# Title: Generate Tab server

# Description: Server for the generate tab. Samples virtual occurrence
# points from a prediction surface, biased or unbiased, for one or more
# saved ellipsoids.

# Date Last Updated: 08/05/2026


# CONTROLS ----------------------------------------------------------------

# TRUE while the form is open on top of an existing result, so the user can
# regenerate without losing what is already there until they confirm.
generate_show_form <- reactiveVal(FALSE)

output$generate_controls_ui <- renderUI({

  has_pred <- length(session_data$ellipsoid_prediction_list) > 0

  if(!has_pred){
    return(
      box(title = tags$span("Generate occurrences", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          p(instructions$generate_needs_prediction, class = "text-instruction"))
    )
  }

  occ <- session_data$ellipsoid_occurrence_list
  has_occ <- length(occ) > 0

  if(has_occ && !isTRUE(generate_show_form())){

    n_sets <- sum(vapply(occ, length, integer(1)))
    n_pts <- sum(vapply(occ, function(ell_res){
      sum(vapply(ell_res, function(df){
        if(is.null(df)) 0L else nrow(df)
      }, integer(1)))
    }, integer(1)))

    return(
      box(title = tags$span("Generate occurrences", class = "text-section-header"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          p(paste0(n_pts, " occurrence(s) across ", n_sets, " set(s) from ",
                   length(occ), " ellipsoid(s)."),
            class = "text-instruction"),
          fluidRow(
            column(width = 6, class = "btn-spaced",
                   actionLink("generate_edit_link",
                              label = tagList(icon("pen"), "Generate again"))),
            column(width = 6, class = "btn-spaced",
                   downloadButton("generate_download_btn",
                                  "Download occurrences",
                                  class = "btn-save"))
          )
      )
    )
  }

  box(title = tags$span("Generate occurrences", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$generate_intro, class = "text-instruction"),

      uiOutput("generate_ellipsoid_selector_ui"),

      fluidRow(
        column(width = 6,
               tags$span("Number of occurrences", class = "text-widget-title"),
               numericInput("generate_n_occ",
                            label = NULL,
                            value = 100,
                            min = 1,
                            max = 1000000,
                            step = 10))
      ),

      fluidRow(
        column(width = 12,
               tags$div(class = "tooltip-label-row",
                        tags$span("Sampling strategy", class = "text-widget-title"),
                        tags$span(icon("circle-info"),
                                  title = instructions$generate_sampling_tooltip,
                                  class = "tooltip-icon")),
               radioButtons("generate_sampling",
                            label = NULL,
                            choiceNames = list(
                              tags$span("Centroid", class = "text-widget-inner"),
                              tags$span("Edge", class = "text-widget-inner"),
                              tags$span("Random", class = "text-widget-inner")
                            ),
                            choiceValues = c("centroid", "edge", "random"),
                            selected = "centroid",
                            inline = TRUE))
      ),

      fluidRow(
        column(width = 12,
               tags$div(class = "tooltip-label-row",
                        tags$span("Prediction surface", class = "text-widget-title"),
                        tags$span(icon("circle-info"),
                                  title = instructions$generate_surface_tooltip,
                                  class = "tooltip-icon")),
               uiOutput("generate_surface_ui"))
      ),

      uiOutput("generate_method_msg_ui"),

      fluidRow(
        column(width = 12,
               tags$div(class = "tooltip-label-row",
                        tags$span("Strict filtering", class = "text-widget-title"),
                        tags$span(icon("circle-info"),
                                  title = instructions$generate_strict_tooltip,
                                  class = "tooltip-icon")),
               radioButtons("generate_strict",
                            label = NULL,
                            choiceNames = list("True", "False"),
                            choiceValues = c("TRUE", "FALSE"),
                            selected = "TRUE",
                            inline = TRUE))
      ),

      box(title = tagList(
        tags$span("Advanced settings", class = "text-section-header"),
        tags$span(icon("circle-info"),
                  title = instructions$generate_advanced_tooltip,
                  class = "tooltip-icon")),
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,

        fluidRow(
          column(width = 6,
                 fileInput("generate_mask_file",
                           label = tags$span("Sampling mask (optional)",
                                             class = "text-widget-title"),
                           multiple = FALSE,
                           accept = c(".tif", ".tiff", ".rds"))),
          column(width = 6,
                 tags$div(class = "tooltip-label-row",
                          tags$span("Random seed", class = "text-widget-title"),
                          tags$span(icon("circle-info"),
                                    title = instructions$generate_seed_tooltip,
                                    class = "tooltip-icon")),
                 numericInput("generate_seed",
                              label = NULL,
                              value = 123,
                              min = 1,
                              step = 1))
        ),
        p(instructions$generate_mask, class = "text-instruction")
      ),

      fluidRow(
        column(width = 12,
               div(class = "action-btn-row",
                   actionButton("generate_run_btn",
                                tagList(icon("play"), "Generate"),
                                class = "btn-continue"))
        )
      )
  )
})

output$generate_ellipsoid_selector_ui <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  versions <- session_data$ellipsoid_list
  pred_ids <- names(session_data$ellipsoid_prediction_list)

  ell_choices <- c(
    "All versions" = "all",
    setNames(pred_ids,
             vapply(pred_ids, function(id){
               ell <- versions[[id]]
               if(!is.null(ell) && !is.null(ell$ell_name)) ell$ell_name else id
             }, character(1)))
  )

  keep <- if(!is.null(input$generate_ellipsoid_selected) &&
             input$generate_ellipsoid_selected %in% ell_choices){
    input$generate_ellipsoid_selected
  } else {
    "all"
  }

  selectInput(inputId = "generate_ellipsoid_selected",
              label = tagList(
                tags$span("Ellipsoid version", class = "text-widget-title"),
                tags$span(icon("circle-info"),
                          title = instructions$generate_ellipsoid_select_tooltip,
                          class = "tooltip-icon")
              ),
              choices = ell_choices,
              selected = keep)
})

# Picking a specific version loads it into the working slot so the plots
# follow. "All versions" leaves the slot alone, so the library keeps control.
observeEvent(input$generate_ellipsoid_selected, {

  sel <- input$generate_ellipsoid_selected
  req(sel)

  if(identical(sel, "all")) return()

  ell <- session_data$ellipsoid_list[[sel]]
  req(ell)

  if(identical(session_data$current_ellipsoid$ell_id, sel)) return()

  set_working_ellipsoid(ell, mode = "view")
})

output$generate_surface_ui <- renderUI({

  req(length(session_data$ellipsoid_prediction_list) > 0)

  pred_list <- session_data$ellipsoid_prediction_list
  bias_list <- session_data$ellipsoid_prediction_list_biased

  sel <- input$generate_ellipsoid_selected

  ids <- if(!is.null(sel) && !identical(sel, "all")) sel else names(pred_list)

  layer_names <- function(lst){
    unique(unlist(lapply(ids, function(id){
      r <- lst[[id]]
      if(inherits(r, "SpatRaster")) names(r) else character(0)
    })))
  }

  unbiased_lyrs <- layer_names(pred_list)
  bias_lyrs <- layer_names(bias_list)

  all_layers <- unique(c(unbiased_lyrs, bias_lyrs))
  req(length(all_layers) > 0)

  keep <- if(!is.null(input$generate_surface) &&
             any(input$generate_surface %in% all_layers)){
    intersect(input$generate_surface, all_layers)
  } else if("suitability_trunc" %in% unbiased_lyrs){
    "suitability_trunc"
  } else if(length(unbiased_lyrs) > 0){
    unbiased_lyrs[1]
  } else {
    all_layers[1]
  }

  checkboxGroupInput("generate_surface",
                     label = NULL,
                     choiceNames = lapply(all_layers, function(nm){
                       is_biased <- nm %in% bias_lyrs && !nm %in% unbiased_lyrs
                       tags$span(nm,
                                 style = if(is_biased) "color: #c47c16;" else "",
                                 class = "text-widget-inner")
                     }),
                     choiceValues = all_layers,
                     selected = keep,
                     inline = TRUE)
})

# Mirrors how generate_occ_for_ell picks a method, so the user sees what
# will happen before pressing Generate
output$generate_method_msg_ui <- renderUI({

  req(input$generate_surface)
  req(length(input$generate_surface) > 0)

  methods <- unique(vapply(input$generate_surface, function(layer){
    if(grepl("mahalanobis", layer, ignore.case = TRUE)){
      "mahalanobis"
    } else {
      "suitability"
    }
  }, character(1)))

  fluidRow(
    column(width = 12,
           tags$p(icon("circle-info"), " ",
                  paste0("Method(s) detected: ", paste(methods, collapse = ", "), "."),
                  style = "font-size: 10px; color: #aaa; margin: 4px 0 8px;"))
  )
})


# GENERATE ----------------------------------------------------------------

observeEvent(input$generate_run_btn, {

  req(input$generate_ellipsoid_selected)
  req(input$generate_n_occ)
  req(input$generate_sampling)

  if(is.null(input$generate_surface) || length(input$generate_surface) == 0){
    showNotification(instructions$generate_no_surface,
                     type = "warning", duration = 5)
    return()
  }

  pred_list <- session_data$ellipsoid_prediction_list
  bias_list <- session_data$ellipsoid_prediction_list_biased

  selected_ids <- if(identical(input$generate_ellipsoid_selected, "all")){
    names(pred_list)
  } else {
    input$generate_ellipsoid_selected
  }

  target_layers <- input$generate_surface
  n_occ <- as.integer(input$generate_n_occ)
  sampling <- input$generate_sampling

  # Default to TRUE rather than NULL if the radio has not reported yet
  strict <- !identical(input$generate_strict, "FALSE")

  seed <- if(!is.null(input$generate_seed) && is.finite(input$generate_seed)){
    as.integer(input$generate_seed)
  } else {
    123L
  }

  sampling_mask <- if(!is.null(input$generate_mask_file)){
    ext <- tolower(tools::file_ext(input$generate_mask_file$name))
    tryCatch(
      load_raster_file(input$generate_mask_file$datapath, ext),
      error = function(e){
        showNotification(paste("Could not load sampling mask:", e$message),
                         type = "error", duration = 5)
        NULL
      }
    )
  } else {
    NULL
  }

  session_data$sampling_mask <- sampling_mask

  n_success <- 0L
  n_attempted <- 0L

  # Assigned per id rather than replacing the whole list, so one failed
  # ellipsoid does not discard occurrences that already succeeded
  for(id in selected_ids){

    res <- generate_occ_for_ell(ell_id = id,
                                pred_list = pred_list,
                                biased_list = bias_list,
                                layers = target_layers,
                                n_occ = n_occ,
                                sampling = sampling,
                                strict = strict,
                                sampling_mask = sampling_mask,
                                seed = seed)

    n_attempted <- n_attempted + length(target_layers)

    ok <- vapply(res, function(df){
      !is.null(df) && nrow(df) > 0
    }, logical(1))

    if(length(ok) == 0 || !any(ok)) next

    session_data$ellipsoid_occurrence_list[[id]] <- res[ok]
    n_success <- n_success + sum(ok)
  }

  if(n_success == 0L){
    showNotification("Occurrence generation failed for all selected combinations.",
                     type = "error", duration = 5)
    return()
  }

  generate_show_form(FALSE)

  n_skipped <- n_attempted - n_success

  msg <- paste0(n_success, " occurrence set(s) generated.")
  if(n_skipped > 0L) msg <- paste0(msg, " ", n_skipped, " skipped.")

  showNotification(msg, type = "message", duration = 4)
})

observeEvent(input$generate_edit_link, {
  generate_show_form(TRUE)
})

# Flattens every occurrence set into one long-format table, so a single
# download covers all ellipsoids and layers
output$generate_download_btn <- downloadHandler(

  filename = function(){
    paste0("nicheR_occurrences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },

  content = function(file){

    occ <- session_data$ellipsoid_occurrence_list
    req(length(occ) > 0)

    versions <- session_data$ellipsoid_list

    rows <- lapply(names(occ), function(id){

      ell_name <- if(!is.null(versions[[id]])) versions[[id]]$ell_name else id

      lapply(names(occ[[id]]), function(layer){
        df <- occ[[id]][[layer]]
        if(is.null(df) || nrow(df) == 0) return(NULL)
        data.frame(ell_id = id,
                   ell_name = ell_name,
                   layer = layer,
                   df,
                   stringsAsFactors = FALSE)
      })
    })

    out <- do.call(rbind, unlist(rows, recursive = FALSE))
    req(!is.null(out))

    write.csv(out, file, row.names = FALSE)
  }
)


# ELLIPSOID LIBRARY -------------------------------------------------------

output$generate_ellipsoid_library_ui <- renderUI({

  cur_ell <- session_data$current_ellipsoid
  versions <- session_data$ellipsoid_list
  ids <- names(versions)

  req(!is.null(cur_ell) || length(ids) > 0)

  predicted <- names(session_data$ellipsoid_prediction_list)
  biased <- names(session_data$ellipsoid_prediction_list_biased)
  generated <- names(session_data$ellipsoid_occurrence_list)

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

    status <- if(id %in% generated){
      n <- sum(vapply(session_data$ellipsoid_occurrence_list[[id]],
                      function(df) if(is.null(df)) 0L else nrow(df),
                      integer(1)))
      list(txt = paste0(n, " occurrences"), col = "#097a21")
    } else if(id %in% biased){
      list(txt = "biased, not generated", col = "#aaa")
    } else if(id %in% predicted){
      list(txt = "predicted, not generated", col = "#aaa")
    } else {
      list(txt = "not predicted", col = "#bbb")
    }

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
             tags$span(status$txt,
                       style = paste0("font-size: 10px; color: ", status$col, ";"))),
      column(width = 3,
             class = "ell-actions",
             tags$a(href = "#",
                    onclick = sprintf("Shiny.setInputValue('generate_ell_view', '%s', {priority: 'event'}); return false;", id),
                    title = paste0("View ", ell$ell_name, " (read-only)"),
                    icon("eye")),
             tags$a(href = "#",
                    class = "ell-action-danger",
                    onclick = sprintf("Shiny.setInputValue('generate_ell_delete', '%s', {priority: 'event'}); return false;", id),
                    title = paste0("Delete ", ell$ell_name),
                    icon("trash-can")))
    )
  })

  box(title = tags$span("Ellipsoid library", class = "text-section-header"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      p(instructions$generate_library, class = "text-instruction"),

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
        p(instructions$generate_library_empty, class = "text-muted-small")
      }
  )
})

# View, read-only
observeEvent(input$generate_ell_view, {

  ell <- session_data$ellipsoid_list[[input$generate_ell_view]]
  req(ell)

  set_working_ellipsoid(ell, mode = "view")

  showNotification(paste0("Viewing ", ell$ell_name, "."),
                   type = "message", duration = 3)
})

# Delete, asks first
observeEvent(input$generate_ell_delete, {

  ell <- session_data$ellipsoid_list[[input$generate_ell_delete]]
  req(ell)

  session_data$pending_ell_delete <- input$generate_ell_delete

  n_children <- sum(vapply(session_data$ellipsoid_list, function(e){
    identical(e$parent_id, ell$ell_id)
  }, logical(1)))

  showModal(modalDialog(
    title = paste0("Delete ", ell$ell_name, "?"),
    p(instructions$generate_delete_ell, class = "text-instruction"),
    if(n_children > 0){
      p(paste0(n_children, " ellipsoid(s) were copied from this one. ",
               "They will be kept, but will no longer have a parent."),
        class = "text-muted-small")
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("generate_confirm_ell_delete_btn",
                   "Yes, delete",
                   class = "btn-cancel")
    ),
    easyClose = FALSE
  ))
})

observeEvent(input$generate_confirm_ell_delete_btn, {

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
