# Title: Helper functions
# Description: Pure functions shared across server scripts
# Date last updated: 08/03/2026

# Function to read raster files
#' Title
#'
#' @param path
#' @param ext
#'
#' @returns
#' @keywords internal
#'
#' @noRd
load_raster_file <- function(path, ext) {
  obj <- switch(ext,
                "tif" = terra::rast(path),
                "tiff" = terra::rast(path),
                "rds" = readRDS(path),
                stop("Unsupported file type: ", ext))

  if(!inherits(obj, c("SpatRaster", "RasterStack", "RasterBrick", "RasterLayer"))){
    stop("File does not contain a raster object. Found class: ", paste(class(obj), collapse = ", "))
  }

  # convert raster to terra if needed
  if(inherits(obj, c("RasterStack", "RasterBrick", "RasterLayer"))){
    obj <- terra::rast(obj)
  }

  return(obj)
}


# Function to read Df files
#' Title
#'
#' @param path
#' @param ext
#'
#' @returns
#' @keywords internal
#'
#' @noRd
load_df_file <- function(path, ext) {
  obj <- switch(ext,
                "csv" = read.csv(path),
                "rds" = readRDS(path),
                stop("Unsupported file type: ", ext))

  if(!inherits(obj, c("data.frame", "tbl_df", "tbl"))){
    stop("File does not contain a data frame. Found class: ", paste(class(obj), collapse = ", "))
  }

  return(as.data.frame(obj))
}


# Session-level counter, increments every time a new ellipsoid is created
ell_id_counter <- reactiveVal(0L)

make_ell_id <- function(){
  n <- ell_id_counter() + 1L
  ell_id_counter(n)
  paste0("E", n, "_", format(Sys.time(), "%d%m%y"))
}

tag_ellipsoid <- function(ell, name){
  ell$ell_id <- make_ell_id()
  ell$ell_name <- name
  ell
}

#' Build elliposid helper function
#'
#' @description
#' helper function is used to keep track of the ellipsoid version being ran,
#' so if user click on build ellipsoid again it can get prompted that the
#' other ellipsoid will be overwritten.
#'
#' @returns does not return, only calls to main function build_ellipsoid()
#' @keywords internal
#'
#' @noRd
build_ellipsoid_shiny <- function(){
  req(session_data$vars)

  ranges <- range_preview()

  if(is.null(ranges)){
    showNotification("Please check your range inputs before building.",
                     type = "error")
    return()
  }

  # Convert mins/maxs back to range_df format for build_ellipsoid
  vars <- session_data$vars
  range_df <- as.data.frame(rbind(unlist(ranges$mins),
                                  unlist(ranges$maxs)),
                            row.names = c("min", "max"))
  colnames(range_df) <- vars

  cl <- if(!is.null(input$cl_range)) input$cl_range else 0.95

  tryCatch({

    raw_ell <- build_ellipsoid(range = range_df, cl = cl, verbose = FALSE)

    # Base: immutable reference, never edited directly
    base_ell <- tag_ellipsoid(raw_ell, name = paste0("ellipsoid_",
                                                     ell_id_counter()))
    session_data$ellipsoid_list[["base"]] <- base_ell

    # Working copy: what the user edits and eventually names and saves
    working_ell <- tag_ellipsoid(raw_ell, name = paste0("ellipsoid_",
                                                        ell_id_counter()))
    session_data$current_ellipsoid <- working_ell


    showNotification("Ellipsoid built successfully.", type = "message")

  }, error = function(e){
    showNotification(paste("Error building ellipsoid:", e$message),
                     type = "error")
  })
}

# Read a single plot input with a NULL-safe default.
# Keeps collect_plot_settings() concise without repeating the pattern.
get_input <- function(id, default){
  if(!is.null(input[[id]])){
    input[[id]]
  } else {
    default
  }
}

# Helper to avoid repeating the mutual-exclusion update logic in varibales.
update_axis_selectors <- function(x_id, y_id, vars){
  x_sel <- input[[x_id]]
  y_sel <- input[[y_id]]

  if(is.null(x_sel) || !x_sel %in% vars) x_sel <- vars[1]

  y_choices <- setdiff(vars, x_sel)
  if(is.null(y_sel) || !y_sel %in% y_choices) y_sel <- y_choices[1]

  x_choices <- setdiff(vars, y_sel)

  updateSelectInput(session, x_id, choices = x_choices, selected = x_sel)
  updateSelectInput(session, y_id, choices = y_choices, selected = y_sel)
}

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


generate_occ_for_ell <- function(ell_id, pred_list, biased_list,
                                 layers, n_occ, sampling,
                                 strict, sampling_mask, seed = 123L){
  results <- list()

  for(layer in layers){
    pred <- pred_list[[ell_id]]
    bias <- biased_list[[ell_id]]

    # Determine which raster contains this layer
    source_rast <- NULL
    source_name <- NULL

    if(!is.null(pred) && inherits(pred, "SpatRaster") && layer %in% names(pred)){
      source_rast <- pred
      source_name <- "pred"
    } else if(!is.null(bias) && inherits(bias, "SpatRaster") && layer %in% names(bias)){
      source_rast <- bias
      source_name <- "bias"
    }

    if(is.null(source_rast)){
      message("Layer '", layer, "' not found for ", ell_id, ". Skipping.")
      next
    }

    method <- if(grepl("mahalanobis", layer, ignore.case = TRUE)) "mahalanobis" else "suitability"

    occ <- tryCatch(
      sample_data(n_occ = n_occ,
                  prediction = source_rast,
                  prediction_layer = layer,
                  sampling = sampling,
                  method = method,
                  sampling_mask = sampling_mask,
                  seed = seed,
                  strict = strict,
                  verbose = FALSE),
      error = function(e){
        message("Generate failed for ", ell_id, " (", layer, "): ", e$message)
        NULL
      }
    )

    if(!is.null(occ)){
      results[[layer]] <- occ[, c("x", "y"), drop = FALSE]
    }
  }

  results
}
