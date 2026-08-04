# Title: Helper functions
# Description: Functions shared across server scripts. Some are pure, but
# several read input, session, and session_data from the server environment and
# only work because this file is sourced with local = TRUE from server.R.
# Date last updated: 08/03/2026



# Resolves an ellipsoid's parent, or NULL if it has none or the parent has
# been deleted. NOTE: list[[NULL]] errors, so parent_id must be checked
# before indexing.
ell_parent <- function(ell){

  if(is.null(ell)) return(NULL)

  pid <- ell$parent_id
  if(is.null(pid) || !nzchar(pid)) return(NULL)
  if(!pid %in% names(session_data$ellipsoid_list)) return(NULL)

  session_data$ellipsoid_list[[pid]]
}

# Where a reset returns to. A copy resets to its parent; an ellipsoid with
# no parent resets to what its own ranges produce, which is zero covariance
# and the centroid at the range midpoint.
ell_reset_target <- function(ell){

  parent <- ell_parent(ell)
  if(!is.null(parent)) return(parent)

  inputs <- ell$range_inputs
  if(is.null(inputs)) return(NULL)

  range_df <- as.data.frame(rbind(unlist(inputs$min), unlist(inputs$max)),
                            row.names = c("min", "max"))

  tryCatch(build_ellipsoid(range = range_df, cl = ell$cl, verbose = FALSE),
           error = function(e) NULL)
}

# Label for the reset link, so the user knows what it returns to
ell_reset_label <- function(ell){
  parent <- ell_parent(ell)
  if(!is.null(parent)) paste0("Reset to ", parent$ell_name) else "Reset to original"
}

# Lineage shown in the library
ell_lineage_label <- function(ell){
  parent <- ell_parent(ell)
  if(!is.null(parent)) paste0("from ", parent$ell_name) else "root"
}


# FILE READERS ------------------------------------------------------------

#' Read a raster file and return a SpatRaster
#'
#' @param path File path to read from.
#' @param ext Lowercase file extension, one of tif, tiff, or rds.
#'
#' @returns A SpatRaster.
#' @keywords internal
#'
#' @noRd
load_raster_file <- function(path, ext){

  obj <- switch(ext,
                "tif" = terra::rast(path),
                "tiff" = terra::rast(path),
                "rds" = readRDS(path),
                stop("Unsupported file type: ", ext))

  if(!inherits(obj, c("SpatRaster", "RasterStack", "RasterBrick", "RasterLayer"))){
    stop("File does not contain a raster object. Found class: ",
         paste(class(obj), collapse = ", "))
  }

  # Convert raster package objects to terra if needed
  if(inherits(obj, c("RasterStack", "RasterBrick", "RasterLayer"))){
    obj <- terra::rast(obj)
  }

  obj
}


#' Read a tabular file and return a data frame
#'
#' @param path File path to read from.
#' @param ext Lowercase file extension, one of csv or rds.
#'
#' @returns A data frame.
#' @keywords internal
#'
#' @noRd
load_df_file <- function(path, ext){

  obj <- switch(ext,
                "csv" = read.csv(path),
                "rds" = readRDS(path),
                stop("Unsupported file type: ", ext))

  if(!inherits(obj, c("data.frame", "tbl_df", "tbl"))){
    stop("File does not contain a data frame. Found class: ",
         paste(class(obj), collapse = ", "))
  }

  as.data.frame(obj)
}


# ELLIPSOID IDENTITY ------------------------------------------------------

# Session-level counter, increments every time a new ellipsoid is tagged
ell_id_counter <- reactiveVal(0L)

make_ell_id <- function(){
  n <- ell_id_counter() + 1L
  ell_id_counter(n)
  paste0("E", n, "_", format(Sys.time(), "%d%m%y"))
}

# Gives an ellipsoid a fresh id and, unless a name is supplied, a default
# name matching that id. The name is read after make_ell_id() has already
# incremented the counter, so the number in the name matches the number in
# the id.
tag_ellipsoid <- function(ell, name = NULL){
  ell$ell_id <- make_ell_id()
  ell$ell_name <- if(is.null(name)) paste0("ellipsoid_", ell_id_counter()) else name
  ell
}


# BUILD TAB ---------------------------------------------------------------

# Default values for the range panel. Prefers the stored range inputs of the
# working ellipsoid, then quartiles of the background data, then a plain
# 0 to 1 scale for virtual mode. Reads session_data from the server
# environment.
build_range_defaults <- function(vars, has_bg_df){

  if(!is.null(session_data$session_range)){
    return(session_data$session_range)
  }

  zeros <- setNames(as.list(rep(0, length(vars))), vars)
  ones <- setNames(as.list(rep(1, length(vars))), vars)

  if(has_bg_df){
    q <- round(apply(session_data$bg_df[, vars, drop = FALSE], 2,
                     quantile, na.rm = TRUE), 2)

    return(list(min = as.list(q[2, ]),
                max = as.list(q[4, ]),
                mean = as.list((q[2, ] + q[4, ]) / 2),
                sd = as.list((q[4, ] - q[2, ]) / 4),
                expand_min = zeros,
                expand_max = zeros))
  }

  list(min = zeros,
       max = ones,
       mean = zeros,
       sd = ones,
       expand_min = zeros,
       expand_max = zeros)
}


# PLOTTING ----------------------------------------------------------------

# Read a single plot input with a NULL-safe default.
# Keeps collect_plot_settings() concise without repeating the pattern.
get_input <- function(id, default){
  if(!is.null(input[[id]])){
    input[[id]]
  } else {
    default
  }
}

# Keeps the x and y axis selectors mutually exclusive, so the same variable
# cannot be chosen on both axes of a 2D plot.
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


# BIAS TAB ----------------------------------------------------------------

# Applies bias to one layer for one ellipsoid and merges the result into the
# biased list. If ell_id already exists, the new layer is appended to the
# existing SpatRaster stack. If the layer is already in the stack, it is
# skipped silently.
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

  # Extract the SpatRaster from the result, dropping combination_formula
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


# GENERATE TAB ------------------------------------------------------------

# Samples occurrences for one ellipsoid across the requested layers, pulling
# each layer from either the prediction or the biased prediction, whichever
# contains it.
generate_occ_for_ell <- function(ell_id, pred_list, biased_list,
                                 layers, n_occ, sampling,
                                 strict, sampling_mask, seed = 123L){

  results <- list()

  for(layer in layers){

    pred <- pred_list[[ell_id]]
    bias <- biased_list[[ell_id]]

    # Determine which raster contains this layer
    source_rast <- NULL

    if(!is.null(pred) && inherits(pred, "SpatRaster") && layer %in% names(pred)){
      source_rast <- pred
    } else if(!is.null(bias) && inherits(bias, "SpatRaster") && layer %in% names(bias)){
      source_rast <- bias
    }

    if(is.null(source_rast)){
      message("Layer '", layer, "' not found for ", ell_id, ". Skipping.")
      next
    }

    method <- if(grepl("mahalanobis", layer, ignore.case = TRUE)){
      "mahalanobis"
    } else {
      "suitability"
    }

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
