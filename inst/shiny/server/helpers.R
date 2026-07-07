# Title: Helper functions
# Description: Pure functions shared across server scripts
# Date last updated: 05/28/2026

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
    base_ell <- tag_ellipsoid(raw_ell, name = "base")
    session_data$ellipsoid_list[["base"]] <- base_ell

    # Working copy: what the user edits and eventually names and saves
    working_ell <- tag_ellipsoid(raw_ell, name = "ellipsoid_2")
    session_data$current_ellipsoid <- working_ell


    showNotification("Ellipsoid built successfully.", type = "message")

  }, error = function(e){
    showNotification(paste("Error building ellipsoid:", e$message),
                     type = "error")
  })
}


