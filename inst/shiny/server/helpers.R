# Title: Helper functions
# Description: Pure functions shared across server scripts
# Date last updated: 05/28/2026

# Function to read raster files
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

# To get the variables availble in the session basth on uploaded data
get_var_names <- function(session_data) {
  if(!is.null(session_data$raster)) names(session_data$raster)
  else if(!is.null(session_data$df)) names(session_data$df)
  else NULL
}
