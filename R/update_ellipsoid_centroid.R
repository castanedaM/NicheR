#' Move the centroid of a nicheR ellipsoid
#'
#' @param ell A \code{nicheR_ellipsoid} object.
#' @param new_centroid A named numeric vector with the new centroid position.
#'   Names must match \code{ell$var_names}.
#' @param verbose Logical. If TRUE prints progress messages. Default FALSE.
#'
#' @return A \code{nicheR_ellipsoid} object with updated centroid and ranges.
#'   The covariance matrix, volume, and chi-square cutoff are unchanged.
#'
#' @keywords internal
#' @noRd
update_ellipsoid_centroid <- function(ell, new_centroid, verbose = FALSE){

  if(!inherits(ell, "nicheR_ellipsoid")){
    stop("ell must be a nicheR_ellipsoid object.")
  }

  vars <- ell$var_names

  if(is.null(names(new_centroid)) || !all(vars %in% names(new_centroid))){
    stop("new_centroid must be a named numeric vector with names matching ",
         "ell$var_names: ", paste(vars, collapse = ", "))
  }

  new_centroid <- new_centroid[vars]

  if(any(!is.finite(new_centroid))){
    stop("new_centroid contains non-finite values.")
  }

  if(verbose) message("Starting: moving ellipsoid centroid...")

  old_centroid <- ell$centroid
  delta <- new_centroid - old_centroid

  # Shift ranges by the same delta so the ellipsoid translates rigidly
  new_ranges <- ell$ranges
  new_ranges["min", ] <- new_ranges["min", ] + delta
  new_ranges["max", ] <- new_ranges["max", ] + delta

  ell$centroid <- new_centroid
  ell$ranges <- new_ranges

  if(verbose) message("Done: centroid moved.")

  ell
}
