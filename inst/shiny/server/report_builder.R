# Title: Session report builder

# Description: Turns the current session into an Rmd holding a methods
# narrative and the nicheR code that reproduces it. The generated document
# uses real code chunks, so one file is both the runnable script the user
# takes away and the source knitted to produce the HTML report.

# Layout: shared primitives first, then one block per workflow step. Each step
# keeps its inspection helpers, code emitters, prose, and section assembly
# together, so adding Bias and Generate means appending two more blocks in the
# same shape rather than threading through what is here.

# Ellipsoids are emitted self contained, built from their own range_inputs
# rather than chained off a parent. The library records current state, not
# click history, so a chained script stops reproducing the session as soon as a
# parent is edited after being copied. Lineage appears in the prose instead.

# Date last updated: 08/12/2026


# FORMATTING --------------------------------------------------------------

#' Format a number for emitted code
#'
#' Seven significant digits keeps typed values exact and computed values short.
#' trim = TRUE suppresses padding to a common width.
#'
#' @param x Numeric vector.
#'
#' @returns A character vector the same length as `x`.
#'
#' @noRd
report_num <- function(x){
  format(x, trim = TRUE, digits = 7, scientific = FALSE)
}


#' Quote a name that is not a syntactic R identifier
#'
#' Variable names come from raster layers and covariance pair labels contain a
#' hyphen, so backticks are needed more often than not.
#'
#' @param nm Character vector of names.
#'
#' @returns `nm`, with non-syntactic entries wrapped in backticks.
#'
#' @noRd
report_name <- function(nm){
  ifelse(nm == make.names(nm), nm, paste0("`", nm, "`"))
}


#' Character vector as an R literal
#'
#' @param x Character vector.
#'
#' @returns A single string of the form "c(\"a\", \"b\")".
#'
#' @noRd
report_chr <- function(x){
  paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
}


#' Named numeric vector as an R literal
#'
#' @param x Named numeric vector.
#'
#' @returns A single string of the form "c(a = 1, b = 2)".
#'
#' @noRd
report_vec <- function(x){
  paste0("c(",
         paste0(report_name(names(x)), " = ", report_num(unname(x)),
                collapse = ", "),
         ")")
}


#' Join a character vector into a readable list
#'
#' @param x Character vector.
#'
#' @returns A single string.
#'
#' @noRd
report_and <- function(x){
  if(length(x) < 2) return(paste(x, collapse = ""))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}


#' Object names for the emitted script
#'
#' Ellipsoid names are user supplied, so they need sanitising, and two
#' ellipsoids may share a name.
#'
#' @param nms Character vector of display names.
#' @param prefix Fallback prefix when a name sanitises to nothing.
#'
#' @returns A character vector of unique syntactic names.
#'
#' @noRd
report_obj_names <- function(nms, prefix = "object"){
  out <- gsub("[^A-Za-z0-9]+", "_", nms)
  out <- gsub("^_+|_+$", "", out)
  out[!nzchar(out)] <- prefix
  out[grepl("^[0-9]", out)] <- paste0(prefix, "_", out[grepl("^[0-9]", out)])
  make.unique(out, sep = "_")
}


# CODE EMISSION -----------------------------------------------------------

#' Format a call as assignment code
#'
#' Every emitter builds calls, so comma and indent handling lives here once.
#' Values in args must already be formatted as code.
#'
#' @param obj Name to assign to.
#' @param fn Function name.
#' @param args Named list of formatted argument values.
#'
#' @returns A character vector of code lines.
#'
#' @noRd
report_call <- function(obj, fn, args){

  # An argument formatted as more than one string would recycle against the
  # names below and emit duplicated arguments, which reads as valid code until
  # it is run
  bad <- names(args)[lengths(args) != 1]
  if(length(bad) > 0){
    stop("report_call: arguments must be single strings, got several for ",
         paste(bad, collapse = ", "), call. = FALSE)
  }

  vals <- unlist(args, use.names = FALSE)
  c(paste0(obj, " <- ", fn, "("),
    paste0("  ", names(args), " = ", vals,
           c(rep(",", length(args) - 1), "")),
    ")")
}


#' Wrap emitted code as a knitr chunk
#'
#' @param lines Character vector of code lines.
#' @param label Chunk label, unique within the document.
#' @param eval Logical, whether the chunk runs when the report is knitted.
#'
#' @returns A character vector including the chunk delimiters.
#'
#' @noRd
report_chunk <- function(lines, label, eval = TRUE){
  c(paste0("```{r ", label, ", eval = ", eval, "}"), lines, "```", "")
}


# SESSION DATA ------------------------------------------------------------

#' Whether the emitted script can run at report time
#'
#' Sessions built on uploaded files refer to a folder only the user can point
#' at, so their chunks are emitted unevaluated. Example and virtual sessions
#' are self contained and are evaluated, which checks that the script runs.
#' Reads session_data from the enclosing server environment.
#'
#' @returns A single logical.
#'
#' @noRd
report_can_eval <- function(){
  isTRUE(session_data$input_mode %in% c("example", "virtual"))
}


#' Name the emitted script gives the background data
#'
#' @returns A single string.
#'
#' @noRd
report_data_object <- function(){
  if(identical(session_data$file_type, "df") &&
     is.null(session_data$bg_raster)) "bg" else "bios"
}


#' Object names for every ellipsoid in the library
#'
#' Computed over the whole library rather than a subset, so the name an
#' ellipsoid is given in the Build section is the same one later sections use
#' to refer to it. Reads session_data from the enclosing server environment.
#'
#' @returns A character vector of object names, named by ell_id.
#'
#' @noRd
report_ell_objects <- function(){
  ells <- session_data$ellipsoid_list
  if(length(ells) == 0) return(character(0))
  setNames(report_obj_names(vapply(ells, function(e) e$ell_name, character(1)),
                            prefix = "ellipsoid"),
           vapply(ells, function(e) e$ell_id, character(1)))
}


#' Code that reloads the environmental data
#'
#' Uploaded files cannot be located from the app. The browser sends only a file
#' name and Shiny's copy of the upload is deleted when the session ends, so the
#' emitted script declares the folder as a variable for the user to set rather
#' than writing a path that will not exist. Reads session_data from the
#' enclosing server environment.
#'
#' @returns A character vector of code lines.
#'
#' @noRd
report_data_code <- function(){

  vars <- session_data$vars

  if(identical(session_data$input_mode, "virtual")){
    return(c("# Virtual mode: the niche is defined in environmental space",
             "# only, so there are no environmental layers to load.",
             paste0("vars <- ", report_chr(vars))))
  }

  if(identical(session_data$input_mode, "example")){
    return(c("# Bundled example data",
             "bios <- terra::rast(",
             "  system.file(\"extdata/ma_bios.tif\", package = \"nicheR\"))",
             paste0("bios <- terra::subset(bios, ", report_chr(vars), ")")))
  }

  src <- session_data$data_source
  if(is.null(src) || length(src) == 0){
    src <- if(identical(session_data$file_type, "df")){
      "your_data.csv"
    } else {
      "your_layers.tif"
    }
  }

  head <- c("# Set this to the folder holding the files you uploaded to the app",
            "data_dir <- \".\"",
            "")

  if(identical(session_data$file_type, "df")){
    return(c(head,
             paste0("bg <- read.csv(file.path(data_dir, \"", src[1], "\"))"),
             paste0("bg <- bg[, ", report_chr(vars), "]")))
  }

  load <- if(length(src) == 1){
    paste0("bios <- terra::rast(file.path(data_dir, \"", src, "\"))")
  } else {
    c("bios <- terra::rast(file.path(data_dir,",
      paste0("                            ", report_chr(src), "))"))
  }

  c(head, load,
    paste0("bios <- terra::subset(bios, ", report_chr(vars), ")"))
}


# ELLIPSOID INSPECTION ----------------------------------------------------

#' Off diagonal covariance entries
#'
#' Returned as a table rather than a named vector so that callers read the two
#' variables from their own columns. Splitting a "v1-v2" label back apart
#' breaks as soon as a raster layer name contains a hyphen.
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A data frame with columns v1, v2, value. Zero rows when every pair
#' is zero.
#'
#' @noRd
report_cov_pairs <- function(ell){

  v <- ell$var_names
  if(length(v) < 2) return(data.frame(v1 = character(0), v2 = character(0),
                                      value = numeric(0)))

  idx <- which(upper.tri(ell$cov_matrix), arr.ind = TRUE)
  out <- data.frame(v1 = v[idx[, 1]], v2 = v[idx[, 2]],
                    value = ell$cov_matrix[idx],
                    stringsAsFactors = FALSE)

  out[out$value != 0, , drop = FALSE]
}


#' Covariance entries formatted for update_ellipsoid_covariance()
#'
#' @param cov A table from report_cov_pairs().
#'
#' @returns A named numeric vector.
#'
#' @noRd
report_cov_vec <- function(cov){
  setNames(cov$value, paste0(cov$v1, "-", cov$v2))
}


#' Centroid an ellipsoid had when it was built from its ranges
#'
#' Derived from range_inputs rather than stored, since build_ellipsoid() places
#' the centroid at the midpoint of each range and range_inputs is frozen at
#' build time.
#'
#' @param ell A nicheR ellipsoid carrying the app fields.
#'
#' @returns A named numeric vector.
#'
#' @noRd
report_centroid_origin <- function(ell){
  vars <- ell$var_names
  ri <- ell$range_inputs
  (unlist(ri$min[vars]) + unlist(ri$max[vars])) / 2
}


#' Whether the centroid differs meaningfully from a reference position
#'
#' The centroid sliders round to two decimals, so no move a user can make is
#' smaller than 0.01. A difference below that came from rounding.
#'
#' @param x Named numeric vector, the current centroid.
#' @param ref Named numeric vector, the position to compare against.
#' @param ell A nicheR ellipsoid.
#'
#' @returns A logical vector, one entry per variable.
#'
#' @noRd
report_centroid_diff <- function(x, ref, ell){
  vars <- ell$var_names
  abs(x[vars] - ref[vars]) >= 0.01
}


#' Whether the centroid was moved away from where the ranges put it
#'
#' Compared against the range midpoint, not against a parent, because this
#' decides whether the emitted script needs an update call and the script
#' rebuilds each ellipsoid from its own range_inputs.
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A single logical.
#'
#' @noRd
report_centroid_moved <- function(ell){
  any(report_centroid_diff(ell$centroid, report_centroid_origin(ell), ell))
}


# BUILD, CODE -------------------------------------------------------------

#' Code that rebuilds one range specification
#'
#' The three branches mirror the three ways the Build tab sets ranges. Manual
#' and data derived ranges become a data frame literal, stats ranges become a
#' call, so the script documents the method where it can.
#'
#' @param ell A nicheR ellipsoid carrying the app fields.
#' @param obj Name to give the range object.
#'
#' @returns A list with `pre`, lines to emit first, and `code`, the assignment.
#'
#' @noRd
report_range_code <- function(ell, obj){

  ri <- ell$range_inputs
  vars <- ell$var_names
  emin <- unlist(ri$expand_min[vars])
  emax <- unlist(ri$expand_max[vars])

  if(identical(ri$method, "stats")){
    return(list(pre = character(0),
                code = report_call(obj, "ranges_from_stats",
                                   list(mean = report_vec(unlist(ri$mean[vars])),
                                        sd = report_vec(unlist(ri$sd[vars])),
                                        cl = report_num(ell$cl),
                                        expand_min = report_vec(emin),
                                        expand_max = report_vec(emax)))))
  }

  mins <- unlist(ri$min[vars])
  maxs <- unlist(ri$max[vars])
  args <- as.list(setNames(paste0("c(", report_num(mins), ", ",
                                  report_num(maxs), ")"),
                           report_name(vars)))

  # The occurrence table behind a data derived range is not carried out of the
  # app, so the resolved bounds are given directly and the method is recorded
  # in a comment instead
  pre <- if(identical(ri$method, "df")){
    c("# Ranges derived in the app from an uploaded occurrence table with",
      paste0("# ranges_from_data(), expanding by ",
             report_and(paste0(vars, " ", report_num(emin), "/",
                               report_num(emax), "%")), "."),
      "# The resolved bounds are given directly so this runs without that table.")
  } else {
    character(0)
  }

  list(pre = pre, code = report_call(obj, "data.frame", args))
}


#' Code that rebuilds one ellipsoid
#'
#' Emitted in the order the app applies them: build, rotate, translate.
#' update_ellipsoid_covariance() rebuilds the object from its components, so a
#' centroid move written before it would be discarded.
#'
#' @param ell A nicheR ellipsoid.
#' @param obj Name to give the ellipsoid object.
#' @param range_obj Name of the range object it is built from.
#' @param parent_obj Name of the parent's object, or NULL for a root.
#'
#' @returns A character vector of code lines.
#'
#' @noRd
report_ell_code <- function(ell, obj, range_obj, parent_obj = NULL){

  out <- c(if(is.null(parent_obj)){
    paste0("# ", ell$ell_name, ", built from ranges")
  } else {
    paste0("# ", ell$ell_name, ", copied from ", parent_obj)
  },
  report_call(obj, "build_ellipsoid",
              list(range = range_obj, cl = report_num(ell$cl))))

  cov <- report_cov_pairs(ell)
  if(nrow(cov) > 0){
    out <- c(out, "",
             report_call(obj, "update_ellipsoid_covariance",
                         list(object = obj,
                              covariance = report_vec(report_cov_vec(cov)))))
  }

  if(report_centroid_moved(ell)){
    out <- c(out, "",
             report_call(obj, "update_ellipsoid_centroid",
                         list(ell = obj,
                              new_centroid = report_vec(round(ell$centroid[ell$var_names], 2)))))
  }

  out
}


# BUILD, PROSE ------------------------------------------------------------

#' Sentence describing where the ranges came from
#'
#' @param ell A nicheR ellipsoid carrying the app fields.
#'
#' @returns A single string.
#'
#' @noRd
report_range_prose <- function(ell){

  ri <- ell$range_inputs
  vars <- ell$var_names
  mins <- unlist(ri$min[vars])
  maxs <- unlist(ri$max[vars])
  spans <- report_and(paste0(vars, " from ", report_num(mins), " to ",
                             report_num(maxs), " (a span of ",
                             report_num(maxs - mins), ")"))

  lead <- switch(ri$method,
                 "man" = "Its bounds were entered directly",
                 "stats" = paste0("Its bounds were derived from a mean and standard ",
                                  "deviation per variable, so they describe a tolerance ",
                                  "reported as a distribution rather than one observed ",
                                  "directly"),
                 "df" = paste0("Its bounds were derived from the observed spread of an ",
                               "occurrence table and then expanded, so they describe the ",
                               "recorded conditions plus a margin"),
                 "Its bounds were set")

  paste0(lead, ", giving ", spans, ".")
}


#' Sentence describing the confidence level
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A single string.
#'
#' @noRd
report_cl_prose <- function(ell){
  paste0("The confidence level of ", report_num(ell$cl), " sets where the ",
         "boundary falls, so conditions beyond it are treated as outside the ",
         "niche.")
}


#' Sentence describing the covariance, if any was set
#'
#' Describes the sign and magnitude of each pair and what that implies about
#' the orientation of the niche. No claim is made about what the variables
#' represent, since the app knows only their names.
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A single string, or NULL when every pair is zero.
#'
#' @noRd
report_cov_prose <- function(ell){

  cov <- report_cov_pairs(ell)
  if(nrow(cov) == 0) return(NULL)

  parts <- paste0("a ", ifelse(cov$value > 0, "positive", "negative"),
                  " covariance of ", report_num(cov$value),
                  " between ", cov$v1, " and ", cov$v2)

  paste0("Covariance was set for ", nrow(cov),
         if(nrow(cov) == 1) " variable pair: " else " variable pairs: ",
         report_and(parts), ". ",
         "A non-zero covariance tilts the ellipsoid away from the variable ",
         "axes, so the range of ", cov$v1[1], " the niche includes depends on ",
         "the value of ", cov$v2[1], " rather than being the same throughout. ",
         "A positive value means the two increase together across the niche; ",
         "a negative value means one increases as the other decreases. ",
         "Because the covariance matrix determines the volume, this also ",
         "changes how much environmental space the niche occupies.")
}


#' Sentence describing the centroid move, if any
#'
#' Compared against the parent where there is one, since for a copy the useful
#' number is how far it sits from what it was copied from. This differs from
#' report_centroid_moved(), which decides what the script has to emit and so
#' must measure from the range midpoint.
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A single string, or NULL when the centroid was not moved.
#'
#' @noRd
report_centroid_prose <- function(ell){

  vars <- ell$var_names
  parent <- ell_parent(ell)

  ref <- if(!is.null(parent)){
    parent$centroid[vars]
  } else {
    report_centroid_origin(ell)
  }

  ref_txt <- if(!is.null(parent)){
    paste0("relative to ", parent$ell_name)
  } else {
    "relative to the midpoint of its ranges"
  }

  moved <- vars[report_centroid_diff(ell$centroid, ref, ell)]
  if(length(moved) == 0) return(NULL)

  dv <- ell$centroid[moved] - ref[moved]
  parts <- paste0(moved, " by ", ifelse(dv > 0, "+", ""),
                  report_num(unname(dv)), " (from ",
                  report_num(unname(ref[moved])), " to ",
                  report_num(unname(ell$centroid[moved])), ")")

  paste0("The centroid sits ", ref_txt, " shifted in ", report_and(parts),
         ". Translating the centroid changes only where the niche sits, so ",
         "the ellipsoid keeps whatever shape and orientation it had before ",
         "the move. It describes the same set of tolerances centred on ",
         "different values of ", report_and(moved), ".")
}


#' Paragraph introducing one ellipsoid
#'
#' @param ell A nicheR ellipsoid.
#' @param parent_obj Name of the parent's object, or NULL for a root.
#'
#' @returns A character vector of markdown lines.
#'
#' @noRd
report_ell_prose <- function(ell, parent_obj = NULL){

  open <- if(is.null(parent_obj)){
    paste0("**", ell$ell_name, "** is defined directly from environmental ",
           "ranges, so it does not inherit anything from another niche. ")
  } else {
    paste0("**", ell$ell_name, "** was derived from `", parent_obj,
           "`, which means the two are alternative descriptions of a niche ",
           "rather than independent hypotheses. ")
  }

  vol <- if(!is.null(ell$volume)){
    paste0(" Its niche volume is ", report_num(signif(ell$volume, 4)),
           ", which is the measure to compare across niches: a larger volume ",
           "means a broader set of tolerable conditions.")
  } else {
    ""
  }

  cov_txt <- report_cov_prose(ell)
  cen_txt <- report_centroid_prose(ell)

  c(paste0(open, report_range_prose(ell), " ", report_cl_prose(ell), vol), "",
    if(!is.null(cov_txt)) c(cov_txt, ""),
    if(!is.null(cen_txt)) c(cen_txt, ""))
}


#' Paragraph describing the library as a whole
#'
#' Generalisations live here and not in the code, because a description that is
#' slightly loose is still true while a loop that is slightly wrong produces a
#' script that does not reproduce the session.
#'
#' @param ells The ellipsoid list, in creation order.
#' @param n_range Number of distinct range specifications.
#'
#' @returns A character vector of markdown lines.
#'
#' @noRd
report_build_prose <- function(ells, n_range){

  n <- length(ells)
  vars <- ells[[1]]$var_names
  cls <- unique(vapply(ells, function(e) e$cl, numeric(1)))
  n_root <- sum(vapply(ells, function(e) is.null(e$parent_id), logical(1)))

  out <- paste0(
    "We defined ", n, " ellipsoidal niche", if(n == 1) "" else "s", " in ",
    length(vars), " environmental dimensions (", paste(vars, collapse = ", "),
    ") using ",
    if(length(cls) == 1){
      paste0("a confidence level of ", report_num(cls))
    } else {
      paste0("confidence levels between ", report_num(min(cls)), " and ",
             report_num(max(cls)))
    },
    ". ",
    if(n_range == 1){
      "All were built from a single set of environmental ranges. "
    } else {
      paste0("These were built from ", n_range,
             " distinct sets of environmental ranges. ")
    },
    n_root, " of the ", n, " were defined directly from ranges",
    if(n_root < n){
      paste0(", and the remaining ", n - n_root, " were derived from these.")
    } else {
      "."
    })

  cov_n <- vapply(ells, function(e) nrow(report_cov_pairs(e)) > 0, logical(1))
  if(any(cov_n)){
    rng <- range(unlist(lapply(ells[cov_n], function(e) report_cov_pairs(e)$value)))
    out <- c(out, "",
             paste0("Covariance between environmental variables was set in ",
                    sum(cov_n), " of the ", n,
                    " niches, with off-diagonal values from ",
                    report_num(rng[1]), " to ", report_num(rng[2]),
                    ", rotating the ellipsoid relative to the variable axes."))
  }

  moved <- vapply(ells, report_centroid_moved, logical(1))
  if(any(moved)){
    out <- c(out, "",
             paste0("The centroid was translated in ", sum(moved), " of the ",
                    n, " niches, shifting the position of the niche in ",
                    "environmental space."))
  }

  c(out, "")
}


# BUILD, SECTION ----------------------------------------------------------

#' Rmd source for the Build section
#'
#' Reads session_data from the enclosing server environment.
#'
#' @returns A character vector of Rmd lines.
#'
#' @noRd
report_build_section <- function(){

  ells <- session_data$ellipsoid_list
  if(length(ells) == 0){
    return(c("## Building ellipsoidal niches", "",
             "No ellipsoids were saved in this session.", ""))
  }

  obj <- report_ell_objects()
  ev <- report_can_eval()

  # Ranges are deduplicated on the code they produce, so two ellipsoids share a
  # range object exactly when rebuilding them would emit the same lines
  drafts <- lapply(ells, report_range_code, obj = "range")
  keys <- vapply(drafts, function(d) paste(c(d$pre, d$code), collapse = "\n"),
                 character(1))
  uniq <- unique(keys)
  range_obj <- paste0("range_", seq_along(uniq))
  range_of <- range_obj[match(keys, uniq)]

  range_lines <- unlist(lapply(seq_along(uniq), function(i){
    d <- report_range_code(ells[[match(uniq[i], keys)]], range_obj[i])
    c(d$pre, d$code, "")
  }))

  # One chunk per ellipsoid, each introduced by its own paragraph
  blocks <- unlist(lapply(seq_along(ells), function(i){
    p <- ells[[i]]$parent_id
    p_obj <- if(!is.null(p) && p %in% names(obj)) obj[[p]] else NULL
    c(paste0("### ", ells[[i]]$ell_name), "",
      report_ell_prose(ells[[i]], p_obj),
      report_chunk(report_ell_code(ells[[i]], obj[[i]], range_of[i], p_obj),
                   label = paste0("build-", i), eval = ev))
  }))

  c("## Building ellipsoidal niches", "",
    report_build_prose(ells, length(uniq)),
    report_chunk(report_data_code(), label = "build-data", eval = ev),
    paste0("The ", length(uniq), " set", if(length(uniq) == 1) "" else "s",
           " of environmental ranges below ",
           if(length(uniq) == 1) "is" else "are",
           " shared by the niches that follow."),
    "",
    report_chunk(range_lines, label = "build-ranges", eval = ev),
    blocks)
}


# PREDICT, INSPECTION -----------------------------------------------------

#' Output layers present in a stored prediction
#'
#' Coordinates and predictor variables are dropped, leaving only what predict()
#' added. With keep_data = TRUE the predictors are stacked in front of the
#' outputs, so this filter is what separates the two.
#'
#' @param pred A stored prediction, SpatRaster or data frame.
#' @param ell The ellipsoid it belongs to.
#'
#' @returns A character vector.
#'
#' @noRd
report_pred_layer_names <- function(pred, ell){
  if(is.null(pred)) return(character(0))
  nms <- if(inherits(pred, "SpatRaster")) names(pred) else colnames(pred)
  setdiff(nms, c("x", "y", ell$var_names))
}


#' Layer arguments implied by a stored prediction
#'
#' Recovered from the layers that came back, which is exact: a truncated layer
#' appears only when it was asked for.
#'
#' @param pred A stored prediction.
#' @param ell The ellipsoid it belongs to.
#'
#' @returns A named list of logicals.
#'
#' @noRd
report_pred_layers <- function(pred, ell){
  nms <- report_pred_layer_names(pred, ell)
  list(include_mahalanobis = "Mahalanobis" %in% nms,
       include_suitability = "suitability" %in% nms,
       mahalanobis_truncated = "Mahalanobis_trunc" %in% nms,
       suitability_truncated = "suitability_trunc" %in% nms)
}


#' Predict arguments for one stored prediction
#'
#' The layer arguments are recovered from the output. The truncation level
#' leaves no trace there, so it is read from what the app recorded, and is
#' omitted for sessions saved before that was stored.
#'
#' @param id The ell_id the prediction is stored under.
#' @param pred The stored prediction.
#' @param ell The ellipsoid it belongs to.
#'
#' @returns A named list of formatted argument values.
#'
#' @noRd
report_pred_args <- function(id, pred, ell){

  out <- lapply(report_pred_layers(pred, ell),
                function(x) if(isTRUE(x)) "TRUE" else "FALSE")

  lvl <- session_data$prediction_settings[[id]]$adjust_truncation_level
  if(length(lvl) == 1 && is.finite(lvl)){
    out$adjust_truncation_level <- report_num(lvl)
  }

  out
}


#' Value range of each layer a prediction produced
#'
#' Read from the stored surface rather than recomputed, so the numbers are the
#' ones the app showed.
#'
#' @param pred A stored prediction.
#' @param ell The ellipsoid it belongs to.
#'
#' @returns A data frame with columns layer, min, max.
#'
#' @noRd
report_pred_layer_stats <- function(pred, ell){

  lyrs <- report_pred_layer_names(pred, ell)
  if(length(lyrs) == 0){
    return(data.frame(layer = character(0), min = numeric(0),
                      max = numeric(0), stringsAsFactors = FALSE))
  }

  rng <- vapply(lyrs, function(l){
    v <- if(inherits(pred, "SpatRaster")){
      as.numeric(terra::minmax(pred[[l]]))
    } else {
      range(pred[[l]], na.rm = TRUE)
    }
    if(length(v) != 2 || any(!is.finite(v))) c(NA_real_, NA_real_) else v
  }, numeric(2))

  data.frame(layer = lyrs, min = rng[1, ], max = rng[2, ],
             stringsAsFactors = FALSE)
}


#' Proportion of the study area a prediction marks as suitable
#'
#' Read from the truncated suitability layer where it exists, since that is the
#' layer with a hard niche boundary. Returns NA when no such layer was
#' produced, rather than inventing a threshold on the continuous surface.
#'
#' @param pred A stored prediction.
#'
#' @returns A single numeric between 0 and 1, or NA.
#'
#' @noRd
report_pred_suitable <- function(pred){

  if(!inherits(pred, "SpatRaster")) return(NA_real_)
  if(!"suitability_trunc" %in% names(pred)) return(NA_real_)

  r <- pred[["suitability_trunc"]]
  tot <- terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1]
  if(is.na(tot) || tot == 0) return(NA_real_)

  terra::global(r > 0, "sum", na.rm = TRUE)[1, 1] / tot
}


# PREDICT, CODE -----------------------------------------------------------

#' Code that predicts a group of ellipsoids
#'
#' Prediction applies the same call to each ellipsoid, so a loop here states
#' only what the stored arguments already show. Ellipsoids predicted
#' differently land in different groups and so in different calls.
#'
#' @param objs Object names of the ellipsoids in the group.
#' @param args The predict arguments they share, already formatted.
#' @param out Name to give the resulting list.
#'
#' @returns A character vector of code lines.
#'
#' @noRd
report_pred_code <- function(objs, args, out){
  c(paste0(out, "_ellipsoids <- list(",
           paste0(objs, " = ", objs, collapse = ", "), ")"),
    "",
    paste0(out, " <- lapply(", out, "_ellipsoids, function(e){"),
    "  predict(e,",
    paste0("          newdata = ", report_data_object(), ","),
    paste0("          ", names(args), " = ", unlist(args), ","),
    "          verbose = FALSE)",
    "})")
}


# PREDICT, PROSE ----------------------------------------------------------

#' Paragraphs introducing the predict step
#'
#' @param ells The predicted ellipsoids.
#' @param preds Their stored predictions.
#' @param n_group Number of distinct argument groups.
#'
#' @returns A character vector of markdown lines.
#'
#' @noRd
report_predict_prose <- function(ells, preds, n_group){

  n <- length(ells)
  spatial <- inherits(preds[[1]], "SpatRaster")

  out <- paste0(
    "Each of the ", n, " niche", if(n == 1) "" else "s",
    " defined above was projected ",
    if(spatial) "across the study area" else "onto the background records",
    ". For every ", if(spatial) "cell" else "record",
    ", the environmental values are compared against the ellipsoid to give a ",
    "measure of how close those conditions sit to the centre of the niche.",
    if(n_group > 1){
      paste0(" Not every niche was projected the same way, so the calls below ",
             "are grouped by the arguments they share.")
    } else {
      ""
    })

  layers <- unique(unlist(lapply(seq_along(ells), function(i){
    report_pred_layer_names(preds[[i]], ells[[i]])
  })))

  meanings <- c(
    "Mahalanobis" = paste0("**Mahalanobis** is the distance from the centre ",
                           "of the niche, measured in units that account for ",
                           "the spread and correlation of the variables. ",
                           "Larger values are further from the conditions the ",
                           "niche describes."),
    "suitability" = paste0("**suitability** rescales that distance to run ",
                           "from 1 at the centre toward 0 further out. It is ",
                           "continuous everywhere, so conditions outside the ",
                           "niche boundary still receive a small non-zero ",
                           "value."),
    "Mahalanobis_trunc" = paste0("**Mahalanobis_trunc** is the same distance ",
                                 "with everything beyond the niche boundary ",
                                 "set to `NA`, so only conditions inside the ",
                                 "niche carry a value."),
    "suitability_trunc" = paste0("**suitability_trunc** is the suitability ",
                                 "surface with everything beyond the boundary ",
                                 "set to zero. This is the layer with a hard ",
                                 "edge, and the one to use when the question ",
                                 "is whether conditions fall inside the niche ",
                                 "rather than how close to its centre they sit."))

  c(out, "",
    "The outputs produced were:", "",
    paste0("- ", unname(meanings[names(meanings) %in% layers])), "")
}


#' Paragraph describing what one prediction produced
#'
#' Describes the surface that came back, not the call that produced it. The
#' call is shared across a group and is shown once; what differs between the
#' niches is the result.
#'
#' @param ell The ellipsoid.
#' @param pred Its stored prediction.
#' @param id The ell_id, used to read the recorded predict settings.
#' @param obj Its object name in the emitted script.
#' @param areas Named numeric vector of suitable area proportions for every
#' predicted ellipsoid, used for the comparison sentence.
#'
#' @returns A character vector of markdown lines.
#'
#' @noRd
report_pred_ell_prose <- function(ell, pred, id, obj, areas){

  spatial <- inherits(pred, "SpatRaster")
  stats <- report_pred_layer_stats(pred, ell)
  p <- report_pred_suitable(pred)

  n_units <- if(spatial){
    tot <- terra::global(!is.na(pred[[stats$layer[1]]]), "sum",
                         na.rm = TRUE)[1, 1]
    paste0(format(tot, big.mark = ","), " cells")
  } else {
    paste0(format(nrow(pred), big.mark = ","), " records")
  }

  open <- paste0("Projecting **", ell$ell_name, "** onto ", n_units,
                 " produced ", nrow(stats), " layer",
                 if(nrow(stats) == 1) "" else "s", ".")

  # The truncation override leaves no trace in the returned layers, so without
  # this sentence a reader has no way to know the boundary was moved
  lvl <- session_data$prediction_settings[[id]]$adjust_truncation_level
  trunc_txt <- if(length(lvl) == 1 && is.finite(lvl)){
    paste0(" The truncation level was set to ", report_num(lvl),
           " rather than the confidence level of ", report_num(ell$cl),
           " this niche was built with, so the boundary of the prediction sits ",
           if(lvl < ell$cl) "tighter" else "wider", " than the ellipsoid.")
  } else {
    ""
  }

  ranges <- paste0("- `", stats$layer, "` ranges from ",
                   report_num(signif(stats$min, 4)), " to ",
                   report_num(signif(stats$max, 4)))

  area <- if(is.na(p)){
    paste0("No truncated suitability layer was produced for this niche, so ",
           "the extent of the suitable area is not reported. Suitability ",
           "falls off continuously and never reaches zero, which means there ",
           "is no boundary to measure without one.")
  } else {

    txt <- paste0(report_num(round(p * 100, 1)), "% of the ",
                  if(spatial) "study area" else "background records",
                  " falls inside the niche boundary.")

    rest <- areas[names(areas) != id]
    rest <- rest[!is.na(rest)]

    if(length(rest) >= 2){
      txt <- paste0(txt, if(p >= max(rest)){
        " This is the broadest of the niches projected here."
      } else if(p <= min(rest)){
        " This is the narrowest of the niches projected here."
      } else {
        paste0(" The others projected here range from ",
               report_num(round(min(rest) * 100, 1)), "% to ",
               report_num(round(max(rest) * 100, 1)), "%.")
      })
    } else if(length(rest) == 1){
      other <- session_data$ellipsoid_list[[names(rest)]]$ell_name
      txt <- paste0(txt, " ", other, " covers ",
                    report_num(round(rest * 100, 1)), "%.")
    }

    txt
  }

  c(paste0(open, trunc_txt), "",
    ranges, "",
    area, "",
    paste0("These surfaces are in `predictions[[\"", obj, "\"]]`."), "")
}


# PREDICT, SECTION --------------------------------------------------------

#' Rmd source for the Predict section
#'
#' Reads session_data from the enclosing server environment.
#'
#' @returns A character vector of Rmd lines.
#'
#' @noRd
report_predict_section <- function(){

  preds <- session_data$ellipsoid_prediction_list

  # Virtual sessions never reach this step, and a session can be saved before
  # anything has been predicted
  if(identical(session_data$input_mode, "virtual") || length(preds) == 0){
    return(character(0))
  }

  ids <- names(preds)
  ells <- session_data$ellipsoid_list[ids]
  keep <- !vapply(ells, is.null, logical(1))
  ids <- ids[keep]
  ells <- ells[keep]
  preds <- preds[ids]

  if(length(ells) == 0) return(character(0))

  objs <- unname(report_ell_objects()[ids])

  args <- lapply(seq_along(ells), function(i){
    report_pred_args(ids[i], preds[[i]], ells[[i]])
  })

  # Grouped on names as well as values, since a group with the truncation
  # argument and one without differ in length and values alone could collide
  keys <- vapply(args, function(a) paste(names(a), unlist(a), collapse = "|"),
                 character(1))
  uniq <- unique(keys)

  # Computed once so each paragraph can place its niche against the others
  areas <- setNames(vapply(preds, report_pred_suitable, numeric(1)), ids)

  # Each group is followed by the niches it covers, so a reader moves from a
  # call to its results rather than back through every call first
  multi <- length(uniq) > 1

  blocks <- unlist(lapply(seq_along(uniq), function(g){

    sel <- which(keys == uniq[g])
    out <- if(multi) paste0("predictions_", g) else "predictions"

    # A rule and a heading with several niches under it, so the code reads as
    # belonging to what follows rather than to the summaries above. Niche
    # headings drop a level to sit inside the group.
    open <- if(multi){
      c("***", "",
        paste0("### Prediction ", g), "",
        paste0("The following ", length(sel), " niche",
               if(length(sel) == 1) " was" else "s were",
               " projected together, since they were run with the same ",
               "settings."), "")
    } else {
      character(0)
    }

    # Shown once. Repeating it under every group would say the same thing
    # about a differently named list each time.
    how <- if(g == 1){
      lyr <- report_pred_layer_names(preds[[sel[1]]], ells[[sel[1]]])[1]
      c(paste0("Each element of `", out, "` is named after the niche it came ",
               "from, so a single surface can be pulled out with, for ",
               "example, `", out, "[[\"", objs[sel[1]], "\"]][[\"", lyr,
               "\"]]`."), "")
    } else {
      character(0)
    }

    detail <- unlist(lapply(sel, function(i){
      c(paste0(if(multi) "#### " else "### ", ells[[i]]$ell_name), "",
        report_pred_ell_prose(ells[[i]], preds[[i]], ids[i], objs[i], areas))
    }))

    c(open,
      report_chunk(report_pred_code(objs[sel], args[[sel[1]]], out),
                   label = paste0("predict-", g), eval = report_can_eval()),
      how,
      detail)
  }))

  c("## Projecting the niches", "",
    report_predict_prose(ells, preds, length(uniq)),
    blocks)
}


# CITATION ----------------------------------------------------------------

#' Citation block for the report
#'
#' Built from citation() rather than written out, so a change to inst/CITATION
#' or DESCRIPTION propagates without editing this file.
#'
#' @returns A character vector of markdown lines.
#'
#' @noRd
report_citation <- function(){

  cit <- tryCatch(format(utils::citation("nicheR"), style = "text"),
                  error = function(e) NULL)

  c("## Citation", "",
    "This analysis was produced with the nicheR R package. If you use these",
    "results in a publication, please cite:", "",
    if(!is.null(cit)){
      paste0("> ", unlist(strsplit(cit, "\n")))
    } else {
      "> Castaneda-Guzman M, Hughes C, Paansri P, Cobos M (2026). nicheR."
    },
    "",
    "You can regenerate this citation at any time with `citation(\"nicheR\")`.",
    "")
}
