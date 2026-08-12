# Title: Session report, build section, shared helpers

# Description: Emits the Rmd source for the Build step. The generated document
# holds real code chunks, so one file is both the runnable script the user
# takes away and the source knitted to produce the HTML report.

# Each ellipsoid is emitted self contained, built from its own range_inputs
# rather than chained off its parent. The library records current state, not
# click history, so a chained script stops reproducing the session as soon as
# a parent is edited after being copied. Lineage appears in the prose and in a
# comment on each block.

# Description: Primitives used by every report section. Sourced before
# report.R. Anything that formats a value as code, names an object, or decides
# how the emitted script reaches its data lives here, so the section
# generators only decide what to emit and not how to write it.

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

#' Citation and licence block for the report
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
    if(!is.null(cit)) paste0("> ", unlist(strsplit(cit, "\n"))) else
      "> Castaneda-Guzman M, Hughes C, Paansri P, Cobos M (2026). nicheR.",
    "",
    "You can regenerate this citation at any time with",
    "`citation(\"nicheR\")`.", "")
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

# DATA --------------------------------------------------------------------

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

# EMISSION PRIMITIVES -----------------------------------------------------

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


#' Whether the emitted script can run at report time
#'
#' Sessions built on uploaded files refer to a folder only the user can point
#' at, so their chunks are emitted unevaluated. Example and virtual sessions
#' are self contained and are evaluated, which checks that the script runs.
#'
#' @returns A single logical.
#'
#' @noRd
report_can_eval <- function(){
  session_data$input_mode %in% c("example", "virtual")
}


# ELLIPSOID EMISSION ------------------------------------------------------

#' Code that rebuilds one range specification
#'
#' The three branches mirror the three ways the Build tab sets ranges. Manual
#' ranges become a data frame literal, the other two become calls, so the
#' script documents the method and not only its result.
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

  if(identical(ell$range_method, "data")){
    src <- if(is.null(ri$source_file)) "occurrences.csv" else ri$source_file
    tbl <- paste0(obj, "_occ")
    cols <- paste0("[, c(", paste0("\"", vars, "\"", collapse = ", "), ")]")
    return(list(pre = paste0(tbl, " <- read.csv(file.path(data_dir, \"",
                             src, "\"))"),
                code = report_call(obj, "ranges_from_data",
                                   list(data = paste0(tbl, cols),
                                        expand_min = report_num(ri$expand_min),
                                        expand_max = report_num(ri$expand_max)))))
  }

  mins <- vapply(vars, function(v) ri$min[[v]], numeric(1))
  maxs <- vapply(vars, function(v) ri$max[[v]], numeric(1))
  args <- as.list(setNames(paste0("c(", report_num(mins), ", ",
                                  report_num(maxs), ")"),
                           report_name(vars)))

  list(pre = character(0), code = report_call(obj, "data.frame", args))
}


#' Off diagonal covariance entries
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A named numeric vector, empty when every pair is zero.
#'
#' @noRd
report_cov_pairs <- function(ell){
  v <- ell$var_names
  if(length(v) < 2) return(numeric(0))
  idx <- which(upper.tri(ell$cov_matrix), arr.ind = TRUE)
  out <- setNames(ell$cov_matrix[idx], paste0(v[idx[, 1]], "-", v[idx[, 2]]))
  out[out != 0]
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

report_centroid_moved <- function(ell){
  any(report_centroid_diff(ell$centroid, report_centroid_origin(ell), ell))
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
  if(length(cov) > 0){
    out <- c(out, "",
             report_call(obj, "update_ellipsoid_covariance",
                         list(object = obj, covariance = report_vec(cov))))
  }

  if(report_centroid_moved(ell)){
    out <- c(out, "",
             report_call(obj, "update_ellipsoid_centroid",
                         list(ell = obj,
                              new_centroid = report_vec(round(ell$centroid[ell$var_names], 2)))))
  }

  out
}


# SECTION -----------------------------------------------------------------

#' Prose describing what was built
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

  cov_n <- vapply(ells, function(e) length(report_cov_pairs(e)) > 0, logical(1))
  if(any(cov_n)){
    rng <- range(unlist(lapply(ells[cov_n], report_cov_pairs)))
    out <- c(out, "",
             paste0("Covariance between environmental variables was set in ",
                    sum(cov_n), " of the ", n,
                    " niches, with off-diagonal values from ",
                    report_num(rng[1]), " to ", report_num(rng[2]),
                    ", rotating the ellipsoid relative to the variable axes ",
                    "without changing its tolerance breadth."))
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

  obj <- report_obj_names(vapply(ells, function(e) e$ell_name, character(1)),
                          prefix = "ellipsoid")

  names(obj) <- vapply(ells, function(e) e$ell_id, character(1))

  # Ranges are deduplicated on the code they produce, so two ellipsoids share
  # a range object exactly when rebuilding them would emit the same lines
  drafts <- lapply(ells, report_range_code, obj = "range")
  keys <- vapply(drafts, function(d) paste(c(d$pre, d$code), collapse = "\n"),
                 character(1))
  uniq <- unique(keys)
  range_obj <- paste0("range_", seq_along(uniq))
  range_of <- range_obj[match(keys, uniq)]

  ev <- report_can_eval()

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
           " of environmental ranges below ", if(length(uniq) == 1) "is" else "are",
           " shared by the niches that follow."),
    "",
    report_chunk(range_lines, label = "build-ranges", eval = ev),
    blocks)
}

# PER ELLIPSOID PROSE -----------------------------------------------------

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


#' Sentence describing the confidence level
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A single string.
#'
#' @noRd
report_cl_prose <- function(ell){
  paste0("The confidence level of ", report_num(ell$cl), " sets where the ",
         "boundary falls, so conditions beyond it are treated as
         outside the niche.")
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
  if(length(cov) == 0) return(NULL)

  v1 <- sub("-.*$", "", names(cov))
  v2 <- sub("^.*-", "", names(cov))
  sign_txt <- ifelse(cov > 0, "positive", "negative")

  parts <- paste0("a ", sign_txt, " covariance of ", report_num(unname(cov)),
                  " between ", v1, " and ", v2)

  paste0("Covariance was set for ",
         length(cov), if(length(cov) == 1) " variable pair: " else " variable pairs: ",
         report_and(parts), ". ",
         "A non-zero covariance tilts the ellipsoid away from the variable ",
         "axes, so the range of ", v1[1], " the niche includes depends on the ",
         "value of ", v2[1], " rather than being the same throughout. ",
         "A positive value means the two increase together across the niche; ",
         "a negative value means one increases as the other decreases. ",
         "Because the covariance matrix determines the volume, this also ",
         "changes how much environmental space the niche occupies.")
}


#' Sentence describing the centroid move, if any
#'
#' Reports the shift per variable in the units of that variable. No direction
#' is characterised beyond its sign, since the app does not know what any
#' variable measures.
#'
#' @param ell A nicheR ellipsoid.
#'
#' @returns A single string, or NULL when the centroid was not moved.
#'
#' @noRd
report_centroid_prose <- function(ell){

  vars <- ell$var_names
  parent <- ell_parent(ell)

  ref <- if(!is.null(parent)) parent$centroid[vars] else report_centroid_origin(ell)
  ref_txt <- if(!is.null(parent)){
    paste0("relative to ", parent$ell_name)
  } else {
    "relative to the midpoint of its ranges"
  }

  d <- ell$centroid[vars] - ref
  moved <- vars[abs(d) > 1e-8]
  if(length(moved) == 0) return(NULL)

  dv <- d[moved]
  parts <- paste0(moved, " by ", ifelse(dv > 0, "+", ""),
                  report_num(unname(dv)), " (from ", report_num(unname(ref[moved])),
                  " to ", report_num(unname(ell$centroid[moved])), ")")

  paste0("The centroid sits ", ref_txt, " shifted in ", report_and(parts),
         ". Translating the centroid changes only where the niche sits, so ",
         "the ellipsoid keeps whatever shape and orientation it had before ",
         "the move. It describes the same set of tolerances centred on ",
         "different values of ", report_and(moved), ".")
}


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

  c(paste0(open, report_range_prose(ell), " ", report_cl_prose(ell), vol),
    "",
    report_cov_prose(ell),
    if(!is.null(report_cov_prose(ell))) "",
    report_centroid_prose(ell),
    if(!is.null(report_centroid_prose(ell))) "")
}




# predict section ---------------------------------------------------------


# ARGUMENTS ---------------------------------------------------------------

#' Predict arguments implied by a stored prediction
#'
#' The app does not record which arguments were passed, so they are recovered
#' from the layers that came back. This is exact for the truncated layers,
#' which appear only when asked for.
#'
#' @param pred A stored prediction, SpatRaster or data frame.
#' @param ell The ellipsoid it belongs to, used to drop predictor columns.
#'
#' @returns A named list of logicals.
#'
#' @noRd
report_pred_layers <- function(pred, ell){
  nms <- if(inherits(pred, "SpatRaster")) names(pred) else colnames(pred)
  nms <- setdiff(nms, c("x", "y", ell$var_names))
  list(include_mahalanobis = "Mahalanobis" %in% nms,
       include_suitability = "suitability" %in% nms,
       mahalanobis_truncated = "Mahalanobis_trunc" %in% nms,
       suitability_truncated = "suitability_trunc" %in% nms)
}


#' Signature grouping ellipsoids predicted the same way
#'
#' @param pred A stored prediction.
#' @param ell The ellipsoid it belongs to.
#'
#' @returns A single string.
#'
#' @noRd
report_pred_signature <- function(pred, ell){
  paste(unlist(report_pred_layers(pred, ell)), collapse = "|")
}


#' Code that predicts a group of ellipsoids
#'
#' @param ells The ellipsoids in the group.
#' @param objs Their object names in the emitted script.
#' @param pred One stored prediction from the group, used for the arguments.
#' @param out Name to give the resulting list.
#'
#' @returns A character vector of code lines.
#'
#' @noRd
report_pred_code <- function(ells, objs, pred, out){

  args <- report_pred_layers(pred, ells[[1]])
  arg_lines <- paste0("                  ", names(args), " = ",
                      ifelse(unlist(args), "TRUE", "FALSE"), ",")

  c(paste0(out, "_ellipsoids <- list(",
           paste0(objs, " = ", objs, collapse = ", "), ")"),
    "",
    paste0(out, " <- lapply(", out, "_ellipsoids, function(e){"),
    paste0("  predict(e,"),
    paste0("          newdata = ", report_data_object(), ","),
    sub("^ {18}", "          ", arg_lines),
    "          verbose = FALSE)",
    "})")
}


# SUMMARY -----------------------------------------------------------------

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


#' Table of what each ellipsoid was predicted to
#'
#' @param ells The predicted ellipsoids.
#' @param preds Their stored predictions.
#'
#' @returns A character vector of markdown lines.
#'
#' @noRd
report_pred_table <- function(ells, preds){

  rows <- vapply(seq_along(ells), function(i){

    nms <- if(inherits(preds[[i]], "SpatRaster")){
      names(preds[[i]])
    } else {
      colnames(preds[[i]])
    }
    nms <- setdiff(nms, c("x", "y", ells[[i]]$var_names))

    p <- report_pred_suitable(preds[[i]])
    p_txt <- if(is.na(p)) "not calculated" else paste0(report_num(round(p * 100, 1)), "%")

    paste0("| ", ells[[i]]$ell_name, " | ", paste(nms, collapse = ", "),
           " | ", p_txt, " |")
  }, character(1))

  c("| Niche | Layers produced | Study area within the niche |",
    "|---|---|---|",
    rows, "")
}


# PROSE -------------------------------------------------------------------

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
  where <- if(spatial) "across the study area" else "for each row of the background data"

  out <- c(paste0("Each of the ", n, " niche", if(n == 1) "" else "s",
                  " defined above was projected ", where,
                  ". For every ", if(spatial) "cell" else "record",
                  ", the environmental values are compared against the ",
                  "ellipsoid to give a measure of how close those conditions ",
                  "sit to the centre of the niche.",
                  if(n_group > 1){
                    paste0(" Not every niche was projected with the same ",
                           "outputs requested, so the calls below are grouped ",
                           "by what was asked for.")
                  } else {
                    ""
                  }))

  layers <- unique(unlist(lapply(seq_along(ells), function(i){
    nms <- if(spatial) names(preds[[i]]) else colnames(preds[[i]])
    setdiff(nms, c("x", "y", ells[[i]]$var_names))
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
                                 "rather than how close to its centre they ",
                                 "sit."))

  have <- meanings[names(meanings) %in% layers]

  c(out, "",
    "The outputs produced were:", "",
    paste0("- ", unname(have)), "",
    report_pred_table(ells, preds))
}


# SECTION -----------------------------------------------------------------

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

  all_obj <- report_obj_names(vapply(session_data$ellipsoid_list,
                                     function(e) e$ell_name, character(1)),
                              prefix = "ellipsoid")
  names(all_obj) <- names(session_data$ellipsoid_list)
  objs <- unname(all_obj[ids])

  keys <- vapply(seq_along(ells), function(i){
    report_pred_signature(preds[[i]], ells[[i]])
  }, character(1))
  uniq <- unique(keys)

  # One lapply per group of ellipsoids that were predicted with the same
  # arguments. With a single group, which is the usual case, this is one call.
  blocks <- unlist(lapply(seq_along(uniq), function(g){
    sel <- which(keys == uniq[g])
    out <- if(length(uniq) == 1) "predictions" else paste0("predictions_", g)
    report_chunk(report_pred_code(ells[sel], objs[sel], preds[[sel[1]]], out),
                 label = paste0("predict-", g), eval = report_can_eval())
  }))

  c("## Projecting the niches", "",
    report_predict_prose(ells, preds, length(uniq)),
    blocks,
    paste0("Each element of `predictions` is named after the niche it came ",
           "from, so a single surface can be pulled out with, for example, ",
           "`predictions[[\"", objs[1], "\"]][[\"suitability\"]]`."),
    "")
}


