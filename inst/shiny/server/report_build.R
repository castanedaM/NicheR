# Title: Session report, build section

# Description: Emits the Rmd source for the Build step. The generated document
# holds real code chunks, so one file is both the runnable script the user
# takes away and the source knitted to produce the HTML report.

# Each ellipsoid is emitted self contained, built from its own range_inputs
# rather than chained off its parent. The library records current state, not
# click history, so a chained script stops reproducing the session as soon as
# a parent is edited after being copied. Lineage appears in the prose and in a
# comment on each block.

# Date last updated: 08/12/2026


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

  mins <- unlist(ri$min[vars])
  maxs <- unlist(ri$max[vars])
  args <- as.list(setNames(paste0("c(", report_num(mins), ", ",
                                  report_num(maxs), ")"),
                           report_name(vars)))

  pre <- if(identical(ri$method, "df")){
    c("# Ranges derived in the app from an uploaded occurrence table using",
      paste0("# ranges_from_data(), with expansion of ",
             paste(report_num(emin), collapse = ", "), " and ",
             paste(report_num(emax), collapse = ", "),
             ". The resolved bounds are"),
      "# given directly so this script runs without that table.")
  } else {
    character(0)
  }

  list(pre = pre, code = report_call(obj, "data.frame", args))
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

  if(!isTRUE(all.equal(ell$centroid, ell$centroid_build))){
    out <- c(out, "",
             report_call(obj, "update_ellipsoid_centroid",
                         list(ell = obj,
                              new_centroid = report_vec(ell$centroid[ell$var_names]))))
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
                    "without changing its volume."))
  }

  moved <- vapply(ells, function(e){
    !isTRUE(all.equal(e$centroid, e$centroid_build))
  }, logical(1))
  if(any(moved)){
    out <- c(out, "",
             paste0("The centroid was translated in ", sum(moved), " of the ",
                    n, " niches, shifting the position of the niche in ",
                    "environmental space while holding its shape, size, and ",
                    "orientation constant."))
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

  range_lines <- unlist(lapply(seq_along(uniq), function(i){
    d <- report_range_code(ells[[match(uniq[i], keys)]], range_obj[i])
    c(d$pre, d$code, "")
  }))

  ell_lines <- unlist(lapply(seq_along(ells), function(i){
    p <- ells[[i]]$parent_id
    p_obj <- if(!is.null(p) && p %in% names(obj)) obj[[p]] else NULL
    c(report_ell_code(ells[[i]], obj[[i]], range_of[i], p_obj), "")
  }))

  c("## Building ellipsoidal niches", "",
    report_build_prose(ells, length(uniq)),
    report_chunk(c(report_data_code(), "", range_lines, ell_lines),
                 label = "build", eval = report_can_eval()),
    "The ellipsoids created above are used in the sections that follow.", "")
}
