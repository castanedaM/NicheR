#' nicheR Shiny App
#'
#' @export
 run_app <- function(){

  app_dir <- system.file("shiny", package = "nicheR")

  if(app_dir  == ""){
    stop("Could not find shiny directory. Try re-installing `nicheR`.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
