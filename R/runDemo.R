#' @title Estimate area function
#'
#' @author Corinne & Laurene
#' @export
runDemo <- function() {
  appDir <- system.file("shiny-examples", "area", package = "pkghw4g1")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing pkghw4g1.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}

