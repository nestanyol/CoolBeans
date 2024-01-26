#' Runs the shiny app example.
#'
#' @description This function runs the shiny app example. Using runShiny()
#' @export
#' 
runShiny <- function() {
  appDir <- system.file("shiny-apps", "CoolBeans_shinyApp", package = "CoolBeans")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CoolBeans`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}