#' Run the Shiny app, launching the browser
#' #initial commit -adding project fies

#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "Base5x3")
  if (app_dir == "") {
    stop("Could not find example Shiny app. Try reinstalling `Base5x3`.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, launch.browser = TRUE)
}

