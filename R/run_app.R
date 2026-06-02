#' Run the Shiny app, launching the browser
#' #initial commit -adding project fies

#' @export
run_app <- function() {

  # --- Source functions ---
  my_f <- list.files(path="R", pattern= "\\.R$|\\.r$", full.names=T)
  sapply(my_f,source)

  app_dir <- system.file("app", package = "Base5x3")
  if (app_dir == "") {
    stop("Could not find example Shiny app. Try reinstalling `Base5x3`.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, launch.browser = TRUE)
}

