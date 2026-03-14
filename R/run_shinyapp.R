#' Run Shiny App for IOBRportal
#'
#' Launch the Shiny application included in the IOBRportal package.
#'
#' @param app_dir A character string specifying the directory of the Shiny app.
#'   Default is NULL, which uses the built-in app folder `shinyapp` from the installed package.
#'
#' @return Launches a Shiny app in the default web browser.
#' @export
#' @author Qingcong Luo
#'
#' @examples
#' # Run the Shiny app bundled in the package
#' run_shinyapp()
run_shinyapp <- function(app_dir = NULL) {
  if (is.null(app_dir)) {
    app_dir <- system.file("shinyapp", package = "IOBRportal")
  }

  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Shiny app folder 'shinyapp' not found in the package directory.")
  }

  shiny::runApp(appDir = app_dir)
}