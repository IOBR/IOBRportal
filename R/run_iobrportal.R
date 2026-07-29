#' Run IOBRportal
#'
#' Launch the Shiny application included in the IOBRportal package.
#'
#' @param app_dir A character string specifying the directory of the Shiny app.
#'   The default is `NULL`, which uses the bundled `inst/shinyapp` directory.
#'
#' @return No return value. This function launches the IOBRportal application.
#'
#' @export
#' @author Qingcong Luo
#'
#' @examples
#' \dontrun{
#' run_iobrportal()
#' }
run_iobrportal <- function(app_dir = NULL) {

  if (is.null(app_dir)) {
    app_dir <- system.file(
      "shinyapp",
      package = "IOBRportal"
    )
  }

  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "Shiny app folder 'shinyapp' was not found ",
      "in the installed IOBRportal package."
    )
  }

  shiny::runApp(appDir = app_dir)
}