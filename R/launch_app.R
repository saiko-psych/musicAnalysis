#' Launch the musicAnalysis Shiny app
#'
#' Opens an interactive UI to run common workflows:
#' - Scan KLAWA PDFs
#' - Parse Musical Experience (LimeSurvey CSV)
#' - Merge datasets by `code`
#' - Adjust basic options
#'
#' @return Invisibly TRUE if app directory is found; otherwise an error is thrown.
#' @export
launch_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install it with install.packages('shiny').", call. = FALSE)
  }

  app_dir <- system.file("shiny", package = "musicAnalysis")
  if (identical(app_dir, "") || !dir.exists(app_dir)) {
    stop("Could not find Shiny app directory in this package. Did you install all package files?", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
  invisible(TRUE)
}



