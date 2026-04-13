# Start the Shiny app for testing
app_dir <- file.path("C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/inst/shiny")
cat("App dir:", app_dir, "\n")
cat("Exists:", dir.exists(app_dir), "\n")

if (dir.exists(app_dir)) {
  shiny::runApp(app_dir, port = 7654L, launch.browser = FALSE, host = "127.0.0.1")
}
