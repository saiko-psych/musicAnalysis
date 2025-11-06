# inst/shiny/app.R

# Load required libraries
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(plotly)
library(stringr)
library(readr)

# Source modules
mod_path <- file.path(system.file("shiny/modules", package = "musicAnalysis"))
source(file.path(mod_path, "mod_home.R"), local = TRUE)
source(file.path(mod_path, "mod_klawa.R"), local = TRUE)
source(file.path(mod_path, "mod_mexp.R"), local = TRUE)
source(file.path(mod_path, "mod_merge.R"), local = TRUE)

ui <- navbarPage(
  title = "musicAnalysis",
  id = "main_nav",  # Add ID for programmatic navigation
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tabPanel("Home", value = "home", mod_home_ui("home")),
  tabPanel("KLAWA", value = "klawa", mod_klawa_ui("klawa")),
  tabPanel("Musical Experience", value = "mexp", mod_mexp_ui("mexp")),
  tabPanel("Merge", value = "merge", mod_merge_ui("merge"))
)

server <- function(input, output, session) {
  mod_home_server("home", parent_session = session)
  mod_klawa_server("klawa")
  mod_mexp_server("mexp")
  mod_merge_server("merge")
}

shinyApp(ui, server)
