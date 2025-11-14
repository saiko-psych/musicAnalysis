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
source(file.path(mod_path, "mod_aat.R"), local = TRUE)
source(file.path(mod_path, "mod_pppt.R"), local = TRUE)
source(file.path(mod_path, "mod_merge.R"), local = TRUE)

ui <- tagList(
  # Custom CSS for navbar and spacing
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      /* Musical notes background for navbar */
      .navbar-default {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .navbar-default::before {
        content: '♪ ♫ ♪ ♬ ♪ ♫ ♪ ♬ ♪ ♫ ♪ ♬';
        position: absolute;
        top: 5px;
        left: 0;
        right: 0;
        text-align: center;
        font-size: 12px;
        color: rgba(255,255,255,0.15);
        letter-spacing: 20px;
        z-index: 0;
        pointer-events: none;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: 500;
        position: relative;
        z-index: 1;
      }
      .navbar-default .navbar-nav > .active > a {
        background-color: rgba(255,255,255,0.2) !important;
        color: white !important;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: rgba(255,255,255,0.15) !important;
      }

      /* Better spacing between sections */
      .well, .panel, .info-card {
        margin-bottom: 25px !important;
      }
      .tab-content {
        padding-top: 20px;
      }
      .form-group {
        margin-bottom: 20px !important;
      }

      /* Icon styling in navbar */
      .navbar-nav .fa, .navbar-nav .fab {
        margin-right: 6px;
      }
    "))
  ),

  navbarPage(
    title = tags$span(
      tags$i(class = "fa fa-music", style = "margin-right: 8px;"),
      "musicAnalysis"
    ),
    id = "main_nav",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    tabPanel(
      tags$span(tags$i(class = "fa fa-home"), "Home"),
      value = "home",
      mod_home_ui("home")
    ),
    tabPanel(
      tags$span(tags$i(class = "fa fa-file-pdf"), "KLAWA"),
      value = "klawa",
      mod_klawa_ui("klawa")
    ),
    tabPanel(
      tags$span(tags$i(class = "fa fa-guitar"), "Musical Experience"),
      value = "mexp",
      mod_mexp_ui("mexp")
    ),
    tabPanel(
      tags$span(tags$i(class = "fa fa-headphones"), "AAT"),
      value = "aat",
      mod_aat_ui("aat")
    ),
    tabPanel(
      tags$span(tags$i(class = "fa fa-wave-square"), "PPPT"),
      value = "pppt",
      mod_pppt_ui("pppt")
    ),
    tabPanel(
      tags$span(tags$i(class = "fa fa-code-merge"), "Merge"),
      value = "merge",
      mod_merge_ui("merge")
    ),

    # Contact links in navbar (right side)
    navbarMenu(
      tags$span(tags$i(class = "fa fa-address-book"), "Contact"),
      tabPanel(
        tags$a(
          href = "https://github.com/saiko-psych/musicAnalysis",
          target = "_blank",
          tags$i(class = "fab fa-github"), "GitHub",
          style = "color: inherit; text-decoration: none;"
        )
      ),
      tabPanel(
        tags$a(
          href = "mailto:david.matischek@uni-graz.at",
          tags$i(class = "fa fa-envelope"), "Email",
          style = "color: inherit; text-decoration: none;"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mod_home_server("home", parent_session = session)
  mod_klawa_server("klawa")
  mod_mexp_server("mexp")
  mod_aat_server("aat")
  mod_pppt_server("pppt")
  mod_merge_server("merge")
}

shinyApp(ui, server)
