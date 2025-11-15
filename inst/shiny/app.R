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
  # Custom CSS for navbar, background, and spacing
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML('
      /* Music sheet background for all pages */
      body {
        background-image: url("music_sheet.jpg");
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
        background-repeat: no-repeat;
      }

      /* Semi-transparent overlay for better readability - but NOT for navbar container */
      body > .container-fluid {
        background-color: rgba(255, 255, 255, 0.75);
        min-height: 100vh;
        padding-bottom: 60px;
      }

      /* Ensure navbar container-fluid stays compact */
      .navbar .container-fluid {
        min-height: auto !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
      }

      /* Navbar – compact Bootstrap 3 version */
      .navbar {
        margin-bottom: 0 !important;
        border-radius: 0 !important;
      }

      .navbar-default {
        background: linear-gradient(135deg, #8B7355 0%, #6B5D52 100%);
        border: none;
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        min-height: 40px !important;
        max-height: 40px !important;
      }

      .navbar-header {
        min-height: 40px !important;
        max-height: 40px !important;
      }

      .navbar-default .navbar-brand {
        color: #2c1810 !important;
        font-weight: 600;
        font-size: 16px !important;
        padding: 10px 15px !important;
        height: 40px !important;
        line-height: 20px !important;
      }

      .navbar-default .navbar-nav {
        margin: 0 !important;
      }

      .navbar-default .navbar-nav > li > a {
        color: #3d2817 !important;
        font-weight: 500;
        font-size: 14px !important;
        padding: 10px 12px !important;
        height: 40px !important;
        line-height: 20px !important;
      }

      /* Move Contact menu to the right */
      .navbar-right {
        float: right !important;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: rgba(61, 40, 23, 0.15) !important;
        color: #2c1810 !important;
      }

      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        background-color: rgba(61, 40, 23, 0.10) !important;
        color: #2c1810 !important;
      }

      .navbar-default .navbar-toggle {
        margin-top: 3px !important;
        margin-bottom: 3px !important;
        padding: 6px 8px !important;
      }

      /* Ensure navbar dropdown menus also compact */
      .navbar-default .navbar-nav .open .dropdown-menu > li > a {
        padding: 8px 20px !important;
        line-height: 18px !important;
      }

      /* Better spacing between sections - vintage sepia backgrounds */
      .well, .panel, .info-card {
        margin-bottom: 25px !important;
        background-color: rgba(250, 245, 235, 0.95) !important;
        border: 1px solid rgba(139, 115, 85, 0.3) !important;
      }

      /* Tab content and form spacing */
      .tab-content {
        padding-top: 20px;
      }
      .form-group {
        margin-bottom: 20px !important;
      }

      /* Button spacing - prevent overly wide buttons */
      .btn {
        padding: 6px 12px !important;
        font-size: 14px !important;
        line-height: 1.42857143 !important;
      }

      .btn-lg {
        padding: 10px 16px !important;
        font-size: 16px !important;
        line-height: 1.3333333 !important;
      }

      .btn-sm {
        padding: 5px 10px !important;
        font-size: 12px !important;
        line-height: 1.5 !important;
      }

      /* Ensure btn-block does not override reasonable sizing */
      .btn-block {
        display: block !important;
        width: 100% !important;
      }

      /* Icon styling in navbar */
      .navbar-nav .fa, .navbar-nav .fab {
        margin-right: 6px;
      }

      /* Attribution footer */
      .attribution {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        background-color: rgba(139, 115, 85, 0.9);
        color: #F5E6D3;
        padding: 8px 15px;
        font-size: 11px;
        text-align: center;
        z-index: 1000;
      }
      .attribution a {
        color: #FFE4B5;
        text-decoration: none;
      }
      .attribution a:hover {
        text-decoration: underline;
      }
      .attribution img {
        height: 1em;
        margin-right: 0.125em;
        display: inline;
      }
    ')),

  ),

  navbarPage(
    title = "musicAnalysis",
    id = "main_nav",
    theme = bslib::bs_theme(version = 3, bootswatch = "flatly"),
    position = "static-top",

    tabPanel(
      "Home",
      value = "home",
      mod_home_ui("home")
    ),
    tabPanel(
      "KLAWA",
      value = "klawa",
      mod_klawa_ui("klawa")
    ),
    tabPanel(
      "Musical Experience",
      value = "mexp",
      mod_mexp_ui("mexp")
    ),
    tabPanel(
      "AAT",
      value = "aat",
      mod_aat_ui("aat")
    ),
    tabPanel(
      "PPPT",
      value = "pppt",
      mod_pppt_ui("pppt")
    ),
    tabPanel(
      "Merge",
      value = "merge",
      mod_merge_ui("merge")
    ),

    # Contact menu - on the right
    tags$li(
      class = "dropdown navbar-right",
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        "Contact ",
        tags$b(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(
          tags$a(
            href = "https://github.com/saiko-psych/musicAnalysis",
            target = "_blank",
            tags$i(class = "fab fa-github"), " GitHub"
          )
        ),
        tags$li(
          tags$a(
            href = "mailto:david.matischek@uni-graz.at",
            tags$i(class = "fa fa-envelope"), " Email"
          )
        )
      )
    )
  ),

  # Attribution footer
  tags$div(
    class = "attribution",
    HTML('<p style="margin: 0;">"<a rel="noopener noreferrer" href="https://www.flickr.com/photos/24029425@N06/3573510810">Take Me Out to the Ball-Game [Page 4]</a>" by <a rel="noopener noreferrer" href="https://www.flickr.com/photos/24029425@N06">Boston Public Library</a> is licensed under <a rel="noopener noreferrer" href="https://creativecommons.org/licenses/by/2.0/?ref=openverse">CC BY 2.0 <img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg" style="height: 1em; margin-right: 0.125em; display: inline;" /><img src="https://mirrors.creativecommons.org/presskit/icons/by.svg" style="height: 1em; margin-right: 0.125em; display: inline;" /></a>.</p>')
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
