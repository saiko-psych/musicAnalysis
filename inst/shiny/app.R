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

      /* Default wellPanel styling - remove Bootstrap defaults */
      .well {
        background-image: none !important;
        box-shadow: none !important;
        border: 1px solid rgba(139, 115, 85, 0.3);
        margin-bottom: 25px !important;
      }

      /* Apply vintage background to wellPanels that DO NOT have explicit transparent style */
      .well:not([style*="transparent"]) {
        background-color: rgba(250, 245, 235, 0.95) !important;
      }

      /* Ensure transparent wellPanels stay transparent */
      .well[style*="transparent"] {
        background-color: transparent !important;
      }

      .panel, .info-card {
        margin-bottom: 25px !important;
        background-color: rgba(250, 245, 235, 0.95) !important;
        border: 1px solid rgba(139, 115, 85, 0.3) !important;
      }

      /* Make data tables less transparent for better readability */
      .dataTables_wrapper {
        background-color: rgba(255, 255, 255, 0.95) !important;
        padding: 15px;
        border-radius: 4px;
      }

      table.dataTable {
        background-color: rgba(255, 255, 255, 0.98) !important;
      }

      table.dataTable thead th,
      table.dataTable tbody td {
        background-color: rgba(255, 255, 255, 0.98) !important;
      }

      /* Tab content and form spacing */
      .tab-content {
        padding-top: 20px;
      }
      .form-group {
        margin-bottom: 20px !important;
      }

      /* Button spacing - use Bootstrap 3 defaults but ensure they are not overly large */
      .btn {
        padding: 6px 12px;
        font-size: 14px;
      }

      .btn-lg {
        padding: 10px 16px;
        font-size: 18px;
      }

      .btn-sm {
        padding: 5px 10px;
        font-size: 12px;
      }

      /* Icon styling in navbar */
      .navbar-nav .fa, .navbar-nav .fab {
        margin-right: 6px;
      }

      /* Underline all clickable links and make them visible */
      a {
        text-decoration: underline !important;
        cursor: pointer;
      }

      /* Except navbar links */
      .navbar a {
        text-decoration: none !important;
      }

      /* And buttons styled as links */
      .btn {
        text-decoration: none !important;
      }

      /* Override Bootstrap Flatly turquoise colors with brown */
      a {
        color: #8B7355 !important;
      }

      a:hover, a:focus {
        color: #6B5D52 !important;
      }

      /* Tab colors - brown instead of turquoise */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #2c1810 !important;
        background-color: #fff !important;
        border-bottom-color: transparent !important;
        border-top: 3px solid #8B7355 !important;
      }

      .nav-tabs > li > a:hover {
        border-top: 3px solid #6B5D52 !important;
        background-color: rgba(139, 115, 85, 0.1) !important;
      }

      /* Details/summary elements - MAKE THEM OBVIOUSLY CLICKABLE */
      summary {
        cursor: pointer !important;
        text-decoration: underline !important;
        color: #8B7355 !important;
        font-weight: bold !important;
      }

      summary:hover {
        color: #6B5D52 !important;
        text-decoration: underline !important;
      }

      /* Ensure summary strong elements also get the color */
      summary strong {
        color: #8B7355 !important;
      }

      summary:hover strong {
        color: #6B5D52 !important;
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

    # Contact menu - using navbarMenu for proper placement on right
    navbarMenu(
      "Contact",
      icon = NULL,
      tabPanel(
        tags$a(
          href = "https://github.com/saiko-psych/musicAnalysis",
          target = "_blank",
          style = "display: block; padding: 3px 20px; color: #3d2817 !important;",
          tags$i(class = "fab fa-github"), " GitHub"
        ),
        value = "github_link"
      ),
      tabPanel(
        tags$a(
          href = "mailto:david.matischek@uni-graz.at",
          style = "display: block; padding: 3px 20px; color: #3d2817 !important;",
          tags$i(class = "fa fa-envelope"), " Email"
        ),
        value = "email_link"
      )
    )
  ),

  # Add CSS to move Contact menu to the right
  tags$head(
    tags$script(HTML('
      $(document).ready(function() {
        $("li:has(a:contains(Contact))").addClass("navbar-right");
      });
    '))
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
