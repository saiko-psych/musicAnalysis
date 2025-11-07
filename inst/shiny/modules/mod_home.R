# inst/shiny/modules/mod_home.R

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # ASCII Art Banner
      wellPanel(
        style = "background-color: #2c3e50; color: #ecf0f1; font-family: monospace; padding: 20px;",
        tags$pre(
          style = "margin: 0; font-size: 14px; line-height: 1.2;",
"
                            ╔═══════════════════════════════════════════════════════════════════════╗
                            ║                                                                       ║
                            ║   ███╗   ███╗██╗   ██╗███████╗██╗ ██████╗                             ║
                            ║   ████╗ ████║██║   ██║██╔════╝██║██╔════╝                             ║
                            ║   ██╔████╔██║██║   ██║███████╗██║██║                                  ║
                            ║   ██║╚██╔╝██║██║   ██║╚════██║██║██║                                  ║
                            ║   ██║ ╚═╝ ██║╚██████╔╝███████║██║╚██████╗                             ║
                            ║   ╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═╝ ╚═════╝                             ║
                            ║                                                                       ║
                            ║            █████╗ ███╗   ██╗ █████╗ ██╗  ██╗   ██╗███████╗██╗███████╗ ║
                            ║           ██╔══██╗████╗  ██║██╔══██╗██║  ╚██╗ ██╔╝██╔════╝██║██╔════╝ ║
                            ║           ███████║██╔██╗ ██║███████║██║   ╚████╔╝ ███████╗██║███████╗ ║
                            ║           ██╔══██║██║╚██╗██║██╔══██║██║    ╚██╔╝  ╚════██║██║╚════██║ ║
                            ║           ██║  ██║██║ ╚████║██║  ██║███████╗██║   ███████║██║███████║ ║
                            ║           ╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝╚═╝   ╚══════╝╚═╝╚══════╝ ║
                            ║                                                                       ║
                            ║        Music Psychology Data Preparation for University of Graz       ║
                            ║                                                                       ║
                            ╚═══════════════════════════════════════════════════════════════════════╝
"
        )
      ),

      # Version Information
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
        fluidRow(
          column(
            width = 6,
            tags$div(
              style = "font-size: 16px;",
              tags$strong("📦 Package Version: "),
              tags$span(
                style = "color: #3498db; font-family: monospace;",
                as.character(utils::packageVersion("musicAnalysis"))
              )
            )
          ),
          column(
            width = 6,
            tags$div(
              style = "font-size: 16px;",
              tags$strong("🕐 Build Date: "),
              tags$span(
                style = "color: #27ae60; font-family: monospace;",
                "2025-11-07"
              )
            )
          )
        )
      ),

      # What's New Section
      wellPanel(
        style = "background-color: #e8f5e9; border-left: 4px solid #4caf50;",
        h4("✨ What's New in v0.0.0.9031 (2025-11-07)"),
        tags$ul(
          style = "line-height: 1.8;",
          tags$li(
            tags$strong("FIXED (CRITICAL): "),
            "AAT calculation now uses TONE PAIR aggregation method - matches user-provided formula"
          ),
          tags$li(
            tags$strong("METHOD: "),
            "Groups individual responses into tone pairs, then calculates: AAT Score = 100 * sum(# F0 per pair) / sum(# Items per pair)"
          ),
          tags$li(
            tags$strong("PATTERN: "),
            "Control items identified by Nmin same harmonics (e.g., '3 3', '4 4'). Ambiguous items have different harmonics (e.g., '5 2', '7 3')"
          ),
          tags$li(
            tags$strong("VERIFIED: "),
            "100% success rate - all 25 item-level .rsl files match their .itl files perfectly!"
          ),
          tags$li(
            tags$strong("TESTING: "),
            "All 136 tests passing"
          )
        )
      ),

      # Version History - Collapsible
      wellPanel(
        style = "background-color: #f5f5f5; border-left: 4px solid #9e9e9e;",
        tags$details(
          tags$summary(
            style = "cursor: pointer; padding: 10px; font-size: 18px; font-weight: bold; color: #424242; user-select: none;",
            "📜 Version History (Click to expand/collapse)"
          ),
          tags$div(
            style = "padding: 15px 10px;",

            # v0.0.0.9026
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9026 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " AAT .rsl parsing extracts Ambiguous % and Control % correctly"),
                tags$li(tags$strong("FIXED:"), " AAT .itl parsing handles column names with suffixes"),
                tags$li(tags$strong("ADDED:"), " AAT module description on home page, WIP warning added"),
                tags$li(tags$strong("WORKFLOW:"), " Implemented feature branch workflow (dev → feature/* → PR)")
              )
            ),

            # v0.0.0.9025
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9025 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("REFINED:"), " AAT module now filters for files containing 'AAT' in filename only"),
                tags$li(tags$strong("ADDED:"), " Automatic file type detection: .itl.csv (raw) vs .rsl.csv (computed)"),
                tags$li(tags$strong("ADDED:"), " Metadata extraction from filenames: participant code and date"),
                tags$li(tags$strong("ADDED:"), " Date format selector in AAT Shiny interface")
              )
            ),

            # v0.0.0.9024
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9024 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("NEW:"), " AAT (Auditory Ambiguity Test) module implemented"),
                tags$li(tags$strong("ADDED:"), " Complete AAT analysis workflow with 31 passing tests"),
                tags$li(tags$strong("ADDED:"), " AAT Shiny interface with quality issue detection")
              )
            ),

            # v0.0.0.9023
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9023 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " Category_sum plot labels now show proper format (CODE - total Category)"),
                tags$li(tags$strong("FIXED:"), " Each participant gets unique color in category_sum plots"),
                tags$li(tags$strong("IMPROVED:"), " Plotting quality restored to v0.0.0.9018/9019 standards")
              )
            ),

            # v0.0.0.9022
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9022 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " Category_sum plot labels (initial attempt, refined in v0.0.0.9023)"),
                tags$li(tags$strong("IMPROVED:"), " Consolidated all version history into single collapsible section")
              )
            ),

            # v0.0.0.9021
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9021 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " Individual plot labels in Shiny app now show instrument names"),
                tags$li(tags$strong("FIXED:"), " Critical syntax error in mod_home.R preventing app launch"),
                tags$li(tags$strong("ENHANCED:"), " Clickable module panels on home page for easy navigation"),
                tags$li(tags$strong("ADDED:"), " Collapsible version history with expandable sections")
              )
            ),

            # v0.0.0.9019
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9019 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("ADDED:"), " number_of_instruments, number_of_singing, number_of_othermusic variables"),
                tags$li(tags$strong("ADDED:"), " nodme (number of different musical experiences) - total count")
              )
            ),

            # v0.0.0.9018
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #fff3e0; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #e65100;", "v0.0.0.9018 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " KLAWA Data Quality Analysis now properly refreshes on each scan with custom parameters"),
                tags$li(tags$strong("FIXED:"), " Musical Experience category_sum plotting error (was: 'In argument: category_label = .get_category_label')"),
                tags$li(tags$strong("ENHANCED:"), " Plot legends now show instrument names (e.g., '0102SICH - Klavier') instead of codes"),
                tags$li(tags$strong("IMPROVED:"), " Enhanced hover tooltips to display instrument/singing/othermusic names in all plots")
              )
            ),

            # v0.0.0.9017
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #e3f2fd; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #1565c0;", "v0.0.0.9017 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " Musical experience parser now handles both Y/N and 1/2 formats for played columns"),
                tags$li(tags$strong("FIXED:"), " music_13042025.csv now shows correct practice counts (was showing '0 practiced, 0 not practiced')"),
                tags$li(tags$strong("IMPROVED:"), " Automatic normalization of survey response formats for maximum compatibility"),
                tags$li(tags$strong("IMPROVED:"), " Code cleanup and optimization across multiple modules")
              )
            ),

            # v0.0.0.9016
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #e3f2fd; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #1565c0;", "v0.0.0.9016 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " Category_sum now creates separate plots (not subplots) - one per category"),
                tags$li(tags$strong("RECALCULATED IMP:"), " IMP = average weekly hours × years practiced (total across all domains)"),
                tags$li(tags$strong("ADDED:"), " IMP_instrument, IMP_singing, IMP_othermusic, IMP_total variables in wide format")
              )
            ),

            # v0.0.0.9013
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #e8f5e9; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #2e7d32;", "v0.0.0.9013 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " Shiny app now loads correctly - fixed missing library imports"),
                tags$li(tags$strong("FIXED:"), " Corrected syntax error in Musical Experience module")
              )
            ),

            # v0.0.0.9012
            tags$div(
              style = "margin-bottom: 20px; padding: 10px; background-color: #e8f5e9; border-radius: 4px;",
              tags$h5(style = "margin-top: 0; color: #2e7d32;", "v0.0.0.9012 (2025-11-06)"),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(tags$strong("FIXED:"), " IMP variables now properly appear in wide format with correct naming"),
                tags$li(tags$strong("FIXED:"), " Multiple plots now display properly with equal sizing and consistent layout"),
                tags$li(tags$strong("IMPROVED:"), " Plot titles are now meaningful with participant/category counts"),
                tags$li(tags$strong("ADDED:"), " Download button for displayed graphs, Show/download R code feature")
              )
            ),

            # Even older versions - nested collapsible
            tags$div(
              style = "margin-top: 20px; padding: 10px; background-color: #fafafa; border-radius: 4px;",
              tags$details(
                tags$summary(
                  style = "cursor: pointer; color: #757575; font-weight: bold;",
                  "📋 Earlier Versions (v0.0.0.9005-9011)"
                ),
                tags$ul(
                  style = "margin-top: 10px; line-height: 1.6; color: #666;",
                  tags$li(tags$strong("v0.0.0.9011:"), " Enhanced practice growth curves, total musical experience, IMP"),
                  tags$li(tags$strong("v0.0.0.9010:"), " Fixed plotly customdata error"),
                  tags$li(tags$strong("v0.0.0.9009:"), " Starting age calculation, interactive growth curves"),
                  tags$li(tags$strong("v0.0.0.9008:"), " Umlaut support, date handling"),
                  tags$li(tags$strong("v0.0.0.9007:"), " Date extraction from KLAWA filenames"),
                  tags$li(tags$strong("v0.0.0.9006:"), " Editable data table, quality reports"),
                  tags$li(tags$strong("v0.0.0.9005:"), " Code mismatch detection")
                )
              )
            )
          )    # Close tags$div (line 106 - outer container)
        )      # Close tags$details (line 101)
      ),       # Close wellPanel (line 99)

      # Main Welcome Content
      h3("Welcome to musicAnalysis"),
      p(
        style = "font-size: 16px; line-height: 1.6;",
        "This Shiny application provides interactive tools for preparing and analyzing ",
        "music psychology research data at the University of Graz. Navigate to each module ",
        "using the tabs above to process your data."
      ),

      br(),

      # Module Descriptions
      h4("📚 Available Modules:"),

      wellPanel(
        style = "background-color: #e8f5e9; cursor: pointer;",
        onclick = "Shiny.setInputValue('home-navigate_to', 'klawa', {priority: 'event'});",
        tags$div(
          style = "padding: 10px;",
          tags$h5(
            style = "margin-top: 0; color: #2e7d32;",
            "🎵 KLAWA - Voice/Singing Performance Scanner ",
            tags$span(
              style = "font-size: 12px; color: #666; font-weight: normal;",
              "(Click to open)"
            )
          ),
          tags$p(
            style = "margin-bottom: 10px; font-weight: 500;",
            "Extract voice and singing performance metrics from KLAWA PDF test results"
          ),
          tags$p(
            style = "margin-bottom: 10px;",
            "KLAWA (Kindliche Leistungen im Auditiven Wahrnehmungsbereich) is a standardized test ",
            "for measuring auditory perception skills in children, particularly singing accuracy. ",
            "This module automatically scans hierarchical folder structures containing PDF reports ",
            "and extracts quantitative measurements."
          ),
          tags$div(
            style = "background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 4px; margin-top: 10px;",
            tags$strong("Key Features:"),
            tags$ul(
              style = "margin-top: 5px; margin-bottom: 0;",
              tags$li("Auto-detects metadata from folder structure (groups, pre/post measurements, PC identifiers)"),
              tags$li("Extracts metrics: volume difference, pitch accuracy, onset timing, duration precision"),
              tags$li("Visual folder tree display to verify structure"),
              tags$li("Validates participant codes and detects conflicts"),
              tags$li("Data quality analysis with detailed problem reports"),
              tags$li("Editable results table for manual corrections"),
              tags$li("CSV export of cleaned data")
            )
          )
        )
      ),

      wellPanel(
        style = "background-color: #e3f2fd; cursor: pointer;",
        onclick = "Shiny.setInputValue('home-navigate_to', 'mexp', {priority: 'event'});",
        tags$div(
          style = "padding: 10px;",
          tags$h5(
            style = "margin-top: 0; color: #1565c0;",
            "🎼 Musical Experience - Practice History & Proficiency ",
            tags$span(
              style = "font-size: 12px; color: #666; font-weight: normal;",
              "(Click to open)"
            )
          ),
          tags$p(
            style = "margin-bottom: 10px; font-weight: 500;",
            "Process musical training questionnaires and visualize practice trajectories"
          ),
          tags$p(
            style = "margin-bottom: 10px;",
            "Parses LimeSurvey exports containing detailed musical background information including ",
            "instrument practice history, singing experience, and other musical activities. ",
            "Converts complex time formats and generates comprehensive experience profiles."
          ),
          tags$div(
            style = "background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 4px; margin-top: 10px;",
            tags$strong("Key Features:"),
            tags$ul(
              style = "margin-top: 5px; margin-bottom: 0;",
              tags$li("Parses practice time strings (e.g., '2d', '1.5w', '3m', '2y') into yearly hours"),
              tags$li("Calculates total practice hours per instrument/singing/other music"),
              tags$li("Computes starting ages for each musical activity"),
              tags$li("Generates count variables: number_of_instruments, nodme (total musical experiences)"),
              tags$li("Interactive practice growth curves with proper instrument names in legends"),
              tags$li("Three plot types: individual instruments, category sums, total musical experience"),
              tags$li("Flags unrealistic values for manual review"),
              tags$li("Wide and long format outputs for analysis flexibility")
            )
          )
        )
      ),

      wellPanel(
        style = "background-color: #f3e5f5; cursor: pointer;",
        onclick = "Shiny.setInputValue('home-navigate_to', 'aat', {priority: 'event'});",
        tags$div(
          style = "padding: 10px;",
          tags$h5(
            style = "margin-top: 0; color: #6a1b9a;",
            "🎧 AAT - Auditory Ambiguity Test ",
            tags$span(
              style = "font-size: 12px; color: #666; font-weight: normal;",
              "(Click to open)"
            )
          ),
          tags$p(
            style = "margin-bottom: 10px; font-weight: 500;",
            "Extract pitch perception metrics from AAT CSV export files"
          ),
          tags$p(
            style = "margin-bottom: 10px;",
            "The Auditory Ambiguity Test (AAT) measures the tendency to perceive fundamental (f0) tones ",
            "versus spectral/overtone components in ambiguous pitch stimuli, plus control item accuracy. ",
            "This module processes both raw response files (.itl.csv) and computed result files (.rsl.csv)."
          ),
          tags$div(
            style = "background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 4px; margin-top: 10px;",
            tags$strong("Key Features:"),
            tags$ul(
              style = "margin-top: 5px; margin-bottom: 0;",
              tags$li("Scans folders recursively for all AAT files"),
              tags$li("Extracts participant code and date from filenames"),
              tags$li("Handles two file types: .itl (raw responses) and .rsl (computed results)"),
              tags$li("Calculates Ambiguous % (f0 tendency) and Control % (accuracy) from .rsl files"),
              tags$li("Provides quality metrics: ambivalent responses, 'don't know' responses, total evaluable items"),
              tags$li("Configurable date format recognition (DDMMYY, DDMMYYYY, etc.)"),
              tags$li("CSV export of all extracted metrics")
            )
          )
        )
      ),

      wellPanel(
        style = "background-color: #fff3e0; cursor: pointer;",
        onclick = "Shiny.setInputValue('home-navigate_to', 'merge', {priority: 'event'});",
        tags$div(
          style = "padding: 10px;",
          tags$h5(
            style = "margin-top: 0; color: #e65100;",
            "🔗 Merge Datasets - Combine Multiple Data Sources ",
            tags$span(
              style = "font-size: 12px; color: #666; font-weight: normal;",
              "(Click to open)"
            )
          ),
          tags$p(
            style = "margin-bottom: 10px; font-weight: 500;",
            "Combine KLAWA results with musical experience data and other sources"
          ),
          tags$p(
            style = "margin-bottom: 10px;",
            "Safely merge multiple datasets by participant code with automatic validation. ",
            "Handles code normalization, duplicate detection, and provides merge quality reports."
          ),
          tags$div(
            style = "background-color: rgba(255,255,255,0.5); padding: 10px; border-radius: 4px; margin-top: 10px;",
            tags$strong("Key Features:"),
            tags$ul(
              style = "margin-top: 5px; margin-bottom: 0;",
              tags$li("Safe left join by participant code"),
              tags$li("Automatic code normalization (trim whitespace, handle empty values)"),
              tags$li("Duplicate detection and warnings"),
              tags$li("NA inflation tracking to assess merge quality"),
              tags$li("Preview merged data before export"),
              tags$li("CSV export of combined dataset")
            )
          )
        )
      ),

      br(),

      # Quick Tips
      wellPanel(
        style = "background-color: #fff9c4; border-left: 4px solid #fbc02d;",
        tags$h5(
          style = "margin-top: 0;",
          "💡 Quick Tips"
        ),
        tags$ul(
          tags$li(
            tags$strong("KLAWA Auto-Detection: "),
            "Click 'Analyze Folder Structure' to see what the tool detects in your data"
          ),
          tags$li(
            tags$strong("Custom Patterns: "),
            "Need different metadata? Use the customization panel to override auto-detected values"
          ),
          tags$li(
            tags$strong("Data Quality: "),
            "Always check the validation and problem reports before proceeding with analysis"
          ),
          tags$li(
            tags$strong("Export: "),
            "Download processed data as CSV for use in R scripts or other tools"
          )
        )
      ),

      br(),

      # Footer
      tags$div(
        style = "text-align: center; color: #7f8c8d; font-size: 14px; margin-top: 30px; padding-top: 20px; border-top: 1px solid #ecf0f1;",
        tags$p(
          "Developed by David Matischek | ",
          tags$a(href = "mailto:david.matischek@uni-graz.at", "david.matischek@uni-graz.at"),
          " | University of Graz"
        ),
        tags$p(
          style = "font-family: monospace; font-size: 12px;",
          "Package: musicAnalysis | License: MIT"
        )
      )
    )
  )
}

mod_home_server <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    # Handle navigation to other tabs
    observeEvent(input$navigate_to, {
      if (!is.null(parent_session)) {
        updateTabsetPanel(parent_session, "main_nav", selected = input$navigate_to)
      }
    })
  })
}
