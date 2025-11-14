# inst/shiny/modules/mod_pppt.R

mod_pppt_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("PPPT Scanner (Pitch Perception Proficiency Test)"),

    # Help/Instructions Panel
    wellPanel(
      style = "background-color: #f8f9fa;",
      h4("🎵 How PPPT Scanning Works"),
      p("This tool extracts PPPT (Pitch Perception Proficiency Test) metrics from .rsl.csv files."),
      tags$ul(
        tags$li(tags$strong("PPP Index:"), " Pitch Perception Proficiency index for each UCF frequency"),
        tags$li(tags$strong("UCF Frequencies:"), " 294, 523, 932, 1661, 2960, 5274 Hz"),
        tags$li(tags$strong("Overall Index:"), " Overall PPP index across all frequencies")
      ),
      p(
        style = "margin-top: 10px; padding: 8px; background-color: #e8f5e9; border-left: 3px solid #4caf50;",
        tags$strong("💡 Tip:"), " The scanner automatically detects PPPT files by their content (UCF column),",
        " even if the filename doesn't contain 'PPPT'!"
      )
    ),

    hr(),

    # Folder Selection
    h4("1. Select PPPT Data Folder"),
    fluidRow(
      column(
        width = 6,
        if (requireNamespace("shinyFiles", quietly = TRUE)) {
          tagList(
            shinyFiles::shinyDirButton(
              ns("root_dir"),
              "Browse for folder",
              "Select the folder containing PPPT .rsl.csv files",
              class = "btn-primary btn-lg"
            ),
            tags$div(
              style = "margin-top: 10px; padding: 10px; background-color: #e9ecef; border-radius: 4px;",
              tags$strong("Selected folder:"),
              tags$br(),
              textOutput(ns("root_path"))
            )
          )
        } else {
          tags$div(
            class = "alert alert-warning",
            "Package 'shinyFiles' not installed. Please install it to browse for folders:",
            tags$code("install.packages('shinyFiles')")
          )
        }
      ),
      column(
        width = 6,
        textInput(
          ns("root_manual"),
          "Or paste folder path manually:",
          value = "",
          placeholder = "C:/path/to/PPPT"
        ),
        tags$small(
          class = "form-text text-muted",
          "Paste the full path to your PPPT data directory"
        ),
        br(), br(),
        actionButton(
          ns("analyze"),
          "🔍 Analyze Folder Structure",
          class = "btn-info btn-block"
        )
      )
    ),

    br(),

    # Folder Structure Display (conditional)
    conditionalPanel(
      condition = sprintf("output['%s'] != ''", ns("structure_summary")),
      wellPanel(
        style = "background-color: #e7ffe7;",
        h5("📊 Detected Folder Structure"),
        htmlOutput(ns("structure_summary")),
        br(),
        p(
          style = "font-size: 0.9em; color: #666;",
          "The scanner found PPPT .rsl.csv files. Configure settings below and click 'Scan Files'."
        )
      )
    ),

    hr(),

    # Advanced Settings
    h4("2. Configure Settings (Optional)"),
    wellPanel(
      fluidRow(
        column(
          width = 4,
          textInput(
            ns("code_pattern"),
            "Participant Code Pattern:",
            value = "\\d{4}[A-Za-z]{4}",
            placeholder = "Regex pattern"
          ),
          tags$small(
            class = "form-text text-muted",
            "Default: 4 digits + 4 letters (e.g., 9905MAHE)"
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("date_format"),
            "Date Format in Filenames:",
            choices = c(
              "DDMMYY (e.g., 160325 → 16/03/2025)" = "DDMMYY",
              "DDMMYYYY (e.g., 16032025)" = "DDMMYYYY",
              "YYMMDD (e.g., 250316)" = "YYMMDD",
              "YYYYMMDD (e.g., 20250316)" = "YYYYMMDD",
              "MMDDYY (e.g., 031625)" = "MMDDYY",
              "MMDDYYYY (e.g., 03162025)" = "MMDDYYYY"
            ),
            selected = "DDMMYY"
          ),
          tags$small(
            class = "form-text text-muted",
            "Select the date format used in your filenames"
          )
        )
      )
    ),

    hr(),

    # Scan Button
    h4("3. Scan Files"),
    actionButton(
      ns("scan"),
      "📁 Scan PPPT Files",
      class = "btn-success btn-lg btn-block",
      icon = icon("search")
    ),

    br(), br(),

    # Progress indicator
    conditionalPanel(
      condition = sprintf("input['%s'] > 0", ns("scan")),
      wellPanel(
        style = "background-color: #fff;",
        h5("Processing..."),
        verbatimTextOutput(ns("scan_status"))
      )
    ),

    # Results
    uiOutput(ns("results_panel"))
  )
}

mod_pppt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      root_path = NULL,
      structure_info = NULL,
      pppt_data = NULL,
      edited_data = NULL
    )

    # Folder browser (if shinyFiles available)
    if (requireNamespace("shinyFiles", quietly = TRUE)) {
      volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
      shinyFiles::shinyDirChoose(input, "root_dir", roots = volumes, session = session)

      observeEvent(input$root_dir, {
        if (!is.integer(input$root_dir)) {
          rv$root_path <- shinyFiles::parseDirPath(volumes, input$root_dir)
        }
      })
    }

    # Update root path from manual input
    observeEvent(input$root_manual, {
      if (nzchar(input$root_manual)) {
        rv$root_path <- input$root_manual
      }
    })

    # Display selected path
    output$root_path <- renderText({
      if (!is.null(rv$root_path) && length(rv$root_path) > 0) {
        as.character(rv$root_path)
      } else {
        "No folder selected"
      }
    })

    # Analyze folder structure
    observeEvent(input$analyze, {
      req(rv$root_path)

      tryCatch({
        rv$structure_info <- pppt_analyze_structure(rv$root_path)
      }, error = function(e) {
        showNotification(
          paste("Error analyzing folder:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Display structure summary
    output$structure_summary <- renderUI({
      req(rv$structure_info)

      tagList(
        tags$p(
          tags$strong("📊 Found:"),
          tags$ul(
            tags$li(sprintf("%d .rsl.csv files", rv$structure_info$n_rsl_files)),
            tags$li(sprintf("%d .itl.csv files", rv$structure_info$n_itl_files))
          )
        )
      )
    })

    # Scan PPPT files
    observeEvent(input$scan, {
      req(rv$root_path)

      # Show progress
      output$scan_status <- renderText({
        paste("Scanning folder:", rv$root_path, "\n",
              "Code pattern:", input$code_pattern, "\n",
              "Date format:", input$date_format, "\n",
              "Please wait...")
      })

      tryCatch({
        # Perform scan
        result <- pppt_scan(
          root = rv$root_path,
          code_pattern = input$code_pattern,
          date_format = input$date_format
        )

        rv$pppt_data <- result
        rv$edited_data <- result  # Initialize edited data

        output$scan_status <- renderText({
          sprintf("✓ Successfully scanned %d PPPT file(s)", nrow(result))
        })

        showNotification(
          sprintf("Successfully processed %d PPPT file(s)", nrow(result)),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        output$scan_status <- renderText({
          paste("✗ Error:", e$message)
        })
        showNotification(
          paste("Error scanning files:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Render results panel
    output$results_panel <- renderUI({
      req(rv$pppt_data)

      tagList(
        hr(),
        h4("4. Review Results"),

        # Summary statistics
        wellPanel(
          style = "background-color: #e3f2fd;",
          h5("📊 Summary Statistics"),
          tags$p(
            tags$strong("Total participants:"), nrow(rv$pppt_data), tags$br(),
            tags$strong("Mean PPP Index (Overall):"),
            sprintf("%.2f", mean(rv$pppt_data$ppp_index_overall, na.rm = TRUE)), tags$br(),
            tags$strong("Range:"),
            sprintf("[%.2f, %.2f]",
                    min(rv$pppt_data$ppp_index_overall, na.rm = TRUE),
                    max(rv$pppt_data$ppp_index_overall, na.rm = TRUE))
          )
        ),

        # Data table
        wellPanel(
          h5("📄 PPPT Data"),
          p(
            style = "color: #666; font-size: 0.9em;",
            tags$strong("💡 Tip:"), " Double-click any cell to edit its value.",
            " Your changes will be reflected in the downloaded CSV."
          ),
          DT::dataTableOutput(ns("data_table"))
        ),

        # Action buttons
        fluidRow(
          column(
            width = 4,
            downloadButton(
              ns("download_csv"),
              "💾 Download CSV",
              class = "btn-primary btn-lg btn-block"
            )
          ),
          column(
            width = 4,
            actionButton(
              ns("show_code"),
              "📝 Show R Code",
              class = "btn-info btn-lg btn-block"
            )
          )
        ),

        br(),

        # R Code display (conditional)
        conditionalPanel(
          condition = "input.show_code > 0",
          ns = ns,
          wellPanel(
            style = "background-color: #f8f9fa;",
            h4("R Code to Scan PPPT Files"),
            p("Copy and paste this code into your R console to reproduce this scan:"),
            verbatimTextOutput(ns("r_code")),
            downloadButton(ns("download_code"), "Download Code (.R)")
          )
        )
      )
    })

    # Data table
    output$data_table <- DT::renderDataTable({
      req(rv$pppt_data)

      DT::datatable(
        rv$edited_data,
        editable = list(target = "cell", disable = list(columns = c(0))), # Protect first column
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE
      )
    })

    # Handle edits
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      rv$edited_data[info$row, info$col + 1] <- info$value
    })

    # R Code output
    output$r_code <- renderText({
      req(rv$root_path)

      code <- sprintf('library(musicAnalysis)

# Scan PPPT files
pppt_data <- pppt_scan(
  root = "%s",
  code_pattern = "%s",
  date_format = "%s"
)

# View results
View(pppt_data)

# Save to CSV
write.csv(pppt_data, "pppt_data.csv", row.names = FALSE)
',
        rv$root_path,
        input$code_pattern,
        input$date_format
      )

      code
    })

    # Download CSV handler
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("pppt_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(rv$edited_data, file)
      }
    )

    # Download R code handler
    output$download_code <- downloadHandler(
      filename = function() {
        paste0("pppt_scan_code_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) {
        code <- sprintf('library(musicAnalysis)

# Scan PPPT files
pppt_data <- pppt_scan(
  root = "%s",
  code_pattern = "%s",
  date_format = "%s"
)

# View results
View(pppt_data)

# Save to CSV
write.csv(pppt_data, "pppt_data.csv", row.names = FALSE)
',
          rv$root_path,
          input$code_pattern,
          input$date_format
        )
        writeLines(code, file)
      }
    )
  })
}
