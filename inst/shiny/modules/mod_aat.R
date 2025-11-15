# inst/shiny/modules/mod_aat.R

mod_aat_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("AAT Scanner (Auditory Ambiguity Test)"),

    # WIP Warning
    wellPanel(
      style = "background-color: #fff3cd; border-left: 4px solid #ffc107;",
      tags$h4(
        style = "color: #856404; margin-top: 0;",
        "⚠️ Work in Progress - Not Yet Ready for Production Use"
      ),
      tags$p(
        style = "color: #856404; margin-bottom: 0;",
        tags$strong("This module is currently under active development and testing."),
        " While basic functionality works, the parsing logic for different AAT file formats ",
        "is still being validated with real data. Please use with caution and report any issues. ",
        "For production analysis, manually verify all extracted values against source files."
      )
    ),

    # Help/Instructions Panel
    wellPanel(
      style = "background-color: #f8f9fa;",
      h4("📊 How AAT Scanning Works"),
      p("This tool extracts AAT (Auditory Ambiguity Test) metrics from CSV response files (*.itl.csv or *.csv)."),
      tags$ul(
        tags$li(tags$strong("Ambiguous (%):"), " Percentage of f0-responses (fundamental) in ambiguous items"),
        tags$li(tags$strong("Control (%):"), " Percentage of correct responses in control items"),
        tags$li(tags$strong("Quality metrics:"), " Counts of ambivalent (2) and 'don't know' (-1) responses")
      ),
      p(
        style = "margin-top: 10px; padding: 8px; background-color: #e8f5e9; border-left: 3px solid #4caf50;",
        tags$strong("💡 Tip:"), " Place all AAT CSV files in one folder or organize them into subfolders by group/condition.",
        " The scanner will find all CSV files recursively!"
      )
    ),

    hr(),

    # Folder Selection
    h4("1. Select AAT Data Folder"),
    fluidRow(
      column(
        width = 6,
        if (requireNamespace("shinyFiles", quietly = TRUE)) {
          tagList(
            shinyFiles::shinyDirButton(
              ns("root_dir"),
              "Browse for folder",
              "Select the folder containing AAT CSV files",
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
          placeholder = "C:/path/to/AAT"
        ),
        tags$small(
          class = "form-text text-muted",
          "Paste the full path to your AAT data directory"
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
        hr(),
        tags$details(
          tags$summary(tags$strong("📁 Sample File Paths (click to expand)")),
          tags$pre(
            style = "background-color: #f8f9fa; padding: 10px; max-height: 400px; overflow-y: auto; font-family: monospace; font-size: 12px;",
            verbatimTextOutput(ns("sample_paths"))
          )
        )
      )
    ),

    # Advanced Settings
    h4("2. Advanced Settings (Optional)"),
    wellPanel(
      style = "background-color: #f0f0f0;",
      tags$details(
        tags$summary(tags$strong("⚙ Advanced Settings (click to expand)")),
        br(),
        fluidRow(
          column(
            width = 4,
            textInput(
              ns("code_pattern"),
              "Participant Code Pattern (regex):",
              value = "(\\d{4}[A-Za-zÄÖÜäöüß]{4})",
              placeholder = "(\\d{4}[A-Za-zÄÖÜäöüß]{4})"
            ),
            tags$small(class = "text-muted", "Default: 4 digits + 4 letters (includes Ä,Ö,Ü,ß)")
          ),
          column(
            width = 4,
            selectInput(
              ns("date_format"),
              "Date Format in Filenames:",
              choices = c(
                "DD/MM/YY (e.g., 13/03/25)" = "DDMMYY",
                "DD/MM/YYYY (e.g., 13/03/2025)" = "DDMMYYYY",
                "YY/MM/DD (e.g., 25/03/13)" = "YYMMDD",
                "YYYY/MM/DD (e.g., 2025/03/13)" = "YYYYMMDD",
                "MM/DD/YY (e.g., 03/13/25)" = "MMDDYY",
                "MM/DD/YYYY (e.g., 03/13/2025)" = "MMDDYYYY"
              ),
              selected = "DDMMYY"
            ),
            tags$small(class = "text-muted", "Date format used in AAT filenames")
          ),
          column(
            width = 4,
            checkboxInput(
              ns("show_quality_only"),
              "Show only participants with quality issues",
              value = FALSE
            ),
            tags$small(class = "text-muted", "Filter for high ambivalent/don't-know counts")
          )
        )
      )
    ),

    hr(),

    # Scan Button
    h4("3. Start Scanning"),
    fluidRow(
      column(
        width = 8,
        actionButton(
          ns("scan"),
          "▶ Scan AAT Files",
          class = "btn-success btn-lg btn-block"
        )
      ),
      column(
        width = 4,
        actionButton(
          ns("show_r_code"),
          "📜 Show R Code",
          class = "btn-info btn-block",
          style = "margin-top: 0;"
        )
      )
    ),

    br(), br(),

    # Progress and Results
    conditionalPanel(
      condition = sprintf("output['%s']", ns("show_results")),
      wellPanel(
        style = "background-color: #f5f5f5;",
        h4("📊 Scan Results"),

        # Summary Statistics
        htmlOutput(ns("summary_stats")),

        hr(),

        # Data Table
        h5("Participant-Level Results"),
        p("Double-click any cell to edit. Download includes all edits."),
        DT::DTOutput(ns("aat_table")),

        br(),

        # Download Button
        downloadButton(ns("download_csv"), "💾 Download Results as CSV", class = "btn-primary"),

        br(), br(),

        # Quality Report
        h5("Quality Report"),
        tags$details(
          tags$summary(tags$strong("🔍 Participants with Quality Issues (click to expand)")),
          br(),
          DT::DTOutput(ns("quality_issues_table"))
        )
      )
    ),

    # Expected File Format Panel
    hr(),
    wellPanel(
      style = "background-color: #fffbea; border-left: 4px solid #ffb74d;",
      h5("📄 Expected File Format"),
      p(tags$strong("Filename:"), " AAT_response_<CODE>_<DATE>.itl.csv (or any CSV file with participant code)"),
      p(tags$strong("Required Column:"), " 'Pitch Classification' with codes:"),
      tags$ul(
        tags$li("0 = spectral/overtone response"),
        tags$li("1 = fundamental (f0) response"),
        tags$li("2 = ambivalent"),
        tags$li("-1 = don't know")
      ),
      p(tags$strong("Optional Columns:")),
      tags$ul(
        tags$li("'Item Type' or 'Trial Type': Marks items as 'ambiguous' or 'control'"),
        tags$li("'Correct Answer': Required for calculating control percentage")
      )
    )
  )
}

mod_aat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      root_path = NULL,
      structure_info = NULL,
      aat_data = NULL,
      aat_data_edited = NULL
    )

    # Setup shinyFiles directory chooser
    if (requireNamespace("shinyFiles", quietly = TRUE)) {
      roots <- c(home = normalizePath("~"), wd = getwd())
      shinyFiles::shinyDirChoose(input, "root_dir", roots = roots)

      # Update root path when folder selected via browser
      observeEvent(input$root_dir, {
        if (!is.null(input$root_dir) && !is.integer(input$root_dir)) {
          dir_path <- shinyFiles::parseDirPath(roots, input$root_dir)
          if (length(dir_path) > 0) {
            rv$root_path <- as.character(dir_path)
          }
        }
      })
    }

    # Update root path when manually entered
    observeEvent(input$root_manual, {
      if (nchar(input$root_manual) > 0 && dir.exists(input$root_manual)) {
        rv$root_path <- input$root_manual
      }
    })

    # Display selected root path
    output$root_path <- renderText({
      if (is.null(rv$root_path)) {
        "No folder selected"
      } else {
        rv$root_path
      }
    })

    # Analyze folder structure
    observeEvent(input$analyze, {
      req(rv$root_path)

      withProgress(message = "Analyzing folder structure...", value = 0.5, {
        tryCatch({
          rv$structure_info <- aat_analyze_structure(rv$root_path)
        }, error = function(e) {
          showNotification(
            paste("Error analyzing structure:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Display structure summary
    output$structure_summary <- renderUI({
      req(rv$structure_info)

      info <- rv$structure_info

      HTML(paste0(
        "<p><strong>Structure Type:</strong> ", info$structure, "</p>",
        "<p><strong>Total CSV files found:</strong> ", info$n_files, "</p>",
        "<p><strong>Subfolders with files:</strong> ", nrow(info$subfolder_summary), "</p>"
      ))
    })

    # Display sample file paths
    output$sample_paths <- renderText({
      req(rv$structure_info)
      paste(rv$structure_info$sample_paths, collapse = "\n")
    })

    # Scan AAT files
    observeEvent(input$scan, {
      req(rv$root_path)

      withProgress(message = "Scanning AAT files...", value = 0, {
        tryCatch({
          # Get parameters
          code_pattern <- input$code_pattern
          if (is.null(code_pattern) || code_pattern == "") {
            code_pattern <- "(\\d{4}[A-Za-z]{4})"
          }

          date_format <- input$date_format
          if (is.null(date_format) || date_format == "") {
            date_format <- "DDMMYY"
          }

          # Scan files
          rv$aat_data <- aat_scan(
            root = rv$root_path,
            code_pattern = code_pattern,
            date_format = date_format
          )

          # Initialize edited data
          rv$aat_data_edited <- rv$aat_data

          showNotification(
            paste("Successfully scanned", nrow(rv$aat_data), "files!"),
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          showNotification(
            paste("Error during scan:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # --- Show R Code ----------------------------------------------------------
    observeEvent(input$show_r_code, {
      root_path <- rv$root_path
      req(root_path)

      # Escape backslashes for R code
      escaped_path <- gsub("\\\\", "\\\\\\\\", root_path)

      r_code <- sprintf('# Load the musicAnalysis package
library(musicAnalysis)

# Scan AAT CSV files
aat_data <- aat_scan(
  root = "%s",
  code_pattern = "path",  # Options: "path", "filename", or custom regex
  date_format = "DDMMYY"  # Options: "DDMMYY", "DDMMYYYY", "YYMMDD", "YYYYMMDD"
)

# View the data
View(aat_data)

# Save to CSV
write.csv(aat_data, "aat_results.csv", row.names = FALSE)', escaped_path)

      showModal(modalDialog(
        title = "📜 R Code for AAT Scanning",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("copy_code"), "📋 Copy to Clipboard", class = "btn-primary"),
          downloadButton(ns("download_r_code"), "💾 Download .R File", class = "btn-success"),
          modalButton("Close")
        ),
        tags$div(
          tags$p("Use this R code to reproduce the AAT scan outside of the Shiny app:"),
          tags$pre(
            style = "background-color: #f4f4f4; padding: 15px; border-radius: 5px; overflow-x: auto; max-height: 400px;",
            tags$code(r_code)
          ),
          tags$div(
            id = ns("copy_notification"),
            style = "display: none; color: #28a745; margin-top: 10px;",
            "✓ Code copied to clipboard!"
          )
        ),
        tags$script(HTML(sprintf('
          $("#%s").click(function() {
            var code = $(this).closest(".modal-content").find("code").text();
            navigator.clipboard.writeText(code).then(function() {
              $("#%s").show().delay(2000).fadeOut();
            });
          });
        ', ns("copy_code"), ns("copy_notification"))))
      ))
    })

    output$download_r_code <- downloadHandler(
      filename = function() {
        paste0("aat_scan_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        root_path <- rv$root_path
        escaped_path <- gsub("\\\\", "\\\\\\\\", root_path)

        r_code <- sprintf('# Load the musicAnalysis package
library(musicAnalysis)

# Scan AAT CSV files
aat_data <- aat_scan(
  root = "%s",
  code_pattern = "path",  # Options: "path", "filename", or custom regex
  date_format = "DDMMYY"  # Options: "DDMMYY", "DDMMYYYY", "YYMMDD", "YYYYMMDD"
)

# View the data
View(aat_data)

# Save to CSV
write.csv(aat_data, "aat_results.csv", row.names = FALSE)', escaped_path)

        writeLines(r_code, file)
      }
    )

    # Show results panel
    output$show_results <- reactive({
      !is.null(rv$aat_data) && nrow(rv$aat_data) > 0
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    # Summary statistics
    output$summary_stats <- renderUI({
      req(rv$aat_data)

      data <- rv$aat_data

      # Calculate statistics
      n_participants <- nrow(data)
      mean_ambiguous <- round(mean(data$ambiguous_pct, na.rm = TRUE), 1)
      mean_control <- round(mean(data$control_pct, na.rm = TRUE), 1)
      n_with_quality_issues <- sum(data$n_ambivalent > 5 | data$n_dont_know > 3, na.rm = TRUE)

      HTML(paste0(
        "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 5px;'>",
        "<p><strong>Total Participants:</strong> ", n_participants, "</p>",
        "<p><strong>Mean Ambiguous %:</strong> ", mean_ambiguous, "%</p>",
        "<p><strong>Mean Control %:</strong> ", mean_control, "%</p>",
        "<p><strong>Participants with Quality Issues:</strong> ", n_with_quality_issues,
        " (>5 ambivalent or >3 don't know)</p>",
        "</div>"
      ))
    })

    # Data table
    output$aat_table <- DT::renderDT({
      req(rv$aat_data_edited)

      data_display <- rv$aat_data_edited

      # Apply quality filter if enabled
      if (isTRUE(input$show_quality_only)) {
        data_display <- data_display %>%
          dplyr::filter(n_ambivalent > 5 | n_dont_know > 3)
      }

      DT::datatable(
        data_display,
        editable = list(target = "cell", disable = list(columns = c(ncol(data_display) - 1))),  # Disable file column
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        caption = "AAT Results - Double-click to edit cells"
      )
    })

    # Handle edits to data table
    observeEvent(input$aat_table_cell_edit, {
      info <- input$aat_table_cell_edit
      rv$aat_data_edited[info$row, info$col] <- info$value
    })

    # Quality issues table
    output$quality_issues_table <- DT::renderDT({
      req(rv$aat_data)

      quality_issues <- rv$aat_data %>%
        dplyr::filter(n_ambivalent > 5 | n_dont_know > 3) %>%
        dplyr::select(code, ambiguous_pct, control_pct, n_ambivalent, n_dont_know, file)

      if (nrow(quality_issues) == 0) {
        return(DT::datatable(
          data.frame(Message = "No quality issues detected!"),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        quality_issues,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    # Download CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("AAT_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(rv$aat_data_edited, file)
      }
    )
  })
}
