# inst/shiny/modules/mod_klawa.R

mod_klawa_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("KLAWA PDF Scanner"),

    # Help/Instructions Panel
    wellPanel(
      style = "background-color: #f8f9fa;",
      h4(icon("folder-open"), " How KLAWA Scanning Works"),
      p("This tool extracts voice/singing performance metrics from KLAWA PDF files. It can auto-detect your folder structure or work with any organization."),
      p(
        style = "margin-top: 10px; padding: 8px; background-color: #e8f5e9; border-left: 3px solid #4caf50;",
        icon("lightbulb"), tags$strong(" Tip:"), " Auto-detection analyzes your folders and identifies groups, measurements, and PCs automatically!",
        " No need for pre-defined structure."
      )
    ),

    hr(),

    # Folder Selection
    h4("1. Select KLAWA Root Folder"),
    fluidRow(
      column(
        width = 6,
        if (requireNamespace("shinyFiles", quietly = TRUE)) {
          tagList(
            shinyFiles::shinyDirButton(
              ns("root_dir"),
              "Browse for folder",
              "Select the KLAWA root directory",
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
          placeholder = "C:/path/to/KLAWA"
        ),
        tags$small(
          class = "form-text text-muted",
          "Paste the full path to your KLAWA root directory"
        ),
        br(), br(),
        actionButton(
          ns("analyze"),
          "Analyze Folder Structure",
          class = "btn-info btn-block",
          icon = icon("search")
        )
      )
    ),

    br(),

    # Folder Structure Display (conditional)
    conditionalPanel(
      condition = sprintf("output['%s'] != ''", ns("structure_summary")),
      wellPanel(
        style = "background-color: #e7ffe7;",
        h5("Detected Folder Structure"),
        htmlOutput(ns("structure_summary")),
        hr(),
        tags$details(
          tags$summary(tags$strong("Folder Tree (click to expand)")),
          tags$pre(
            style = "background-color: #f8f9fa; padding: 10px; max-height: 400px; overflow-y: auto; font-family: monospace; font-size: 12px;",
            verbatimTextOutput(ns("structure_tree"))
          )
        )
      )
    ),

    # Customization Panel
    h4("2. Customize Metadata Extraction (Optional)"),
    wellPanel(
      style = "background-color: #f0f0f0;",
      tags$details(
        tags$summary(tags$strong("Advanced Settings (click to expand)")),
        br(),
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("custom_groups"),
              "Groups (comma-separated):",
              value = "",
              placeholder = "Leave empty for auto-detection"
            ),
            tags$small(class = "text-muted", "Example: LEN, LEO, MAR, MEL")
          ),
          column(
            width = 6,
            textInput(
              ns("custom_measurements"),
              "Measurements (comma-separated):",
              value = "",
              placeholder = "Leave empty for auto-detection"
            ),
            tags$small(class = "text-muted", "Example: pre, post, baseline, followup")
          )
        ),
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("custom_pcs"),
              "PCs (comma-separated):",
              value = "",
              placeholder = "Leave empty for auto-detection"
            ),
            tags$small(class = "text-muted", "Example: Jeki 4, Jeki 7, PC1, PC2")
          ),
          column(
            width = 6,
            textInput(
              ns("code_pattern"),
              "Participant Code Pattern (regex):",
              value = "(\\d{4}[A-Za-zÄÖÜäöüß]{4})",
              placeholder = "(\\d{4}[A-Za-zÄÖÜäöüß]{4})"
            ),
            tags$small(class = "text-muted", "Default: 4 digits + 4 letters (includes Ä,Ö,Ü,ß)")
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("date_format"),
              "Date Format in Filenames:",
              choices = c(
                "DD/MM/YY (e.g., 08/04/25)" = "DDMMYY",
                "DD/MM/YYYY (e.g., 08/04/2025)" = "DDMMYYYY",
                "YY/MM/DD (e.g., 25/04/08)" = "YYMMDD",
                "YYYY/MM/DD (e.g., 2025/04/08)" = "YYYYMMDD",
                "MM/DD/YY (e.g., 04/08/25)" = "MMDDYY",
                "MM/DD/YYYY (e.g., 04/08/2025)" = "MMDDYYYY"
              ),
              selected = "DDMMYY"
            ),
            tags$small(class = "text-muted", "Format of dates in your PDF filenames")
          ),
          column(
            width = 6,
            checkboxInput(
              ns("auto_detect"),
              "Enable auto-detection (recommended)",
              value = TRUE
            )
          )
        )
      )
    ),

    # Metadata Source Selection
    h4("3. Choose Metadata Source"),
    wellPanel(
      style = "background-color: #e7f3ff;",
      radioButtons(
        ns("metadata_source"),
        "Where should metadata be extracted from?",
        choices = c(
          "From folder structure & filenames (recommended)" = "path",
          "From PDF content only (participant codes only)" = "pdf"
        ),
        selected = "path"
      ),
      tags$small(
        class = "text-muted",
        "• ",tags$strong("path"), ": Extracts group, measurement, PC, and code from folder structure and filenames",
        tags$br(),
        "• ", tags$strong("pdf"), ": Extracts only participant codes from PDF content (group/measurement/PC will be NA)"
      )
    ),

    # Scan Action
    h4("4. Scan PDFs"),
    actionButton(
      ns("scan"),
      "Scan All PDFs",
      icon = icon("search"),
      class = "btn-success btn-lg"
    ),
    actionButton(
      ns("show_r_code"),
      "Show R Code",
      icon = icon("file-code"),
      class = "btn-info",
      style = "margin-left: 10px;"
    ),
    tags$span(
      style = "margin-left: 15px; color: #6c757d;",
      "Runs", tags$code("musicAnalysis::klawa_scan()"), "on the selected folder"
    ),

    br(), br(),

    # R Code Modal (hidden initially)
    uiOutput(ns("r_code_output")),

    # Results Table
    h4("5. Scanned Data"),
    tags$p(
      style = "color: #666; margin-bottom: 10px;",
      tags$strong("Tip:"), " Double-click any cell to edit its value. Edits are saved automatically and included in the CSV download."
    ),
    DT::DTOutput(ns("tbl")),

    br(),

    # Download button
    downloadButton(ns("dl_csv"), "📥 Download CSV", class = "btn-primary"),

    br(), br(),

    # Auto-generated quality analysis (detailed tables only)
    uiOutput(ns("auto_reports_ui")),

    br(), br(),

    # Folder Structure Guide
    h4("KLAWA Folder Structure & Data Preparation Guide"),
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #17a2b8;",

      h5("Ideal Folder Structure"),
      tags$pre(
        style = "background-color: #ffffff; padding: 15px; border-radius: 4px; font-family: 'Courier New', monospace; font-size: 12px; line-height: 1.4;",
"KLAWA/
├── Rechner Jeki 4/          (PC identifier)
│   ├── LEN/                 (Group name)
│   │   ├── pre/             (Measurement: before)
│   │   │   ├── 0102SICH_pre_080425.pdf
│   │   │   └── 0202MARE_pre_210525.pdf
│   │   └── post/            (Measurement: after)
│   │       ├── 0102SICH_post_080425.pdf
│   │       └── 0202MARE_post_210525.pdf
│   ├── LHG/
│   │   ├── pre/
│   │   └── post/
│   └── MAR/
│       ├── pre/
│       └── post/
└── Rechner Jeki 7/
    ├── LEO/
    │   ├── pre/
    │   └── post/
    └── LHG/
        ├── pre/
        └── post/"
      ),

      tags$hr(style = "margin: 20px 0;"),

      h5("What KLAWA Scan Does"),
      tags$ol(
        style = "line-height: 1.8;",
        tags$li(tags$strong("Detects structure:"), " Automatically identifies PCs, groups, and measurements from folder names"),
        tags$li(tags$strong("Extracts metadata:"), " Gets group, measurement, PC, and participant code from folder paths and filenames"),
        tags$li(tags$strong("Reads PDF content:"), " Extracts voice/singing metrics (volume, pitch, onset, duration) from each PDF"),
        tags$li(tags$strong("Validates codes:"), " Compares participant code in filename vs PDF content to detect mismatches"),
        tags$li(tags$strong("Quality checks:"), " Identifies missing codes, missing metadata, missing values, and conflicts")
      ),

      tags$hr(style = "margin: 20px 0;"),

      h5("File Naming Convention"),
      tags$p(
        tags$strong("Format: "), tags$code("XXXXNAME_measurement_date.pdf"),
        tags$br(),
        tags$strong("Example: "), tags$code("0102SICH_post_080425.pdf"),
        tags$br(),
        tags$span(
          style = "color: #666; font-size: 14px;",
          "• 0102 = 4 digits (participant ID)",
          tags$br(),
          "• SICH = 4 letters (participant initials)",
          tags$br(),
          "• post = measurement type",
          tags$br(),
          "• 080425 = date (optional)"
        )
      )
    )

  )
}

mod_klawa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Directory selection via shinyFiles ----------------------------------
    roots <- c(
      Home = normalizePath("~", mustWork = FALSE),
      "Working Dir" = normalizePath(".", mustWork = FALSE)
    )

    if (requireNamespace("shinyFiles", quietly = TRUE)) {
      vfun <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (!inherits(vfun, "try-error")) {
        roots <- c(roots, vfun)
      }
      shinyFiles::shinyDirChoose(input, "root_dir", roots = roots, session = session)
    }

    # Reactive state
    root_rv  <- reactiveVal(NULL)
    structure_rv <- reactiveVal(NULL)
    klawa_rv <- reactiveVal(NULL)
    problems_detailed_rv <- reactiveVal(NULL)

    output$root_path <- renderText({
      rp <- root_rv()
      if (is.null(rp) || !nzchar(rp)) {
        "No folder selected yet"
      } else {
        rp
      }
    })

    # When directory chosen via dialog
    observeEvent(input$root_dir, {
      req(requireNamespace("shinyFiles", quietly = TRUE))
      try({
        dirpath <- shinyFiles::parseDirPath(roots, input$root_dir)
        if (length(dirpath) == 1 && nzchar(dirpath)) {
          root_rv(normalizePath(dirpath, mustWork = FALSE))
        }
      }, silent = TRUE)
    })

    # If user pastes a path manually
    observeEvent(input$root_manual, {
      p <- trimws(if (is.null(input$root_manual)) "" else input$root_manual)
      if (nzchar(p)) root_rv(p)
    })

    # --- Analyze folder structure ---------------------------------------------
    # Initialize outputs
    output$structure_summary <- renderText({ "" })
    output$structure_tree <- renderText({ "" })

    # Make sure outputs are initially empty but render properly
    outputOptions(output, "structure_summary", suspendWhenHidden = FALSE)
    outputOptions(output, "structure_tree", suspendWhenHidden = FALSE)

    observeEvent(input$analyze, {
      root <- root_rv()
      validate(need(nzchar(root), "Please select or paste a root directory first."))
      validate(need(dir.exists(root), sprintf("Directory does not exist: %s", root)))

      withProgress(message = "Analyzing folder structure...", value = 0, {
        incProgress(0.3)
        structure <- try(musicAnalysis::klawa_analyze_structure(root), silent = TRUE)

        if (inherits(structure, "try-error")) {
          showNotification(
            paste("Error analyzing structure:", conditionMessage(attr(structure, "condition"))),
            type = "error",
            duration = 8
          )
          structure_rv(NULL)
        } else {
          incProgress(0.6)
          structure_rv(structure)

          # Build summary HTML
          summary_html <- sprintf(
            "<p><strong>Total PDFs found:</strong> %d</p>",
            structure$n_pdfs
          )

          if (length(structure$groups) > 0) {
            summary_html <- paste0(
              summary_html,
              sprintf("<p><strong>👥 Detected Groups (%d):</strong> %s</p>",
                      length(structure$groups),
                      paste(structure$groups, collapse = ", "))
            )
          } else {
            summary_html <- paste0(summary_html, "<p><strong>👥 Groups:</strong> <em>None detected</em></p>")
          }

          if (length(structure$measurements) > 0) {
            summary_html <- paste0(
              summary_html,
              sprintf("<p><strong>📏 Detected Measurements (%d):</strong> %s</p>",
                      length(structure$measurements),
                      paste(structure$measurements, collapse = ", "))
            )
          } else {
            summary_html <- paste0(summary_html, "<p><strong>📏 Measurements:</strong> <em>None detected</em></p>")
          }

          if (length(structure$pcs) > 0) {
            summary_html <- paste0(
              summary_html,
              sprintf("<p><strong>💻 Detected PCs (%d):</strong> %s</p>",
                      length(structure$pcs),
                      paste(structure$pcs, collapse = ", "))
            )
          } else {
            summary_html <- paste0(summary_html, "<p><strong>💻 PCs:</strong> <em>None detected</em></p>")
          }

          output$structure_summary <- renderText({ summary_html })
          output$structure_tree <- renderText({
            paste(structure$tree_text, collapse = "\n")
          })

          showNotification(
            "Folder structure analyzed successfully!",
            type = "message",
            duration = 3
          )
        }
      })
    }, ignoreInit = TRUE)

    # --- Scan action ----------------------------------------------------------
    observeEvent(input$scan, {
      root <- root_rv()
      validate(need(nzchar(root), "Please select or paste a root directory."))
      validate(need(dir.exists(root), sprintf("Directory does not exist: %s", root)))

      # Reset all reactive values to ensure fresh data
      klawa_rv(NULL)
      problems_detailed_rv(NULL)
      edited_data(NULL)

      withProgress(message = "Scanning PDFs...", value = 0, {
        incProgress(0.15, detail = "Looking for PDF files")

        metadata_src <- input$metadata_source
        if (is.null(metadata_src)) metadata_src <- "path"

        # Parse custom inputs
        custom_groups <- NULL
        if (nzchar(trimws(input$custom_groups))) {
          custom_groups <- strsplit(input$custom_groups, ",")[[1]]
          custom_groups <- trimws(custom_groups)
        }

        custom_measurements <- NULL
        if (nzchar(trimws(input$custom_measurements))) {
          custom_measurements <- strsplit(input$custom_measurements, ",")[[1]]
          custom_measurements <- trimws(custom_measurements)
        }

        custom_pcs <- NULL
        if (nzchar(trimws(input$custom_pcs))) {
          custom_pcs <- strsplit(input$custom_pcs, ",")[[1]]
          custom_pcs <- trimws(custom_pcs)
        }

        code_pattern <- input$code_pattern
        if (is.null(code_pattern) || !nzchar(trimws(code_pattern))) {
          code_pattern <- "(\\d{4}[A-Za-zÄÖÜäöüß]{4})"
        }

        date_format <- input$date_format
        if (is.null(date_format) || !nzchar(date_format)) {
          date_format <- "DDMMYY"
        }

        auto_detect <- isTRUE(input$auto_detect)

        incProgress(0.1, detail = "Configuring parameters")

        out <- try(musicAnalysis::klawa_scan(
          root,
          metadata_source = metadata_src,
          groups = custom_groups,
          measurements = custom_measurements,
          pcs = custom_pcs,
          code_pattern = code_pattern,
          date_format = date_format,
          auto_detect = auto_detect
        ), silent = TRUE)

        if (inherits(out, "try-error")) {
          msg <- conditionMessage(attr(out, "condition"))
          showNotification(
            if (!is.null(msg) && nzchar(msg)) msg else "klawa_scan() failed.",
            type = "error",
            duration = 8
          )
          klawa_rv(NULL)
        } else {
          incProgress(0.5, detail = sprintf("Found %d files", nrow(out)))
          klawa_rv(out)

          # Reset edited data on new scan
          edited_data(NULL)

          # Auto-run detailed problem analysis only
          incProgress(0.4, detail = "Analyzing data quality...")
          prob_result <- try({
            musicAnalysis::peek_problems(out, return_type = "detailed")
          }, silent = FALSE)  # Show errors for debugging

          if (inherits(prob_result, "try-error")) {
            showNotification(
              paste("Warning: Problem analysis failed:", conditionMessage(attr(prob_result, "condition"))),
              type = "warning",
              duration = 5
            )
            problems_detailed_rv(NULL)
          } else {
            problems_detailed_rv(prob_result)
          }

          showNotification(
            sprintf("Scan completed. %d files processed.", nrow(out)),
            type = "message",
            duration = 4
          )
        }
      })
    }, ignoreInit = TRUE)

    # --- Show R Code ----------------------------------------------------------
    observeEvent(input$show_r_code, {
      root_path <- input$root

      # Show template code if no path selected, or actual code if path exists
      if (is.null(root_path) || root_path == "") {
        r_code <- '# Load the musicAnalysis package
library(musicAnalysis)

# Scan KLAWA PDFs
klawa_data <- klawa_scan(
  root = "path/to/your/KLAWA/folder"
)

# View the data
View(klawa_data)

# Save to CSV
write.csv(klawa_data, "klawa_results.csv", row.names = FALSE)'
      } else {
        # Escape backslashes for R code
        escaped_path <- gsub("\\\\", "\\\\\\\\", root_path)

        r_code <- sprintf('# Load the musicAnalysis package
library(musicAnalysis)

# Scan KLAWA PDFs
klawa_data <- klawa_scan(
  root = "%s"
)

# View the data
View(klawa_data)

# Save to CSV
write.csv(klawa_data, "klawa_results.csv", row.names = FALSE)', escaped_path)
      }

      showModal(modalDialog(
        title = tagList(icon("file-code"), " R Code for KLAWA Scanning"),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("copy_code"), "Copy to Clipboard",
      icon = icon("copy"), class = "btn-primary"),
          downloadButton(ns("download_r_code"), "Download .R File",
      icon = icon("download"), class = "btn-success"),
          modalButton("Close")
        ),
        tags$div(
          tags$p("Use this R code to reproduce the KLAWA scan outside of the Shiny app:"),
          tags$pre(
            style = "background-color: #f4f4f4; padding: 15px; border-radius: 5px; overflow-x: auto; max-height: 400px;",
            tags$code(r_code)
          ),
          tags$div(
            id = ns("copy_notification"),
            style = "display: none; color: #28a745; margin-top: 10px;",
            "Code copied to clipboard!"
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
        paste0("klawa_scan_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        root_path <- input$root
        escaped_path <- gsub("\\\\", "\\\\\\\\", root_path)

        r_code <- sprintf('# Load the musicAnalysis package
library(musicAnalysis)

# Scan KLAWA PDFs
klawa_data <- klawa_scan(
  root = "%s"
)

# View the data
View(klawa_data)

# Save to CSV
write.csv(klawa_data, "klawa_results.csv", row.names = FALSE)', escaped_path)

        writeLines(r_code, file)
      }
    )

    # --- Table + download -----------------------------------------------------
    # Reactive value to store edited data
    edited_data <- reactiveVal(NULL)

    output$tbl <- DT::renderDT({
      req(klawa_rv())

      # Use edited data if available, otherwise use original
      display_data <- if (!is.null(edited_data())) edited_data() else klawa_rv()

      DT::datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          order = list(list(0, 'asc'))
        ),
        filter = "top",
        editable = list(target = "cell", disable = list(columns = c(which(names(display_data) == "file") - 1)))  # Make all columns editable except 'file'
      )
    })

    # Handle cell edits
    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit

      # Get current data (edited or original)
      current_data <- if (!is.null(edited_data())) edited_data() else klawa_rv()

      # Update the edited cell
      current_data[info$row, info$col] <- info$value

      # Store edited data
      edited_data(current_data)
    })

    output$dl_csv <- downloadHandler(
      filename = function() paste0("klawa_scan_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content  = function(file) {
        req(klawa_rv())
        # Download edited data if available, otherwise original
        data_to_save <- if (!is.null(edited_data())) edited_data() else klawa_rv()
        readr::write_csv(data_to_save, file)
      }
    )

    # --- Auto-generated reports (detailed tables only) -----------------------
    # (problems_detailed_rv is already defined earlier with other reactive values)

    # Unified UI showing only detailed problem analysis tables
    output$auto_reports_ui <- renderUI({
      req(klawa_rv())

      # Only show detailed problem tables
      if (!is.null(problems_detailed_rv()) && !inherits(problems_detailed_rv(), "try-error")) {
        wellPanel(
          style = "background-color: #f5f5f5; margin-top: 20px; padding: 20px;",
          h4(style = "margin-top: 0; color: #333;", "Data Quality Analysis"),
          p(
            style = "color: #666; margin-bottom: 15px;",
            "Detailed breakdown of all detected issues. Click tabs to explore specific problem types."
          ),
          uiOutput(ns("problem_tabs_ui"))
        )
      } else {
        NULL
      }
    })

    # Dynamic UI for problem tabs
    output$problem_tabs_ui <- renderUI({
      req(problems_detailed_rv())
      detailed <- problems_detailed_rv()

      tabs <- list(
        tabPanel("Summary", DT::DTOutput(ns("prob_summary"))),
        tabPanel("Code Mismatches", DT::DTOutput(ns("prob_code_mismatches"))),
        tabPanel("Code Conflicts", DT::DTOutput(ns("prob_conflicts"))),
        tabPanel("Missing Codes", DT::DTOutput(ns("prob_missing_codes"))),
        tabPanel("Missing Metadata", DT::DTOutput(ns("prob_missing_meta"))),
        tabPanel("Missing Values", DT::DTOutput(ns("prob_missing_vals")))
      )

      do.call(tabsetPanel, c(list(id = ns("problem_tabs")), tabs))
    })

    output$prob_summary <- DT::renderDT({
      req(problems_detailed_rv())
      DT::datatable(
        problems_detailed_rv()$summary,
        options = list(dom = 't', paging = FALSE),
        rownames = FALSE
      )
    })

    output$prob_code_mismatches <- DT::renderDT({
      req(problems_detailed_rv())
      DT::datatable(
        problems_detailed_rv()$code_mismatches,
        options = list(scrollX = TRUE, pageLength = 25),
        filter = "top"
      )
    })

    output$prob_conflicts <- DT::renderDT({
      req(problems_detailed_rv())
      DT::datatable(
        problems_detailed_rv()$code_conflicts,
        options = list(scrollX = TRUE, pageLength = 25),
        filter = "top"
      )
    })

    output$prob_missing_codes <- DT::renderDT({
      req(problems_detailed_rv())
      DT::datatable(
        problems_detailed_rv()$missing_codes,
        options = list(scrollX = TRUE, pageLength = 25),
        filter = "top"
      )
    })

    output$prob_missing_meta <- DT::renderDT({
      req(problems_detailed_rv())
      DT::datatable(
        problems_detailed_rv()$missing_metadata,
        options = list(scrollX = TRUE, pageLength = 25),
        filter = "top"
      )
    })

    output$prob_missing_vals <- DT::renderDT({
      req(problems_detailed_rv())
      DT::datatable(
        problems_detailed_rv()$missing_values,
        options = list(scrollX = TRUE, pageLength = 25),
        filter = "top"
      )
    })

  })
}
