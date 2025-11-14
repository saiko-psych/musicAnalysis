# inst/shiny/modules/mod_pppt.R

mod_pppt_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("PPPT Scanner (Pitch Perception Proficiency Test)"),

    # Tabset for Data Scanning and Visualization
    tabsetPanel(
      id = ns("pppt_tabs"),
      type = "tabs",

      # Tab 1: Data Scanning
      tabPanel(
        "Data Scanning",
        value = "scanning",
        br(),

    # Help/Instructions Panel
    wellPanel(
      style = "background-color: #f8f9fa;",
      h4("🎵 How PPPT Scanning Works"),
      p("This tool extracts PPPT (Pitch Perception Proficiency Test) metrics from .rsl.csv files."),
      tags$ul(
        tags$li(tags$strong("PPP Index:"), " Pitch Perception Proficiency index for each UCF frequency"),
        tags$li(tags$strong("UCF Frequencies:"), " 294, 523, 932, 1661, 2960, 5274 Hz"),
        tags$li(tags$strong("Overall Index:"), " Overall PPP index across all frequencies"),
        tags$li(tags$strong("Groups:"), " Optionally extract group information from folder structure"),
        tags$li(tags$strong("Validation:"), " Check data quality and identify problems")
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
    uiOutput(ns("structure_panel")),

    hr(),

    # Advanced Settings
    h4("2. Configure Settings"),
    wellPanel(
      fluidRow(
        column(
          width = 3,
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
          width = 3,
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
          )
        ),
        column(
          width = 3,
          checkboxInput(
            ns("extract_groups"),
            "Extract Group Information",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.extract_groups",
            ns = ns,
            textInput(
              ns("group_names"),
              "Group Names (comma-separated):",
              value = "VG,KG,EG",
              placeholder = "e.g., VG,KG,EG"
            ),
            tags$small(
              class = "form-text text-muted",
              "Group names to look for in folder paths"
            )
          )
        ),
        column(
          width = 3,
          checkboxInput(
            ns("remove_duplicates"),
            "Remove Duplicate Codes",
            value = TRUE
          ),
          tags$small(
            class = "form-text text-muted",
            "Keep only first occurrence of each code"
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
      ), # End Data Scanning tab

      # Tab 2: Visualization
      tabPanel(
        "Visualization",
        value = "visualization",
        br(),

        wellPanel(
          style = "background-color: #f0f8ff;",
          h4("📊 PPPT Profile Visualization"),
          p("Visualize PPP indices across frequency bands. The frequencies are shown as connected lines, while the Overall index appears as a separate diamond marker."),
          tags$ul(
            tags$li("X-axis: PPP Index (-1 to 1)"),
            tags$li("Y-axis: Frequency bands (294, 523, 932, 1661, 2960, 5274 Hz) + Overall"),
            tags$li("Frequencies: Connected with lines"),
            tags$li("Overall: Diamond marker (not connected)")
          )
        ),

        # Require data first
        conditionalPanel(
          condition = sprintf("output['%s']", ns("has_data")),
          ns = ns,

          fluidRow(
            column(
              width = 3,
              wellPanel(
                h4("Plot Controls"),

                selectInput(
                  ns("plot_type"),
                  "Plot Type:",
                  choices = c(
                    "All participants - Mean profile" = "all_combined",
                    "All participants - Overlaid lines" = "all_overlaid",
                    "Individual plots per participant" = "individual",
                    "Plots by group" = "by_group"
                  ),
                  selected = "all_combined"
                ),

                conditionalPanel(
                  condition = "input.plot_type == 'all_overlaid' || input.plot_type == 'by_group'",
                  ns = ns,
                  selectInput(
                    ns("color_by"),
                    "Color lines by:",
                    choices = c(
                      "Participant" = "participant",
                      "Group" = "group",
                      "Custom color" = "custom"
                    ),
                    selected = "participant"
                  )
                ),

                conditionalPanel(
                  condition = "input.color_by == 'custom' || input.plot_type == 'all_combined' || input.plot_type == 'individual'",
                  ns = ns,
                  textInput(
                    ns("line_color"),
                    "Line color:",
                    value = "steelblue",
                    placeholder = "e.g., blue, #FF5733"
                  )
                ),

                checkboxInput(
                  ns("show_legend"),
                  "Show legend",
                  value = TRUE
                ),

                br(),
                actionButton(
                  ns("update_plot"),
                  "Update Plot",
                  class = "btn-primary btn-block"
                ),

                br(),
                downloadButton(
                  ns("download_plot"),
                  "Download Plot (HTML)",
                  class = "btn-success btn-block"
                ),

                br(),
                actionButton(
                  ns("show_plot_code"),
                  "📝 Show R Code",
                  class = "btn-info btn-block"
                )
              )
            ),

            column(
              width = 9,
              uiOutput(ns("plot_output")),

              br(),
              conditionalPanel(
                condition = "input.show_plot_code > 0",
                ns = ns,
                wellPanel(
                  style = "background-color: #f8f9fa;",
                  h4("R Code to Reproduce This Plot"),
                  p("Copy and paste this code into your R console to recreate the plot:"),
                  verbatimTextOutput(ns("plot_r_code")),
                  downloadButton(ns("download_plot_code"), "Download Code (.R)")
                )
              )
            )
          )
        ),

        # Message when no data
        conditionalPanel(
          condition = sprintf("!output['%s']", ns("has_data")),
          ns = ns,
          wellPanel(
            style = "background-color: #fff3cd; border-left: 4px solid #ffc107;",
            h5("⚠️ No Data Available"),
            p("Please scan PPPT files in the 'Data Scanning' tab first before creating visualizations.")
          )
        )
      ) # End Visualization tab
    ) # End tabsetPanel
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
      edited_data = NULL,
      validation_results = NULL
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

    # Display structure panel
    output$structure_panel <- renderUI({
      req(rv$structure_info)

      info <- rv$structure_info

      wellPanel(
        style = "background-color: #e7ffe7;",
        h5("📊 Detected Folder Structure"),
        tags$div(
          style = "padding: 10px;",
          tags$p(
            tags$strong("File Analysis:"),
            tags$ul(
              tags$li(sprintf("Files with 'PPPT' + .rsl.csv extension: %d", info$n_pppt_rsl)),
              tags$li(sprintf("Files with 'PPPT' + .itl.csv extension: %d", info$n_pppt_itl)),
              tags$li(sprintf("Total .rsl.csv files found: %d", info$n_rsl_total)),
              tags$li(sprintf("Valid PPPT files (with UCF column): %d", info$n_valid_pppt),
                      style = "color: #2e7d32; font-weight: bold;")
            )
          )
        ),
        tags$p(
          style = "font-size: 0.9em; color: #666;",
          "Configure settings below and click 'Scan Files' to extract data."
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
              if (input$extract_groups) paste0("Groups: ", input$group_names, "\n"),
              "Please wait...")
      })

      tryCatch({
        # Parse group names
        group_names_vec <- if (input$extract_groups && nzchar(input$group_names)) {
          trimws(strsplit(input$group_names, ",")[[1]])
        } else {
          character()
        }

        # Perform scan
        result <- pppt_scan(
          root = rv$root_path,
          code_pattern = input$code_pattern,
          date_format = input$date_format,
          extract_groups = input$extract_groups,
          group_names = group_names_vec,
          remove_duplicates = input$remove_duplicates
        )

        rv$pppt_data <- result
        rv$edited_data <- result  # Initialize edited data

        # Run validation
        rv$validation_results <- pppt_validate(
          root = rv$root_path,
          scanned_data = result,
          code_pattern = input$code_pattern
        )

        output$scan_status <- renderText({
          sprintf("✓ Successfully scanned %d PPPT file(s)\n\nValidation: %s",
                  nrow(result),
                  rv$validation_results$validation_summary)
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

        # Validation Results
        wellPanel(
          style = "background-color: #fff3e0;",
          h5("✅ Validation Results"),
          verbatimTextOutput(ns("validation_summary")),
          conditionalPanel(
            condition = sprintf("output['%s'] != null && output['%s'].length > 0",
                                ns("validation_details"), ns("validation_details")),
            hr(),
            h6("⚠️ Problematic Files:"),
            verbatimTextOutput(ns("validation_details"))
          )
        ),

        # Summary statistics
        wellPanel(
          style = "background-color: #e3f2fd;",
          h5("📊 Summary Statistics"),
          htmlOutput(ns("summary_stats"))
        ),

        # Data table
        wellPanel(
          h5("📄 PPPT Data"),
          p(
            style = "color: #666; font-size: 0.9em;",
            tags$strong("💡 Tip:"), " Double-click any cell to edit its value.",
            " Your changes will be reflected in the downloaded CSV."
          ),
          fluidRow(
            column(
              width = 3,
              selectInput(
                ns("page_length"),
                "Rows per page:",
                choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
                selected = 25
              )
            )
          ),
          DT::dataTableOutput(ns("data_table"))
        ),

        # Action buttons
        fluidRow(
          column(
            width = 3,
            downloadButton(
              ns("download_csv"),
              "💾 Download CSV",
              class = "btn-primary btn-lg btn-block"
            )
          ),
          column(
            width = 3,
            actionButton(
              ns("show_code"),
              "📝 Show R Code",
              class = "btn-info btn-lg btn-block"
            )
          ),
          column(
            width = 3,
            actionButton(
              ns("show_validation"),
              "🔍 Validation Details",
              class = "btn-warning btn-lg btn-block"
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
        ),

        # Validation details modal trigger
        conditionalPanel(
          condition = "input.show_validation > 0",
          ns = ns,
          wellPanel(
            style = "background-color: #fff3e0;",
            h4("Detailed Validation Report"),
            htmlOutput(ns("validation_report"))
          )
        )
      )
    })

    # Validation summary
    output$validation_summary <- renderText({
      req(rv$validation_results)
      rv$validation_results$validation_summary
    })

    # Validation details
    output$validation_details <- renderText({
      req(rv$validation_results)

      details <- character()

      if (length(rv$validation_results$missing_code_files) > 0) {
        details <- c(details, "Files with missing codes:",
                     paste("  -", rv$validation_results$missing_code_files))
      }

      if (!is.null(rv$validation_results$duplicate_codes) &&
          nrow(rv$validation_results$duplicate_codes) > 0) {
        details <- c(details, "\nDuplicate codes found:")
        for (code in unique(rv$validation_results$duplicate_codes$code)) {
          files <- rv$validation_results$duplicate_codes$file[
            rv$validation_results$duplicate_codes$code == code
          ]
          details <- c(details, sprintf("  %s:", code),
                       paste("    -", files))
        }
      }

      if (length(details) > 0) {
        paste(details, collapse = "\n")
      } else {
        NULL
      }
    })

    # Detailed validation report
    output$validation_report <- renderUI({
      req(rv$validation_results)

      val <- rv$validation_results

      tagList(
        tags$h5("Summary:"),
        tags$ul(
          tags$li(sprintf("Expected PPPT files: %d", val$n_expected)),
          tags$li(sprintf("Successfully scanned: %d", val$n_scanned)),
          tags$li(sprintf("Files with missing codes: %d", val$n_missing_code)),
          tags$li(sprintf("Duplicate codes: %d", val$n_duplicates))
        ),

        if (length(val$missing_code_files) > 0) {
          tagList(
            tags$hr(),
            tags$h5("Files with Missing Codes:"),
            tags$ul(
              lapply(val$missing_code_files, function(f) tags$li(f))
            )
          )
        },

        if (!is.null(val$duplicate_codes) && nrow(val$duplicate_codes) > 0) {
          tagList(
            tags$hr(),
            tags$h5("Duplicate Codes:"),
            DT::renderDataTable(val$duplicate_codes, options = list(pageLength = 10))
          )
        }
      )
    })

    # Summary statistics
    output$summary_stats <- renderUI({
      req(rv$pppt_data)

      data <- rv$pppt_data

      stats_html <- tags$p(
        tags$strong("Total participants:"), nrow(data), tags$br(),
        tags$strong("Mean PPP Index (Overall):"),
        sprintf("%.2f", mean(data$ppp_index_overall, na.rm = TRUE)), tags$br(),
        tags$strong("Range:"),
        sprintf("[%.2f, %.2f]",
                min(data$ppp_index_overall, na.rm = TRUE),
                max(data$ppp_index_overall, na.rm = TRUE))
      )

      if ("group" %in% names(data)) {
        group_counts <- table(data$group)
        group_info <- tags$div(
          tags$strong("Groups:"), tags$br(),
          tags$ul(
            lapply(names(group_counts), function(g) {
              tags$li(sprintf("%s: %d participants", g, group_counts[g]))
            })
          )
        )
        tagList(stats_html, group_info)
      } else {
        stats_html
      }
    })

    # Data table
    output$data_table <- DT::renderDataTable({
      req(rv$pppt_data)

      page_len <- as.numeric(input$page_length)

      DT::datatable(
        rv$edited_data,
        editable = list(target = "cell", disable = list(columns = c(0))), # Protect first column
        options = list(
          pageLength = ifelse(page_len == -1, nrow(rv$edited_data), page_len),
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

      group_code <- if (input$extract_groups) {
        sprintf('  extract_groups = TRUE,\n  group_names = c(%s),\n',
                paste0('"', trimws(strsplit(input$group_names, ",")[[1]]), '"', collapse = ", "))
      } else {
        ""
      }

      code <- sprintf('library(musicAnalysis)

# Scan PPPT files
pppt_data <- pppt_scan(
  root = "%s",
  code_pattern = "%s",
  date_format = "%s",
%s  remove_duplicates = %s
)

# View results
View(pppt_data)

# Validate results
validation <- pppt_validate(
  root = "%s",
  scanned_data = pppt_data,
  code_pattern = "%s"
)
print(validation$validation_summary)

# Save to CSV
write.csv(pppt_data, "pppt_data.csv", row.names = FALSE)
',
        rv$root_path,
        input$code_pattern,
        input$date_format,
        group_code,
        input$remove_duplicates,
        rv$root_path,
        input$code_pattern
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
        group_code <- if (input$extract_groups) {
          sprintf('  extract_groups = TRUE,\n  group_names = c(%s),\n',
                  paste0('"', trimws(strsplit(input$group_names, ",")[[1]]), '"', collapse = ", "))
        } else {
          ""
        }

        code <- sprintf('library(musicAnalysis)

# Scan PPPT files
pppt_data <- pppt_scan(
  root = "%s",
  code_pattern = "%s",
  date_format = "%s",
%s  remove_duplicates = %s
)

# View results
View(pppt_data)

# Validate results
validation <- pppt_validate(
  root = "%s",
  scanned_data = pppt_data,
  code_pattern = "%s"
)
print(validation$validation_summary)

# Save to CSV
write.csv(pppt_data, "pppt_data.csv", row.names = FALSE)
',
          rv$root_path,
          input$code_pattern,
          input$date_format,
          group_code,
          input$remove_duplicates,
          rv$root_path,
          input$code_pattern
        )
        writeLines(code, file)
      }
    )

    # ========== Visualization Tab Server Logic ==========

    # Check if data exists for conditional panels
    output$has_data <- reactive({
      !is.null(rv$pppt_data) && nrow(rv$pppt_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # Reactive for current plot
    current_plot <- reactiveVal(NULL)

    # Generate plots when Update Plot is clicked
    observeEvent(input$update_plot, {
      req(rv$pppt_data)

      tryCatch({
        data <- rv$edited_data  # Use edited data if user made changes

        # Determine color_by parameter
        color_by_param <- if (input$plot_type %in% c("all_overlaid", "by_group")) {
          input$color_by
        } else {
          "custom"  # For all_combined and individual
        }

        # Generate plot(s)
        plot_result <- pppt_plot_profile(
          data = data,
          plot_type = input$plot_type,
          color_by = color_by_param,
          line_color = input$line_color,
          show_legend = input$show_legend,
          title = NULL  # Let function generate default titles
        )

        # Store for download and code generation
        current_plot(plot_result)

        showNotification(
          "Plot updated successfully!",
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(
          paste("Error generating plot:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Render plot output
    output$plot_output <- renderUI({
      req(current_plot())

      plot_result <- current_plot()

      # Handle different return types
      if (input$plot_type %in% c("all_combined", "all_overlaid")) {
        # Single plot
        tagList(
          h4("PPPT Frequency Profile"),
          plotly::plotlyOutput(ns("main_plot"), height = "600px")
        )
      } else if (input$plot_type == "individual") {
        # Multiple individual plots
        tagList(
          h4("Individual PPPT Profiles"),
          lapply(seq_along(plot_result), function(i) {
            tagList(
              plotly::plotlyOutput(ns(paste0("individual_plot_", i)), height = "500px"),
              br()
            )
          })
        )
      } else if (input$plot_type == "by_group") {
        # Group plots
        tagList(
          h4("PPPT Profiles by Group"),
          lapply(seq_along(plot_result), function(i) {
            tagList(
              plotly::plotlyOutput(ns(paste0("group_plot_", i)), height = "500px"),
              br()
            )
          })
        )
      }
    })

    # Render main plot (for all_combined and all_overlaid)
    output$main_plot <- plotly::renderPlotly({
      req(current_plot())
      req(input$plot_type %in% c("all_combined", "all_overlaid"))
      current_plot()
    })

    # Render individual plots dynamically
    observe({
      req(current_plot())
      req(input$plot_type == "individual")

      plot_list <- current_plot()

      lapply(seq_along(plot_list), function(i) {
        output_name <- paste0("individual_plot_", i)
        output[[output_name]] <- plotly::renderPlotly({
          plot_list[[i]]
        })
      })
    })

    # Render group plots dynamically
    observe({
      req(current_plot())
      req(input$plot_type == "by_group")

      plot_list <- current_plot()

      lapply(seq_along(plot_list), function(i) {
        output_name <- paste0("group_plot_", i)
        output[[output_name]] <- plotly::renderPlotly({
          plot_list[[i]]
        })
      })
    })

    # Generate R code for plot
    output$plot_r_code <- renderText({
      req(rv$pppt_data)
      req(input$update_plot)  # Only show after plot created

      # Determine color_by parameter
      color_by_param <- if (input$plot_type %in% c("all_overlaid", "by_group")) {
        sprintf('  color_by = "%s",\n', input$color_by)
      } else {
        ""
      }

      code <- sprintf('library(musicAnalysis)
library(plotly)

# Load your PPPT data
# (Assuming you already have pppt_data from scanning)

# Generate PPPT frequency profile plot
plot <- pppt_plot_profile(
  data = pppt_data,
  plot_type = "%s",
%s  line_color = "%s",
  show_legend = %s,
  title = NULL
)

# Display plot
plot

# Save plot as HTML
htmlwidgets::saveWidget(plot, "pppt_profile_plot.html")
',
        input$plot_type,
        color_by_param,
        input$line_color,
        input$show_legend
      )

      code
    })

    # Download plot code
    output$download_plot_code <- downloadHandler(
      filename = function() {
        paste0("pppt_plot_code_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) {
        # Same code as above
        color_by_param <- if (input$plot_type %in% c("all_overlaid", "by_group")) {
          sprintf('  color_by = "%s",\n', input$color_by)
        } else {
          ""
        }

        code <- sprintf('library(musicAnalysis)
library(plotly)

# Load your PPPT data
# (Assuming you already have pppt_data from scanning)

# Generate PPPT frequency profile plot
plot <- pppt_plot_profile(
  data = pppt_data,
  plot_type = "%s",
%s  line_color = "%s",
  show_legend = %s,
  title = NULL
)

# Display plot
plot

# Save plot as HTML
htmlwidgets::saveWidget(plot, "pppt_profile_plot.html")
',
          input$plot_type,
          color_by_param,
          input$line_color,
          input$show_legend
        )

        writeLines(code, file)
      }
    )

    # Download plot as HTML
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("pppt_profile_plot_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        req(current_plot())

        # For single plots, save directly
        if (input$plot_type %in% c("all_combined", "all_overlaid")) {
          htmlwidgets::saveWidget(current_plot(), file)
        } else {
          # For multiple plots, combine or save first one
          # (Could enhance this to save all as separate files or combined HTML)
          plot_list <- current_plot()
          if (length(plot_list) > 0) {
            htmlwidgets::saveWidget(plot_list[[1]], file)
            showNotification(
              "Note: Only the first plot was saved. Future enhancement will save all plots.",
              type = "warning",
              duration = 5
            )
          }
        }
      }
    )
  })
}
