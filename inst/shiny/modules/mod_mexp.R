# inst/shiny/modules/mod_mexp.R

mod_mexp_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Musical Experience (LimeSurvey CSV)"),

    # Explanation panel
    wellPanel(
      style = "background-color: #f8f9fa;",
      h4("About Musical Experience Processing"),
      p("This module processes LimeSurvey data about musical training history and instrument proficiency."),

      h5("Data Requirements:"),
      tags$ul(
        tags$li("LimeSurvey CSV export with practice time data by age"),
        tags$li("Columns following pattern: ", tags$code("category##[decade_year]"), " (e.g., ", tags$code("instrument12[1_3]"), " = instrument 1, age 23)"),
        tags$li("Optional: ", tags$code("whichinstrument#"), ", ", tags$code("singingtype#"), ", ", tags$code("whichothermusic#"), " columns")
      ),

      h5("What this does:"),
      tags$ol(
        tags$li("Parses practice time strings (e.g., '2d', '1.5w', '3m', '2y') into yearly hours"),
        tags$li("Validates practice hours for unrealistic values"),
        tags$li("Computes starting age for each instrument/singing/other music"),
        tags$li("Calculates total hours per category (", tags$code("instrument_total"), ", ", tags$code("singing_total"), ", ", tags$code("othermusic_total"), ")"),
        tags$li("Calculates ", tags$strong("total_musical_experience"), " (sum across all categories)"),
        tags$li("Computes ", tags$strong("IMP"), " (Index of Musical Practice = weekly_hours × years_practiced)")
      ),

      h5("Output:"),
      tags$ul(
        tags$li(tags$strong("Wide format:"), " One row per participant with aggregated variables (use for analysis)"),
        tags$li(tags$strong("Long format:"), " Detailed time series data (use for growth curve plots)"),
        tags$li(tags$strong("Flags:"), " Problematic time entries for manual review")
      )
    ),

    fileInput(ns("csv"), "CSV file", accept = c(".csv", "text/csv")),
    checkboxInput(ns("instrument_checks"), "Enable instrument checks", value = TRUE),
    fluidRow(
      column(
        width = 6,
        actionButton(ns("parse"), "Parse CSV", icon = icon("play"), class = "btn-primary")
      ),
      column(
        width = 6,
        actionButton(ns("show_r_code"), "Show R Code", icon = icon("file-code"), class = "btn-info")
      )
    ),
    br(), br(),
    tabsetPanel(
      tabPanel(
        "Wide",
        br(),
        fluidRow(
          column(
            width = 6,
            p("Wide format: one row per participant with aggregated variables")
          ),
          column(
            width = 6,
            selectInput(
              ns("rows_to_display_wide"),
              "Rows to display:",
              choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
              selected = 25,
              width = "150px"
            )
          )
        ),
        DTOutput(ns("wide_tbl"))
      ),
      tabPanel(
        "Long",
        br(),
        fluidRow(
          column(
            width = 6,
            p("Long format: one row per instrument/activity")
          ),
          column(
            width = 6,
            selectInput(
              ns("rows_to_display_long"),
              "Rows to display:",
              choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
              selected = 25,
              width = "150px"
            )
          )
        ),
        DTOutput(ns("long_tbl"))
      ),
      tabPanel(
        "Flags",
        br(),
        fluidRow(
          column(
            width = 6,
            p("Problematic entries for manual review")
          ),
          column(
            width = 6,
            selectInput(
              ns("rows_to_display_flags"),
              "Rows to display:",
              choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
              selected = 25,
              width = "150px"
            )
          )
        ),
        DTOutput(ns("flags_tbl"))
      ),
      tabPanel(
        "Profile",
        br(),
        fluidRow(
          column(
            width = 6,
            p("Musical background profile: demographics, preferences, inner hearing, rankings")
          ),
          column(
            width = 6,
            selectInput(
              ns("rows_to_display_profile"),
              "Rows to display:",
              choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
              selected = 25,
              width = "150px"
            )
          )
        ),
        DTOutput(ns("profile_tbl"))
      ),
      tabPanel(
        "Practice Growth Curves",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              h4("Graph Controls"),

              # What to plot
              radioButtons(
                ns("plot_type"),
                "What to plot:",
                choices = c(
                  "Total musical experience (all categories combined)" = "total",
                  "Sum by category (instruments/singing/other)" = "category_sum",
                  "Individual instruments/singing/other" = "individual"
                ),
                selected = "total"
              ),

              # Category selection (shown conditionally)
              conditionalPanel(
                condition = "input.plot_type != 'total'",
                ns = ns,
                checkboxGroupInput(
                  ns("graph_categories"),
                  "Select categories to display:",
                  choices = c("Instruments" = "instrument",
                             "Singing" = "singing",
                             "Other Music" = "othermusic"),
                  selected = c("instrument", "singing", "othermusic")
                )
              ),

              # Category sum combination option
              conditionalPanel(
                condition = "input.plot_type == 'category_sum'",
                ns = ns,
                checkboxInput(
                  ns("category_sum_combined"),
                  "Show all categories in one combined plot",
                  value = FALSE
                )
              ),

              # Individual instrument selection (dynamically updated based on data)
              conditionalPanel(
                condition = "input.plot_type == 'individual'",
                ns = ns,
                checkboxGroupInput(
                  ns("category_ids_simple"),
                  "Select specific items to plot (leave empty for all):",
                  choices = NULL  # Will be updated dynamically
                )
              ),

              # Participant selection
              radioButtons(
                ns("participant_selection"),
                "Participants to Display:",
                choices = c(
                  "All participants" = "all",
                  "Top N (highest practice)" = "highest",
                  "Bottom N (lowest practice)" = "lowest",
                  "Random N participants" = "random"
                ),
                selected = "all"
              ),

              conditionalPanel(
                condition = "input.participant_selection != 'all'",
                ns = ns,
                numericInput(
                  ns("n_participants"),
                  "Number of participants:",
                  value = 10,
                  min = 1,
                  max = 100,
                  step = 1
                )
              ),

              # Visual options
              selectInput(
                ns("facet_by"),
                "Organize plots by:",
                choices = c(
                  "Single plot (all together)" = "none",
                  "By participant (one plot per person)" = "participant",
                  "By category (instrument/singing/other)" = "category",
                  "By specific instrument/category" = "category_id",
                  "By grouping variable (custom)" = "group"
                ),
                selected = "none"
              ),

              # Grouping variable selector (only shown when facet_by = "group")
              conditionalPanel(
                condition = "input.facet_by == 'group'",
                ns = ns,
                uiOutput(ns("group_var_ui"))
              ),

              selectInput(
                ns("color_by"),
                "Color curves by:",
                choices = c(
                  "Participant code" = "code",
                  "Category type" = "category",
                  "Specific instrument/category" = "category_id",
                  "Custom color (specify below)" = "custom"
                ),
                selected = "code"
              ),

              conditionalPanel(
                condition = "input.color_by == 'custom'",
                ns = ns,
                textInput(
                  ns("line_color"),
                  "Line color:",
                  value = "blue",
                  placeholder = "e.g., blue, #FF5733, rgb(255,0,0)"
                )
              ),

              checkboxInput(ns("show_legend"), "Show legend", value = TRUE),

              br(),
              actionButton(ns("update_graph"), "Update Graph", class = "btn-primary"),
              br(), br(),
              downloadButton(ns("dl_plot_html"), "Download Plot (HTML)"),
              br(), br(),
              actionButton(ns("show_code"), "Show R Code", class = "btn-info")
            )
          ),
          column(
            width = 9,
            # Plot output area - can show single plot or multiple plots
            uiOutput(ns("plot_container")),
            br(),
            conditionalPanel(
              condition = "input.show_code > 0",
              ns = ns,
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("R Code to Reproduce This Plot"),
                p("Copy and paste this code into your R console to recreate the plot:"),
                verbatimTextOutput(ns("plot_code")),
                downloadButton(ns("dl_code"), "Download Code (.R)")
              )
            )
          )
        )
      ),
      tabPanel(
        "Practice History (Time Windows)",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              h4("Time Window Settings"),

              textInput(
                ns("time_windows"),
                "Years to look back (comma-separated):",
                value = "1, 2, 5, 10",
                placeholder = "e.g., 1, 2, 5, 10"
              ),

              actionButton(ns("compute_history"), "Compute History", class = "btn-primary"),

              br(), br(),
              downloadButton(ns("dl_history"), "Download History CSV")
            )
          ),
          column(
            width = 9,
            fluidRow(
              column(
                width = 6,
                p("Practice hours within specified time windows")
              ),
              column(
                width = 6,
                selectInput(
                  ns("rows_to_display_history"),
                  "Rows to display:",
                  choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
                  selected = 25,
                  width = "150px"
                )
              )
            ),
            DTOutput(ns("history_tbl"))
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(width = 4,
        h5("CSV Export"),
        downloadButton(ns("dl_wide"), "Download WIDE CSV", icon = icon("download")),
        downloadButton(ns("dl_long"), "Download LONG CSV", icon = icon("download")),
        downloadButton(ns("dl_flags"), "Download FLAGS CSV", icon = icon("download")),
        downloadButton(ns("dl_profile"), "Download PROFILE CSV", icon = icon("download"))
      ),
      column(width = 4,
        h5("SPSS/Stata Export (preserves labels)"),
        downloadButton(ns("dl_wide_sav"), "Download WIDE .sav (SPSS)", icon = icon("download")),
        downloadButton(ns("dl_wide_dta"), "Download WIDE .dta (Stata)", icon = icon("download")),
        downloadButton(ns("dl_profile_sav"), "Download PROFILE .sav (SPSS)", icon = icon("download"))
      ),
      column(width = 4,
        h5("Info"),
        p(tags$small(class = "text-muted",
          "WIDE = Time + Profile merged. PROFILE = profile-only variables. ",
          "SPSS/Stata formats preserve variable labels."
        ))
      )
    )
  )
}

mod_mexp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    res_rv <- reactiveVal(NULL)
    history_rv <- reactiveVal(NULL)

    observeEvent(input$parse, {
      req(input$csv)
      withProgress(message = "Parsing CSV...", value = 0, {
        incProgress(0.1)
        out <- try({
          musicAnalysis::musical_experience(
            file = input$csv$datapath,
            time_args = list(check_instruments = isTRUE(input$instrument_checks)),
            verbose = TRUE
          )
        }, silent = TRUE)

        if (inherits(out, "try-error")) {
          showNotification(conditionMessage(attr(out, "condition")), type = "error", duration = 8)
          res_rv(NULL)
        } else {
          res_rv(out)
          showNotification("CSV parsed successfully! Check the starting ages in Wide tab.", type = "message")
        }
      })
    })

    # --- Show R Code ----------------------------------------------------------
    observeEvent(input$show_r_code, {
      csv_file <- input$csv

      # Show template code if no file selected, otherwise show generic template
      # (MEXP uses uploaded files, so we always show template with placeholder path)
      r_code <- '# Load the musicAnalysis package
library(musicAnalysis)

# Parse Musical Experience CSV
# Replace "path/to/your/file.csv" with your actual file path
mexp_data <- musical_experience(
  file = "path/to/your/file.csv",
  check_instruments = TRUE  # Set to FALSE to disable instrument validation
)

# Access the different components:
# - Wide format (one row per participant)
wide_data <- mexp_data$wide
View(wide_data)

# - Long format (one row per instrument/activity)
long_data <- mexp_data$sections$time$long
View(long_data)

# - Flags (problematic entries for review)
flags_data <- mexp_data$flags
View(flags_data)

# Save to CSV
write.csv(wide_data, "musical_experience_wide.csv", row.names = FALSE)
write.csv(long_data, "musical_experience_long.csv", row.names = FALSE)
write.csv(flags_data, "musical_experience_flags.csv", row.names = FALSE)'

      showModal(modalDialog(
        title = tagList(icon("file-code"), " R Code for Musical Experience Parsing"),
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
          tags$p("Use this R code to parse Musical Experience data outside of the Shiny app:"),
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
        paste0("musical_experience_parse_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        r_code <- '# Load the musicAnalysis package
library(musicAnalysis)

# Parse Musical Experience CSV
# Replace "path/to/your/file.csv" with your actual file path
mexp_data <- musical_experience(
  file = "path/to/your/file.csv",
  check_instruments = TRUE  # Set to FALSE to disable instrument validation
)

# Access the different components:
# - Wide format (one row per participant)
wide_data <- mexp_data$wide
View(wide_data)

# - Long format (one row per instrument/activity)
long_data <- mexp_data$sections$time$long
View(long_data)

# - Flags (problematic entries for review)
flags_data <- mexp_data$flags
View(flags_data)

# Save to CSV
write.csv(wide_data, "musical_experience_wide.csv", row.names = FALSE)
write.csv(long_data, "musical_experience_long.csv", row.names = FALSE)
write.csv(flags_data, "musical_experience_flags.csv", row.names = FALSE)'

        writeLines(r_code, file)
      }
    )

    # Dynamically update category IDs based on parsed data (simplified - no instrument names)
    observe({
      req(res_rv())
      long_data <- res_rv()$sections$time$long

      if (nrow(long_data) == 0) return(NULL)

      # Create choices based on what categories are selected
      categories_selected <- input$graph_categories
      if (is.null(categories_selected) || length(categories_selected) == 0) {
        categories_selected <- c("instrument", "singing", "othermusic")
      }

      # Build simplified list (just category+id, no names)
      choices_list <- c()
      for (cat in categories_selected) {
        cat_data <- long_data %>% dplyr::filter(category == cat)
        if (nrow(cat_data) == 0) next

        cat_ids <- sort(unique(cat_data$category_id))
        cat_ids <- cat_ids[!is.na(cat_ids)]

        for (id in cat_ids) {
          label <- paste0(cat, id)
          choices_list <- c(choices_list, setNames(label, label))
        }
      }

      if (length(choices_list) == 0) choices_list <- NULL

      updateCheckboxGroupInput(
        session,
        "category_ids_simple",
        choices = choices_list,
        selected = NULL
      )
    })

    # Group variable selector (must not suspend when hidden by conditionalPanel)
    output$group_var_ui <- renderUI({
      req(res_rv())
      wide_data <- res_rv()$wide

      # Get all column names except code
      group_vars <- setdiff(names(wide_data), "code")

      selectInput(
        ns("group_var"),
        "Select grouping variable:",
        choices = group_vars,
        selected = NULL
      )
    })
    outputOptions(output, "group_var_ui", suspendWhenHidden = FALSE)

    # Reactive value to store current plot and code
    current_plot_rv <- reactiveVal(NULL)
    current_code_rv <- reactiveVal(NULL)

    # Practice growth curves
    observeEvent(input$update_graph, {
      req(res_rv())
      long_data <- res_rv()$sections$time$long
      wide_data <- res_rv()$wide

      if (nrow(long_data) == 0) {
        showNotification("No data to plot", type = "warning")
        return(NULL)
      }

      # Determine plot_type from UI
      plot_type_ui <- input$plot_type
      if (is.null(plot_type_ui)) plot_type_ui <- "total"

      # Parse inputs based on plot_type
      categories <- if (plot_type_ui != "total") {
        input$graph_categories
      } else {
        NULL
      }

      # For individual mode, parse category_ids
      category_ids <- if (plot_type_ui == "individual" && !is.null(input$category_ids_simple) && length(input$category_ids_simple) > 0) {
        parsed <- stringr::str_match(input$category_ids_simple, "^(instrument|singing|othermusic)(\\d+)$")
        if (!is.null(parsed) && nrow(parsed) > 0) {
          as.numeric(parsed[, 3])
        } else {
          NULL
        }
      } else {
        NULL
      }

      # Category sum combination option
      category_sum_combined <- if (plot_type_ui == "category_sum") {
        isTRUE(input$category_sum_combined)
      } else {
        FALSE
      }

      n_participants <- if (input$participant_selection == "all") {
        NULL
      } else {
        input$n_participants
      }

      facet_by_val <- input$facet_by
      group_var_name <- NULL
      if (facet_by_val == "group") {
        group_var_name <- input$group_var
        if (is.null(group_var_name) || !group_var_name %in% names(wide_data)) {
          showNotification("Please select a valid grouping variable", type = "warning")
          return(NULL)
        }
        facet_by_val <- "none"
      }

      # Generate R code for reproduction
      code_lines <- c(
        "# Load required package",
        "library(musicAnalysis)",
        "",
        "# Assuming you have parsed your data:",
        "# res <- musical_experience('your_file.csv')",
        "# long_data <- res$long",
        "",
        "# Generate plot",
        "plot <- plot_practice_curves(",
        "  long_data = long_data,",
        sprintf("  plot_type = '%s',", plot_type_ui)
      )

      if (!is.null(categories)) {
        code_lines <- c(code_lines, sprintf("  categories = c(%s),",
          paste0("'", categories, "'", collapse = ", ")))
      }
      if (!is.null(category_ids)) {
        code_lines <- c(code_lines, sprintf("  category_ids = c(%s),",
          paste(category_ids, collapse = ", ")))
      }
      if (!is.null(n_participants)) {
        code_lines <- c(code_lines,
          sprintf("  n_participants = %d,", n_participants),
          sprintf("  subset_by = '%s',", input$participant_selection))
      }
      if (plot_type_ui == "category_sum") {
        code_lines <- c(code_lines,
          sprintf("  category_sum_combined = %s,", ifelse(category_sum_combined, "TRUE", "FALSE")))
      }
      code_lines <- c(code_lines,
        sprintf("  facet_by = '%s',", facet_by_val),
        sprintf("  color_by = '%s',", input$color_by),
        sprintf("  show_legend = %s", ifelse(input$show_legend, "TRUE", "FALSE")),
        ")",
        "",
        "# Display plot",
        "plot"
      )

      current_code_rv(paste(code_lines, collapse = "\n"))

      # Get line color if custom
      line_color_val <- if (input$color_by == "custom" && !is.null(input$line_color) && nzchar(input$line_color)) {
        input$line_color
      } else {
        NULL
      }

      # Generate plot (might be single plot or list of plots)
      .make_plot <- function(ld, wd) {
        musicAnalysis::plot_practice_curves(
          long_data = ld,
          wide_data = wd,
          plot_type = plot_type_ui,
          categories = categories,
          category_ids = category_ids,
          n_participants = n_participants,
          subset_by = input$participant_selection,
          smooth = FALSE,
          facet_by = facet_by_val,
          category_sum_combined = category_sum_combined,
          color_by = if (is.null(line_color_val)) input$color_by else "code",
          line_color = line_color_val,
          show_legend = input$show_legend
        )
      }

      result <- tryCatch({
        if (!is.null(group_var_name)) {
          # Group faceting: split data by chosen variable, create separate plots
          group_vals <- wide_data[[group_var_name]]
          group_lookup <- dplyr::tibble(code = wide_data$code, .group_val = group_vals)
          long_grouped <- dplyr::left_join(long_data, group_lookup, by = "code")
          unique_groups <- sort(unique(stats::na.omit(long_grouped$.group_val)))

          if (length(unique_groups) == 0) {
            showNotification("Grouping variable has no valid values", type = "warning")
            .make_plot(long_data, wide_data)
          } else if (length(unique_groups) > 20) {
            showNotification(paste0("Too many groups (", length(unique_groups), "). Max 20. Showing ungrouped."), type = "warning")
            .make_plot(long_data, wide_data)
          } else {
            plot_list <- lapply(unique_groups, function(gval) {
              codes_in_group <- group_lookup$code[group_lookup$.group_val == gval]
              ld_sub <- long_data %>% dplyr::filter(.data$code %in% codes_in_group)
              wd_sub <- wide_data %>% dplyr::filter(.data$code %in% codes_in_group)
              if (nrow(ld_sub) == 0) return(NULL)
              p <- .make_plot(ld_sub, wd_sub)
              if (inherits(p, "plotly")) {
                p <- p %>% plotly::layout(title = paste0(group_var_name, ": ", gval))
              }
              p
            })
            plot_list <- Filter(Negate(is.null), plot_list)
            if (length(plot_list) == 1) {
              plot_list[[1]]
            } else {
              attr(plot_list, "facet_info") <- list(
                type = "group",
                n_plots = length(plot_list),
                group_var = group_var_name,
                group_values = unique_groups
              )
              plot_list
            }
          }
        } else {
          .make_plot(long_data, wide_data)
        }
      }, error = function(e) {
        showNotification(paste("Error creating plot:", e$message), type = "error")
        NULL
      })

      current_plot_rv(result)

      # Render plot(s) - handle both single plot and list of plots
      output$plot_container <- renderUI({
        req(result)

        # Check if result is a list of plots (from faceting)
        if (is.list(result) && !inherits(result, "plotly") && length(result) > 0 && inherits(result[[1]], "plotly")) {
          # Multiple plots - render each one
          facet_info <- attr(result, "facet_info")
          n_plots <- if (!is.null(facet_info)) facet_info$n_plots else length(result)

          plot_outputs <- lapply(seq_along(result), function(i) {
            output_id <- paste0("multi_plot_", i)
            output[[output_id]] <- plotly::renderPlotly({
              result[[i]]
            })
            tagList(
              plotly::plotlyOutput(ns(output_id), height = "500px"),
              br()
            )
          })

          tagList(
            h4(paste("Showing", n_plots, "separate plots:")),
            plot_outputs
          )
        } else {
          # Single plot
          output$practice_plot <- plotly::renderPlotly({
            result
          })
          plotly::plotlyOutput(ns("practice_plot"), height = "600px")
        }
      })
    })

    # Display R code
    output$plot_code <- renderText({
      req(current_code_rv())
      current_code_rv()
    })

    # Helper function to generate dynamic filename
    get_plot_filename <- reactive({
      req(input$update_graph)

      # Build descriptive filename
      plot_type_ui <- input$plot_type
      facet_val <- input$facet_by
      participant_sel <- input$participant_selection

      parts <- c("practice_curves")

      # Add plot type
      if (!is.null(plot_type_ui)) {
        parts <- c(parts, plot_type_ui)
      }

      # Add faceting info
      if (!is.null(facet_val) && facet_val != "none") {
        parts <- c(parts, paste0("facet_", facet_val))
      }

      # Add participant selection info
      if (!is.null(participant_sel) && participant_sel != "all") {
        if (!is.null(input$n_participants)) {
          parts <- c(parts, paste0(participant_sel, "_", input$n_participants))
        }
      }

      # Add timestamp
      parts <- c(parts, format(Sys.time(), "%Y%m%d-%H%M%S"))

      paste(parts, collapse = "_")
    })

    # Download plot as HTML
    output$dl_plot_html <- downloadHandler(
      filename = function() paste0(get_plot_filename(), ".html"),
      content = function(file) {
        req(current_plot_rv())
        plot_obj <- current_plot_rv()

        # Handle list of plots vs single plot
        if (is.list(plot_obj) && !inherits(plot_obj, "plotly") && length(plot_obj) > 0 && inherits(plot_obj[[1]], "plotly")) {
          # Multiple plots - combine into single HTML file
          widgets <- lapply(plot_obj, plotly::as_widget)
          htmltools::save_html(htmltools::tagList(widgets), file)
        } else {
          # Single plot
          htmlwidgets::saveWidget(plotly::as_widget(plot_obj), file, selfcontained = TRUE)
        }
      }
    )

    # Download R code
    output$dl_code <- downloadHandler(
      filename = function() paste0("practice_curves_code_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".R"),
      content = function(file) {
        req(current_code_rv())
        writeLines(current_code_rv(), file)
      }
    )

    # Practice history computation
    observeEvent(input$compute_history, {
      req(res_rv())
      long_data <- res_rv()$sections$time$long

      if (nrow(long_data) == 0) {
        showNotification("No data available", type = "warning")
        return(NULL)
      }

      # Parse time windows
      windows_str <- trimws(unlist(strsplit(input$time_windows, ",")))
      windows <- suppressWarnings(as.numeric(windows_str))
      windows <- windows[!is.na(windows)]

      if (length(windows) == 0) {
        showNotification("Please provide valid time windows (e.g., 1, 2, 5, 10)", type = "warning")
        return(NULL)
      }

      withProgress(message = "Computing practice history...", value = 0, {
        history <- try({
          musicAnalysis::compute_practice_history(
            long_data = long_data,
            current_age = NULL,  # uses max age from data
            time_windows = windows
          )
        }, silent = TRUE)

        if (inherits(history, "try-error")) {
          showNotification(paste("Error:", conditionMessage(attr(history, "condition"))),
                          type = "error", duration = 8)
          history_rv(NULL)
        } else {
          history_rv(history)
          showNotification("Practice history computed successfully!", type = "message")
        }
      })
    })

    output$history_tbl <- DT::renderDT({
      req(history_rv())

      # Get rows to display from input
      rows_display <- as.integer(input$rows_to_display_history)
      if (is.na(rows_display)) rows_display <- 25  # default

      DT::datatable(
        history_rv(),
        options = list(scrollX = TRUE, pageLength = rows_display),
        filter = "top"
      )
    })

    output$wide_tbl  <- DT::renderDT({
      req(res_rv())

      # Get rows to display from input
      rows_display <- as.integer(input$rows_to_display_wide)
      if (is.na(rows_display)) rows_display <- 25  # default

      DT::datatable(res_rv()$wide, options = list(scrollX = TRUE, pageLength = rows_display), filter = "top")
    })
    output$long_tbl  <- DT::renderDT({
      req(res_rv())

      # Get rows to display from input
      rows_display <- as.integer(input$rows_to_display_long)
      if (is.na(rows_display)) rows_display <- 25  # default

      DT::datatable(res_rv()$sections$time$long, options = list(scrollX = TRUE, pageLength = rows_display), filter = "top")
    })
    output$flags_tbl <- DT::renderDT({
      req(res_rv())

      # Get rows to display from input
      rows_display <- as.integer(input$rows_to_display_flags)
      if (is.na(rows_display)) rows_display <- 25  # default

      DT::datatable(res_rv()$flags, options = list(scrollX = TRUE, pageLength = rows_display), filter = "top")
    })
    output$profile_tbl <- DT::renderDT({
      req(res_rv())
      req(res_rv()$sections$profile)

      rows_display <- as.integer(input$rows_to_display_profile)
      if (is.na(rows_display)) rows_display <- 25

      DT::datatable(res_rv()$sections$profile,
                    options = list(scrollX = TRUE, pageLength = rows_display),
                    filter = "top")
    })

    # DT outputs in hidden tabs need suspendWhenHidden = FALSE to render on tab switch
    outputOptions(output, "long_tbl", suspendWhenHidden = FALSE)
    outputOptions(output, "flags_tbl", suspendWhenHidden = FALSE)
    outputOptions(output, "profile_tbl", suspendWhenHidden = FALSE)

    output$dl_wide  <- downloadHandler(
      filename = function() paste0("musical_experience_wide_",  format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) { req(res_rv()); readr::write_csv(res_rv()$wide,  file) }
    )
    output$dl_long  <- downloadHandler(
      filename = function() paste0("musical_experience_long_",  format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) { req(res_rv()); readr::write_csv(res_rv()$sections$time$long,  file) }
    )
    output$dl_flags <- downloadHandler(
      filename = function() paste0("musical_experience_flags_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) { req(res_rv()); readr::write_csv(res_rv()$flags, file) }
    )
    output$dl_profile <- downloadHandler(
      filename = function() paste0("musical_experience_profile_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) { req(res_rv()); readr::write_csv(res_rv()$sections$profile, file) }
    )
    output$dl_history <- downloadHandler(
      filename = function() paste0("practice_history_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) { req(history_rv()); readr::write_csv(history_rv(), file) }
    )

    # SPSS/Stata exports (haven reads attr("label") natively)
    .require_haven <- function() {
      if (!requireNamespace("haven", quietly = TRUE)) {
        showNotification("Install the 'haven' package for SPSS/Stata export: install.packages('haven')", type = "error")
        return(FALSE)
      }
      TRUE
    }

    output$dl_wide_sav <- downloadHandler(
      filename = function() paste0("musical_experience_wide_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".sav"),
      content = function(file) {
        req(res_rv(), .require_haven())
        haven::write_sav(res_rv()$wide, file)
      }
    )

    output$dl_wide_dta <- downloadHandler(
      filename = function() paste0("musical_experience_wide_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".dta"),
      content = function(file) {
        req(res_rv(), .require_haven())
        wide <- res_rv()$wide
        names(wide) <- make.names(substr(names(wide), 1, 32), unique = TRUE)
        haven::write_dta(wide, file)
      }
    )

    output$dl_profile_sav <- downloadHandler(
      filename = function() paste0("musical_experience_profile_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".sav"),
      content = function(file) {
        req(res_rv(), .require_haven())
        haven::write_sav(res_rv()$sections$profile, file)
      }
    )
  })
}
