# inst/shiny/modules/mod_merge.R

mod_merge_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Merge datasets by `code`"),
    p("Provide two CSV files with a `code` column. Left dataset (X) will be the anchor."),
    fluidRow(
      column(6, fileInput(ns("x_csv"), "Left dataset (X)", accept = ".csv")),
      column(6, fileInput(ns("y_csv"), "Right dataset (Y)", accept = ".csv"))
    ),
    textInput(ns("suffix_x"), "Suffix for X", value = ".x"),
    textInput(ns("suffix_y"), "Suffix for Y", value = ".y"),
    actionButton(ns("do_merge"), "Merge"),
    br(), br(),
    fluidRow(
      column(
        width = 6,
        p("Merged dataset by participant code")
      ),
      column(
        width = 6,
        selectInput(
          ns("rows_to_display"),
          "Rows to display:",
          choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
          selected = 25,
          width = "150px"
        )
      )
    ),
    DTOutput(ns("merged_tbl")),
    br(),
    downloadButton(ns("dl_merged"), "Download merged CSV")
  )
}

mod_merge_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    merged_rv <- reactiveVal(NULL)

    observeEvent(input$do_merge, {
      req(input$x_csv, input$y_csv)
      withProgress(message = "Merging...", value = 0, {
        x <- try(readr::read_csv(input$x_csv$datapath, show_col_types = FALSE), silent = TRUE)
        y <- try(readr::read_csv(input$y_csv$datapath, show_col_types = FALSE), silent = TRUE)
        if (inherits(x, "try-error") || inherits(y, "try-error")) {
          showNotification("Failed to read one of the CSV files.", type = "error")
          return()
        }
        out <- try(musicAnalysis::merge_by_code(x, y, suffix = c(input$suffix_x, input$suffix_y)), silent = TRUE)
        if (inherits(out, "try-error")) {
          showNotification(conditionMessage(attr(out, "condition")), type = "error", duration = 8)
        } else {
          merged_rv(out)
        }
      })
    })

    output$merged_tbl <- renderDT({
      req(merged_rv())

      # Get rows to display from input
      rows_display <- as.integer(input$rows_to_display)
      if (is.na(rows_display)) rows_display <- 25  # default

      datatable(merged_rv(), options = list(scrollX = TRUE, pageLength = rows_display))
    })

    output$dl_merged <- downloadHandler(
      filename = function() paste0("merged_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) {
        req(merged_rv())
        readr::write_csv(merged_rv(), file)
      }
    )
  })
}
