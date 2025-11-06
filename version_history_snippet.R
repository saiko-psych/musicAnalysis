# Collapsible Version History Section for mod_home.R
# Insert this after "What's New" section

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

            # v0.0.0.9019
            tags$div(
              style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-left: 3px solid #2196f3;",
              tags$h5(style = "margin-top: 0; color: #1565c0;", "v0.0.0.9019"),
              tags$ul(
                style = "margin-bottom: 0; line-height: 1.6;",
                tags$li(tags$strong("ADDED:"), " number_of_instruments, number_of_singing, number_of_othermusic"),
                tags$li(tags$strong("ADDED:"), " nodme (total count of musical experiences)")
              )
            ),

            # v0.0.0.9018
            tags$div(
              style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-left: 3px solid #2196f3;",
              tags$h5(style = "margin-top: 0; color: #1565c0;", "v0.0.0.9018"),
              tags$ul(
                style = "margin-bottom: 0; line-height: 1.6;",
                tags$li(tags$strong("FIXED:"), " KLAWA Data Quality Analysis refresh with custom parameters"),
                tags$li(tags$strong("FIXED:"), " category_sum plotting error"),
                tags$li(tags$strong("ENHANCED:"), " Plot legends show instrument names")
              )
            ),

            # v0.0.0.9017
            tags$div(
              style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-left: 3px solid #2196f3;",
              tags$h5(style = "margin-top: 0; color: #1565c0;", "v0.0.0.9017"),
              tags$ul(
                style = "margin-bottom: 0; line-height: 1.6;",
                tags$li(tags$strong("FIXED:"), " Y/N and 1/2 format handling"),
                tags$li(tags$strong("IMPROVED:"), " Response format normalization")
              )
            ),

            # v0.0.0.9016
            tags$div(
              style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-left: 3px solid #2196f3;",
              tags$h5(style = "margin-top: 0; color: #1565c0;", "v0.0.0.9016"),
              tags$ul(
                style = "margin-bottom: 0; line-height: 1.6;",
                tags$li(tags$strong("FIXED:"), " Category_sum separate plots"),
                tags$li(tags$strong("RECALCULATED:"), " IMP variables")
              )
            ),

            # v0.0.0.9013
            tags$div(
              style = "margin-bottom: 15px; padding: 10px; background-color: #e8f5e9; border-left: 3px solid #4caf50;",
              tags$h5(style = "margin-top: 0; color: #2e7d32;", "v0.0.0.9013"),
              tags$ul(
                style = "margin-bottom: 0; line-height: 1.6;",
                tags$li(tags$strong("FIXED:"), " Shiny app loading errors")
              )
            ),

            # v0.0.0.9012
            tags$div(
              style = "margin-bottom: 15px; padding: 10px; background-color: #e8f5e9; border-left: 3px solid #4caf50;",
              tags$h5(style = "margin-top: 0; color: #2e7d32;", "v0.0.0.9012"),
              tags$ul(
                style = "margin-bottom: 0; line-height: 1.6;",
                tags$li(tags$strong("FIXED:"), " IMP variable naming"),
                tags$li(tags$strong("IMPROVED:"), " Plot layout and titles"),
                tags$li(tags$strong("ADDED:"), " Download graphs and R code features")
              )
            ),

            # Even older versions
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
          )
        )
      ),
