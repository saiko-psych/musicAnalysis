#' Plot interactive practice growth curves
#'
#' @description
#' Creates an interactive plotly visualization showing practice hours over age
#' for selected participants and categories (instruments/singing/othermusic).
#'
#' Supports three aggregation modes:
#' \itemize{
#'   \item \code{individual}: separate lines for each participant+instrument/singing/other combination
#'   \item \code{category_sum}: sum hours by category (all instruments, all singing, all other) per participant per age
#'   \item \code{total}: grand total across all musical activities per participant per age
#' }
#'
#' @param long_data Long-format data from `musical_experience_time()$long`,
#'   with columns: `code`, `category`, `category_id`, `age_years`, `yearly_hours`.
#' @param wide_data Optional wide-format data from `musical_experience_time()$wide`.
#'   If provided, instrument/singing/othermusic names will be added to legends and hover text.
#' @param plot_type Character: what to plot. Options:
#'   - "individual" (default): separate line for each instrument/singing/other
#'   - "category_sum": sum by category (instruments/singing/other music)
#'   - "total": total musical experience (all categories combined)
#' @param categories Character vector of categories to include (default: all).
#'   Options: "instrument", "singing", "othermusic".
#'   Only used when plot_type = "individual" or "category_sum".
#' @param category_ids Numeric vector of category IDs to include (default: all).
#'   Example: `c(1, 2)` to show only instrument1, instrument2, singing1, singing2, etc.
#'   Only used when plot_type = "individual".
#' @param codes Character vector of participant codes to include (default: all).
#' @param n_participants Integer: maximum number of participants to display.
#'   If NULL, shows all selected participants.
#' @param subset_by Character: how to select subset when `n_participants` is specified.
#'   Options:
#'   - "all" (default): random subset
#'   - "highest": participants with highest total practice hours
#'   - "lowest": participants with lowest total practice hours
#'   - "random": random subset
#' @param smooth Logical: if TRUE, applies smoothing to curves (default: FALSE).
#' @param facet_by Character: how to organize plots. Options:
#'   - "none" (default): all curves on one plot
#'   - "category": separate plot for each category (instrument/singing/othermusic)
#'   - "category_id": separate plot for each specific instrument/singing/othermusic instance
#'   - "participant": separate plot for each participant
#' @param category_sum_combined Logical: when plot_type = "category_sum", should categories
#'   be plotted together in one plot (TRUE) or in separate plots (FALSE, default)?
#' @param color_by Character: what to color curves by. Options:
#'   - "code" (default): each participant gets unique color
#'   - "category": color by instrument/singing/othermusic
#'   - "category_id": color by specific instrument number
#' @param line_color Character: specific color for all lines (optional). If provided,
#'   overrides color_by. Examples: "blue", "#FF5733", "rgb(255,0,0)".
#' @param title Character: plot title (default: auto-generated based on plot_type).
#' @param show_legend Logical: show legend (default: TRUE).
#'
#' @return A plotly object (interactive plot).
#'
#' @examples
#' \dontrun{
#' res <- musical_experience_time("data/survey.csv")
#'
#' # Total musical experience for all participants
#' plot_practice_curves(res$long, plot_type = "total")
#'
#' # Sum by category for top 10 participants
#' plot_practice_curves(res$long,
#'                     plot_type = "category_sum",
#'                     n_participants = 10,
#'                     subset_by = "highest")
#'
#' # Individual instruments, faceted by participant
#' plot_practice_curves(res$long,
#'                     plot_type = "individual",
#'                     categories = "instrument",
#'                     facet_by = "participant",
#'                     n_participants = 5)
#'
#' }
#' @export
plot_practice_curves <- function(
    long_data,
    wide_data = NULL,
    plot_type = c("individual", "category_sum", "total"),
    categories = NULL,
    category_ids = NULL,
    codes = NULL,
    n_participants = NULL,
    subset_by = c("all", "highest", "lowest", "random"),
    smooth = FALSE,
    facet_by = c("none", "category", "category_id", "participant"),
    category_sum_combined = FALSE,
    color_by = c("code", "category", "category_id"),
    line_color = NULL,
    title = NULL,
    show_legend = TRUE
) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots. Install with: install.packages('plotly')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required")

  plot_type <- match.arg(plot_type)
  subset_by <- match.arg(subset_by)
  facet_by <- match.arg(facet_by)
  color_by <- match.arg(color_by)

  `%>%` <- magrittr::`%>%`

  # Helper: Get display label for a category
  .get_category_label <- function(cat) {
    switch(cat,
      "instrument" = "Instruments",
      "singing" = "Singing",
      "othermusic" = "Other Music",
      cat
    )
  }

  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- switch(plot_type,
      "total" = "Total Musical Experience Over Age",
      "category_sum" = "Practice Hours by Category Over Age",
      "individual" = "Practice Hours Over Age (Individual Instruments)"
    )
  }

  # Prepare data based on plot_type
  if (plot_type == "total") {
    # Sum all musical activities per participant per age
    plot_data <- long_data %>%
      dplyr::group_by(.data$code, .data$age_years) %>%
      dplyr::summarise(
        yearly_hours = sum(.data$yearly_hours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        category = "total",
        category_id = 1,
        category_label = "Total"
      )
  } else if (plot_type == "category_sum") {
    # Sum by category (all instruments, all singing, all other) per participant per age
    plot_data <- long_data
    if (!is.null(categories)) {
      plot_data <- plot_data %>% dplyr::filter(.data$category %in% categories)
    }
    plot_data <- plot_data %>%
      dplyr::group_by(.data$code, .data$category, .data$age_years) %>%
      dplyr::summarise(
        yearly_hours = sum(.data$yearly_hours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        category_id = 1,  # dummy value for grouping
        category_label = dplyr::case_when(
          .data$category == "instrument" ~ "Instruments",
          .data$category == "singing" ~ "Singing",
          .data$category == "othermusic" ~ "Other Music",
          TRUE ~ .data$category
        )
      )
  } else {
    # Individual: keep separate lines for each instrument/singing/other
    plot_data <- long_data
    if (!is.null(categories)) {
      plot_data <- plot_data %>% dplyr::filter(.data$category %in% categories)
    }
    if (!is.null(category_ids)) {
      plot_data <- plot_data %>% dplyr::filter(.data$category_id %in% category_ids)
    }
    plot_data <- plot_data %>%
      dplyr::mutate(
        category_label = paste0(.data$category, .data$category_id)
      )
  }

  # subset participants if requested
  if (!is.null(codes)) {
    plot_data <- plot_data %>% dplyr::filter(.data$code %in% codes)
  } else if (!is.null(n_participants)) {
    # compute total hours per participant to rank
    participant_totals <- plot_data %>%
      dplyr::group_by(.data$code) %>%
      dplyr::summarise(total_hours = sum(.data$yearly_hours, na.rm = TRUE), .groups = "drop")

    selected_codes <- if (subset_by == "highest") {
      participant_totals %>%
        dplyr::arrange(dplyr::desc(.data$total_hours)) %>%
        dplyr::slice(1:min(n_participants, dplyr::n())) %>%
        dplyr::pull(.data$code)
    } else if (subset_by == "lowest") {
      participant_totals %>%
        dplyr::arrange(.data$total_hours) %>%
        dplyr::slice(1:min(n_participants, dplyr::n())) %>%
        dplyr::pull(.data$code)
    } else {
      # random or all
      set.seed(123)  # reproducible random
      sample(participant_totals$code, min(n_participants, nrow(participant_totals)))
    }
    plot_data <- plot_data %>% dplyr::filter(.data$code %in% selected_codes)
  }

  # Join instrument/singing/othermusic names from wide_data if provided
  # BUT ONLY for individual plots - category_sum should show aggregated labels
  if (!is.null(wide_data) && "code" %in% names(wide_data) && plot_type == "individual") {
    # Extract name columns (which*, singingtype*, whichothermusic*)
    name_patterns <- c("^whichinstrument\\d+$", "^singingtype\\d+$", "^whichothermusic\\d+$")
    name_cols <- unique(unlist(lapply(name_patterns, function(p) {
      grep(p, names(wide_data), value = TRUE)
    })))

    if (length(name_cols) > 0) {
      # Create long format of names
      name_data <- wide_data %>%
        dplyr::select(.data$code, dplyr::all_of(name_cols)) %>%
        tidyr::pivot_longer(
          cols = -.data$code,
          names_to = "name_col",
          values_to = "item_name"
        ) %>%
        dplyr::mutate(
          category = dplyr::case_when(
            grepl("^whichinstrument", .data$name_col) ~ "instrument",
            grepl("^singingtype", .data$name_col) ~ "singing",
            grepl("^whichothermusic", .data$name_col) ~ "othermusic",
            TRUE ~ NA_character_
          ),
          category_id = as.integer(stringr::str_extract(.data$name_col, "\\d+$"))
        ) %>%
        dplyr::filter(!is.na(.data$item_name) & .data$item_name != "") %>%
        dplyr::select(.data$code, .data$category, .data$category_id, .data$item_name)

      # Join names to plot_data
      plot_data <- plot_data %>%
        dplyr::left_join(name_data, by = c("code", "category", "category_id"))
    }
  }

  # Add item_name to plot_data if not present
  if (!"item_name" %in% names(plot_data)) {
    plot_data <- plot_data %>% dplyr::mutate(item_name = NA_character_)
  }

  # create composite variable for plotting
  plot_data <- plot_data %>%
    dplyr::mutate(
      # group_var: used for split parameter to separate lines
      # IMPORTANT: This will be REPLACED with category_label_display after we create it
      # to avoid duplicate legend names (plotly shows both color and split if different)
      group_var = paste0(.data$code, "_", .data$category, "_", .data$category_id),
      # Enhanced category_label with instrument name for legend display
      # For individual with instrument names: "0102SICH - Klavier"
      # For category_sum: "0102SICH - Instruments" (convert category to readable form)
      # Fallback: "0102SICH_instrument1"
      category_label_display = dplyr::case_when(
        # If we have the actual instrument/singing/othermusic name, use it (individual plots)
        !is.na(.data$item_name) & .data$item_name != "" ~ paste0(.data$code, " - ", .data$item_name),
        # For category_sum aggregated data, show "CODE - total Category"
        # This format includes participant code so each participant gets unique color
        .data$category_label == "Instruments" ~ paste0(.data$code, " - total Instruments"),
        .data$category_label == "Singing" ~ paste0(.data$code, " - total Singing"),
        .data$category_label == "Other Music" ~ paste0(.data$code, " - total Other Music"),
        # Fallback for any other case
        TRUE ~ paste0(.data$code, "_", .data$category_label)
      ),
      # CRITICAL: color_var determines LEGEND names in plotly
      # Now for category_sum it will be "CODE - total Category" (unique per participant)
      # For individual it will be "CODE - Instrument" (unique per instrument)
      color_var = .data$category_label_display,
      # group_var: used for split parameter to separate lines
      # Must match color_var to prevent duplicate legend entries
      group_var = .data$category_label_display
    ) %>%
    dplyr::mutate(
      hover_text = paste0(
        "Code: ", .data$code, "<br>",
        "Category: ", .data$category_label,
        dplyr::if_else(
          !is.na(.data$item_name) & .data$item_name != "",
          paste0("<br>Instrument: ", .data$item_name),
          ""
        ),
        "<br>Age: ", .data$age_years
      )
    ) %>%
    # Sort by age within each group for proper line connection
    dplyr::arrange(.data$group_var, .data$age_years)

  # smoothing if requested
  if (smooth) {
    plot_data <- plot_data %>%
      dplyr::group_by(.data$code, .data$category, .data$category_id) %>%
      dplyr::arrange(.data$age_years) %>%
      dplyr::mutate(
        yearly_hours_smooth = stats::predict(
          stats::loess(yearly_hours ~ age_years, span = 0.3, data = dplyr::cur_data())
        )
      ) %>%
      dplyr::ungroup()
    y_var <- "yearly_hours_smooth"
    y_axis_label <- "Practice Hours per Year (smoothed)"
    # Update hover text with smoothed values
    plot_data <- plot_data %>%
      dplyr::mutate(
        hover_text = paste0(.data$hover_text, "<br>Hours/Year: ", round(.data$yearly_hours_smooth, 1))
      )
  } else {
    y_var <- "yearly_hours"
    y_axis_label <- "Practice Hours per Year"
    # Update hover text with actual values
    plot_data <- plot_data %>%
      dplyr::mutate(
        hover_text = paste0(.data$hover_text, "<br>Hours/Year: ", round(.data$yearly_hours, 1))
      )
  }

  # base plot
  if (!is.null(line_color)) {
    # Use specific color for all lines
    p <- plotly::plot_ly(
      data = plot_data,
      x = ~age_years,
      y = stats::as.formula(paste0("~", y_var)),
      split = ~group_var,  # CRITICAL: ensures each participant+category gets own line
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = line_color),
      marker = list(color = line_color),
      text = ~hover_text,
      hoverinfo = 'text',
      showlegend = show_legend
    ) %>%
      plotly::layout(
        title = title,
        xaxis = list(title = "Age (years)"),
        yaxis = list(title = y_axis_label),
        hovermode = "closest"
      )
  } else {
    # Use color_by variable
    p <- plotly::plot_ly(
      data = plot_data,
      x = ~age_years,
      y = stats::as.formula(paste0("~", y_var)),
      color = ~color_var,
      split = ~group_var,  # CRITICAL: ensures each participant+category gets own line
      type = 'scatter',
      mode = 'lines+markers',
      text = ~hover_text,
      hoverinfo = 'text',
      showlegend = show_legend
    ) %>%
      plotly::layout(
        title = title,
        xaxis = list(title = "Age (years)"),
        yaxis = list(title = y_axis_label),
        hovermode = "closest"
      )
  }

  # Handle category_sum faceting - create SEPARATE PLOTS for each category unless combined
  if (plot_type == "category_sum" && !category_sum_combined) {
    categories_present <- sort(unique(plot_data$category))
    n_cats <- length(categories_present)
    if (n_cats > 1) {
      # Create LIST of separate plots for each category
      plot_list <- lapply(categories_present, function(cat) {
        cat_data <- plot_data %>% dplyr::filter(.data$category == cat)
        cat_title <- .get_category_label(cat)
        plotly::plot_ly(
          data = cat_data,
          x = ~age_years,
          y = stats::as.formula(paste0("~", y_var)),
          color = ~color_var,
          split = ~group_var,
          type = 'scatter',
          mode = 'lines+markers',
          text = ~hover_text,
          hoverinfo = 'text',
          showlegend = show_legend
        ) %>%
          plotly::layout(
            title = cat_title,
            xaxis = list(title = "Age (years)"),
            yaxis = list(title = y_axis_label),
            hovermode = "closest"
          )
      })
      # Return list with metadata
      attr(plot_list, "facet_info") <- list(
        type = "category_sum",
        n_plots = n_cats,
        categories = categories_present
      )
      return(plot_list)
    }
  }

  # faceting if requested - creates SEPARATE PLOTS, not subplots
  if (facet_by == "participant") {
    # Return a LIST of separate plots, one per participant
    participants_present <- sort(unique(plot_data$code))
    n_parts <- length(participants_present)
    if (n_parts > 1) {
      plot_list <- lapply(participants_present, function(part_code) {
        part_data <- plot_data %>% dplyr::filter(.data$code == part_code)
        plotly::plot_ly(
          data = part_data,
          x = ~age_years,
          y = stats::as.formula(paste0("~", y_var)),
          color = ~category_label,
          split = ~group_var,  # proper grouping
          type = 'scatter',
          mode = 'lines+markers',
          text = ~hover_text,
          hoverinfo = 'text',
          showlegend = show_legend
        ) %>%
          plotly::layout(
            title = paste("Participant:", part_code),
            xaxis = list(title = "Age (years)"),
            yaxis = list(title = y_axis_label),
            hovermode = "closest"
          )
      })
      # Return list with metadata
      attr(plot_list, "facet_info") <- list(
        type = "participant",
        n_plots = n_parts,
        participants = participants_present
      )
      return(plot_list)
    }
  } else if (facet_by == "category") {
    # Return a LIST of separate plots, one per category
    categories_present <- sort(unique(plot_data$category))
    n_cats <- length(categories_present)
    if (n_cats > 1) {
      plot_list <- lapply(categories_present, function(cat) {
        cat_data <- plot_data %>% dplyr::filter(.data$category == cat)
        cat_title <- .get_category_label(cat)
        plotly::plot_ly(
          data = cat_data,
          x = ~age_years,
          y = stats::as.formula(paste0("~", y_var)),
          color = ~color_var,
          split = ~group_var,  # proper grouping
          type = 'scatter',
          mode = 'lines+markers',
          text = ~hover_text,
          hoverinfo = 'text',
          showlegend = show_legend
        ) %>%
          plotly::layout(
            title = cat_title,
            xaxis = list(title = "Age (years)"),
            yaxis = list(title = y_axis_label),
            hovermode = "closest"
          )
      })
      # Return list with metadata
      attr(plot_list, "facet_info") <- list(
        type = "category",
        n_plots = n_cats,
        categories = categories_present
      )
      return(plot_list)
    }
  } else if (facet_by == "category_id") {
    # Return a LIST of separate plots, one per category_id
    labels_present <- sort(unique(plot_data$category_label))
    n_labels <- length(labels_present)
    if (n_labels > 1) {
      plot_list <- lapply(labels_present, function(lbl) {
        lbl_data <- plot_data %>% dplyr::filter(.data$category_label == lbl)
        plotly::plot_ly(
          data = lbl_data,
          x = ~age_years,
          y = stats::as.formula(paste0("~", y_var)),
          color = ~color_var,
          split = ~group_var,  # proper grouping
          type = 'scatter',
          mode = 'lines+markers',
          text = ~hover_text,
          hoverinfo = 'text',
          showlegend = show_legend
        ) %>%
          plotly::layout(
            title = lbl,
            xaxis = list(title = "Age (years)"),
            yaxis = list(title = y_axis_label),
            hovermode = "closest"
          )
      })
      # Return list with metadata
      attr(plot_list, "facet_info") <- list(
        type = "category_id",
        n_plots = n_labels,
        category_labels = labels_present
      )
      return(plot_list)
    }
  }

  p
}
