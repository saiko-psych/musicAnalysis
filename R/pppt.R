#' Scan folder for PPPT data files
#'
#' Recursively scans a folder structure for PPPT .rsl.csv files and extracts
#' PPP (Pitch Perception Proficiency) indices per participant.
#'
#' @param root Path to root folder containing PPPT data
#' @param code_pattern Regex pattern for participant codes (default: 4 digits + 4 letters)
#' @param date_format Date format in filenames (default: "DDMMYY")
#' @param extract_groups Logical, whether to extract group information from folder structure
#' @param group_names Character vector of group names to look for in paths
#' @param remove_duplicates Logical, whether to remove duplicate participant codes (keeps first)
#' @return Tibble with columns: code, ppp_index_overall, ppp_index_294, ppp_index_523,
#'   ppp_index_932, ppp_index_1661, ppp_index_2960, ppp_index_5274, date, file, and optionally group
#' @export
#' @examples
#' \dontrun{
#' pppt_data <- pppt_scan("data/PPPT")
#' pppt_data_with_groups <- pppt_scan("data/PPPT", extract_groups = TRUE,
#'                                     group_names = c("VG", "KG", "EG"))
#' }
pppt_scan <- function(root,
                      code_pattern = "\\d{4}[A-Za-z]{4}",
                      date_format = "DDMMYY",
                      extract_groups = FALSE,
                      group_names = character(),
                      remove_duplicates = TRUE) {

  # Validate root directory
  if (!fs::dir_exists(root)) {
    cli::cli_abort("Directory {.path {root}} does not exist.")
  }

  # Find all .rsl.csv files (will filter by content later if needed)
  # Some PPPT files don't have "PPPT" in filename, so we need to check content
  all_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                          regexp = "\\.rsl\\.csv$")

  if (length(all_files) == 0) {
    cli::cli_warn("No PPPT .rsl.csv files found under {.path {root}}.")
    return(.empty_pppt_tibble(extract_groups))
  }

  cli::cli_inform("Found {length(all_files)} .rsl.csv file{?s} to process.")

  # Process each file
  results <- purrr::map_dfr(all_files, function(file_path) {
    tryCatch({
      pppt_parse_one(file_path, code_pattern = code_pattern,
                     date_format = date_format, root = root,
                     extract_groups = extract_groups, group_names = group_names)
    }, error = function(e) {
      cli::cli_warn("Error processing {.path {basename(file_path)}}: {e$message}")
      NULL
    })
  })

  if (nrow(results) == 0) {
    cli::cli_warn("No valid PPPT files found (files must contain UCF column).")
    return(.empty_pppt_tibble(extract_groups))
  }

  # Make file paths relative to root
  results$file <- fs::path_rel(results$file, start = root)

  # Handle duplicates
  if (remove_duplicates) {
    n_before <- nrow(results)
    results <- results[!duplicated(results$code), ]
    n_after <- nrow(results)
    n_removed <- n_before - n_after
    if (n_removed > 0) {
      cli::cli_inform("Removed {n_removed} duplicate participant code{?s} (kept first occurrence).")
    }
  }

  tibble::as_tibble(results)
}

#' Parse a single PPPT .rsl.csv file
#'
#' @param file_path Path to PPPT .rsl.csv file
#' @param code_pattern Regex pattern for participant codes
#' @param date_format Date format in filename
#' @param root Root directory for relative paths
#' @param extract_groups Whether to extract group information
#' @param group_names Group names to look for
#' @return Tibble with one row containing extracted data
#' @keywords internal
pppt_parse_one <- function(file_path, code_pattern, date_format, root = NULL,
                            extract_groups = FALSE, group_names = character()) {

  # Extract metadata from filename
  filename <- basename(file_path)

  # Extract participant code with improved pattern matching
  code <- .extract_code_improved(filename, code_pattern)

  # Extract date
  date <- .extract_date_from_filename(filename, date_format)

  # Read CSV file
  data <- readr::read_csv(
    file_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  # Check if required columns exist (filter out non-PPPT files)
  required_cols <- c("UCF", "PPP Index")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    # This is not a PPPT file, skip it silently
    return(NULL)
  }

  # Extract PPP indices for each UCF frequency
  ppp_294 <- .extract_ppp_for_ucf(data, "294")
  ppp_523 <- .extract_ppp_for_ucf(data, "523")
  ppp_932 <- .extract_ppp_for_ucf(data, "932")
  ppp_1661 <- .extract_ppp_for_ucf(data, "1661")
  ppp_2960 <- .extract_ppp_for_ucf(data, "2960")
  ppp_5274 <- .extract_ppp_for_ucf(data, "5274")

  # Extract overall PPP index
  overall_row <- data[data$UCF == "Overall", ]
  ppp_overall <- if (nrow(overall_row) > 0) {
    as.numeric(overall_row$`PPP Index`[1])
  } else {
    NA_real_
  }

  # Build result tibble
  result <- tibble::tibble(
    code = code,
    ppp_index_overall = ppp_overall,
    ppp_index_294 = ppp_294,
    ppp_index_523 = ppp_523,
    ppp_index_932 = ppp_932,
    ppp_index_1661 = ppp_1661,
    ppp_index_2960 = ppp_2960,
    ppp_index_5274 = ppp_5274,
    date = date,
    file = as.character(file_path)
  )

  # Add group if requested
  if (extract_groups && !is.null(root)) {
    result$group <- .extract_group_from_path(file_path, root, group_names)
  }

  result
}

#' Extract PPP index for a specific UCF frequency
#'
#' @param data Data frame with UCF and PPP Index columns
#' @param ucf_value UCF value to extract (as string)
#' @return Numeric PPP index value or NA
#' @keywords internal
.extract_ppp_for_ucf <- function(data, ucf_value) {
  row <- data[data$UCF == ucf_value, ]
  if (nrow(row) > 0) {
    as.numeric(row$`PPP Index`[1])
  } else {
    NA_real_
  }
}

#' Improved code extraction with fallback strategies
#'
#' @param filename Filename to extract code from
#' @param pattern Primary regex pattern
#' @return Extracted code or NA
#' @keywords internal
.extract_code_improved <- function(filename, pattern) {
  # Try primary pattern
  code <- extract_and_check_code(filename, pattern = pattern)

  if (is.na(code)) {
    # Fallback 1: Try looser pattern (any 4 digits followed by 4 letters)
    code <- stringr::str_extract(filename, "\\d{4}[A-Za-z]{4}")
    if (!is.na(code)) {
      return(code)
    }

    # Fallback 2: Try finding code after common prefixes
    prefixes <- c("_", "-", "\\.", "^")
    for (prefix in prefixes) {
      pattern_with_prefix <- paste0(prefix, "\\d{4}[A-Za-z]{4}")
      match <- stringr::str_extract(filename, pattern_with_prefix)
      if (!is.na(match)) {
        code <- stringr::str_extract(match, "\\d{4}[A-Za-z]{4}")
        if (!is.na(code)) {
          return(code)
        }
      }
    }
  }

  code
}

#' Extract group from file path
#'
#' @param file_path Full file path
#' @param root Root directory
#' @param group_names Group names to look for
#' @return Group name or "no_group"
#' @keywords internal
.extract_group_from_path <- function(file_path, root, group_names) {
  # Get relative path
  rel_path <- fs::path_rel(file_path, start = root)

  # Split path into components
  path_parts <- strsplit(rel_path, .Platform$file.sep)[[1]]

  # Look for group names in path components
  if (length(group_names) > 0) {
    for (group in group_names) {
      if (any(grepl(group, path_parts, ignore.case = TRUE))) {
        return(group)
      }
    }
  }

  # Default to no_group
  "no_group"
}

#' Create empty PPPT tibble with correct structure
#'
#' @param with_group Whether to include group column
#' @return Empty tibble
#' @keywords internal
.empty_pppt_tibble <- function(with_group = FALSE) {
  base_tibble <- tibble::tibble(
    code = character(),
    ppp_index_overall = numeric(),
    ppp_index_294 = numeric(),
    ppp_index_523 = numeric(),
    ppp_index_932 = numeric(),
    ppp_index_1661 = numeric(),
    ppp_index_2960 = numeric(),
    ppp_index_5274 = numeric(),
    date = character(),
    file = character()
  )

  if (with_group) {
    base_tibble$group <- character()
  }

  base_tibble
}

#' Analyze PPPT folder structure
#'
#' Scans folder to identify PPPT files and their organization.
#' Provides detailed counts of different file types.
#'
#' @param root Path to root folder
#' @return List with structure information including:
#'   - n_pppt_rsl: Files with "PPPT" in filename AND .rsl.csv extension
#'   - n_pppt_itl: Files with "PPPT" in filename AND .itl.csv extension
#'   - n_rsl_total: All .rsl.csv files
#'   - n_valid_pppt: .rsl.csv files that contain valid PPPT data (UCF column)
#'   - pppt_rsl_files: List of PPPT .rsl.csv filenames
#'   - pppt_itl_files: List of PPPT .itl.csv filenames
#' @export
pppt_analyze_structure <- function(root) {
  if (!fs::dir_exists(root)) {
    cli::cli_abort("Directory {.path {root}} does not exist.")
  }

  # Files with "PPPT" in filename
  pppt_rsl_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                                regexp = "PPPT.*\\.rsl\\.csv$", ignore.case = TRUE)

  pppt_itl_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                                regexp = "PPPT.*\\.itl\\.csv$", ignore.case = TRUE)

  # All .rsl.csv files (to check which ones are actually PPPT)
  all_rsl_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                               regexp = "\\.rsl\\.csv$")

  # Check which .rsl files contain valid PPPT data
  valid_pppt_count <- 0
  for (file in all_rsl_files) {
    has_pppt <- tryCatch({
      data <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()),
                              show_col_types = FALSE, n_max = 1)
      "UCF" %in% names(data) && "PPP Index" %in% names(data)
    }, error = function(e) FALSE)

    if (has_pppt) {
      valid_pppt_count <- valid_pppt_count + 1
    }
  }

  list(
    n_pppt_rsl = length(pppt_rsl_files),
    n_pppt_itl = length(pppt_itl_files),
    n_rsl_total = length(all_rsl_files),
    n_valid_pppt = valid_pppt_count,
    pppt_rsl_files = basename(pppt_rsl_files),
    pppt_itl_files = basename(pppt_itl_files),
    all_rsl_files = basename(all_rsl_files)
  )
}

#' Validate PPPT scan results
#'
#' Validates PPPT scan results similar to KLAWA validation.
#' Checks for missing codes, duplicate codes, and data quality issues.
#'
#' @param root Path to root folder
#' @param scanned_data Tibble from pppt_scan()
#' @param code_pattern Regex pattern for participant codes
#' @return List with validation results including:
#'   - n_expected: Total .rsl.csv files with valid PPPT structure
#'   - n_scanned: Number of successfully scanned participants
#'   - n_missing_code: Files where code extraction failed
#'   - n_duplicates: Number of duplicate participant codes
#'   - missing_code_files: List of files with missing codes
#'   - duplicate_codes: Data frame of duplicate codes
#'   - validation_summary: Text summary
#' @export
pppt_validate <- function(root, scanned_data, code_pattern = "\\d{4}[A-Za-z]{4}") {
  if (!fs::dir_exists(root)) {
    cli::cli_abort("Directory {.path {root}} does not exist.")
  }

  # Get all .rsl.csv files
  all_rsl_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                               regexp = "\\.rsl\\.csv$")

  # Check which are valid PPPT files
  valid_pppt_files <- character()
  for (file in all_rsl_files) {
    has_pppt <- tryCatch({
      data <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()),
                              show_col_types = FALSE, n_max = 1)
      "UCF" %in% names(data) && "PPP Index" %in% names(data)
    }, error = function(e) FALSE)

    if (has_pppt) {
      valid_pppt_files <- c(valid_pppt_files, file)
    }
  }

  n_expected <- length(valid_pppt_files)
  n_scanned <- nrow(scanned_data)

  # Find files with missing codes
  missing_code_files <- character()
  for (file in valid_pppt_files) {
    code <- .extract_code_improved(basename(file), code_pattern)
    if (is.na(code)) {
      missing_code_files <- c(missing_code_files, basename(file))
    }
  }

  # Find duplicate codes
  code_counts <- table(scanned_data$code)
  duplicate_codes <- names(code_counts[code_counts > 1])

  duplicate_df <- NULL
  if (length(duplicate_codes) > 0) {
    duplicate_df <- scanned_data[scanned_data$code %in% duplicate_codes,
                                  c("code", "file")]
    duplicate_df <- duplicate_df[order(duplicate_df$code), ]
  }

  # Create summary
  summary_text <- sprintf(
    "PPPT Validation Results:\n  Expected files: %d\n  Successfully scanned: %d\n  Missing codes: %d\n  Duplicate codes: %d",
    n_expected, n_scanned, length(missing_code_files), length(duplicate_codes)
  )

  list(
    n_expected = n_expected,
    n_scanned = n_scanned,
    n_missing_code = length(missing_code_files),
    n_duplicates = length(duplicate_codes),
    missing_code_files = missing_code_files,
    duplicate_codes = duplicate_df,
    validation_summary = summary_text,
    all_valid_pppt_files = basename(valid_pppt_files)
  )
}

#' Create PPPT frequency profile plot
#'
#' Creates a line plot showing PPP indices across UCF frequencies.
#' X-axis: PPP Index (-1 to 1), Y-axis: Frequency bands plus Overall.
#'
#' @param data Tibble from pppt_scan() with PPP index columns
#' @param plot_type Type of plot: "all_combined" (one line), "all_overlaid" (one plot, multiple lines),
#'   "individual" (separate plots per participant), "by_group" (one plot per group)
#' @param color_by How to color lines: "participant", "group", or specific color
#' @param line_color Specific color if color_by is not "participant" or "group"
#' @param show_legend Whether to show legend
#' @param title Plot title (optional)
#' @return Plotly object or list of plotly objects
#' @export
pppt_plot_profile <- function(data,
                               plot_type = c("all_combined", "all_overlaid", "individual", "by_group"),
                               color_by = c("participant", "group", "custom"),
                               line_color = "blue",
                               show_legend = TRUE,
                               title = NULL) {

  plot_type <- match.arg(plot_type)
  color_by <- match.arg(color_by)

  # Validate data
  required_cols <- c("code", "ppp_index_294", "ppp_index_523", "ppp_index_932",
                     "ppp_index_1661", "ppp_index_2960", "ppp_index_5274", "ppp_index_overall")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.field {missing}}")
  }

  # Convert to long format
  long_data <- .pppt_to_long(data)

  # Create plots based on type
  if (plot_type == "all_combined") {
    # Average across all participants
    avg_data <- long_data %>%
      dplyr::group_by(frequency_label, frequency_order, is_overall) %>%
      dplyr::summarise(ppp_index = mean(ppp_index, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(frequency_order)

    plot <- .create_single_profile_plot(
      avg_data,
      title = title %||% "PPPT Profile - All Participants (Mean)",
      color = line_color,
      show_legend = FALSE,
      label = "Mean"
    )

    return(plot)

  } else if (plot_type == "all_overlaid") {
    # All participants on one plot
    plot <- .create_overlaid_profile_plot(
      long_data,
      color_by = color_by,
      line_color = line_color,
      show_legend = show_legend,
      title = title %||% "PPPT Profiles - All Participants"
    )

    return(plot)

  } else if (plot_type == "individual") {
    # Separate plot for each participant
    plots <- list()

    for (participant in unique(long_data$code)) {
      participant_data <- long_data %>%
        dplyr::filter(code == participant) %>%
        dplyr::arrange(frequency_order)

      plot <- .create_single_profile_plot(
        participant_data,
        title = sprintf("PPPT Profile - %s", participant),
        color = line_color,
        show_legend = FALSE,
        label = participant
      )

      plots[[participant]] <- plot
    }

    return(plots)

  } else if (plot_type == "by_group") {
    # Check if group column exists
    if (!"group" %in% names(data)) {
      cli::cli_abort("Group column not found. Run pppt_scan() with extract_groups = TRUE.")
    }

    plots <- list()

    for (grp in unique(data$group)) {
      group_data <- long_data %>%
        dplyr::filter(group == grp)

      plot <- .create_overlaid_profile_plot(
        group_data,
        color_by = "participant",
        line_color = line_color,
        show_legend = show_legend,
        title = sprintf("PPPT Profiles - Group: %s", grp)
      )

      plots[[grp]] <- plot
    }

    return(plots)
  }
}

#' Convert PPPT data to long format for plotting
#'
#' @param data Wide format PPPT data
#' @return Long format tibble
#' @keywords internal
.pppt_to_long <- function(data) {
  # Define frequency labels and order
  freq_info <- tibble::tibble(
    freq_col = c("ppp_index_294", "ppp_index_523", "ppp_index_932",
                 "ppp_index_1661", "ppp_index_2960", "ppp_index_5274", "ppp_index_overall"),
    frequency_label = c("294 Hz", "523 Hz", "932 Hz", "1661 Hz", "2960 Hz", "5274 Hz", "Overall"),
    frequency_order = 1:7,
    is_overall = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )

  # Reshape to long
  long_data <- data %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("ppp_index_"),
      names_to = "freq_col",
      values_to = "ppp_index"
    ) %>%
    dplyr::left_join(freq_info, by = "freq_col") %>%
    dplyr::select(-freq_col) %>%
    dplyr::filter(!is.na(ppp_index))

  long_data
}

#' Create single PPPT profile plot
#'
#' @param data Long format data for one line
#' @param title Plot title
#' @param color Line color
#' @param show_legend Show legend
#' @param label Line label
#' @return Plotly object
#' @keywords internal
.create_single_profile_plot <- function(data, title, color, show_legend, label) {
  # Separate overall from frequencies
  freq_data <- data %>% dplyr::filter(!is_overall)
  overall_data <- data %>% dplyr::filter(is_overall)

  # Create plotly
  plot <- plotly::plot_ly()

  # Add frequency profile line
  if (nrow(freq_data) > 0) {
    plot <- plot %>%
      plotly::add_trace(
        data = freq_data,
        x = ~ppp_index,
        y = ~frequency_label,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = color, width = 2),
        marker = list(size = 8, color = color),
        name = label,
        showlegend = show_legend,
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "PPP Index: %{x:.3f}<br>",
          "<extra></extra>"
        )
      )
  }

  # Add overall point (not connected)
  if (nrow(overall_data) > 0) {
    plot <- plot %>%
      plotly::add_trace(
        data = overall_data,
        x = ~ppp_index,
        y = ~frequency_label,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = color, symbol = "diamond"),
        name = paste0(label, " (Overall)"),
        showlegend = show_legend,
        hovertemplate = paste0(
          "<b>Overall</b><br>",
          "PPP Index: %{x:.3f}<br>",
          "<extra></extra>"
        )
      )
  }

  # Layout
  plot <- plot %>%
    plotly::layout(
      title = title,
      xaxis = list(
        title = "PPP Index",
        range = c(-1.05, 1.05),
        zeroline = TRUE,
        zerolinewidth = 1,
        zerolinecolor = "gray"
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = rev(c("294 Hz", "523 Hz", "932 Hz", "1661 Hz", "2960 Hz", "5274 Hz", "Overall"))
      ),
      hovermode = "closest"
    )

  plot
}

#' Create overlaid PPPT profile plot
#'
#' @param data Long format data for multiple lines
#' @param color_by How to color lines
#' @param line_color Default line color
#' @param show_legend Show legend
#' @param title Plot title
#' @return Plotly object
#' @keywords internal
.create_overlaid_profile_plot <- function(data, color_by, line_color, show_legend, title) {
  plot <- plotly::plot_ly()

  # Determine grouping variable
  if (color_by == "participant") {
    group_var <- "code"
  } else if (color_by == "group" && "group" %in% names(data)) {
    group_var <- "group"
  } else {
    group_var <- NULL
  }

  # Split by overall vs frequencies
  freq_data <- data %>% dplyr::filter(!is_overall)
  overall_data <- data %>% dplyr::filter(is_overall)

  # Plot frequency profiles
  if (nrow(freq_data) > 0) {
    if (!is.null(group_var)) {
      # Color by group variable
      for (grp in unique(freq_data[[group_var]])) {
        grp_data <- freq_data %>% dplyr::filter(.data[[group_var]] == grp)

        plot <- plot %>%
          plotly::add_trace(
            data = grp_data,
            x = ~ppp_index,
            y = ~frequency_label,
            type = "scatter",
            mode = "lines+markers",
            line = list(width = 2),
            marker = list(size = 6),
            name = as.character(grp),
            showlegend = show_legend,
            legendgroup = as.character(grp),
            hovertemplate = paste0(
              "<b>%{y}</b><br>",
              group_var, ": ", grp, "<br>",
              "PPP Index: %{x:.3f}<br>",
              "<extra></extra>"
            )
          )
      }
    } else {
      # Single color for all
      plot <- plot %>%
        plotly::add_trace(
          data = freq_data,
          x = ~ppp_index,
          y = ~frequency_label,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = line_color, width = 2),
          marker = list(size = 6, color = line_color),
          name = "Frequencies",
          showlegend = show_legend,
          hovertemplate = paste0(
            "<b>%{y}</b><br>",
            "PPP Index: %{x:.3f}<br>",
            "<extra></extra>"
          )
        )
    }
  }

  # Plot overall points (not connected)
  if (nrow(overall_data) > 0) {
    if (!is.null(group_var)) {
      for (grp in unique(overall_data[[group_var]])) {
        grp_data <- overall_data %>% dplyr::filter(.data[[group_var]] == grp)

        plot <- plot %>%
          plotly::add_trace(
            data = grp_data,
            x = ~ppp_index,
            y = ~frequency_label,
            type = "scatter",
            mode = "markers",
            marker = list(size = 8, symbol = "diamond"),
            name = paste0(as.character(grp), " (Overall)"),
            showlegend = show_legend,
            legendgroup = as.character(grp),
            hovertemplate = paste0(
              "<b>Overall</b><br>",
              group_var, ": ", grp, "<br>",
              "PPP Index: %{x:.3f}<br>",
              "<extra></extra>"
            )
          )
      }
    } else {
      plot <- plot %>%
        plotly::add_trace(
          data = overall_data,
          x = ~ppp_index,
          y = ~frequency_label,
          type = "scatter",
          mode = "markers",
          marker = list(size = 8, color = line_color, symbol = "diamond"),
          name = "Overall",
          showlegend = show_legend,
          hovertemplate = paste0(
            "<b>Overall</b><br>",
            "PPP Index: %{x:.3f}<br>",
            "<extra></extra>"
          )
        )
    }
  }

  # Layout
  plot <- plot %>%
    plotly::layout(
      title = title,
      xaxis = list(
        title = "PPP Index",
        range = c(-1.05, 1.05),
        zeroline = TRUE,
        zerolinewidth = 1,
        zerolinecolor = "gray"
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = rev(c("294 Hz", "523 Hz", "932 Hz", "1661 Hz", "2960 Hz", "5274 Hz", "Overall"))
      ),
      hovermode = "closest"
    )

  plot
}
