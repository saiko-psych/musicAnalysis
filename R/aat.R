#' Scan AAT (Auditory Ambiguity Test) CSV files
#'
#' Recursively scans a directory for AAT files (filenames must contain "AAT").
#' Supports two file types:
#' - .itl.csv files: Raw responses (Pitch Classification column)
#' - .rsl.csv files: Computed results (already has ambiguous %, control %)
#'
#' Extracts participant codes and dates from filenames, then calculates or reads AAT metrics:
#' - Ambiguous (%): Percentage of f0-responses (code 1) in ambiguous items
#' - Control (%): Percentage correct in control items
#' - Quality metrics: counts of ambivalent (2) and "don't know" (-1) responses (for .itl only)
#'
#' @param root Directory containing AAT files (will scan recursively).
#' @param code_pattern Regex pattern for extracting participant codes from filenames.
#'   Default: `"(\\d{4}[A-Za-z]{4})"` (4 digits + 4 letters).
#' @param date_format Date format in filenames. One of: "DDMMYY", "DDMMYYYY", "YYMMDD",
#'   "YYYYMMDD", "MMDDYY", "MMDDYYYY". Default: "DDMMYY".
#' @param ambiguous_items Optional numeric vector of item indices that are ambiguous (for .itl files).
#'   If NULL, will attempt to detect from data or assume all non-control items.
#' @param control_items Optional numeric vector of item indices that are control items (for .itl files).
#'   If NULL, will attempt to detect from data.
#'
#' @return A tibble with columns:
#'   - `code`: Participant code (from filename)
#'   - `date`: Date from filename (formatted as DD/MM/YYYY)
#'   - `file_type`: "itl" or "rsl"
#'   - `ambiguous_pct`: Percentage of f0-responses (1) in ambiguous items (rounded to 1 decimal)
#'   - `control_pct`: Percentage correct in control items (rounded to 1 decimal)
#'   - `n_ambivalent`: Count of ambivalent responses (code 2) - NA for .rsl files
#'   - `n_dont_know`: Count of "don't know" responses (code -1) - NA for .rsl files
#'   - `n_evaluable`: Number of evaluable trials (codes 0 or 1) - NA for .rsl files
#'   - `n_total`: Total number of trials - NA for .rsl files
#'   - `file`: Relative path to source CSV file
#'
#' @details
#' **File format expected**:
#' - CSV files with column "Pitch Classification" containing:
#'   - 0 = spectral/overtone response
#'   - 1 = fundamental (f0) response
#'   - 2 = ambivalent
#'   - -1 = don't know
#' - Filename format: `AAT_response_<CODE>_<DATE>.itl.csv` or similar
#'
#' **Calculation logic**:
#' - Only codes 0 and 1 are included in percentage calculations (evaluable responses)
#' - Codes 2 and -1 are excluded from denominators but counted separately
#' - Ambiguous %: (count of 1's in ambiguous items) / (count of 0's and 1's in ambiguous items) * 100
#' - Control %: (count correct in control items) / (total control items) * 100
#'
#' @examples
#' \dontrun{
#' # Scan all AAT files in folder
#' aat_data <- aat_scan("data/AAT")
#'
#' # View results
#' View(aat_data)
#'
#' # Filter for participants with high quality (few ambivalent responses)
#' aat_clean <- aat_data %>%
#'   filter(n_ambivalent < 5, n_dont_know < 3)
#' }
#' @export
aat_scan <- function(root,
                     code_pattern = "(\\d{4}[A-Za-z]{4})",
                     date_format = "DDMMYY",
                     ambiguous_items = NULL,
                     control_items = NULL) {

  # Validate root directory
  if (!fs::dir_exists(root)) {
    rlang::abort(paste0("Directory not found: ", root))
  }

  # Find all CSV files recursively that contain "AAT" in filename
  all_csv_files <- fs::dir_ls(root, recurse = TRUE, regexp = "\\.csv$", type = "file")

  # Filter for files containing "AAT" in the filename
  csv_files <- all_csv_files[stringr::str_detect(basename(all_csv_files), "AAT")]

  if (length(csv_files) == 0) {
    cli::cli_warn("No AAT CSV files found in {root} (filenames must contain 'AAT')")
    return(tibble::tibble(
      code = character(),
      date = character(),
      file_type = character(),
      ambiguous_pct = numeric(),
      control_pct = numeric(),
      n_ambivalent = integer(),
      n_dont_know = integer(),
      n_evaluable = integer(),
      n_total = integer(),
      file = character()
    ))
  }

  # Start progress bar
  progress_id <- cli::cli_progress_bar("Processing AAT files", total = length(csv_files))

  # Process each file
  results <- purrr::map_dfr(csv_files, function(f) {
    rel_path <- fs::path_rel(f, root)
    tryCatch(cli::cli_progress_update(id = progress_id), error = function(e) NULL)
    tryCatch({
      aat_parse_one(f, rel_path, code_pattern, date_format, ambiguous_items, control_items)
    }, error = function(e) {
      cli::cli_warn("Failed to parse {rel_path}: {e$message}")
      tibble::tibble(
        code = NA_character_,
        date = NA_character_,
        file_type = NA_character_,
        ambiguous_pct = NA_real_,
        control_pct = NA_real_,
        n_ambivalent = NA_integer_,
        n_dont_know = NA_integer_,
        n_evaluable = NA_integer_,
        n_total = NA_integer_,
        file = rel_path
      )
    })
  })

  tryCatch(cli::cli_progress_done(id = progress_id), error = function(e) NULL)

  return(results)
}


#' Parse a single AAT CSV file
#'
#' Internal function to extract AAT metrics from one CSV file.
#' Handles both .itl.csv (raw responses) and .rsl.csv (computed results) files.
#'
#' @param file_path Full path to CSV file
#' @param rel_path Relative path for output
#' @param code_pattern Regex for participant code
#' @param date_format Date format in filename
#' @param ambiguous_items Numeric vector of ambiguous item indices (for .itl files)
#' @param control_items Numeric vector of control item indices (for .itl files)
#'
#' @return Single-row tibble with AAT metrics
#' @keywords internal
aat_parse_one <- function(file_path, rel_path, code_pattern, date_format, ambiguous_items, control_items) {

  filename <- basename(file_path)

  # Extract participant code from filename
  code <- extract_and_check_code(filename, code_pattern)

  # Extract date from filename (reuse KLAWA's date extraction logic)
  date <- .extract_date_from_filename(filename, date_format)

  # Determine file type: .itl.csv or .rsl.csv
  if (stringr::str_detect(filename, "\\.itl\\.csv$")) {
    file_type <- "itl"
  } else if (stringr::str_detect(filename, "\\.rsl\\.csv$")) {
    file_type <- "rsl"
  } else {
    # Default: treat as itl if not specified
    file_type <- "unknown"
  }

  # Parse based on file type
  if (file_type == "rsl") {
    return(.aat_parse_rsl(file_path, rel_path, code, date))
  } else {
    return(.aat_parse_itl(file_path, rel_path, code, date, ambiguous_items, control_items))
  }
}


#' Parse .rsl.csv file (computed results)
#'
#' @keywords internal
.aat_parse_rsl <- function(file_path, rel_path, code, date) {
  # Read CSV file
  data <- readr::read_csv(file_path, show_col_types = FALSE)

  # .rsl files come in two formats:
  #
  # Format 1 (Summary format):
  #   Type of Pair, # Tone Pairs, Avg. # Items/Pair, AAT Score [%]
  #   Ambiguous,    50,           2,                 79
  #   Control,      5,            2,                 100
  #
  # Format 2 (Item-level format):
  #   Index, Reference F0 [Hz], F0 Difference [%], ..., # F0, % F0, # Ambi., % Ambi., ...
  #   Each row is one tone pair with statistics
  #
  # We need to detect which format and parse accordingly

  ambiguous_pct <- NA_real_
  control_pct <- NA_real_

  # Check for Format 1 (summary format)
  if ("Type of Pair" %in% names(data) && "AAT Score [%]" %in% names(data)) {
    # Find row where Type of Pair is "Ambiguous"
    ambig_row <- which(stringr::str_detect(data$`Type of Pair`, "(?i)ambiguous"))
    if (length(ambig_row) > 0) {
      ambiguous_pct <- as.numeric(data$`AAT Score [%]`[ambig_row[1]])
    }

    # Find row where Type of Pair is "Control"
    control_row <- which(stringr::str_detect(data$`Type of Pair`, "(?i)control"))
    if (length(control_row) > 0) {
      control_pct <- as.numeric(data$`AAT Score [%]`[control_row[1]])
    }
  }
  # Check for Format 2 (item-level format)
  else if ("% F0" %in% names(data)) {
    # This format has per-item statistics
    # We need to aggregate across all items
    # NOTE: Without item type markers, we can't separate ambiguous from control
    # So we'll compute an overall % F0 tendency
    #
    # For now, return NA for both since we can't distinguish item types
    # Users should use Format 1 (.rsl summary files) for accurate ambiguous/control %
    ambiguous_pct <- NA_real_
    control_pct <- NA_real_
  }

  tibble::tibble(
    code = code,
    date = date,
    file_type = "rsl",
    ambiguous_pct = ambiguous_pct,
    control_pct = control_pct,
    n_ambivalent = NA_integer_,
    n_dont_know = NA_integer_,
    n_evaluable = NA_integer_,
    n_total = NA_integer_,
    file = rel_path
  )
}


#' Parse .itl.csv file (raw responses)
#'
#' @keywords internal
.aat_parse_itl <- function(file_path, rel_path, code, date, ambiguous_items, control_items) {

  # Read CSV file
  data <- readr::read_csv(file_path, show_col_types = FALSE)

  # Find the Pitch Classification column (may have suffix like "[-1;0;1;2]")
  pitch_col <- names(data)[stringr::str_detect(names(data), "^Pitch Classification")]

  if (length(pitch_col) == 0) {
    rlang::abort("Column 'Pitch Classification' not found in CSV")
  }

  # Use the first matching column
  pitch_col <- pitch_col[1]

  # Extract pitch classifications (only rows marked with * in Index column)
  # The Index column format is "1 *", "2 *", etc.
  if ("Index" %in% names(data)) {
    # Filter for rows with "*" in Index
    data <- data[stringr::str_detect(data$Index, "\\*"), ]
  }

  classifications <- data[[pitch_col]]

  # Count response types
  n_total <- length(classifications)
  n_ambivalent <- sum(classifications == 2, na.rm = TRUE)
  n_dont_know <- sum(classifications == -1, na.rm = TRUE)

  # Total evaluable responses (only 0 and 1, excluding 2 and -1)
  n_evaluable <- sum(classifications %in% c(0, 1), na.rm = TRUE)

  # NOTE: .itl files don't contain information about which items are ambiguous vs control
  # The AAT software tracks this internally but doesn't export it to the .itl file
  # Therefore, we can't calculate separate ambiguous % and control % from .itl files alone
  # Users should use .rsl files (computed results) for accurate ambiguous/control percentages
  #
  # What we CAN provide from .itl files:
  # - Overall f0 tendency across all items
  # - Quality metrics (ambivalent, don't know counts)
  #
  # If users need ambiguous/control separation, they must:
  # 1. Use .rsl files instead, OR
  # 2. Manually specify ambiguous_items and control_items parameters

  ambiguous_pct <- NA_real_
  control_pct <- NA_real_

  # If user explicitly provided item indices, calculate them
  if (!is.null(ambiguous_items) && length(ambiguous_items) > 0) {
    amb_responses <- classifications[ambiguous_items]
    evaluable_amb <- amb_responses[amb_responses %in% c(0, 1)]
    n_evaluable_amb <- length(evaluable_amb)
    n_f0_amb <- sum(evaluable_amb == 1, na.rm = TRUE)

    if (n_evaluable_amb > 0) {
      ambiguous_pct <- round((n_f0_amb / n_evaluable_amb) * 100, 1)
    }
  }

  if (!is.null(control_items) && length(control_items) > 0) {
    # For control items, we need to know correct answers
    # Without this information, we cannot calculate control %
    # This would require additional data not present in standard .itl files
    control_pct <- NA_real_
  }

  tibble::tibble(
    code = code,
    date = date,
    file_type = "itl",
    ambiguous_pct = ambiguous_pct,
    control_pct = control_pct,
    n_ambivalent = n_ambivalent,
    n_dont_know = n_dont_know,
    n_evaluable = n_evaluable,
    n_total = n_total,
    file = rel_path
  )
}


#' Analyze AAT folder structure
#'
#' Inspects the directory structure to understand how AAT files are organized.
#' Similar to [klawa_analyze_structure()] but for AAT files.
#'
#' @param root Directory to analyze
#' @return List with components:
#'   - `structure`: Detected folder hierarchy (flat, one-level, multi-level)
#'   - `n_files`: Total number of CSV files found
#'   - `subfolder_summary`: Tibble showing file counts per subfolder
#'   - `sample_paths`: Character vector of example file paths
#'
#' @examples
#' \dontrun{
#' structure_info <- aat_analyze_structure("data/AAT")
#' print(structure_info$structure)
#' View(structure_info$subfolder_summary)
#' }
#' @export
aat_analyze_structure <- function(root) {
  if (!fs::dir_exists(root)) {
    rlang::abort(paste0("Directory not found: ", root))
  }

  # Find all CSV files
  csv_files <- fs::dir_ls(root, recurse = TRUE, regexp = "\\.(csv|itl\\.csv)$", type = "file")
  n_files <- length(csv_files)

  if (n_files == 0) {
    return(list(
      structure = "empty",
      n_files = 0,
      subfolder_summary = tibble::tibble(subfolder = character(), n_files = integer()),
      sample_paths = character()
    ))
  }

  # Analyze folder depth
  rel_paths <- fs::path_rel(csv_files, root)
  depths <- purrr::map_int(rel_paths, ~length(fs::path_split(.)[[1]]))

  # Determine structure type
  if (all(depths == 1)) {
    structure_type <- "flat"  # All files directly in root
  } else if (all(depths == 2)) {
    structure_type <- "one-level"  # Files in subfolders (one level deep)
  } else {
    structure_type <- "multi-level"  # Mixed or deeper hierarchy
  }

  # Count files per subfolder
  parent_dirs <- purrr::map_chr(csv_files, ~as.character(fs::path_dir(.)))
  subfolder_counts <- tibble::tibble(subfolder = parent_dirs) %>%
    dplyr::count(subfolder, name = "n_files") %>%
    dplyr::mutate(subfolder = fs::path_rel(subfolder, root)) %>%
    dplyr::arrange(dplyr::desc(n_files))

  # Sample paths for display
  sample_paths <- head(rel_paths, min(10, length(rel_paths)))

  list(
    structure = structure_type,
    n_files = n_files,
    subfolder_summary = subfolder_counts,
    sample_paths = sample_paths
  )
}
