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
#' @param file_types Character vector specifying which file types to scan. Options:
#'   - "rsl_summary": Real .rsl files with summary format (Type of Pair column)
#'   - "rsl_itemlevel": .rsl files with item-level format (% F0 column)
#'   - "itl": Raw response .itl.csv files
#'   Default: c("rsl_summary") - only scan real .rsl summary files
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
#' - CSV files with columns:
#'   - "Pitch Classification": 0 = spectral, 1 = f0, 2 = ambivalent, -1 = don't know
#'   - "Response": -1 = skip trial (excluded), 0/1/2 = participant response
#'   - "Nmin [-]": Identifies Control (same numbers, e.g. "3 3") vs Ambiguous (different, e.g. "5 2")
#' - Filename format: `AAT_response_<CODE>_<DATE>.itl.csv` or similar
#'
#' **Calculation logic (matching AAT manual and .rsl files)**:
#' 1. Filter out skip trials (Response == -1)
#' 2. Aggregate by TONE PAIR (Reference F0, F0 Difference, Nmin, Phase)
#' 3. Count F0 responses: Pitch Classification in c(1, 2) - BOTH codes count!
#' 4. Formula: AAT Score [%] = 100 * sum(# F0 per pair) / sum(# Items per pair)
#' 5. Separate scores for Ambiguous (different Nmin) and Control (same Nmin)
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
                     control_items = NULL,
                     file_types = c("rsl_summary")) {

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
      a_tone_pairs = integer(),
      c_tone_pairs = integer(),
      a_avg_items_per_pair = numeric(),
      c_avg_items_per_pair = numeric(),
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
      aat_parse_one(f, rel_path, code_pattern, date_format, ambiguous_items, control_items, file_types)
    }, error = function(e) {
      cli::cli_warn("Failed to parse {rel_path}: {e$message}")
      tibble::tibble(
        code = NA_character_,
        date = NA_character_,
        file_type = NA_character_,
        file_subtype = NA_character_,
        ambiguous_pct = NA_real_,
        control_pct = NA_real_,
        a_tone_pairs = NA_integer_,
        c_tone_pairs = NA_integer_,
        a_avg_items_per_pair = NA_real_,
        c_avg_items_per_pair = NA_real_,
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
#' @param file_types Character vector of allowed file types
#'
#' @return Single-row tibble with AAT metrics, or NULL if file type not in file_types
#' @keywords internal
aat_parse_one <- function(file_path, rel_path, code_pattern, date_format, ambiguous_items, control_items, file_types) {

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

  # Parse based on file type and check if it should be included
  if (file_type == "rsl") {
    result <- .aat_parse_rsl(file_path, rel_path, code, date)

    # Check if this rsl subtype is in the requested file_types
    if (result$file_subtype == "rsl_summary" && !"rsl_summary" %in% file_types) {
      return(NULL)
    }
    if (result$file_subtype == "rsl_itemlevel" && !"rsl_itemlevel" %in% file_types) {
      return(NULL)
    }

    return(result)
  } else {
    # .itl files
    if (!"itl" %in% file_types) {
      return(NULL)
    }
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

  # Initialize all variables
  ambiguous_pct <- NA_real_
  control_pct <- NA_real_
  a_tone_pairs <- NA_integer_
  c_tone_pairs <- NA_integer_
  a_avg_items_per_pair <- NA_real_
  c_avg_items_per_pair <- NA_real_
  file_subtype <- NA_character_

  # Check for Format 1 (summary format) - "real" .rsl files
  if ("Type of Pair" %in% names(data) && "AAT Score [%]" %in% names(data)) {
    file_subtype <- "rsl_summary"
    # Find row where Type of Pair is "Ambiguous"
    ambig_row <- which(stringr::str_detect(data$`Type of Pair`, "(?i)ambiguous"))
    if (length(ambig_row) > 0) {
      ambiguous_pct <- as.numeric(data$`AAT Score [%]`[ambig_row[1]])

      # Extract tone pairs and avg items per pair for ambiguous
      if ("# Tone Pairs" %in% names(data)) {
        a_tone_pairs <- as.integer(data$`# Tone Pairs`[ambig_row[1]])
      }
      if ("Avg. # Items/Pair" %in% names(data)) {
        a_avg_items_per_pair <- as.numeric(data$`Avg. # Items/Pair`[ambig_row[1]])
      }
    }

    # Find row where Type of Pair is "Control"
    control_row <- which(stringr::str_detect(data$`Type of Pair`, "(?i)control"))
    if (length(control_row) > 0) {
      control_pct <- as.numeric(data$`AAT Score [%]`[control_row[1]])

      # Extract tone pairs and avg items per pair for control
      if ("# Tone Pairs" %in% names(data)) {
        c_tone_pairs <- as.integer(data$`# Tone Pairs`[control_row[1]])
      }
      if ("Avg. # Items/Pair" %in% names(data)) {
        c_avg_items_per_pair <- as.numeric(data$`Avg. # Items/Pair`[control_row[1]])
      }
    }
  }
  # Check for Format 2 (item-level format) - "fake" .rsl files
  else if ("% F0" %in% names(data) && "Nmin [-]" %in% names(data)) {
    file_subtype <- "rsl_itemlevel"
    # This format has per-tone-pair statistics
    # We MUST use Nmin column to separate ambiguous from control:
    # - Control items: Nmin has SAME harmonic numbers (e.g., "3 3", "4 4", "5 5")
    # - Ambiguous items: Nmin has DIFFERENT harmonic numbers (e.g., "5 2", "7 3", "9 4")

    nmin_col <- data[["Nmin [-]"]]
    n_items <- data[["# Items"]]
    n_f0 <- data[["# F0"]]
    
    # Split Nmin into two parts and check if they're the same
    nmin_parts <- stringr::str_split(nmin_col, "\\s+")
    is_control <- purrr::map_lgl(nmin_parts, ~.x[1] == .x[2])

    # Ambiguous: Nmin parts are DIFFERENT
    ambig_idx <- which(!is_control)
    if (length(ambig_idx) > 0) {
      ambig_total_items <- sum(n_items[ambig_idx], na.rm = TRUE)
      ambig_f0_responses <- sum(n_f0[ambig_idx], na.rm = TRUE)

      if (ambig_total_items > 0) {
        ambiguous_pct <- round((ambig_f0_responses / ambig_total_items) * 100, 1)
      }

      # Calculate tone pairs and avg items per pair
      a_tone_pairs <- length(ambig_idx)
      a_avg_items_per_pair <- if (a_tone_pairs > 0) {
        round(ambig_total_items / a_tone_pairs, 1)
      } else {
        NA_real_
      }
    }

    # Control: Nmin parts are SAME
    control_idx <- which(is_control)
    if (length(control_idx) > 0) {
      control_total_items <- sum(n_items[control_idx], na.rm = TRUE)
      control_f0_responses <- sum(n_f0[control_idx], na.rm = TRUE)

      if (control_total_items > 0) {
        control_pct <- round((control_f0_responses / control_total_items) * 100, 1)
      }

      # Calculate tone pairs and avg items per pair
      c_tone_pairs <- length(control_idx)
      c_avg_items_per_pair <- if (c_tone_pairs > 0) {
        round(control_total_items / c_tone_pairs, 1)
      } else {
        NA_real_
      }
    }
  }


  tibble::tibble(
    code = code,
    date = date,
    file_type = "rsl",
    file_subtype = file_subtype,
    ambiguous_pct = ambiguous_pct,
    control_pct = control_pct,
    a_tone_pairs = a_tone_pairs,
    c_tone_pairs = c_tone_pairs,
    a_avg_items_per_pair = a_avg_items_per_pair,
    c_avg_items_per_pair = c_avg_items_per_pair,
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

  # Find required columns
  pitch_col <- names(data)[stringr::str_detect(names(data), "^Pitch Classification")]
  response_col <- names(data)[stringr::str_detect(names(data), "^Response")]

  if (length(pitch_col) == 0) {
    rlang::abort("Column 'Pitch Classification' not found in CSV")
  }

  pitch_col <- pitch_col[1]

  # CRITICAL FIX: Filter out skip trials (Response == -1)
  # This is the ONLY filtering we do - do NOT filter by "*" in Index!
  if (length(response_col) > 0) {
    response_col <- response_col[1]
    responses <- as.numeric(data[[response_col]])
    skip_idx <- which(is.na(responses) | responses == -1)

    # CRITICAL: Only filter if there ARE skip trials
    # If skip_idx is empty, data[-integer(0), ] would return empty data frame!
    if (length(skip_idx) > 0) {
      data <- data[-skip_idx, ]
    }
  }
  # Fallback for test data without Response column: filter by "*" in Index
  else if ("Index" %in% names(data)) {
    data <- data[stringr::str_detect(data$Index, "\\*"), ]
  }

  # Extract Pitch Classification after filtering
  classifications <- as.numeric(data[[pitch_col]])

  # Count response types for quality metrics (from Pitch Classification)
  n_total <- length(classifications)
  n_ambivalent <- sum(classifications == 2, na.rm = TRUE)
  n_dont_know <- sum(classifications == -1, na.rm = TRUE)
  n_evaluable <- sum(classifications %in% c(0, 1), na.rm = TRUE)

  # Initialize variables
  ambiguous_pct <- NA_real_
  control_pct <- NA_real_
  a_tone_pairs <- NA_integer_
  c_tone_pairs <- NA_integer_
  a_avg_items_per_pair <- NA_real_
  c_avg_items_per_pair <- NA_real_

  # Check if all required columns exist for tone-pair aggregation
  required_cols <- c("Reference F0 [Hz]", "F0 Difference [%]", "Nmin [-]", "Phase [Cos;Alt;Rnd]")

  if (all(required_cols %in% names(data))) {

    # Determine Type (Control vs Ambiguous) using Nmin pattern
    # Control items: Nmin has SAME harmonic numbers (e.g., "3 3", "4 4", "5 5")
    # Ambiguous items: Nmin has DIFFERENT harmonic numbers (e.g., "5 2", "7 3", "9 4")
    data$Type <- ifelse(
      stringr::str_detect(data[["Nmin [-]"]], "^(\\d+)\\s+\\1$"),
      "Control",
      "Ambiguous"
    )

    # Create unique tone pair identifiers (must include ALL defining columns!)
    pair_keys <- c("Reference F0 [Hz]", "F0 Difference [%]", "Nmin [-]", "Phase [Cos;Alt;Rnd]", "Type")

    # CRITICAL FIX: F0 codes are 1 AND 2 (not just 1!)
    # This matches the AAT manual and makes Control scores work correctly
    data$is_f0 <- (classifications %in% c(1, 2))

    # Aggregate by tone pair
    pairs <- data %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(pair_keys))
      ) %>%
      dplyr::summarise(
        n_items = dplyr::n(),
        n_f0 = sum(is_f0, na.rm = TRUE),
        .groups = "drop"
      )

    # Aggregate by Type (Ambiguous / Control)
    type_summary <- pairs %>%
      dplyr::group_by(Type) %>%
      dplyr::summarise(
        n_tone_pairs = dplyr::n(),
        avg_items_per_pair = mean(n_items, na.rm = TRUE),
        # FORMULA: AAT Score = 100 * sum(# F0) / sum(# Items)
        aat_score = 100 * sum(n_f0, na.rm = TRUE) / sum(n_items, na.rm = TRUE),
        .groups = "drop"
      )

    # Extract results for Ambiguous
    ambig_row <- type_summary[type_summary$Type == "Ambiguous", ]
    if (nrow(ambig_row) > 0) {
      ambiguous_pct <- round(ambig_row$aat_score, 1)
      a_tone_pairs <- as.integer(ambig_row$n_tone_pairs)
      a_avg_items_per_pair <- round(ambig_row$avg_items_per_pair, 2)
    }

    # Extract results for Control
    control_row <- type_summary[type_summary$Type == "Control", ]
    if (nrow(control_row) > 0) {
      control_pct <- round(control_row$aat_score, 1)
      c_tone_pairs <- as.integer(control_row$n_tone_pairs)
      c_avg_items_per_pair <- round(control_row$avg_items_per_pair, 2)
    }

  } else {
    # Fallback: if required columns don't exist, calculate overall percentage
    if (n_total > 0) {
      n_f0_total <- sum(classifications %in% c(1, 2), na.rm = TRUE)
      overall_f0_pct <- round((n_f0_total / n_total) * 100, 1)
      ambiguous_pct <- overall_f0_pct
    }
  }

  tibble::tibble(
    code = code,
    date = date,
    file_type = "itl",
    file_subtype = "itl",
    ambiguous_pct = ambiguous_pct,
    control_pct = control_pct,
    a_tone_pairs = a_tone_pairs,
    c_tone_pairs = c_tone_pairs,
    a_avg_items_per_pair = a_avg_items_per_pair,
    c_avg_items_per_pair = c_avg_items_per_pair,
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
#'   - `n_aat_files`: Number of files containing "AAT" in filename
#'   - `n_rsl_summary`: Number of real .rsl files (summary format)
#'   - `n_rsl_itemlevel`: Number of .rsl files (item-level format)
#'   - `n_itl`: Number of .itl files
#'   - `tree`: Character vector representing folder tree structure
#'   - `subfolder_summary`: Tibble showing file counts per subfolder
#'   - `sample_paths`: Character vector of example file paths
#'
#' @examples
#' \dontrun{
#' structure_info <- aat_analyze_structure("data/AAT")
#' print(structure_info$structure)
#' cat(structure_info$tree, sep = "\n")
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
      n_aat_files = 0,
      n_rsl_summary = 0,
      n_rsl_itemlevel = 0,
      n_itl = 0,
      tree = character(0),
      subfolder_summary = tibble::tibble(subfolder = character(), n_files = integer()),
      sample_paths = character()
    ))
  }

  # Filter for AAT files only
  aat_files <- csv_files[stringr::str_detect(basename(csv_files), "AAT")]
  n_aat_files <- length(aat_files)

  # Detect file types by checking file content
  n_rsl_summary <- 0
  n_rsl_itemlevel <- 0
  n_itl <- 0

  for (file in aat_files) {
    tryCatch({
      # Read first few lines to check format
      data <- readr::read_csv(file, n_max = 2, show_col_types = FALSE, col_types = readr::cols())
      col_names <- names(data)

      if (stringr::str_detect(basename(file), "\\.itl\\.csv$")) {
        n_itl <- n_itl + 1
      } else if ("Type of Pair" %in% col_names) {
        n_rsl_summary <- n_rsl_summary + 1
      } else if ("% F0" %in% col_names) {
        n_rsl_itemlevel <- n_rsl_itemlevel + 1
      }
    }, error = function(e) {
      # If can't read file, skip it
    })
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

  # Generate tree structure
  tree <- .generate_tree(root, aat_files)

  list(
    structure = structure_type,
    n_files = n_files,
    n_aat_files = n_aat_files,
    n_rsl_summary = n_rsl_summary,
    n_rsl_itemlevel = n_rsl_itemlevel,
    n_itl = n_itl,
    tree = tree,
    subfolder_summary = subfolder_counts,
    sample_paths = sample_paths
  )
}

#' Generate folder tree structure
#'
#' Creates a visual tree representation showing folder hierarchy with file counts
#'
#' @param root Root directory
#' @param files Vector of file paths to include in tree
#' @return Character vector with tree lines
#' @keywords internal
.generate_tree <- function(root, files) {
  if (length(files) == 0) {
    return(character(0))
  }

  # Get relative paths
  rel_paths <- fs::path_rel(files, root)

  # Build tree structure
  tree_lines <- character()
  tree_lines <- c(tree_lines, basename(root))

  # Group files by directory
  dirs <- unique(dirname(rel_paths))
  dirs <- dirs[dirs != "."]  # Remove root indicator

  if (length(dirs) == 0) {
    # All files in root - show count instead of individual files
    n_files <- length(rel_paths)
    tree_lines <- c(tree_lines, paste0("└── (", n_files, " AAT file", ifelse(n_files != 1, "s", ""), ")"))
  } else {
    # Files in subdirectories - show counts per directory
    # Sort directories for consistent output
    dirs <- sort(dirs)

    for (dir_idx in seq_along(dirs)) {
      dir_path <- dirs[dir_idx]
      is_last_dir <- (dir_idx == length(dirs))

      # Count files in this directory
      files_in_dir <- sum(dirname(rel_paths) == dir_path)

      # Add directory with file count
      dir_parts <- fs::path_split(dir_path)[[1]]
      indent <- paste(rep("│   ", length(dir_parts) - 1), collapse = "")
      prefix <- if (is_last_dir) "└── " else "├── "
      tree_lines <- c(tree_lines, paste0(indent, prefix, basename(dir_path), "/ (", files_in_dir, " AAT file", ifelse(files_in_dir != 1, "s", ""), ")"))
    }
  }

  return(tree_lines)
}
