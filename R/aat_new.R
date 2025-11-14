#' Parse .itl.csv file (raw responses) - CORRECTED VERSION
#'
#' Based on user's correct R formula that matches .rsl files
#'
#' @keywords internal
.aat_parse_itl_CORRECTED <- function(file_path, rel_path, code, date, ambiguous_items = NULL, control_items = NULL) {

  # Read CSV file
  data <- readr::read_csv(file_path, show_col_types = FALSE)

  # Find required columns
  pitch_col <- names(data)[stringr::str_detect(names(data), "^Pitch Classification")]
  response_col <- names(data)[stringr::str_detect(names(data), "^Response")]

  if (length(pitch_col) == 0) {
    rlang::abort("Column 'Pitch Classification' not found in CSV")
  }

  if (length(response_col) == 0) {
    rlang::abort("Column 'Response' not found in CSV")
  }

  pitch_col <- pitch_col[1]
  response_col <- response_col[1]

  # STEP 1: Filter out skip trials (Response == -1)
  # This is the ONLY filtering we do - do NOT filter by "*" in Index!
  responses <- as.numeric(data[[response_col]])
  skip_idx <- which(is.na(responses) | responses == -1)

  if (length(skip_idx) > 0) {
    data <- data[-skip_idx, ]
  }

  # Re-extract after filtering
  classifications <- as.numeric(data[[pitch_col]])
  responses <- as.numeric(data[[response_col]])

  # Count response types for quality metrics (from Pitch Classification)
  n_total <- length(classifications)
  n_ambivalent <- sum(classifications == 2, na.rm = TRUE)
  n_dont_know <- sum(classifications == -1, na.rm = TRUE)
  n_evaluable <- sum(classifications %in% c(0, 1), na.rm = TRUE)

  # Initialize output variables
  ambiguous_pct <- NA_real_
  control_pct <- NA_real_
  a_tone_pairs <- NA_integer_
  c_tone_pairs <- NA_integer_
  a_avg_items_per_pair <- NA_real_
  c_avg_items_per_pair <- NA_real_

  # Check if all required columns exist for tone-pair aggregation
  required_cols <- c("Reference F0 [Hz]", "F0 Difference [%]", "Nmin [-]", "Phase [Cos;Alt;Rnd]")

  if (all(required_cols %in% names(data))) {

    # STEP 2: Determine Type (Control vs Ambiguous) using Nmin pattern
    # Control: Nmin has SAME numbers (e.g., "3 3", "4 4")
    # Ambiguous: Nmin has DIFFERENT numbers (e.g., "5 2", "7 3")
    data$Type <- ifelse(
      stringr::str_detect(data[["Nmin [-]"]], "^(\\d+)\\s+\\1$"),
      "Control",
      "Ambiguous"
    )

    # STEP 3: Create unique tone pair identifiers
    # Must include all keys that define a unique tone pair
    pair_keys <- c("Reference F0 [Hz]", "F0 Difference [%]", "Nmin [-]", "Phase [Cos;Alt;Rnd]", "Type")

    # STEP 4: Classify F0 responses
    # CRITICAL: F0_CODES = c(1, 2) means Pitch Classification == 1 OR 2 counts as F0
    data$is_f0 <- (classifications %in% c(1, 2))

    # STEP 5: Aggregate by tone pair
    pairs <- data %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(pair_keys))
      ) %>%
      dplyr::summarise(
        n_items = dplyr::n(),
        n_f0 = sum(is_f0, na.rm = TRUE),
        .groups = "drop"
      )

    # STEP 6: Aggregate by Type (Ambiguous / Control)
    type_summary <- pairs %>%
      dplyr::group_by(Type) %>%
      dplyr::summarise(
        n_tone_pairs = dplyr::n(),
        avg_items_per_pair = mean(n_items, na.rm = TRUE),
        # FORMULA: AAT Score = 100 * sum(# F0) / sum(# Items)
        aat_score = 100 * sum(n_f0, na.rm = TRUE) / sum(n_items, na.rm = TRUE),
        .groups = "drop"
      )

    # STEP 7: Extract results for Ambiguous
    ambig_row <- type_summary[type_summary$Type == "Ambiguous", ]
    if (nrow(ambig_row) > 0) {
      ambiguous_pct <- round(ambig_row$aat_score, 1)
      a_tone_pairs <- as.integer(ambig_row$n_tone_pairs)
      a_avg_items_per_pair <- round(ambig_row$avg_items_per_pair, 2)
    }

    # STEP 8: Extract results for Control
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
