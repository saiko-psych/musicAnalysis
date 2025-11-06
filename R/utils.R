#' Validate presence of required columns in a parsed dataset
#'
#' Defensive check to run immediately after parsing (e.g., after [klawa_scan()]).
#' Verifies that all required columns exist so downstream merges/summaries do not
#' fail unexpectedly. Returns formatted validation report.
#'
#' @param df A data.frame/tibble produced by one of the parsers.
#' @param required Character vector of required column names. Defaults to the
#'   canonical KLAWA schema:
#'   `c("group","measurement","pc","code",
#'   "volume_difference","pitch","onset_difference",
#'   "pitch_duration_difference","file")`.
#' @param return_type Character: "text" returns formatted text, "logical" returns TRUE/FALSE,
#'   "stop" throws error if validation fails (default for backwards compatibility).
#'
#' @return If `return_type = "text"`, a character string with validation report.
#'   If `return_type = "logical"`, TRUE if valid, FALSE otherwise.
#'   If `return_type = "stop"`, `df` (invisibly) if valid, or throws error.
#' @section When to customize `required`:
#' If you add new parsers to \pkg{musicAnalysis}, extend the required set for
#' that dataset in your analysis scripts (e.g., add new metric columns or keys).
#'
#' @examples
#' \dontrun{
#' df <- klawa_scan("data/KLAWA")
#' validate_dataset(df)  # error if any required column is missing
#' validate_dataset(df, return_type = "text")  # formatted report
#' }
#' @export
validate_dataset <- function(
    df,
    required = c("group","measurement","pc","code","date",
                 "volume_difference","pitch","onset_difference",
                 "pitch_duration_difference","file"),
    return_type = c("stop", "text", "logical")
) {
  return_type <- match.arg(return_type)

  # Check for missing columns
  missing <- setdiff(required, names(df))
  present <- intersect(required, names(df))

  # Check data types and content
  issues <- tibble::tibble()

  # Numeric columns should be numeric
  numeric_expected <- c("volume_difference", "pitch", "onset_difference", "pitch_duration_difference")
  for (col in intersect(numeric_expected, present)) {
    if (!is.numeric(df[[col]])) {
      issues <- dplyr::bind_rows(
        issues,
        tibble::tibble(
          column = col,
          issue = paste0("Expected numeric, got ", class(df[[col]])[1])
        )
      )
    }
  }

  # Character columns
  char_expected <- c("group", "measurement", "pc", "code", "date", "file")
  for (col in intersect(char_expected, present)) {
    if (!is.character(df[[col]])) {
      issues <- dplyr::bind_rows(
        issues,
        tibble::tibble(
          column = col,
          issue = paste0("Expected character, got ", class(df[[col]])[1])
        )
      )
    }
  }

  # Build report
  is_valid <- length(missing) == 0 && nrow(issues) == 0

  if (return_type == "logical") {
    return(is_valid)
  }

  if (return_type == "text") {
    txt <- "=== Dataset Validation Report ===\n\n"

    txt <- paste0(txt, sprintf("Dataset dimensions: %d rows \u00D7 %d columns\n\n", nrow(df), ncol(df)))

    if (length(missing) > 0) {
      txt <- paste0(txt, "\u2717 Missing required columns:\n")
      txt <- paste0(txt, paste0("  - ", missing, collapse = "\n"), "\n\n")
    } else {
      txt <- paste0(txt, "\u2713 All required columns present\n\n")
    }

    if (nrow(issues) > 0) {
      txt <- paste0(txt, "\u2717 Data type issues:\n")
      txt <- paste0(txt, paste(utils::capture.output(print(issues)), collapse = "\n"), "\n\n")
    } else if (length(missing) == 0) {
      txt <- paste0(txt, "\u2713 All column types correct\n\n")
    }

    # Data completeness summary
    if (length(present) > 0) {
      completeness <- tibble::tibble(
        column = present,
        missing_count = vapply(df[present], function(x) sum(is.na(x)), integer(1)),
        missing_pct = round(vapply(df[present], function(x) mean(is.na(x)) * 100, numeric(1)), 1)
      ) %>% dplyr::arrange(dplyr::desc(.data$missing_pct))

      txt <- paste0(txt, "Data Completeness:\n")
      txt <- paste0(txt, paste(utils::capture.output(print(completeness, n = Inf)), collapse = "\n"), "\n\n")
    }

    if (is_valid) {
      txt <- paste0(txt, "\u2713 Validation PASSED\n")
    } else {
      txt <- paste0(txt, "\u2717 Validation FAILED\n")
    }

    return(txt)
  }

  # return_type == "stop" (default behavior)
  if (length(missing)) {
    stop(
      sprintf("Missing required columns: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(df)
}

#' Quick diagnostics for common parsing issues
#'
#' Provides detailed diagnostics for KLAWA data quality, including:
#' - Code conflicts (multiple codes in same file)
#' - Missing codes
#' - Missing metadata (group, measurement, pc)
#' - Missing numeric values
#' - Files with problems
#'
#' @param df Parsed dataset (e.g., from [klawa_scan()]).
#' @param return_type Character: "summary" returns formatted text, "detailed" returns list with tibbles.
#' @return If `return_type = "summary"`, a character string with formatted diagnostics.
#'   If `return_type = "detailed"`, a list with:
#'   \itemize{
#'     \item `summary` - tibble with problem counts
#'     \item `code_conflicts` - tibble of files with code conflicts
#'     \item `missing_codes` - tibble of files with missing codes
#'     \item `missing_metadata` - tibble of files with missing group/measurement/pc
#'     \item `missing_values` - tibble of files with missing numeric values
#'   }
#' @examples
#' \dontrun{
#' d <- klawa_scan("data/KLAWA")
#' peek_problems(d)  # Summary text
#' peek_problems(d, return_type = "detailed")  # Detailed tibbles
#' }
#' @export
peek_problems <- function(df, return_type = c("summary", "detailed")) {
  return_type <- match.arg(return_type)

  # Check for required columns
  if (!"file" %in% names(df)) {
    stop("Column 'file' is required for peek_problems()", call. = FALSE)
  }

  # Initialize problem tracking
  numeric_cols <- intersect(
    c("volume_difference", "pitch", "onset_difference", "pitch_duration_difference"),
    names(df)
  )
  metadata_cols <- intersect(c("group", "measurement", "pc", "date"), names(df))

  # 0. Code mismatches (filename vs PDF content)
  code_mismatches <- tibble::tibble()
  if ("code_mismatch" %in% names(df) && "code" %in% names(df) && "code_pdf" %in% names(df)) {
    mismatches <- df[!is.na(df$code_mismatch) & df$code_mismatch == TRUE, ]
    if (nrow(mismatches) > 0) {
      code_mismatches <- tibble::tibble(
        file = mismatches$file,
        code_filename = mismatches$code,
        code_pdf = mismatches$code_pdf,
        issue = sprintf("Filename code '%s' differs from PDF code '%s'",
                       mismatches$code, mismatches$code_pdf)
      )
    }
  }

  # 1. Code conflicts
  code_conflicts <- tibble::tibble()
  if ("code" %in% names(df)) {
    conflicts <- df[!is.na(df$code) & df$code == "CODE_CONFLICT", ]
    if (nrow(conflicts) > 0) {
      code_conflicts <- tibble::tibble(
        file = conflicts$file,
        issue = "Multiple different codes in filename"
      )
    }
  }

  # 2. Missing codes
  missing_codes <- tibble::tibble()
  if ("code" %in% names(df)) {
    missing <- df[is.na(df$code), ]
    if (nrow(missing) > 0) {
      missing_codes <- tibble::tibble(
        file = missing$file,
        issue = "No participant code found"
      )
    }
  }

  # 3. Missing metadata (group, measurement, pc)
  missing_metadata <- tibble::tibble()
  if (length(metadata_cols) > 0) {
    for (col in metadata_cols) {
      missing <- df[is.na(df[[col]]), ]
      if (nrow(missing) > 0) {
        missing_metadata <- dplyr::bind_rows(
          missing_metadata,
          tibble::tibble(
            file = missing$file,
            column = col,
            issue = paste0("Missing ", col)
          )
        )
      }
    }
  }

  # 4. Missing numeric values
  missing_values <- tibble::tibble()
  if (length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      missing <- df[is.na(df[[col]]), ]
      if (nrow(missing) > 0) {
        missing_values <- dplyr::bind_rows(
          missing_values,
          tibble::tibble(
            file = missing$file,
            column = col,
            issue = paste0("Missing ", col)
          )
        )
      }
    }
  }

  # Summary counts (handle empty tibbles safely)
  problem_types <- c(
    "Code mismatches (filename vs PDF)",
    "Code conflicts",
    "Missing codes",
    "Missing metadata fields",
    "Missing numeric values"
  )
  problem_counts <- c(
    nrow(code_mismatches),
    nrow(code_conflicts),
    nrow(missing_codes),
    if (nrow(missing_metadata) > 0) length(unique(missing_metadata$file)) else 0L,
    if (nrow(missing_values) > 0) length(unique(missing_values$file)) else 0L
  )
  problem_files <- list(
    if (nrow(code_mismatches) > 0) code_mismatches$file else character(0),
    if (nrow(code_conflicts) > 0) code_conflicts$file else character(0),
    if (nrow(missing_codes) > 0) missing_codes$file else character(0),
    if (nrow(missing_metadata) > 0) missing_metadata$file else character(0),
    if (nrow(missing_values) > 0) missing_values$file else character(0)
  )

  summary_tbl <- tibble::tibble(
    problem_type = c(problem_types, "Total files with issues"),
    count = c(
      problem_counts,
      length(unique(unlist(problem_files)))
    ),
    percentage = round(c(
      problem_counts / nrow(df) * 100,
      length(unique(unlist(problem_files))) / nrow(df) * 100
    ), 1)
  )

  # Return detailed or summary
  if (return_type == "detailed") {
    return(list(
      summary = summary_tbl,
      code_mismatches = code_mismatches,
      code_conflicts = code_conflicts,
      missing_codes = missing_codes,
      missing_metadata = missing_metadata,
      missing_values = missing_values
    ))
  } else {
    # Format summary text
    txt <- paste0(
      "=== KLAWA Data Quality Report ===\n",
      sprintf("Total files scanned: %d\n\n", nrow(df)),
      "Problem Summary:\n",
      paste(utils::capture.output(print(summary_tbl, n = Inf)), collapse = "\n"),
      "\n"
    )

    # Add details for each problem type
    if (nrow(code_mismatches) > 0) {
      txt <- paste0(txt, "\n--- Code Mismatches (Filename vs PDF) ---\n")
      txt <- paste0(txt, paste(utils::capture.output(print(code_mismatches[, c("file", "code_filename", "code_pdf")], n = 20)), collapse = "\n"), "\n")
    }

    if (nrow(code_conflicts) > 0) {
      txt <- paste0(txt, "\n--- Code Conflicts ---\n")
      txt <- paste0(txt, paste(utils::capture.output(print(code_conflicts, n = 20)), collapse = "\n"), "\n")
    }

    if (nrow(missing_codes) > 0) {
      txt <- paste0(txt, "\n--- Missing Codes ---\n")
      txt <- paste0(txt, paste(utils::capture.output(print(missing_codes, n = 20)), collapse = "\n"), "\n")
    }

    if (nrow(missing_metadata) > 0) {
      txt <- paste0(txt, "\n--- Missing Metadata (first 20) ---\n")
      txt <- paste0(txt, paste(utils::capture.output(print(missing_metadata, n = 20)), collapse = "\n"), "\n")
    }

    if (nrow(missing_values) > 0) {
      txt <- paste0(txt, "\n--- Missing Numeric Values (first 20) ---\n")
      txt <- paste0(txt, paste(utils::capture.output(print(missing_values, n = 20)), collapse = "\n"), "\n")
    }

    if (sum(summary_tbl$count[1:(nrow(summary_tbl)-1)]) == 0) {
      txt <- paste0(txt, "\n\u2713 No problems detected! All files parsed successfully.\n")
    }

    return(txt)
  }
}
