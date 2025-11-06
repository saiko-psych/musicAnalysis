#' Process LimeSurvey Musical Experience Time Data
#'
#' Reads and processes LimeSurvey data about musical experience, including
#' instrument, singing, and other music practice time by age. Returns both
#' long format (detailed) and wide format (summary) data frames.
#'
#' The per-age columns are assumed to follow the pattern
#' `categoryXY[a_b]`, where `category` is one of `"instrument"`, `"singing"`,
#' or `"othermusic"`, `X` is the category index (1\u20139), `Y` is the age decade
#' (0\u20139), and `b` is the year within the decade (0\u20139). The age in years is
#' computed as `10*Y + b`. Examples:
#' - `instrument10[1_0]` \u2192 instrument 1, age 0
#' - `instrument12[1_3]` \u2192 instrument 1, age 23
#' - `instrument23[1_0]` \u2192 instrument 2, age 30
#'
#' Time inputs accept the following forms (case-insensitive):
#' - `"Nd"` or `"Nw"` (e.g., `"2d"`, `"7w"`)
#' - decimal with dot or comma: `"N.Nd"`, `"N,Nw"` (e.g., `"1.5d"`, `"1,5w"`)
#' - decimal + slash: `"N.N/d"`, `"N,N/w"` (e.g., `"1,5/d"`, `"1,5/w"`)
#' - fractions: `"a/bd"`, `"a/bw"` (e.g., `"1/2w"`)
#' - short fractions: `"a/d"`, `"a/w"` (interpreted as `"ad"`, `"aw"`)
#'
#' Entries above `daily_max_hours` (per day) or `weekly_max_hours` (per week)
#' are flagged as unrealistic.
#'
#' @param file Path to the LimeSurvey CSV export file.
#' @param min_lastpage Keep rows where `lastpage == min_lastpage` (default: 4).
#' @param id_col Optional name of the column that should be used as `code`.
#'   If `NULL` and a `code` column exists, it is used as-is; otherwise a synthetic
#'   `code` is created.
#' @param drop_cols Regex for administrative/instruction columns to drop.
#' @param daily_max_hours Max realistic hours per day (default: 12).
#' @param weekly_max_hours Max realistic hours per week (default: 84).
#' @param weeks_per_year Weeks per year (default: `365/7`).
#' @param check_instruments If `TRUE`, checks `whichinstrument*` against an extended
#'   DE/EN lexicon and reports unrecognized entries.
#' @param verbose Print progress information (default: TRUE).
#'
#' @return A list with:
#' \itemize{
#'   \item `long`: columns `code`, `category` (instrument/singing/othermusic),
#'         `category_id`, `age_years`, `time_str`, `yearly_hours`, `played_category`, `flag_code`.
#'   \item `wide`: one row per `code` with aggregated hours per category/id
#'         (e.g., `instrument1`, `singing2`, `othermusic1`) plus standardized
#'         `which*` / `type*` free-text columns (e.g., `whichothermusic1`).
#'   \item `flags`: problematic time entries (invalid/unrealistic).
#'   \item `instrument_warnings`: unrecognized instruments (if `check_instruments=TRUE`).
#' }
#' @examples
#' \dontrun{
#' res <- musical_experience_time("data/LimeSurvey/.../musikalische_vorerfahrung_05062025.csv")
#' res$long; res$wide; res$flags; res$instrument_warnings
#' }
#' @export
musical_experience_time <- function(
    file,
    min_lastpage = 4,
    id_col = NULL,
    drop_cols = "^(lastpage|startdate|submitdate|startlanguage|seed|datestamp|instruction.*|instructionsinging.*)$",
    daily_max_hours = 12,
    weekly_max_hours = 84,
    weeks_per_year = 365/7,
    check_instruments = TRUE,
    verbose = TRUE
) {
  # dependencies
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required")
  if (!requireNamespace("magrittr", quietly = TRUE)) stop("Package 'magrittr' is required")
  `%>%` <- magrittr::`%>%`

  # 1) read
  if (verbose) cat("Reading data from:", file, "\n")
  raw_data <- readr::read_csv(
    file,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("NA"),  # CRITICAL: Don't treat empty strings as NA, only "NA" strings
    show_col_types = FALSE,
    progress = FALSE
  )
  if (verbose) cat("Raw data:", nrow(raw_data), "rows,", ncol(raw_data), "columns\n")

  # 2) filter lastpage
  filtered_data <- if ("lastpage" %in% names(raw_data)) {
    out <- dplyr::filter(raw_data, suppressWarnings(as.numeric(.data$lastpage)) == min_lastpage)
    if (verbose) cat("After lastpage filter:", nrow(out), "rows remaining\n")
    out
  } else {
    if (verbose) cat("No lastpage column found - keeping all rows\n")
    raw_data
  }

  # 3) ensure ID column is named exactly `code`
  if (!is.null(id_col) && id_col %in% names(filtered_data)) {
    filtered_data <- dplyr::rename(filtered_data, code = !!rlang::sym(id_col))
    if (verbose) cat("Using", id_col, "as code\n")
  } else if (!("code" %in% names(filtered_data))) {
    filtered_data$code <- as.character(seq_len(nrow(filtered_data)))
    if (verbose) cat("No code column found - created synthetic 'code'\n")
  } else {
    if (verbose) cat("Using existing 'code' column\n")
  }

  # 4) drop admin/instruction columns
  cols_to_drop <- grep(drop_cols, names(filtered_data), value = TRUE, ignore.case = TRUE)
  clean_data <- dplyr::select(filtered_data, -dplyr::any_of(cols_to_drop))
  if (verbose) cat("After cleaning:", ncol(clean_data), "columns remaining\n")

  # 5) process categories
  categories <- c("instrument", "singing", "othermusic")
  all_long_data   <- list()
  all_played_data <- list()
  all_which_data  <- list()
  instrument_warnings <- data.frame()

  for (category in categories) {
    if (verbose) cat(sprintf("\nProcessing %s data...\n", category))

    # played flags like instrumentplayed1 / singing1 / othermusic1
    if (category == "instrument") {
      played_pattern <- "^instrumentplayed\\d+$"
      which_prefix <- "whichinstrument"
      type_prefix  <- NULL
    } else if (category == "singing") {
      played_pattern <- "^singing\\d+$"
      which_prefix <- NULL
      type_prefix  <- "singingtype"
    } else { # othermusic
      played_pattern <- "^othermusic\\d+$"
      which_prefix <- "whichothermusic"
      type_prefix  <- NULL
    }

    played_cols <- grep(played_pattern, names(clean_data), value = TRUE)

    # per-age time columns like instrument12[1_3], singing10[1_6], othermusic20[1_4]
    time_pattern <- sprintf("^%s(\\d{2})\\[(\\d+)_([0-9])\\]$", category)
    time_cols <- grep(time_pattern, names(clean_data), value = TRUE)

    if (verbose) {
      cat(sprintf("  Found %d played columns\n", length(played_cols)))
      cat(sprintf("  Found %d time columns\n", length(time_cols)))
    }
    if (!length(time_cols)) {
      if (verbose) cat(sprintf("  No time columns found for %s - skipping\n", category))
      next
    }

    # ensure character before pivot
    time_data <- clean_data %>%
      dplyr::select(.data$code, dplyr::all_of(time_cols)) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(time_cols), as.character))

    long_data <- tidyr::pivot_longer(
      time_data,
      cols = -"code",
      names_to = "colname",
      values_to = "time_str"
    ) %>%
      dplyr::filter(!is.na(.data$time_str) & .data$time_str != "")

    if (!nrow(long_data)) {
      if (verbose) cat(sprintf("  No time data found for %s - skipping\n", category))
      next
    }

    # parse X and age = 10*Y + b from `categoryXY[a_b]`
    matches <- stringr::str_match(long_data$colname, time_pattern)
    long_data$xy          <- suppressWarnings(as.integer(matches[, 2]))
    long_data$b           <- suppressWarnings(as.integer(matches[, 4]))
    long_data$category_id <- floor(long_data$xy / 10)  # X
    long_data$age_years   <- (long_data$xy %% 10) * 10 + long_data$b
    long_data$category    <- category

    # attach played flags (1 = yes, 2 = no) by category_id
    long_data$played_category <- NA_integer_
    if (length(played_cols)) {
      played_df <- dplyr::select(clean_data, .data$code, dplyr::all_of(played_cols))

      # Normalize Y/N to 1/2 before joining (handles both old and new survey formats)
      played_df <- dplyr::mutate(
        played_df,
        dplyr::across(
          dplyr::all_of(played_cols),
          ~ dplyr::case_when(
            toupper(trimws(as.character(.x))) == "Y" ~ "1",
            toupper(trimws(as.character(.x))) == "N" ~ "2",
            TRUE ~ as.character(.x)
          )
        )
      )

      long_data <- dplyr::left_join(long_data, played_df, by = "code")
      for (col_name in played_cols) {
        cat_num <- suppressWarnings(as.integer(stringr::str_extract(col_name, "\\d+$")))
        if (is.na(cat_num) || !(col_name %in% names(long_data))) next
        mask <- long_data$category_id == cat_num
        long_data$played_category[mask] <- suppressWarnings(as.integer(long_data[[col_name]][mask]))
      }
      long_data <- long_data[, setdiff(names(long_data), played_cols), drop = FALSE]
    }

    # parse time strings
    parsed_time <- .parse_time_strings(
      long_data$time_str,
      daily_max_hours  = daily_max_hours,
      weekly_max_hours = weekly_max_hours,
      weeks_per_year   = weeks_per_year
    )
    long_data$yearly_hours <- parsed_time$yearly_hours
    long_data$flag_code    <- parsed_time$flag_code

    # if explicitly "not practiced" (2), set to 0
    long_data$yearly_hours <- ifelse(long_data$played_category == 2, 0, long_data$yearly_hours)

    all_long_data[[category]] <- dplyr::select(
      long_data,
      .data$code, .data$category, .data$category_id, .data$age_years,
      .data$time_str, .data$yearly_hours, .data$played_category, .data$flag_code
    )

    # keep played flags table for summary (use normalized version)
    if (length(played_cols)) {
      all_played_data[[category]] <- played_df
    }

    # standardize which*/type* (supports misspellings like "whitchothermusic")
    if (!is.null(which_prefix)) {
      which_tbl <- .standardize_which_cols(clean_data, which_prefix)
      if (ncol(which_tbl) > 1) {
        all_which_data[[paste0(which_prefix, "_data")]] <- which_tbl
        if (check_instruments && category == "instrument") {
          which_cols_std <- setdiff(names(which_tbl), "code")
          instrument_warnings <- .check_instruments(which_tbl, which_cols_std)
        }
      }
    }
    if (!is.null(type_prefix)) {
      type_tbl <- .standardize_which_cols(clean_data, type_prefix)
      if (ncol(type_tbl) > 1) {
        all_which_data[[paste0(type_prefix, "_data")]] <- type_tbl
      }
    }
  }

  # combine long
  long_final <- if (length(all_long_data)) {
    dplyr::bind_rows(all_long_data) %>%
      dplyr::arrange(.data$code, .data$category, .data$category_id, .data$age_years) %>%
      # Calculate IMP (Index of Musical Practice) = yearly_hours * years_practiced
      dplyr::group_by(.data$code, .data$category, .data$category_id) %>%
      dplyr::ungroup()
  } else {
    data.frame()
  }

  # flags
  flags <- if (nrow(long_final)) {
    dplyr::filter(long_final, !is.na(.data$flag_code) & .data$flag_code != "valid") %>%
      dplyr::select(.data$code, .data$category, .data$category_id, .data$age_years,
                    .data$time_str, .data$flag_code)
  } else {
    data.frame()
  }

  # wide (aggregated hours + standardized which*/type* + starting ages + practice history)
  wide_data <- .create_wide_format(all_long_data, all_which_data, clean_data)

  # summary
  if (verbose) {
    .print_summary(file, long_final, wide_data, flags, all_played_data, instrument_warnings)
  }

  out <- list(long = long_final, wide = wide_data, flags = flags)
  if (check_instruments && nrow(instrument_warnings)) out$instrument_warnings <- instrument_warnings
  out
}

#' Calculate practice history for flexible time windows
#'
#' @description
#' Computes practice hours within user-specified retrospective time windows
#' (e.g., last 1, 2, 5, 10 years) for each participant and category/instrument.
#' Requires the long-format output from [musical_experience_time()].
#'
#' @param long_data Long-format data from `musical_experience_time()$long`,
#'   with columns: `code`, `category`, `category_id`, `age_years`, `yearly_hours`.
#' @param current_age Named numeric vector of current ages per participant code.
#'   Example: `c("0102SICH" = 25, "0103ANDE" = 30)`. If NULL, assumes max age
#'   in data is current age for each participant.
#' @param time_windows Numeric vector of years to look back (default: c(1, 2, 5, 10)).
#'   For each window, computes total practice hours in that retrospective period.
#'
#' @return A wide tibble with columns:
#'   - `code`: participant code
#'   - For each category/id and time window: `{category}{id}_hours_{years}yr`
#'     (e.g., `instrument1_hours_1yr`, `instrument1_hours_5yr`, `singing2_hours_10yr`)
#'
#' @examples
#' \dontrun{
#' res <- musical_experience_time("data/survey.csv")
#' current_ages <- c("0102SICH" = 25, "0103ANDE" = 28)
#' history <- compute_practice_history(res$long, current_ages, time_windows = c(1, 2, 5, 10))
#' }
#' @export
compute_practice_history <- function(
    long_data,
    current_age = NULL,
    time_windows = c(1, 2, 5, 10)
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' required")
  `%>%` <- magrittr::`%>%`

  # if current_age not provided, use max age per participant
  if (is.null(current_age)) {
    current_age <- long_data %>%
      dplyr::group_by(.data$code) %>%
      dplyr::summarise(max_age = max(.data$age_years, na.rm = TRUE), .groups = "drop") %>%
      tibble::deframe()
  }

  # add current age to long data
  long_with_age <- long_data %>%
    dplyr::mutate(
      current_age = current_age[as.character(.data$code)],
      years_ago = .data$current_age - .data$age_years
    )

  # compute hours for each time window
  all_windows <- list()
  for (window in time_windows) {
    windowed <- long_with_age %>%
      dplyr::filter(.data$years_ago >= 0, .data$years_ago < window) %>%
      dplyr::group_by(.data$code, .data$category, .data$category_id) %>%
      dplyr::summarise(hours = sum(.data$yearly_hours, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        var_name = paste0(.data$category, .data$category_id, "_hours_", window, "yr")
      ) %>%
      dplyr::select(.data$code, .data$var_name, .data$hours) %>%
      tidyr::pivot_wider(
        names_from = .data$var_name,
        values_from = .data$hours,
        values_fill = 0
      )
    all_windows[[as.character(window)]] <- windowed
  }

  # merge all windows
  wide_history <- Reduce(function(x, y) dplyr::full_join(x, y, by = "code"), all_windows)
  tibble::as_tibble(wide_history)
}

# ---- internal helpers ------------------------------------------------------

# Define time parsing patterns once to avoid repetition
# @noRd
.get_time_patterns <- function() {
  list(
    decimal_slash     = "^(\\d+[\\.,]\\d+)\\s*/\\s*([dw])$",  # 1,5/w or 1.5/d
    decimal           = "^(\\d+[\\.,]\\d+)\\s*([dw])$",       # 1,5w or 1.5d
    fraction          = "^(\\d+)/(\\d+)\\s*([dw])$",          # 1/2w or 3/4d
    fraction_no_denom = "^(\\d+)/\\s*([dw])$",                # 2/w or 1/d
    integer           = "^(\\d+)\\s*([dw])$"                  # 2w or 5d
  )
}

# Parse a single time pattern match into a numeric value
# @noRd
.parse_single_pattern <- function(match_result, pattern_name) {
  m <- match_result
  if (is.na(m[1])) return(NA_real_)

  if (pattern_name %in% c("decimal_slash", "decimal")) {
    num_str <- gsub(",", ".", m[2])
    return(list(num = suppressWarnings(as.numeric(num_str)), unit = m[3]))
  } else if (pattern_name == "fraction") {
    num <- as.numeric(m[2])
    den <- as.numeric(m[3])
    if (!is.na(num) && !is.na(den) && den != 0) {
      return(list(num = num / den, unit = m[4]))
    }
  } else if (pattern_name %in% c("fraction_no_denom", "integer")) {
    return(list(num = as.numeric(m[2]), unit = m[3]))
  }
  NA_real_
}

# Convert parsed time to yearly hours and generate flag
# @noRd
.convert_to_yearly <- function(parsed_num, unit, daily_max_hours, weekly_max_hours, weeks_per_year) {
  if (is.na(parsed_num) || is.na(unit)) {
    return(list(yearly_hours = NA_real_, flag = "invalid"))
  }

  if (unit == "d") {
    yearly <- parsed_num * 365
    flag <- if (parsed_num > daily_max_hours) "unrealistic_daily" else "valid"
  } else if (unit == "w") {
    yearly <- parsed_num * weeks_per_year
    flag <- if (parsed_num > weekly_max_hours) "unrealistic_weekly" else "valid"
  } else {
    return(list(yearly_hours = NA_real_, flag = "invalid"))
  }

  list(yearly_hours = yearly, flag = flag)
}

# Parse mixed time strings ("2d", "1,5/w", "3/4w", "2/w", "7w") into yearly hours and flags
# @noRd
.parse_time_strings <- function(
    time_strings,
    daily_max_hours = 12,
    weekly_max_hours = 84,
    weeks_per_year = 365/7
) {
  time_clean <- tolower(trimws(as.character(time_strings)))
  n <- length(time_clean)
  yearly_hours <- rep(NA_real_, n)
  flag_code <- rep(NA_character_, n)

  patterns <- .get_time_patterns()

  for (i in seq_along(time_clean)) {
    val <- time_clean[i]
    if (is.na(val) || val == "") {
      flag_code[i] <- NA_character_
      next
    }

    # Try each pattern until we find a match
    parsed <- NULL
    for (nm in names(patterns)) {
      m <- stringr::str_match(val, patterns[[nm]])
      parsed <- .parse_single_pattern(m, nm)
      if (!is.na(parsed[[1]])) break
    }

    # Convert to yearly hours and flag
    if (!is.null(parsed) && is.list(parsed) && !is.na(parsed$num)) {
      result <- .convert_to_yearly(parsed$num, parsed$unit, daily_max_hours, weekly_max_hours, weeks_per_year)
      yearly_hours[i] <- result$yearly_hours
      flag_code[i] <- result$flag
    } else {
      flag_code[i] <- "invalid"
    }
  }

  list(yearly_hours = yearly_hours, flag_code = flag_code)
}

# Coalesce/standardize which*/type* columns.
# Handles misspellings of "which": whitch, wich, whcih, wihch, whitc, whitchother, etc.
# Canonical output names use the given `prefix`, e.g. `whichothermusic1`.
# @noRd
.standardize_which_cols <- function(df, prefix) {
  miss <- c("whitch", "wich", "whcih", "wihch", "whitc", "whitcho", "whitchother")
  if (grepl("^which", prefix, ignore.case = TRUE)) {
    core <- sub("^which", "", prefix, ignore.case = TRUE)
    variants <- paste0(c("which", miss), core)
  } else {
    variants <- prefix
  }

  pat <- paste0("^(", paste(variants, collapse = "|"), ")(\\d+).*$")
  which_cols <- names(df)[ stringr::str_detect(names(df), stringr::regex(pat, ignore_case = TRUE)) ]
  if (!length(which_cols)) return(df[, "code", drop = FALSE])

  m <- stringr::str_match(which_cols, stringr::regex(pat, ignore_case = TRUE))
  nums <- m[,3]
  targets <- paste0(prefix, nums)  # canonical

  out <- df[, "code", drop = FALSE]
  for (tgt in unique(targets)) {
    srcs <- which_cols[targets == tgt]
    is_canonical <- stringr::str_starts(srcs, stringr::regex(paste0("^", prefix), ignore_case = TRUE))
    ord <- c(which(is_canonical), which(!is_canonical))
    srcs <- srcs[ord]

    v <- df[[srcs[1]]]
    if (length(srcs) > 1) {
      for (s in srcs[-1]) v <- ifelse(!is.na(v) & nzchar(trimws(v)), v, df[[s]])
    }
    out[[tgt]] <- v
  }
  out
}

# Build wide table with aggregated hours, starting ages, totals, and merged which*/type* columns
# IMPORTANT: Preserves all original non-music columns from clean_data
# @noRd
.create_wide_format <- function(all_long_data, all_which_data, clean_data) {
  `%>%` <- magrittr::`%>%`
  all_codes <- unique(clean_data$code)

  # Start with ALL original columns (preserve demographic/survey data)
  music_col_patterns <- c(
    "^instrument\\d+\\[", "^singing\\d+\\[", "^othermusic\\d+\\[",  # time columns
    "^instrumentplayed\\d+$", "^singing\\d+$", "^othermusic\\d+$",  # played flags
    "^whichinstrument", "^singingtype", "^whichothermusic"  # which/type columns
  )
  music_cols <- unique(unlist(lapply(music_col_patterns, function(pat) {
    grep(pat, names(clean_data), value = TRUE, ignore.case = TRUE)
  })))

  # Keep all columns EXCEPT the raw music time/played columns (we'll add calculated versions)
  wide_data <- clean_data %>%
    dplyr::select(-dplyr::any_of(music_cols)) %>%
    dplyr::distinct(.data$code, .keep_all = TRUE)

  # aggregated hours per category/id + starting ages + IMP
  for (category in names(all_long_data)) {
    long_cat <- all_long_data[[category]]
    if (!nrow(long_cat)) next

    # total hours per instrument/singing/othermusic
    agg <- long_cat %>%
      dplyr::filter(!is.na(.data$yearly_hours)) %>%
      dplyr::group_by(.data$code, .data$category_id) %>%
      dplyr::summarise(total_hours = sum(.data$yearly_hours, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = .data$category_id,
        values_from = .data$total_hours,
        names_prefix = paste0(category, ""),
        values_fill = NA_real_  # Use NA instead of 0 for consistency
      )
    wide_data <- dplyr::left_join(wide_data, agg, by = "code")

    # starting ages (minimum age where practice occurred)
    starting_ages <- long_cat %>%
      dplyr::filter(!is.na(.data$yearly_hours), .data$yearly_hours > 0) %>%
      dplyr::group_by(.data$code, .data$category_id) %>%
      dplyr::summarise(starting_age = min(.data$age_years, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = .data$category_id,
        values_from = .data$starting_age,
        names_prefix = paste0(category, "_starting_age"),
        values_fill = NA_real_
      )
    wide_data <- dplyr::left_join(wide_data, starting_ages, by = "code")

    # sum across all instruments/singing/othermusic for this category
    sum_col_name <- paste0(category, "_total")
    cat_cols <- grep(paste0("^", category, "\\d+$"), names(wide_data), value = TRUE)
    if (length(cat_cols) > 0) {
      wide_data[[sum_col_name]] <- rowSums(wide_data[, cat_cols, drop = FALSE], na.rm = TRUE)
      # If all values were NA, set sum to NA instead of 0
      all_na <- apply(is.na(wide_data[, cat_cols, drop = FALSE]), 1, all)
      wide_data[[sum_col_name]][all_na] <- NA_real_
    }
  }

  # Grand total across all categories (instrument + singing + othermusic)
  sum_cols <- grep("_(total)$", names(wide_data), value = TRUE)
  if (length(sum_cols) > 0) {
    wide_data$total_musical_experience <- rowSums(wide_data[, sum_cols, drop = FALSE], na.rm = TRUE)
    # If all sums were NA, set grand total to NA
    all_na <- apply(is.na(wide_data[, sum_cols, drop = FALSE]), 1, all)
    wide_data$total_musical_experience[all_na] <- NA_real_
  }

  # Count number of instruments/singing/othermusic per person
  # number_of_instruments: count non-NA instrument columns with practice > 0
  instrument_cols <- grep("^instrument\\d+$", names(wide_data), value = TRUE)
  if (length(instrument_cols) > 0) {
    wide_data$number_of_instruments <- apply(wide_data[, instrument_cols, drop = FALSE], 1, function(row) {
      sum(row > 0, na.rm = TRUE)
    })
  } else {
    wide_data$number_of_instruments <- 0L
  }

  # Count number of singing experiences
  singing_cols <- grep("^singing\\d+$", names(wide_data), value = TRUE)
  if (length(singing_cols) > 0) {
    wide_data$number_of_singing <- apply(wide_data[, singing_cols, drop = FALSE], 1, function(row) {
      sum(row > 0, na.rm = TRUE)
    })
  } else {
    wide_data$number_of_singing <- 0L
  }

  # Count number of other music experiences
  othermusic_cols <- grep("^othermusic\\d+$", names(wide_data), value = TRUE)
  if (length(othermusic_cols) > 0) {
    wide_data$number_of_othermusic <- apply(wide_data[, othermusic_cols, drop = FALSE], 1, function(row) {
      sum(row > 0, na.rm = TRUE)
    })
  } else {
    wide_data$number_of_othermusic <- 0L
  }

  # nodme: number of different musical experiences (sum of all counts)
  wide_data$nodme <- wide_data$number_of_instruments +
                     wide_data$number_of_singing +
                     wide_data$number_of_othermusic

  # attach standardized which*/type* data
  for (nm in names(all_which_data)) {
    tbl <- all_which_data[[nm]]
    wide_data <- dplyr::left_join(wide_data, tbl, by = "code")
  }

  # Calculate IMP (Index of Musical Practice) = weekly_hours \u00D7 years_practiced
  # IMP is calculated per category and overall
  # Formula: total_hours / weeks_per_year \u00D7 number_of_years
  weeks_per_year <- 365/7

  # For each category, calculate IMP based on total hours and years practiced
  for (category in c("instrument", "singing", "othermusic")) {
    # Get total hours for this category
    total_col <- paste0(category, "_total")
    if (total_col %in% names(wide_data)) {
      # Find starting age columns for this category
      starting_age_pattern <- paste0("^", category, "_starting_age\\d+$")
      starting_age_cols <- grep(starting_age_pattern, names(wide_data), value = TRUE)

      if (length(starting_age_cols) > 0) {
        # Find minimum starting age across all instruments/singing/other in this category
        wide_data[[paste0(category, "_min_starting_age")]] <- apply(
          wide_data[, starting_age_cols, drop = FALSE],
          1,
          function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
        )

        # Get hour columns for this category (to find latest age with practice)
        hour_col_pattern <- paste0("^", category, "\\d+$")
        hour_cols <- grep(hour_col_pattern, names(wide_data), value = TRUE)

        if (length(hour_cols) > 0) {
          # For each participant, we need to look at long_data to find max age with practice
          # For now, use a simple approximation: assume practice continues to current age
          # This will need the long data, so let's calculate it differently

          # Calculate years practiced = latest_age - starting_age + 1
          # We'll compute this from the long data for accuracy
        }
      }
    }
  }

  # Calculate IMP from long data (more accurate)
  if (length(all_long_data) > 0) {
    # Combine all long data
    all_long <- dplyr::bind_rows(all_long_data)

    # For each participant and category, calculate:
    # - years_practiced = max_age - min_age_with_practice + 1
    # - average weekly hours = total_hours / years_practiced / weeks_per_year
    # - IMP = average_weekly_hours * years_practiced

    imp_calcs <- all_long %>%
      dplyr::filter(!is.na(.data$yearly_hours), .data$yearly_hours > 0) %>%
      dplyr::group_by(.data$code, .data$category) %>%
      dplyr::summarise(
        min_age = min(.data$age_years, na.rm = TRUE),
        max_age = max(.data$age_years, na.rm = TRUE),
        total_hours = sum(.data$yearly_hours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        years_practiced = .data$max_age - .data$min_age + 1,
        average_weekly_hours = .data$total_hours / (.data$years_practiced * weeks_per_year),
        imp = .data$average_weekly_hours * .data$years_practiced
      ) %>%
      dplyr::select(.data$code, .data$category, .data$imp)

    # Pivot to wide format: IMP_instrument, IMP_singing, IMP_othermusic
    imp_wide <- imp_calcs %>%
      tidyr::pivot_wider(
        names_from = .data$category,
        values_from = .data$imp,
        names_prefix = "IMP_"
      )

    wide_data <- dplyr::left_join(wide_data, imp_wide, by = "code")

    # Calculate total IMP (sum across all categories)
    imp_cols <- grep("^IMP_(instrument|singing|othermusic)$", names(wide_data), value = TRUE)
    if (length(imp_cols) > 0) {
      wide_data$IMP_total <- rowSums(wide_data[, imp_cols, drop = FALSE], na.rm = TRUE)
      # If all IMPs were NA, set total to NA
      all_na_imp <- apply(is.na(wide_data[, imp_cols, drop = FALSE]), 1, all)
      wide_data$IMP_total[all_na_imp] <- NA_real_
    }
  }

  wide_data
}

# Get displayable category name
# @noRd
.get_category_display_name <- function(category) {
  switch(category,
    "instrument" = "Instrument",
    "singing" = "Singing",
    "othermusic" = "Other Music",
    category  # fallback to original
  )
}

# Normalize strings (lowercase, trim, remove NBSP, transliterate umlauts)
# @noRd
.norm_txt <- function(x) {
  x <- gsub("\u00A0", " ", x)  # NBSP \u2192 space
  x <- tolower(trimws(x))
  x2 <- tryCatch(stringi::stri_trans_general(x, "latin-ascii"),
                 error = function(...) x)
  x2 <- iconv(x2, from = "", to = "ASCII//TRANSLIT", sub = " ")
  x2 <- gsub("[^a-z0-9 ]+", " ", x2)
  x2 <- gsub("\\s+", " ", x2)
  trimws(x2)
}

# Fix frequent typos
# @noRd
.fix_typos <- function(x) {
  x <- gsub("\\bchello\\b", "cello", x)
  x <- gsub("\\bgitarrre\\b", "gitarre", x)
  x <- gsub("\\bsaxofon\\b", "saxophon", x)
  x
}

# Extended DE/EN instrument lexicon (regex patterns)
# @noRd
.instrument_lexicon <- function() {
  c(
    # guitars & strings
    "gitarre|guitar|klassische gitarre|classical guitar|electric guitar|e[- ]?gitarre|akustikgitarre|acoustic guitar",
    "bassgitarre|e[- ]?bass|bass guitar|kontrabass|double bass|upright bass|contrabass",
    "cello|violoncello",
    "violine|geige|violin|fiddle",
    "bratsche|viola",
    "harfe|harp|keltenharfe|celtic harp",
    "hackbrett|dulcimer",
    "mandoline|mandolin|banjo|ukulele|bouzouki|laute|lute",
    "tamburica|tamburizza",
    "melodica|melodika|melodion",

    # keyboards & pianos
    "klavier|piano|fluegel|grand piano|e[- ]?piano|keyboard|synth(esizer)?|synthesizer",

    # woodwinds
    "floete|flote|querfloete|querflote|flute|recorder|blockfloete|blockflote|ocarina|panflute|pan floete|pan flote",
    "klarinette|clarinet|bassklarinette|bass clarinet",
    "saxophon|saxophone|altsax|tenorsax|baritonsax|sopransax",
    "obo[e]?|english horn|englisches horn|fagott|bassoon",

    # brass
    "trompete|trumpet|cornet|flugelhorn|fluegelhorn",
    "posaune|trombone",
    "horn|waldhorn|french horn",
    "tuba|euphonium|bariton|baritone horn|sousaphon|sousaphone",

    # percussion
    "schlagzeug|drums?|drum set|drumkit|percussion",
    "conga|congas|bongo|bongos|cajon|djembe|darabuka|doumbek|tabla",
    "trommel|snare|bass drum|grosse trommel|glockenspiel|xylophon|marimba|vibraphon|vibraphone|timpani|pauke|cymbal|becken",

    # accordions & harmonicas
    "akkordeon|accordion|steirische harmonika|diatonic( button)? accordion|button accordion|zugharmonika",
    "mundharmonika|harmonica|mouth organ",

    # organs & others
    "orgel|organ|harmonium",
    "sitar|oud|shamisen|koto|pipa|erhu",
    "viola da gamba|gambe",
    "zither|zitter|cit(h)?er",
    "saz|baglama|ba[g\u011F]lama"
  )
}

# Check standardized whichinstrument* columns against lexicon; return unrecognized originals
# @noRd
.check_instruments <- function(which_tbl, which_cols) {
  if (!length(which_cols)) return(data.frame())
  vals <- c()
  for (col in which_cols) if (col %in% names(which_tbl)) vals <- c(vals, which_tbl[[col]])
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (!length(vals)) return(data.frame())

  norm <- .norm_txt(vals)
  norm <- .fix_typos(norm)

  lex <- .instrument_lexicon()
  recognized <- logical(length(norm))
  for (i in seq_along(norm)) {
    hits <- vapply(lex, function(p) grepl(p, norm[i], perl = TRUE), logical(1))
    recognized[i] <- any(hits)
  }

  unrec <- unique(vals[!recognized])
  if (!length(unrec)) return(data.frame())
  data.frame(instrument = unrec, stringsAsFactors = FALSE)
}

# Print human-readable summary
# @noRd
.print_summary <- function(file, long_final, wide_data, flags, all_played_data, instrument_warnings) {
  cat("\n=== MUSICAL EXPERIENCE PROCESSING SUMMARY ===\n")
  cat("Input file:", basename(file), "\n")
  cat("Total respondents processed:", nrow(wide_data), "\n")
  cat("Total time entries processed:", nrow(long_final), "\n")

  for (category in names(all_played_data)) {
    cat(sprintf("\n%s usage:\n", stringr::str_to_title(category)))
    played_data <- all_played_data[[category]]
    for (col in names(played_data)[-1]) {
      if (!grepl("\\d+$", col)) next
      cat_num <- stringr::str_extract(col, "\\d+$")
      tv <- table(played_data[[col]], useNA = "ifany")
      yes_count <- ifelse("1" %in% names(tv), tv["1"], 0)
      no_count  <- ifelse("2" %in% names(tv), tv["2"], 0)
      na_count  <- sum(is.na(played_data[[col]]))
      category_display <- .get_category_display_name(category)
      cat(sprintf("  %s %s: %d practiced, %d not practiced, %d missing\n",
                  category_display, cat_num, yes_count, no_count, na_count))
    }
  }

  if (nrow(long_final)) {
    valid_entries  <- sum(long_final$flag_code == "valid", na.rm = TRUE)
    total_entries  <- nrow(long_final)
    parsing_success <- round(valid_entries / total_entries * 100, 1)
    cat(sprintf("\nTime parsing: %d/%d entries successfully parsed (%.1f%%)\n",
                valid_entries, total_entries, parsing_success))
    if (nrow(flags)) {
      cat(sprintf("Problematic entries: %d (%.1f%%)\n", nrow(flags),
                  round(nrow(flags) / total_entries * 100, 1)))
      flag_summary <- table(flags$flag_code)
      for (i in seq_along(flag_summary)) {
        cat(sprintf("  - %s: %d entries\n", names(flag_summary)[i], flag_summary[i]))
      }
    } else {
      cat("No problematic entries found\n")
    }
  }

  time_cols <- grep("^(instrument|singing|othermusic)\\d+$", names(wide_data), value = TRUE)
  if (length(time_cols)) {
    cat(sprintf("\nWide format: %d respondents, %d time columns\n",
                nrow(wide_data), length(time_cols)))
    for (category in c("instrument", "singing", "othermusic")) {
      cat_cols <- grep(paste0("^", category, "\\d+$"), names(wide_data), value = TRUE)
      if (!length(cat_cols)) next
      category_display <- .get_category_display_name(category)
      cat(sprintf("\n%s columns:\n", category_display))
      for (col in cat_cols) {
        avg_hours <- round(mean(wide_data[[col]], na.rm = TRUE), 1)
        active_count <- sum(wide_data[[col]] > 0, na.rm = TRUE)
        cat(sprintf("  %s: %.1f avg hours, %d active practitioners\n",
                    col, avg_hours, active_count))
      }
    }
  }

  if (nrow(instrument_warnings)) {
    cat(sprintf("\nUnrecognized instruments: %d entries\n", nrow(instrument_warnings)))
    ui <- unique(instrument_warnings$instrument)
    cat("  ", paste(head(ui, 10), collapse = ", "))
    if (length(ui) > 10) cat(sprintf(" (and %d more)", length(ui) - 10))
    cat("\n")
  }

  cat("=== PROCESSING COMPLETE ===\n\n")
}
