# VALUE VERIFICATION: Compare aat_scan() output with manual file reading
library(musicAnalysis)

test_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT"

# Scan all files
result <- aat_scan(test_dir, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")

cat("=== SCAN RESULT OVERVIEW ===\n")
cat("Total rows:", nrow(result), "\n")
cat("Columns:", paste(names(result), collapse = ", "), "\n\n")

# === MANUAL VERIFICATION FILE 1: Summary format ===
# File: 0206ANDA_170325_AAT_Default_70dBSPL_FM2_~Calib.rsl.csv
# Raw content: Ambiguous=50 tone pairs, AAT Score=79%; Control=5 tone pairs, AAT Score=100%
# Expected: code=0206ANDA, ambiguous_pct=79, control_pct=100

cat("=== VERIFICATION 1: 0206ANDA (Summary .rsl format) ===\n")
row_anda <- result[result$code == "0206ANDA", ]
if (nrow(row_anda) == 0) {
  cat("NOT FOUND in results!\n")
} else {
  cat("Rows found:", nrow(row_anda), "\n")
  for (i in seq_len(nrow(row_anda))) {
    r <- row_anda[i, ]
    cat(sprintf("  [%d] code=%s, date=%s, ambiguous_pct=%s, control_pct=%s, file=%s\n",
                i, r$code, r$date, r$ambiguous_pct, r$control_pct, basename(r$file)))

    # Check expected values
    if (!is.na(r$ambiguous_pct) && r$ambiguous_pct == 79) {
      cat("  -> ambiguous_pct CORRECT (expected 79)\n")
    } else {
      cat("  -> ambiguous_pct WRONG! Expected 79, got", r$ambiguous_pct, "\n")
    }
    if (!is.na(r$control_pct) && r$control_pct == 100) {
      cat("  -> control_pct CORRECT (expected 100)\n")
    } else {
      cat("  -> control_pct WRONG! Expected 100, got", r$control_pct, "\n")
    }
  }
}

# === MANUAL VERIFICATION FILE 2: Item-level format ===
# File: 0403SIMA_170325_AAT_Default_70dBSPL_FM2_~Calib.rsl.csv
# This is an item-level .rsl file with columns: Index, Reference F0, ..., % F0, # Ambi., % Ambi., # Skipped
# 55 rows of data. Nmin column has values like "5 2", "7 3", "9 4" etc.
# "Same Nmin" (e.g., "5 5", "7 7", "4 4") = control items
# "Different Nmin" = ambiguous items
#
# Manual count from the raw data:
# Control items (same Nmin): rows 42 ("5 5"), 49 ("7 7"), 54 ("4 4") = 3 items
# Ambiguous items: all other rows = 52 items
# For ambiguous items, % F0 column shows f0 response rate
# For control items, need to check correct answer

cat("\n=== VERIFICATION 2: 0403SIMA (Item-level .rsl format) ===\n")
row_sima <- result[result$code == "0403SIMA", ]
if (nrow(row_sima) == 0) {
  cat("NOT FOUND in results!\n")
} else {
  cat("Rows found:", nrow(row_sima), "\n")
  for (i in seq_len(nrow(row_sima))) {
    r <- row_sima[i, ]
    cat(sprintf("  [%d] code=%s, date=%s, ambiguous_pct=%s, control_pct=%s\n",
                i, r$code, r$date, r$ambiguous_pct, r$control_pct))
    cat(sprintf("       n_ambivalent=%s, n_dont_know=%s, n_evaluable=%s, n_total=%s\n",
                r$n_ambivalent, r$n_dont_know, r$n_evaluable, r$n_total))
    cat(sprintf("       file_type=%s, file=%s\n", r$file_type, basename(r$file)))
  }
}

# === VERIFICATION 3: Check a third file manually ===
cat("\n=== VERIFICATION 3: 0202DAIV (different folder structure) ===\n")
row_daiv <- result[result$code == "0202DAIV", ]
if (nrow(row_daiv) == 0) {
  cat("NOT FOUND in results!\n")
} else {
  cat("Rows found:", nrow(row_daiv), "\n")
  for (i in seq_len(nrow(row_daiv))) {
    r <- row_daiv[i, ]
    cat(sprintf("  [%d] code=%s, date=%s, ambiguous_pct=%s, control_pct=%s, file=%s\n",
                i, r$code, r$date, r$ambiguous_pct, r$control_pct, basename(r$file)))
  }
}

# Read the raw file to verify
raw <- readr::read_csv(
  "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT/25.03.2025/0202DAIV_results_250325_AAT_Default_70dBSPL_FM2_~Calib.rsl.csv",
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)
cat("\nRaw file columns:", paste(names(raw), collapse = ", "), "\n")
cat("Raw file rows:", nrow(raw), "\n")
if ("AAT Score [%]" %in% names(raw)) {
  cat("AAT Score values:", paste(raw[["AAT Score [%]"]], collapse = ", "), "\n")
  cat("Type of Pair values:", paste(raw[["Type of Pair"]], collapse = ", "), "\n")
}

cat("\n=== SUMMARY ===\n")
cat("Total unique codes:", length(unique(result$code)), "\n")
cat("Rows with NA ambiguous_pct:", sum(is.na(result$ambiguous_pct)), "\n")
cat("Rows with NA control_pct:", sum(is.na(result$control_pct)), "\n")
cat("Range ambiguous_pct:", range(result$ambiguous_pct, na.rm = TRUE), "\n")
cat("Range control_pct:", range(result$control_pct, na.rm = TRUE), "\n")
