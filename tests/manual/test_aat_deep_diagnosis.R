# DEEP DIAGNOSIS: Why does aat_scan produce duplicates and NA rows?
# This script traces exactly what happens for specific files.
library(musicAnalysis)
library(dplyr)

test_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT"

# Step 1: What files does aat_scan find?
all_csv <- fs::dir_ls(test_dir, recurse = TRUE, regexp = "\\.csv$", type = "file")
aat_files <- all_csv[stringr::str_detect(basename(all_csv), "AAT")]
cat("=== FILES FOUND ===\n")
cat("Total CSV files:", length(all_csv), "\n")
cat("AAT files (contain 'AAT' in name):", length(aat_files), "\n\n")

# Step 2: What file types are these?
for (f in aat_files) {
  bn <- basename(f)
  is_itl <- stringr::str_detect(bn, "\\.itl\\.csv$")
  is_rsl <- stringr::str_detect(bn, "\\.rsl\\.csv$")

  # Read first line to detect format
  data <- tryCatch(
    readr::read_csv(f, show_col_types = FALSE, n_max = 2),
    error = function(e) NULL
  )

  cols <- if (!is.null(data)) paste(names(data)[1:min(3, ncol(data))], collapse = ", ") else "ERROR"

  type_label <- if (is_itl) "ITL" else if (is_rsl) "RSL" else "UNKNOWN"

  # Detect subtype
  subtype <- if (!is.null(data)) {
    if ("Type of Pair" %in% names(data)) "rsl_summary"
    else if ("% F0" %in% names(data)) "rsl_itemlevel"
    else if ("Pitch Classification" %in% names(data)) "itl_raw"
    else "unknown_format"
  } else "read_error"

  cat(sprintf("  %-12s %-16s %s\n", type_label, subtype, bn))
}

# Step 3: Run scan with default settings (only rsl_summary)
cat("\n=== SCAN WITH DEFAULT file_types=c('rsl_summary') ===\n")
result_default <- aat_scan(test_dir, file_types = c("rsl_summary"))
cat("Rows returned:", nrow(result_default), "\n")
cat("NA codes:", sum(is.na(result_default$code)), "\n")
cat("Unique non-NA codes:", length(unique(na.omit(result_default$code))), "\n")

# Check for duplicates
dup_codes <- result_default %>%
  filter(!is.na(code)) %>%
  count(code, sort = TRUE) %>%
  filter(n > 1)
if (nrow(dup_codes) > 0) {
  cat("\nDUPLICATE CODES:\n")
  print(dup_codes)

  # Show the duplicated rows
  for (dc in dup_codes$code[1:min(3, nrow(dup_codes))]) {
    cat(sprintf("\n  Details for '%s':\n", dc))
    rows <- result_default %>% filter(code == dc)
    for (i in seq_len(nrow(rows))) {
      cat(sprintf("    [%d] file=%s, ambiguous_pct=%s, control_pct=%s, file_subtype=%s\n",
                  i, rows$file[i], rows$ambiguous_pct[i], rows$control_pct[i], rows$file_subtype[i]))
    }
  }
} else {
  cat("No duplicate codes found.\n")
}

# Check NA rows
na_rows <- result_default %>% filter(is.na(code))
if (nrow(na_rows) > 0) {
  cat("\nNA ROWS (", nrow(na_rows), " total):\n")
  for (i in seq_len(min(5, nrow(na_rows)))) {
    cat(sprintf("  [%d] file=%s, file_type=%s, file_subtype=%s\n",
                i, na_rows$file[i], na_rows$file_type[i], na_rows$file_subtype[i]))
  }
}

# Step 4: Now scan with ALL file types to see what changes
cat("\n=== SCAN WITH ALL file_types ===\n")
result_all <- aat_scan(test_dir, file_types = c("rsl_summary", "rsl_itemlevel", "itl"))
cat("Rows returned:", nrow(result_all), "\n")
cat("NA codes:", sum(is.na(result_all$code)), "\n")
cat("Unique non-NA codes:", length(unique(na.omit(result_all$code))), "\n")

# Step 5: Trace a specific problematic file
cat("\n=== TRACE: What happens with 0206ANDA files? ===\n")
anda_files <- aat_files[stringr::str_detect(basename(aat_files), "0206ANDA")]
cat("Files matching '0206ANDA':\n")
for (f in anda_files) {
  cat("  ", basename(f), "\n")
}

# Step 6: Check if PPPT files accidentally match
pppt_aat_overlap <- aat_files[stringr::str_detect(basename(aat_files), "PPPT")]
cat("\n=== FILES matching BOTH 'AAT' and 'PPPT': ===\n")
cat("Count:", length(pppt_aat_overlap), "\n")
if (length(pppt_aat_overlap) > 0) {
  for (f in pppt_aat_overlap[1:min(5, length(pppt_aat_overlap))]) {
    cat("  ", basename(f), "\n")
  }
}

# Step 7: Value verification for summary format files
cat("\n=== VALUE VERIFICATION (Summary .rsl files) ===\n")
summary_rows <- result_default %>%
  filter(!is.na(code), file_subtype == "rsl_summary") %>%
  distinct(code, .keep_all = TRUE) %>%
  slice(1:5)

for (i in seq_len(nrow(summary_rows))) {
  r <- summary_rows[i, ]
  raw_file <- file.path(test_dir, r$file)
  if (file.exists(raw_file)) {
    raw <- readr::read_csv(raw_file, show_col_types = FALSE)
    raw_ambig <- as.numeric(raw[raw[["Type of Pair"]] == "Ambiguous", "AAT Score [%]"])
    raw_control <- as.numeric(raw[raw[["Type of Pair"]] == "Control", "AAT Score [%]"])
    match_a <- isTRUE(abs(r$ambiguous_pct - raw_ambig) < 0.01)
    match_c <- isTRUE(abs(r$control_pct - raw_control) < 0.01)
    cat(sprintf("  %s: extracted=%s/%s raw=%s/%s %s\n",
                r$code, r$ambiguous_pct, r$control_pct, raw_ambig, raw_control,
                if (match_a && match_c) "MATCH" else "MISMATCH!"))
  }
}

cat("\nDone.\n")
