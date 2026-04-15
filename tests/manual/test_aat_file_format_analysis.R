# ANALYSIS: What different AAT file formats exist in the test data?
# Compare file structures to understand why calculations differ.
library(readr)
library(dplyr)

test_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT"

# Find all AAT files
all_csv <- fs::dir_ls(test_dir, recurse = TRUE, regexp = "\\.csv$", type = "file")
aat_files <- all_csv[stringr::str_detect(basename(all_csv), "AAT")]

cat("=== FILE FORMAT ANALYSIS ===\n")
cat("Total AAT files:", length(aat_files), "\n\n")

# Categorize each file by its column structure
formats <- list()
for (f in aat_files) {
  bn <- basename(f)
  data <- tryCatch(
    read_csv(f, show_col_types = FALSE, n_max = 3),
    error = function(e) NULL
  )
  if (is.null(data)) {
    formats[[bn]] <- list(cols = "READ_ERROR", ncol = 0, nrow_sample = 0)
    next
  }

  col_signature <- paste(sort(names(data)), collapse = " | ")
  formats[[bn]] <- list(
    cols = names(data),
    col_signature = col_signature,
    ncol = ncol(data),
    nrow = nrow(data),
    file = f
  )
}

# Group by unique column signatures
signatures <- sapply(formats, function(x) x$col_signature)
unique_sigs <- unique(signatures)

cat("=== UNIQUE FILE FORMATS FOUND:", length(unique_sigs), "===\n\n")

for (i in seq_along(unique_sigs)) {
  sig <- unique_sigs[i]
  matching_files <- names(signatures)[signatures == sig]

  cat(sprintf("--- FORMAT %d (%d files) ---\n", i, length(matching_files)))

  # Show columns
  example <- formats[[matching_files[1]]]
  cat("Columns:", paste(example$cols, collapse = ", "), "\n")
  cat("N cols:", example$ncol, "\n")

  # Show a few example filenames
  cat("Example files:\n")
  for (fn in head(matching_files, 5)) {
    cat("  ", fn, "\n")
  }

  # For each format, show first 2 rows of data
  data <- tryCatch(
    read_csv(example$file, show_col_types = FALSE, n_max = 2),
    error = function(e) NULL
  )
  if (!is.null(data)) {
    cat("Sample data:\n")
    print(head(data, 2))
  }
  cat("\n")
}

# Now specifically compare MATCHING pairs: same participant, raw vs results
cat("\n=== MATCHED PAIRS: Same participant, different file types ===\n\n")

# Extract participant codes from filenames
code_pattern <- "\\d{4}[A-Za-z]{4}"
file_info <- tibble(
  file = aat_files,
  basename = basename(aat_files),
  code = stringr::str_extract(basename(aat_files), code_pattern),
  is_itl = stringr::str_detect(basename(aat_files), "\\.itl\\.csv$"),
  is_rsl = stringr::str_detect(basename(aat_files), "\\.rsl\\.csv$")
) %>% filter(!is.na(code))

# Find codes with both itl AND rsl
codes_both <- file_info %>%
  group_by(code) %>%
  summarise(has_itl = any(is_itl), has_rsl = any(is_rsl)) %>%
  filter(has_itl & has_rsl) %>%
  pull(code)

cat("Codes with BOTH .itl and .rsl files:", length(codes_both), "\n\n")

# For first 5 matched pairs, compare the actual content structure
for (code in head(codes_both, 5)) {
  itl_file <- file_info$file[file_info$code == code & file_info$is_itl][1]
  rsl_file <- file_info$file[file_info$code == code & file_info$is_rsl][1]

  cat(sprintf("--- %s ---\n", code))
  cat("  ITL:", basename(itl_file), "\n")
  cat("  RSL:", basename(rsl_file), "\n")

  itl_data <- tryCatch(read_csv(itl_file, show_col_types = FALSE), error = function(e) NULL)
  rsl_data <- tryCatch(read_csv(rsl_file, show_col_types = FALSE), error = function(e) NULL)

  if (!is.null(itl_data)) {
    cat("  ITL columns:", paste(names(itl_data), collapse = ", "), "\n")
    cat("  ITL rows:", nrow(itl_data), "\n")

    # Check if "Pitch Classification" exists (expected for real .itl files)
    has_pitch <- "Pitch Classification" %in% names(itl_data)
    has_response <- any(stringr::str_detect(names(itl_data), "^Response"))
    has_nmin <- "Nmin [-]" %in% names(itl_data)
    has_f0_pct <- "% F0" %in% names(itl_data)
    cat(sprintf("  ITL has: PitchClassification=%s, Response=%s, Nmin=%s, %%F0=%s\n",
                has_pitch, has_response, has_nmin, has_f0_pct))

    # Check for unexpected column patterns
    if (!has_pitch && !has_f0_pct) {
      cat("  !! ITL FILE MISSING EXPECTED COLUMNS — possible wrong format!\n")
      cat("  !! Actual columns:", paste(names(itl_data)[1:min(5, ncol(itl_data))], collapse = ", "), "\n")
    }
  }

  if (!is.null(rsl_data)) {
    cat("  RSL columns:", paste(names(rsl_data), collapse = ", "), "\n")
    cat("  RSL rows:", nrow(rsl_data), "\n")

    is_summary <- "Type of Pair" %in% names(rsl_data)
    is_itemlevel <- "% F0" %in% names(rsl_data)
    cat(sprintf("  RSL type: %s\n", if(is_summary) "SUMMARY" else if(is_itemlevel) "ITEM-LEVEL" else "UNKNOWN"))
  }
  cat("\n")
}

# Deep dive: for one mismatched pair, manually compute and compare step by step
cat("\n=== DEEP DIVE: Manual computation for a mismatched pair ===\n")

# Pick 0007CHKL which had summary=62.35/87.5 vs itl=67.7/90
target_code <- "0007CHKL"
target_files <- file_info[file_info$code == target_code, ]

if (nrow(target_files) > 0) {
  cat("Files for", target_code, ":\n")
  for (j in seq_len(nrow(target_files))) {
    cat(sprintf("  %s (itl=%s, rsl=%s)\n",
                basename(target_files$file[j]),
                target_files$is_itl[j], target_files$is_rsl[j]))
  }

  # Read the ITL file
  itl_f <- target_files$file[target_files$is_itl][1]
  if (!is.na(itl_f)) {
    itl <- read_csv(itl_f, show_col_types = FALSE)
    cat("\nITL data structure:\n")
    cat("  Columns:", paste(names(itl), collapse = ", "), "\n")
    cat("  Rows:", nrow(itl), "\n")

    if ("Pitch Classification" %in% names(itl)) {
      pitch_vals <- table(itl[["Pitch Classification"]])
      cat("  Pitch Classification distribution:\n")
      print(pitch_vals)

      if ("Nmin [-]" %in% names(itl)) {
        # Count control vs ambiguous items
        nmin <- itl[["Nmin [-]"]]
        nmin_parts <- stringr::str_split(nmin, "\\s+")
        is_control <- sapply(nmin_parts, function(x) length(x) >= 2 && x[1] == x[2])
        cat(sprintf("  Control items: %d, Ambiguous items: %d\n", sum(is_control), sum(!is_control)))
      }

      if (any(stringr::str_detect(names(itl), "^Response"))) {
        resp_col <- names(itl)[stringr::str_detect(names(itl), "^Response")][1]
        resp_vals <- table(itl[[resp_col]])
        cat("  Response distribution:\n")
        print(resp_vals)
        cat("  Skipped trials (Response=-1):", sum(itl[[resp_col]] == -1, na.rm = TRUE), "\n")
      }
    } else {
      cat("  NO 'Pitch Classification' column — this is NOT a standard .itl format!\n")
      cat("  First 3 columns:", paste(names(itl)[1:min(3, ncol(itl))], collapse = ", "), "\n")
      cat("  First 2 rows:\n")
      print(head(itl, 2))
    }
  }

  # Read the RSL summary file
  rsl_f <- target_files$file[target_files$is_rsl][1]
  if (!is.na(rsl_f)) {
    rsl <- read_csv(rsl_f, show_col_types = FALSE)
    cat("\nRSL data:\n")
    print(rsl)
  }
} else {
  # Fallback to another mismatched code
  cat("Code", target_code, "not found in matched pairs. Trying 0210DOHE...\n")
}

cat("\nDone.\n")
