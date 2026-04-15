# CRITICAL VALIDATION: Do our calculations from raw data match the results files?
# For participants where BOTH a raw (.itl) AND a results (.rsl summary) file exist,
# compare: our computed values from .itl vs. the values in .rsl

library(musicAnalysis)

test_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT"

cat("====================================================\n")
cat("  AAT: Raw (.itl) vs Results (.rsl) Validation\n")
cat("====================================================\n\n")

# Step 1: Scan with ALL file types to get both raw and results
result_all <- aat_scan(test_dir, file_types = c("rsl_summary", "rsl_itemlevel", "itl"))
cat("Total scan rows:", nrow(result_all), "\n")
cat("File types:", paste(unique(result_all$file_type), collapse = ", "), "\n")
cat("File subtypes:", paste(unique(result_all$file_subtype), collapse = ", "), "\n\n")

# Step 2: Find participants with BOTH rsl_summary AND itl results
rsl_summary <- result_all[result_all$file_subtype == "rsl_summary" & !is.na(result_all$code), ]
itl_results <- result_all[result_all$file_type == "itl" & !is.na(result_all$code), ]
rsl_itemlevel <- result_all[result_all$file_subtype == "rsl_itemlevel" & !is.na(result_all$code), ]

cat("rsl_summary rows:", nrow(rsl_summary), " (", length(unique(rsl_summary$code)), "unique codes)\n")
cat("rsl_itemlevel rows:", nrow(rsl_itemlevel), " (", length(unique(rsl_itemlevel$code)), "unique codes)\n")
cat("itl rows:", nrow(itl_results), " (", length(unique(itl_results$code)), "unique codes)\n\n")

# Step 3: Check which codes have both summary AND itl
codes_with_both_summary_itl <- intersect(rsl_summary$code, itl_results$code)
cat("Codes with BOTH rsl_summary AND itl:", length(codes_with_both_summary_itl), "\n")

# Step 4: Check which codes have both summary AND itemlevel
codes_with_both_summary_itemlevel <- intersect(rsl_summary$code, rsl_itemlevel$code)
cat("Codes with BOTH rsl_summary AND rsl_itemlevel:", length(codes_with_both_summary_itemlevel), "\n\n")

# Step 5: Compare rsl_summary vs rsl_itemlevel (both from .rsl files, different formats)
if (length(codes_with_both_summary_itemlevel) > 0) {
  cat("--- RSL Summary vs RSL Itemlevel ---\n")
  for (code in codes_with_both_summary_itemlevel[1:min(10, length(codes_with_both_summary_itemlevel))]) {
    s <- rsl_summary[rsl_summary$code == code, ][1, ]
    il <- rsl_itemlevel[rsl_itemlevel$code == code, ][1, ]

    match_a <- !is.na(s$ambiguous_pct) && !is.na(il$ambiguous_pct) &&
               abs(s$ambiguous_pct - il$ambiguous_pct) < 0.1
    match_c <- !is.na(s$control_pct) && !is.na(il$control_pct) &&
               abs(s$control_pct - il$control_pct) < 0.1

    status <- if (match_a && match_c) "MATCH" else "MISMATCH!"
    cat(sprintf("  %s: summary=%s/%s  itemlevel=%s/%s  %s\n",
                code,
                round(s$ambiguous_pct, 2), round(s$control_pct, 2),
                round(il$ambiguous_pct, 2), round(il$control_pct, 2),
                status))
  }
}

# Step 6: Compare itl computed vs rsl_summary (our calculation vs. software's calculation)
if (length(codes_with_both_summary_itl) > 0) {
  cat("\n--- ITL Computed vs RSL Summary ---\n")
  cat("(This tests: does our .itl parser compute the same values as the AAT software?)\n\n")

  mismatches <- 0
  for (code in codes_with_both_summary_itl[1:min(10, length(codes_with_both_summary_itl))]) {
    s <- rsl_summary[rsl_summary$code == code, ][1, ]
    i <- itl_results[itl_results$code == code, ][1, ]

    match_a <- !is.na(s$ambiguous_pct) && !is.na(i$ambiguous_pct) &&
               abs(s$ambiguous_pct - i$ambiguous_pct) < 0.5
    match_c <- !is.na(s$control_pct) && !is.na(i$control_pct) &&
               abs(s$control_pct - i$control_pct) < 0.5

    if (!match_a || !match_c) mismatches <- mismatches + 1

    status <- if (match_a && match_c) "MATCH"
              else if (is.na(i$ambiguous_pct) && is.na(i$control_pct)) "ITL=NA (parse failed?)"
              else "MISMATCH!"

    cat(sprintf("  %s: summary=%s/%s  itl_computed=%s/%s  %s\n",
                code,
                round(s$ambiguous_pct, 2), round(s$control_pct, 2),
                if(is.na(i$ambiguous_pct)) "NA" else round(i$ambiguous_pct, 2),
                if(is.na(i$control_pct)) "NA" else round(i$control_pct, 2),
                status))
  }
  cat(sprintf("\nMismatches: %d / %d\n", mismatches, min(10, length(codes_with_both_summary_itl))))
}

cat("\n====================================================\n")
cat("  PPPT: Raw (.itl) vs Results (.rsl) Validation\n")
cat("====================================================\n\n")

# PPPT: Check if there are matching raw/results pairs
pppt_result <- pppt_scan(test_dir, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")
cat("PPPT scan rows:", nrow(pppt_result), "\n")
cat("PPPT columns:", paste(names(pppt_result), collapse = ", "), "\n\n")

# Find PPPT files
pppt_rsl <- fs::dir_ls(test_dir, recurse = TRUE, regexp = "PPPT.*\\.rsl\\.csv$", type = "file")
pppt_itl <- fs::dir_ls(test_dir, recurse = TRUE, regexp = "PPPT.*\\.itl\\.csv$", type = "file")
cat("PPPT .rsl files:", length(pppt_rsl), "\n")
cat("PPPT .itl files:", length(pppt_itl), "\n\n")

# For PPPT, check if extracted PPP indices match the raw file content
if (nrow(pppt_result) > 0) {
  cat("--- PPPT Value Spot-Check ---\n")
  # Pick first 3 results and verify against raw file
  for (i in 1:min(3, nrow(pppt_result))) {
    r <- pppt_result[i, ]
    raw_file <- file.path(test_dir, r$file)
    if (file.exists(raw_file)) {
      raw <- readr::read_csv(raw_file, show_col_types = FALSE)
      cat(sprintf("\n  %s (file: %s):\n", r$code, basename(r$file)))
      cat(sprintf("    Raw columns: %s\n", paste(names(raw), collapse = ", ")))
      cat(sprintf("    Raw rows: %d\n", nrow(raw)))

      # Check PPP Index column
      if ("PPP Index" %in% names(raw)) {
        raw_ppp <- as.numeric(raw[["PPP Index"]])
        cat(sprintf("    Raw PPP Index values: %s\n", paste(round(raw_ppp, 2), collapse = ", ")))
      }
      if ("UCF" %in% names(raw)) {
        raw_ucf <- as.numeric(raw[["UCF"]])
        cat(sprintf("    Raw UCF values: %s\n", paste(raw_ucf, collapse = ", ")))
      }

      # Compare with extracted values
      ppp_cols <- grep("^ppp_", names(r), value = TRUE)
      cat(sprintf("    Extracted PPP values: %s\n",
                  paste(sprintf("%s=%s", ppp_cols, round(as.numeric(r[ppp_cols]), 2)), collapse = ", ")))
    }
  }
}

cat("\n\nDone.\n")
