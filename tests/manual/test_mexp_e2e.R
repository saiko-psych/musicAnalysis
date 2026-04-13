# End-to-end test: Musical Experience with real data
library(musicAnalysis)

test_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv"
cat("Testing with:", test_file, "\n\n")

# Parse the data
res <- tryCatch(
  musical_experience_time(test_file),
  error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
)

if (is.null(res)) {
  cat("Failed to parse. Trying second file...\n")
  test_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/musikalische_vorerfahrung_05062025.csv"
  res <- musical_experience_time(test_file)
}

wide <- res$wide
long_data <- res$long

cat("=== COLUMN ORDERING CHECK ===\n")
cat("Total columns:", ncol(wide), "\n")
cat("Column names in order:\n")
for (i in seq_along(names(wide))) {
  lbl <- attr(wide[[names(wide)[i]]], "label")
  lbl_str <- if (!is.null(lbl)) paste0("  [", lbl, "]") else ""
  cat(sprintf("  %2d. %-35s%s\n", i, names(wide)[i], lbl_str))
}

cat("\n=== LABEL CHECK ===\n")
labeled_cols <- sapply(names(wide), function(n) !is.null(attr(wide[[n]], "label")))
cat("Columns with labels:", sum(labeled_cols), "/", ncol(wide), "\n")
cat("Unlabeled columns:", paste(names(wide)[!labeled_cols], collapse = ", "), "\n")

cat("\n=== LONG DATA CHECK ===\n")
cat("Rows:", nrow(long_data), "\n")
cat("Participants:", length(unique(long_data$code)), "\n")
cat("Categories:", paste(unique(long_data$category), collapse = ", "), "\n")

cat("\n=== PLOT GROUPING TEST ===\n")
# Test plot with a grouping variable
if ("number_of_instruments" %in% names(wide)) {
  cat("Testing plot_practice_curves with total type...\n")
  p <- tryCatch(
    plot_practice_curves(long_data, wide_data = wide, plot_type = "total"),
    error = function(e) { cat("Plot ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(p)) cat("Single plot: OK (", class(p)[1], ")\n")

  cat("Testing category_sum plot...\n")
  p2 <- tryCatch(
    plot_practice_curves(long_data, wide_data = wide, plot_type = "category_sum"),
    error = function(e) { cat("Plot ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(p2)) cat("Category sum plot: OK (", class(p2)[1], ")\n")
}

cat("\nAll end-to-end tests completed!\n")
