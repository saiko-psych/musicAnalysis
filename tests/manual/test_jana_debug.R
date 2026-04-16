# Debug: Why does Jana's CSV fail to parse?
library(musicAnalysis)

jana_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/data/LimeSurvey/musikalische vorerfahrung/test_jana.csv"
our_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv"

cat("=== FILE COMPARISON ===\n")
jana <- readr::read_csv(jana_file, show_col_types = FALSE)
ours <- readr::read_csv(our_file, show_col_types = FALSE)

cat("Jana: ", nrow(jana), "rows,", ncol(jana), "cols\n")
cat("Ours: ", nrow(ours), "rows,", ncol(ours), "cols\n\n")

# Compare column names
jana_cols <- names(jana)
our_cols <- names(ours)

# Columns in Jana but NOT in ours
only_jana <- setdiff(jana_cols, our_cols)
cat("Columns ONLY in Jana (", length(only_jana), "):\n")
if (length(only_jana) > 0) cat("  ", paste(head(only_jana, 20), collapse = ", "), "\n")

# Columns in ours but NOT in Jana
only_ours <- setdiff(our_cols, jana_cols)
cat("\nColumns ONLY in ours (", length(only_ours), "):\n")
if (length(only_ours) > 0) cat("  ", paste(head(only_ours, 20), collapse = ", "), "\n")

# Check key columns the parser needs
cat("\n=== KEY COLUMN CHECKS ===\n")
cat("Has 'lastpage':", "lastpage" %in% jana_cols, "\n")
cat("Has 'code':", "code" %in% jana_cols, "\n")
cat("Has 'age':", "age" %in% jana_cols, "\n")

# Check played columns pattern
played_pattern <- "^instrumentplayed\\d+$"
jana_played <- grep(played_pattern, jana_cols, value = TRUE)
our_played <- grep(played_pattern, our_cols, value = TRUE)
cat("\nInstrument played cols - Jana:", paste(jana_played, collapse=", "), "\n")
cat("Instrument played cols - Ours:", paste(our_played, collapse=", "), "\n")

# Check time columns pattern
time_pattern <- "^instrument\\d{2}\\[\\d+_\\d+\\]$"
jana_time <- grep(time_pattern, jana_cols, value = TRUE)
our_time <- grep(time_pattern, our_cols, value = TRUE)
cat("\nInstrument time cols - Jana:", length(jana_time), " Ours:", length(our_time), "\n")
if (length(jana_time) > 0) cat("  Jana sample:", paste(head(jana_time, 5), collapse=", "), "\n")
if (length(our_time) > 0) cat("  Ours sample:", paste(head(our_time, 5), collapse=", "), "\n")

# Check lastpage values
cat("\n=== LASTPAGE VALUES ===\n")
cat("Jana lastpage values:", paste(sort(unique(jana$lastpage)), collapse=", "), "\n")
cat("Ours lastpage values:", paste(sort(unique(head(ours$lastpage, 100))), collapse=", "), "\n")

# Now try parsing
cat("\n=== ATTEMPTING PARSE ===\n")
result <- tryCatch(
  musical_experience(jana_file, verbose = TRUE),
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    cat("Call:", deparse(conditionCall(e)), "\n")
    NULL
  },
  warning = function(w) {
    cat("WARNING:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  }
)

if (!is.null(result)) {
  cat("\nParse succeeded!\n")
  cat("Wide:", nrow(result$wide), "rows,", ncol(result$wide), "cols\n")
} else {
  cat("\nParse failed. Trying musical_experience_time() directly...\n")
  result2 <- tryCatch(
    musical_experience_time(jana_file, verbose = TRUE),
    error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      NULL
    }
  )
}
