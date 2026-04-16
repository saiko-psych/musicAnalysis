# DEEP TRACE: What exactly fails in Jana's file parsing?
library(readr)
library(stringr)

jana_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/data/LimeSurvey/musikalische vorerfahrung/test_jana.csv"
jana <- read_csv(jana_file, show_col_types = FALSE)

# Check played columns
cat("=== PLAYED COLUMNS ===\n")
played_cols <- grep("^instrumentplayed\\d+$", names(jana), value = TRUE)
for (col in played_cols) {
  cat(sprintf("  %s = '%s' (class: %s)\n", col, jana[[col]][1], class(jana[[col]])[1]))
}

singing_played <- grep("^singing\\d+$", names(jana), value = TRUE)
cat("\nSinging played cols:", paste(singing_played, collapse = ", "), "\n")
for (col in singing_played[1:min(3, length(singing_played))]) {
  cat(sprintf("  %s = '%s'\n", col, jana[[col]][1]))
}

othermusic_played <- grep("^othermusic\\d+$", names(jana), value = TRUE)
cat("\nOthermusic played cols:", paste(othermusic_played, collapse = ", "), "\n")
for (col in othermusic_played[1:min(3, length(othermusic_played))]) {
  cat(sprintf("  %s = '%s'\n", col, jana[[col]][1]))
}

# Check time data columns — what are the actual values?
cat("\n=== TIME DATA SAMPLE ===\n")
time_cols <- grep("^instrument1[0-6]\\[", names(jana), value = TRUE)
cat("Instrument 1 time cols:", length(time_cols), "\n")
non_empty <- time_cols[!is.na(jana[1, time_cols]) & jana[1, time_cols] != ""]
cat("Non-empty:", length(non_empty), "\n")
if (length(non_empty) > 0) {
  for (col in non_empty[1:min(10, length(non_empty))]) {
    cat(sprintf("  %s = '%s'\n", col, jana[[col]][1]))
  }
}

# Compare with our working file
cat("\n=== COMPARISON WITH WORKING FILE ===\n")
ours <- read_csv("C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv",
                  show_col_types = FALSE)

# Check one participant who plays instrument
our_player <- ours[ours$instrumentplayed1 %in% c("Ja", "Y", "1"), ][1, ]
if (nrow(our_player) > 0) {
  cat("Working file instrumentplayed1:", our_player$instrumentplayed1, "\n")
  our_time_cols <- grep("^instrument1[0-6]\\[", names(ours), value = TRUE)
  our_non_empty <- our_time_cols[!is.na(our_player[our_time_cols]) & our_player[our_time_cols] != ""]
  cat("Working file non-empty time cols:", length(our_non_empty), "\n")
  if (length(our_non_empty) > 0) {
    for (col in our_non_empty[1:min(5, length(our_non_empty))]) {
      cat(sprintf("  %s = '%s'\n", col, our_player[[col]]))
    }
  }
}

# The KEY difference: Check the time column naming pattern
cat("\n=== TIME COLUMN PATTERN CHECK ===\n")
# Our parser expects: instrument12[1_3] (2-digit number)
# Check if Jana has different numbering
jana_time_all <- grep("^instrument\\d", names(jana), value = TRUE)
jana_time_bracket <- grep("\\[", jana_time_all, value = TRUE)
cat("Jana bracket time cols sample:\n")
cat("  ", paste(head(jana_time_bracket, 10), collapse = "\n  "), "\n")

our_time_all <- grep("^instrument\\d", names(ours), value = TRUE)
our_time_bracket <- grep("\\[", our_time_all, value = TRUE)
cat("\nOurs bracket time cols sample:\n")
cat("  ", paste(head(our_time_bracket, 10), collapse = "\n  "), "\n")

# Check the regex pattern the parser uses
time_pattern <- "^instrument(\\d{2})\\[(\\d+)_(\\d+)\\]$"
jana_matches <- grep(time_pattern, names(jana), value = TRUE)
our_matches <- grep(time_pattern, names(ours), value = TRUE)
cat(sprintf("\nPattern '%s':\n", time_pattern))
cat("  Jana matches:", length(jana_matches), "\n")
cat("  Ours matches:", length(our_matches), "\n")

# What about a broader pattern?
broad_pattern <- "^instrument\\d+\\[\\d+_\\d+\\]$"
jana_broad <- grep(broad_pattern, names(jana), value = TRUE)
cat("\nBroad pattern matches - Jana:", length(jana_broad), "\n")
cat("  Sample:", paste(head(jana_broad, 5), collapse = ", "), "\n")

# Check if the issue is 1-digit vs 2-digit instrument numbers
cat("\n=== INSTRUMENT NUMBERING ===\n")
# Extract the instrument numbers
jana_nums <- unique(str_extract(jana_time_bracket, "(?<=^instrument)\\d+(?=\\[)"))
our_nums <- unique(str_extract(our_time_bracket, "(?<=^instrument)\\d+(?=\\[)"))
cat("Jana instrument numbers:", paste(sort(jana_nums), collapse = ", "), "\n")
cat("Ours instrument numbers:", paste(sort(our_nums), collapse = ", "), "\n")
