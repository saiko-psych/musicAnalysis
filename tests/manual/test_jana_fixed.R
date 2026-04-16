# Verify: Jana's file now parses, AND our existing data still works
library(musicAnalysis)

jana_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/data/LimeSurvey/musikalische vorerfahrung/test_jana.csv"
our_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv"

cat("=== TEST 1: Jana's file ===\n")
jana_result <- tryCatch(
  musical_experience(jana_file, verbose = TRUE),
  error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
)

if (!is.null(jana_result)) {
  cat("\nSUCCESS!\n")
  cat("Wide:", nrow(jana_result$wide), "rows,", ncol(jana_result$wide), "cols\n")
  cat("Long:", nrow(jana_result$sections$time$long), "rows\n")
  cat("Profile:", nrow(jana_result$sections$profile), "rows,",
      ncol(jana_result$sections$profile), "cols\n")

  # Check values
  w <- jana_result$wide
  cat("\nCode:", w$code[1], "\n")
  if ("instrument_total" %in% names(w)) {
    cat("Instrument total:", w$instrument_total[1], "\n")
  }
  if ("othermusic_total" %in% names(w)) {
    cat("Othermusic total:", w$othermusic_total[1], "\n")
  }

  # Manual verification: Jana plays Geige with 5w for several years
  # and dances (othermusic) with 2w and also has othermusic3 with various values
  cat("\nManual check (from CSV):\n")
  cat("  Geige practice: 5w repeated = ~260h/yr for multiple years\n")
  cat("  Tanzen: 2w = ~104h/yr for some years\n")
} else {
  cat("FAILED!\n")
}

cat("\n=== TEST 2: Our existing data still works ===\n")
our_result <- tryCatch(
  musical_experience(our_file, verbose = FALSE),
  error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
)

if (!is.null(our_result)) {
  cat("SUCCESS! Wide:", nrow(our_result$wide), "rows,", ncol(our_result$wide), "cols\n")
  cat("Previous count was 78 rows - ", if(nrow(our_result$wide) == 78) "MATCH" else "CHANGED!", "\n")
} else {
  cat("REGRESSION! Our existing data broke!\n")
}
