# Verify Jana's parsed values are correct
library(musicAnalysis)

jana_file <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/data/LimeSurvey/musikalische vorerfahrung/test_jana.csv"
result <- musical_experience(jana_file, verbose = FALSE)
w <- result$wide

# Key numeric columns
key_cols <- c("instrument_total", "singing_total", "othermusic_total",
              "total_musical_experience", "number_of_instruments",
              "number_of_singing", "number_of_othermusic", "nodme",
              "IMP_instrument", "IMP_othermusic", "IMP_total")

cat("=== JANA'S VALUES ===\n")
for (col in key_cols) {
  if (col %in% names(w)) {
    cat(sprintf("  %-30s = %s\n", col, w[[col]][1]))
  } else {
    cat(sprintf("  %-30s = MISSING\n", col))
  }
}

# Check the long data to see what was actually parsed
long <- result$sections$time$long
cat("\n=== LONG DATA BREAKDOWN ===\n")
cat("Total rows:", nrow(long), "\n")
cat("Categories:", paste(unique(long$category), collapse = ", "), "\n")
for (cat_name in unique(long$category)) {
  cat_data <- long[long$category == cat_name, ]
  cat(sprintf("\n  %s: %d rows, ages %d-%d\n", cat_name, nrow(cat_data),
              min(cat_data$age_years, na.rm = TRUE),
              max(cat_data$age_years, na.rm = TRUE)))
  cat(sprintf("    yearly_hours range: %.1f - %.1f\n",
              min(cat_data$yearly_hours, na.rm = TRUE),
              max(cat_data$yearly_hours, na.rm = TRUE)))
}

# Manual verification from CSV:
# Instrument 1 (Geige): instrumentplayed1=Ja
# instrument10-16 (age blocks): 5w, 5w, 5w, 5w, 5w, 5w, 6w, 6w, 6w, 6w, 6w, 4w, 4w
# 5w = 5 hours/week = 260 hours/year
# 6w = 6 hours/week = 312 hours/year
# 4w = 4 hours/week = 208 hours/year
#
# Othermusic 1 (Tanzen): othermusic1=Ja, whitchothermusic1=Tanzen
# othermusic10-16: 2w repeated for some years
# 2w = 2 hours/week = 104 hours/year
#
# Othermusic 3 (no name, but othermusic30-36 has data):
# Values: 6w,6w,6w,6w,6w,2d,2d,2d,2d,3d,3d,3d,2d,2d,5w,5w,5w

cat("\n=== PROFILE DATA ===\n")
profile <- result$sections$profile
prof_cols <- c("code", "music_status_label", "main_instrument", "handedness_label",
               "absolute_hearing_label", "vocal_knowledge_label")
for (col in prof_cols) {
  if (col %in% names(profile)) {
    cat(sprintf("  %-30s = %s\n", col, profile[[col]][1]))
  }
}
