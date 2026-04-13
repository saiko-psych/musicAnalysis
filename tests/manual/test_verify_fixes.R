# Verify label fix and bracket cleaning
# Must install first: devtools::install(".", quick=TRUE)
library(musicAnalysis)
res <- musical_experience_time(
  "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv",
  verbose = FALSE
)
wide <- res$wide

cat("=== LABEL FIX CHECK ===\n")
cat("number_of_othermusic label:", attr(wide$number_of_othermusic, "label"), "\n")
cat("number_of_instruments label:", attr(wide$number_of_instruments, "label"), "\n")
cat("number_of_singing label:", attr(wide$number_of_singing, "label"), "\n")

cat("\n=== BRACKET CLEANING CHECK ===\n")
has_brackets <- grep("\\[", names(wide), value = TRUE)
cat("Columns with brackets:", length(has_brackets), "\n")
if (length(has_brackets) > 0) cat(paste(has_brackets, collapse = ", "), "\n")

cat("\nPassthrough columns (after which-names):\n")
start_idx <- which(names(wide) == "IMP_total") + 1
for (i in start_idx:min(ncol(wide), start_idx + 25)) {
  cat(sprintf("  %2d. %s\n", i, names(wide)[i]))
}

cat("\n=== LABELS SUMMARY ===\n")
labeled <- sum(sapply(names(wide), function(n) !is.null(attr(wide[[n]], "label"))))
cat("Labeled:", labeled, "/", ncol(wide), "\n")

cat("\nDone!\n")
