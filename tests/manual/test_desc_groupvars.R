# Test: Which columns qualify as grouping variables for descriptive stats?
library(musicAnalysis)
res <- musical_experience(
  "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv",
  verbose = FALSE
)
wide <- res$wide

cat("Total columns:", ncol(wide), "\n\n")
cat("Qualifying columns for group_var selector:\n")
for (col in setdiff(names(wide), "code")) {
  n_unique <- length(unique(stats::na.omit(wide[[col]])))
  is_num <- is.numeric(wide[[col]])
  qualifies <- (n_unique > 0 && n_unique <= 20 && !is_num) ||
               (is_num && n_unique <= 10)
  if (qualifies) {
    cat(sprintf("  %-35s numeric=%-5s unique=%d\n", col, is_num, n_unique))
  }
}
