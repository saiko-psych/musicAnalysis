# Test: Is Top N ranking already context-aware?
library(musicAnalysis)
res <- musical_experience(
  "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv",
  verbose = FALSE
)
long_data <- res$sections$time$long
wide_data <- res$wide

# Top 3 by total musical experience
p1 <- plot_practice_curves(long_data, wide_data, plot_type = "total",
                           n_participants = 3, subset_by = "highest")
# Extract participant codes from plot data
codes_total <- unique(p1$x$data[[1]]$text)
codes_total <- gsub(".*Code: ([^<]+)<.*", "\\1", codes_total)
codes_total <- unique(codes_total)

# Top 3 by singing only
p2 <- plot_practice_curves(long_data, wide_data, plot_type = "category_sum",
                           categories = "singing", n_participants = 3, subset_by = "highest")
if (is.list(p2) && !inherits(p2, "plotly")) {
  codes_singing <- unique(p2[[1]]$x$data[[1]]$text)
} else {
  codes_singing <- unique(p2$x$data[[1]]$text)
}
codes_singing <- gsub(".*Code: ([^<]+)<.*", "\\1", codes_singing)
codes_singing <- unique(codes_singing)

cat("Top 3 by TOTAL:", paste(codes_total, collapse = ", "), "\n")
cat("Top 3 by SINGING:", paste(codes_singing, collapse = ", "), "\n")
cat("Same codes?", setequal(codes_total, codes_singing), "\n")

# Verify: who actually has the most singing hours?
singing_totals <- wide_data[, c("code", "singing_total")]
singing_totals <- singing_totals[order(-singing_totals$singing_total), ]
cat("\nActual top 5 by singing_total:\n")
print(head(singing_totals, 5))

total_totals <- wide_data[, c("code", "total_musical_experience")]
total_totals <- total_totals[order(-total_totals$total_musical_experience), ]
cat("\nActual top 5 by total_musical_experience:\n")
print(head(total_totals, 5))
