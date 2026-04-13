# Direct test of group faceting logic
library(musicAnalysis)

res <- musical_experience_time(
  "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_musical_experience/music_13042025.csv",
  verbose = FALSE
)
long_data <- res$long
wide_data <- res$wide

cat("Wide data:", nrow(wide_data), "rows,", ncol(wide_data), "cols\n")
cat("Long data:", nrow(long_data), "rows\n")

# Simulate the group faceting logic from mod_mexp.R
group_var_name <- "number_of_instruments"
cat("\nGrouping by:", group_var_name, "\n")

group_vals <- wide_data[[group_var_name]]
cat("Group values:", paste(sort(unique(group_vals)), collapse = ", "), "\n")

group_lookup <- dplyr::tibble(code = wide_data$code, .group_val = group_vals)
long_grouped <- dplyr::left_join(long_data, group_lookup, by = "code")
unique_groups <- sort(unique(stats::na.omit(long_grouped$.group_val)))
cat("Unique groups:", length(unique_groups), "-", paste(unique_groups, collapse = ", "), "\n")

# Create plots per group
cat("\nCreating plots per group...\n")
for (gval in unique_groups) {
  codes_in_group <- group_lookup$code[group_lookup$.group_val == gval]
  ld_sub <- long_data[long_data$code %in% codes_in_group, ]
  wd_sub <- wide_data[wide_data$code %in% codes_in_group, ]

  cat(sprintf("  Group %s: %d codes, %d long rows\n", gval, length(codes_in_group), nrow(ld_sub)))

  if (nrow(ld_sub) == 0) {
    cat("    SKIP - no data\n")
    next
  }

  p <- tryCatch(
    plot_practice_curves(ld_sub, wide_data = wd_sub, plot_type = "total"),
    error = function(e) { cat("    ERROR:", e$message, "\n"); NULL }
  )

  if (!is.null(p)) {
    if (inherits(p, "plotly")) {
      p <- p %>% plotly::layout(title = paste0(group_var_name, ": ", gval))
      cat("    OK - plotly object created\n")
    } else {
      cat("    OK - list of", length(p), "plots\n")
    }
  }
}

cat("\nDone!\n")
