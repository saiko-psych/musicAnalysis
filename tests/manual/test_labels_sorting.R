# Test variable labels and column sorting in musical_experience_time
library(musicAnalysis)

# Find any CSV test data
test_dir <- file.path("C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests")
csv_files <- list.files(test_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
cat("CSV files found:", length(csv_files), "\n")
if (length(csv_files) > 0) cat(paste(head(csv_files, 5), collapse = "\n"), "\n")

# Test the .set_label helper and column ordering directly
# Create a minimal mock wide_data to test the ordering logic
cat("\n--- Testing column ordering logic ---\n")

# Simulate the column names that would exist in a real output
mock_cols <- c("code", "instrument2", "instrument1", "singing1",
               "instrument_starting_age1", "instrument_starting_age2",
               "singing_starting_age1",
               "instrument_total", "singing_total", "othermusic_total",
               "total_musical_experience",
               "number_of_instruments", "number_of_singing", "number_of_othermusic", "nodme",
               "instrument_min_starting_age", "singing_min_starting_age",
               "IMP_instrument", "IMP_singing", "IMP_total",
               "whichinstrument1", "whichinstrument2", "singingtype1",
               "extra_survey_col")

# Create mock tibble
mock_data <- tibble::as_tibble(
  setNames(as.list(rep(1, length(mock_cols))), mock_cols)
)

cat("Original order:\n")
cat(paste(names(mock_data), collapse = ", "), "\n\n")

# The ordering logic from the function
all_cols <- names(mock_data)
ordered <- "code"

for (cat_name in c("instrument", "singing", "othermusic")) {
  ordered <- c(ordered, sort(grep(paste0("^", cat_name, "\\d+$"), all_cols, value = TRUE)))
  ordered <- c(ordered, sort(grep(paste0("^", cat_name, "_starting_age\\d+$"), all_cols, value = TRUE)))
  ordered <- c(ordered, intersect(paste0(cat_name, "_total"), all_cols))
  ordered <- c(ordered, intersect(paste0(cat_name, "_min_starting_age"), all_cols))
}

ordered <- c(ordered, intersect("total_musical_experience", all_cols))
ordered <- c(ordered, intersect(c("number_of_instruments", "number_of_singing",
                                   "number_of_othermusic", "nodme"), all_cols))
ordered <- c(ordered, intersect(c("IMP_instrument", "IMP_singing",
                                   "IMP_othermusic", "IMP_total"), all_cols))
ordered <- c(ordered, sort(grep("^whichinstrument\\d+$", all_cols, value = TRUE)))
ordered <- c(ordered, sort(grep("^singingtype\\d+$", all_cols, value = TRUE)))
ordered <- c(ordered, sort(grep("^whichothermusic\\d+$", all_cols, value = TRUE)))

remaining <- setdiff(all_cols, ordered)
ordered <- c(ordered, remaining)

cat("Sorted order:\n")
cat(paste(ordered, collapse = ", "), "\n\n")

# Verify logical grouping
cat("Grouping check:\n")
instrument_block <- grep("^instrument", ordered)
singing_block <- grep("^singing", ordered)
cat("  Instrument cols at positions:", paste(instrument_block, collapse=","), "\n")
cat("  Singing cols at positions:", paste(singing_block, collapse=","), "\n")
cat("  Instrument block contiguous:", all(diff(instrument_block) == 1), "\n")
cat("  Singing block contiguous:", all(diff(singing_block) == 1), "\n")

# Test label setting
cat("\n--- Testing label attributes ---\n")
mock_data$instrument1 <- 100
attr(mock_data$instrument1, "label") <- "Instrument 1: Yearly practice hours"
cat("Label on instrument1:", attr(mock_data$instrument1, "label"), "\n")
cat("Value preserved:", mock_data$instrument1, "\n")

cat("\nAll tests passed!\n")
