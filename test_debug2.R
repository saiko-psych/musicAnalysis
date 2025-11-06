library(readr)
library(musicAnalysis)

data <- read_csv('tests/testdata_musical_experience/music_13042025.csv',
                 col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

row_idx <- 1  # 0211BAMA
cat('Testing participant:', data$code[row_idx], '\n')
cat('instrumentplayed1:', data$instrumentplayed1[row_idx], '\n\n')

# Get time columns manually
inst_cols <- names(data)[grepl('instrument[0-9]+\\[', names(data))]
cat('Found', length(inst_cols), 'time columns\n\n')

# Show first 30 values
cat('First 30 time column values:\n')
for (i in 1:min(30, length(inst_cols))) {
  val <- data[[inst_cols[i]]][row_idx]
  if (is.na(val)) val_display <- 'NA'
  else if (val == '') val_display <- '(empty)'
  else val_display <- val
  cat(sprintf('%-25s: %s\n', inst_cols[i], val_display))
}

# Now test the actual parsing
cat('\n\n=== Running musical_experience_time ===\n')
result <- musical_experience_time('tests/testdata_musical_experience/music_13042025.csv',
                                  verbose = FALSE)
cat('\nLong data rows:', nrow(result$long), '\n')
cat('Non-NA yearly_hours:', sum(!is.na(result$long$yearly_hours)), '\n')
cat('Non-zero yearly_hours:', sum(result$long$yearly_hours > 0, na.rm = TRUE), '\n')

# Check flags
cat('\nFlags:\n')
if (nrow(result$flags) > 0) {
  print(table(result$flags$flag_code))
}
