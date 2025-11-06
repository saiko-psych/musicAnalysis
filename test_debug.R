library(readr)
library(stringr)

data <- read_csv('tests/testdata_musical_experience/music_13042025.csv',
                 col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

# Find time columns with proper escaping
time_pattern <- "^instrument[0-9]{2}\\[[0-9]+_[0-9]+\\]$"
time_cols <- grep(time_pattern, names(data), value = TRUE)

cat('Found', length(time_cols), 'instrument time columns\n')
cat('\nSample column names:\n')
print(head(time_cols, 15))

cat('\n\nFirst participant (0211BAMA) data:\n')
cat('Code:', data$code[2], '\n')
cat('instrumentplayed1:', data$instrumentplayed1[2], '\n')
cat('\nNon-empty time values:\n')
for (col in time_cols) {
  val <- data[[col]][2]
  if (!is.na(val) && nzchar(val)) {
    cat(col, ':', val, '\n')
  }
}

cat('\n\n=== Checking what gets parsed ===\n')
# Test time parsing
source('R/musical_experience_time.R')
test_strings <- c("4w", "3w", "", NA, "2d", "1.5w")
for (s in test_strings) {
  result <- .parse_time_strings(s, 12, 84, 365/7)
  cat('Input:', ifelse(is.na(s), 'NA', ifelse(s == '', '(empty)', s)),
      '-> Hours:', result$yearly_hours,
      'Flag:', result$flag_code, '\n')
}
