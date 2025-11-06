# Quick Start Guide for Developers

## Setup
```r
# Install development version
remotes::install_github("your-org/musicAnalysis")

# Load package
library(musicAnalysis)
library(tidyverse)  # Recommended for data manipulation
```

## Basic Workflow Example
```r
# 1. Configure for your data format
set_ma_options(
  labels = list(
    pitch = "(Tonhöhe|Pitch|F0)",  # Multiple variants
    volume = "Lautstärke.*[Dd]ifferenz"  # Regex pattern
  )
)

# 2. Process KLAWA PDFs
klawa_data <- klawa_scan("path/to/KLAWA") %>%
  filter(measurement == "post")

# 3. Process musical experience
music_exp <- musical_experience(
  file = "path/to/musical_exp.csv",
  profile_select_regex = "^(music_status|main_instrument)"  # Keep only specific columns
)

# 4. Merge all data
final_dataset <- klawa_data %>%
  merge_by_code(music_exp$wide)
```

## Adding Custom Functionality
```r
# Example: Add a new metric extraction
extract_custom_metric <- function(pdf_text, label_pattern = "Custom.*Metric") {
  # Follow existing pattern from klawa_pdf_values()
  value <- stringr::str_extract(
    pdf_text,
    paste0(label_pattern, "\\s*:?\\s*(-?\\d+[.,]?\\d*)")
  )
  as.numeric(gsub(",", ".", value))
}

# Example: Add custom validation
validate_musical_experience <- function(music_data) {
  music_data %>%
    mutate(
      practice_hours_valid = yearly_hours <= 365 * 12,  # Max 12h/day
      instrument_recognized = !is.na(instrument_category)
    ) %>%
    filter(practice_hours_valid)
}
```

## Debugging Tips
```r
# Enable verbose output
result <- musical_experience_time(
  file = "problematic_file.csv",
  verbose = TRUE  # Shows processing steps
)

# Check intermediate results
klawa_file_info("path/to/specific.pdf")  # Test metadata extraction
klawa_pdf_values("path/to/specific.pdf")  # Test value extraction

# Inspect merge issues
merge_result <- merge_by_code(df1, df2)
attr(merge_result, "merge_report")  # If implemented
```
