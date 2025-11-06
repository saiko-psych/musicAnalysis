# Development Guidelines for musicAnalysis

## Code Standards

### Naming Conventions
- Functions: snake_case (e.g., `klawa_scan()`)
- Internal functions: prefix with dot (e.g., `.parse_time_strings()`)
- Constants: UPPER_CASE
- Variables: snake_case

### Function Design
```r
# Public functions should:
# - Have roxygen2 documentation
# - Include parameter validation
# - Return consistent types (preferably tibbles)
# - Use explicit namespacing for dependencies

#' @export
public_function <- function(data, option = "default") {
  # Validation
  stopifnot(is.data.frame(data))
  option <- match.arg(option, c("default", "alternative"))
  
  # Processing
  result <- data %>%
    dplyr::mutate(...) %>%
    dplyr::filter(...)
  
  # Return tibble
  tibble::as_tibble(result)
}
```

### Error Handling
- Use `rlang::abort()` for errors with context
- Use `cli::cli_warn()` for warnings
- Use `cli::cli_inform()` for progress messages
- Wrap file I/O in `tryCatch()`

### Testing Requirements
- Unit tests for all public functions
- Edge case testing (empty data, missing columns)
- Integration tests for full workflows
- Test with real data samples

## Extension Points

### Adding New Parsers
1. Create new R file in `/R` directory
2. Follow existing parser patterns (see `klawa.R`)
3. Return tibble with consistent structure
4. Add validation function in `utils.R`
5. Document with examples

### Adding New Reports
1. Use parameterized Rmd/Quarto templates
2. Include both code and narrative
3. Generate visualizations with ggplot2
4. Support multiple output formats

### Configurable Options
```r
# Use options system for customization
set_ma_options(
  labels = list(
    pitch = "Custom pattern"
  ),
  thresholds = list(
    daily_max_hours = 16
  )
)
```

## Common Issues and Solutions

### PDF Parsing
- **Issue**: Values split across lines
- **Solution**: Check multiple lines, use window-based search

### Character Encoding
- **Issue**: German umlauts, special characters
- **Solution**: UTF-8 throughout, normalization functions

### Missing Data
- **Issue**: Incomplete surveys, failed extractions
- **Solution**: Explicit NA handling, validation reports

### Performance
- **Issue**: Large number of PDFs
- **Solution**: Progress bars, parallel processing option