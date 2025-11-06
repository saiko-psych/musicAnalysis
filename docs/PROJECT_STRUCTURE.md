# Project Structure and Organization

## Current Structure

```
musicAnalysis/
├── R/                          # Core package functions
│   ├── klawa.R                # KLAWA PDF extraction
│   ├── musical_experience.R   # Main musical experience wrapper
│   ├── musical_experience_time.R      # Time-based practice parsing
│   ├── musical_experience_profile.R   # Instrument profile parsing
│   ├── plot_practice_curves.R # Interactive plotting
│   ├── merge.R                # Data merging utilities
│   ├── options.R              # Configuration system
│   ├── utils.R                # Helper functions
│   └── launch_app.R           # Shiny app launcher
│
├── inst/shiny/                # Shiny application
│   ├── app.R                  # Main app file
│   └── modules/               # Modular UI components
│       ├── mod_home.R         # Welcome/version info
│       ├── mod_klawa.R        # KLAWA processing
│       ├── mod_mexp.R         # Musical experience processing
│       └── mod_merge.R        # Data merging interface
│
├── tests/testthat/            # Unit tests
│   ├── test-klawa.R           # KLAWA function tests (60 tests)
│   ├── test-merge.R           # Merge function tests (20 tests)
│   └── test-utils.R           # Utility function tests (28 tests)
│   # Total: 108 tests
│
├── docs/                      # Documentation (not in package)
│   ├── CLAUDE.md              # AI assistant guidelines
│   ├── GITHUB_WORKFLOW.md     # GitHub CLI guide
│   └── PROJECT_STRUCTURE.md   # This file
│
├── man/                       # Generated documentation (roxygen2)
│   └── *.Rd                   # R documentation files
│
├── DESCRIPTION                # Package metadata
├── NAMESPACE                  # Package namespace (auto-generated)
├── LICENSE                    # MIT license
└── README.md                  # Package overview

NOT TRACKED:
├── .Rproj.user/              # RStudio project files (gitignore)
├── *.tar.gz                  # Built packages (gitignore)
└── archived_for_limesurvey_package/  # Old code (gitignore)
```

## Key Principles

### 1. Separation of Concerns

**R/ folder**: Pure R functions, no UI
- Each file handles one major feature
- Functions are composable and testable
- No Shiny dependencies in core functions
- Return tibbles or lists, never print to console

**inst/shiny/ folder**: User interface only
- Shiny modules call R/ functions
- Handle user input validation
- Display results
- No business logic here

### 2. Modular Design

**Benefits**:
- Easy to find code: know which file based on feature
- Easy to test: each module isolated
- Easy to maintain: changes don't ripple everywhere
- Easy to extend: add new modules without breaking existing

**Example**: Musical Experience
```
R/musical_experience.R           → Main wrapper, coordinates others
R/musical_experience_time.R      → Time parsing logic
R/musical_experience_profile.R   → Profile parsing logic
R/plot_practice_curves.R         → Plotting logic

inst/shiny/modules/mod_mexp.R    → UI for all of the above
tests/testthat/test-mexp.R       → Tests (to be created)
```

### 3. Testing Strategy

**What to test** (necessary tests):
- ✅ Data parsing functions (KLAWA, musical experience)
- ✅ Data transformation (merging, filtering, calculating)
- ✅ Edge cases (empty data, missing values, invalid formats)
- ✅ Helper functions (time parsing, code extraction)

**What NOT to test** (useless tests):
- ❌ Shiny UI rendering (changes frequently, hard to maintain)
- ❌ Third-party packages (pdftools, plotly - they test themselves)
- ❌ Trivial getters/setters
- ❌ Visual appearance of plots (manual testing better)

**Current test coverage**: 108 tests
- `test-klawa.R`: 60 tests (PDF extraction, validation, structure analysis)
- `test-merge.R`: 20 tests (data merging, code normalization)
- `test-utils.R`: 28 tests (helper functions, time parsing)

**Missing tests** (should add):
- Musical experience parsing functions
- Plot data preparation (not plotting itself)
- Configuration system (options.R)

### 4. Finding Code Fast

**By Feature**:
```
KLAWA?              → R/klawa.R + inst/shiny/modules/mod_klawa.R
Musical Experience? → R/musical_experience*.R + inst/shiny/modules/mod_mexp.R
Merging?           → R/merge.R + inst/shiny/modules/mod_merge.R
Plotting?          → R/plot_practice_curves.R
Config?            → R/options.R
Tests?             → tests/testthat/test-{feature}.R
```

**By Task Type**:
```
Add new variable?              → R/musical_experience_time.R or R/klawa.R
Fix plot labels?               → R/plot_practice_curves.R
Change UI layout?              → inst/shiny/modules/mod_*.R
Add new data source?           → Create R/{source}.R + tests + module
Update version/changelog?      → DESCRIPTION + inst/shiny/modules/mod_home.R + CLAUDE.md
```

**Quick search commands**:
```bash
# Find where a function is defined
grep -r "klawa_scan <- function" R/

# Find where a function is used
grep -r "klawa_scan(" .

# Find all TODOs in code
grep -r "TODO" R/ inst/

# Find all test files for a feature
ls tests/testthat/test-klawa*
```

## File Naming Conventions

### R Functions
- `feature.R` - Main feature file (e.g., `klawa.R`)
- `feature_subpart.R` - Sub-components (e.g., `musical_experience_time.R`)
- All lowercase, underscores for spaces
- One feature per file (unless very small helpers)

### Shiny Modules
- `mod_feature.R` - Module files (e.g., `mod_klawa.R`)
- Format: `mod_` prefix + feature name
- Each module contains both UI and server functions

### Tests
- `test-feature.R` - Test files (e.g., `test-klawa.R`)
- Format: `test-` prefix + feature name
- Mirror the R/ file structure

### Documentation
- `UPPERCASE.md` - Important docs (README.md, LICENSE)
- `Title_Case.md` - Guide docs (this file)
- Keep in `docs/` folder (except README and LICENSE at root)

## Code Organization Within Files

### Standard R Function File Structure

```r
# File: R/feature.R

#' Function Title
#'
#' Detailed description
#'
#' @param x Description
#' @return Description
#' @export
#' @examples
#' feature_function(x = "test")
main_function <- function(x) {
  # Implementation using helper functions
  result <- .helper_function(x)
  return(result)
}

#' Another exported function
#' @export
another_function <- function(y) {
  # ...
}

# Internal helper functions (not exported)
# Use . prefix to indicate "private"
.helper_function <- function(x) {
  # Implementation
}

.another_helper <- function(x) {
  # Implementation
}
```

### Standard Shiny Module Structure

```r
# File: inst/shiny/modules/mod_feature.R

#' Feature Module UI
#' @param id Module namespace ID
mod_feature_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements
  )
}

#' Feature Module Server
#' @param id Module namespace ID
mod_feature_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic
    # Call R/ functions here
    # Handle reactivity
  })
}
```

### Standard Test File Structure

```r
# File: tests/testthat/test-feature.R

test_that("feature_function works with valid input", {
  result <- feature_function("valid")
  expect_equal(result$status, "success")
  expect_type(result$data, "list")
})

test_that("feature_function handles edge cases", {
  expect_error(feature_function(NULL))
  expect_warning(feature_function(""))
  expect_equal(feature_function(NA), expected_na_behavior)
})

test_that("feature_function validates input types", {
  expect_error(feature_function(123), "must be character")
  expect_error(feature_function(list()), "must be character")
})
```

## .gitignore Recommendations

```gitignore
# R package build artifacts
*.Rcheck/
*.tar.gz
*.Rproj.user/
.Rhistory
.RData
.Ruserdata

# Temporary files
*~
*.swp
*.swo
.DS_Store
Thumbs.db

# Archived/deprecated code
archived_*/
old_*/
deprecated_*/

# User-specific files
.vscode/
.idea/
*.Rproj

# Build outputs (keep in parent dir)
../*.tar.gz

# Data files (if you add example data later)
data-raw/
*.csv
*.xlsx
*.pdf
!inst/extdata/*.pdf  # But keep example PDFs

# Documentation builds
docs/*.html
```

## Adding New Features

### Checklist for New Feature

1. **Plan** (GitHub Issue)
   - [ ] Create issue with clear description
   - [ ] Add labels and milestone
   - [ ] Outline acceptance criteria

2. **Create Branch**
   - [ ] `gh issue develop {issue_number}`

3. **Implement Core Logic** (R/ folder)
   - [ ] Create/modify R/{feature}.R
   - [ ] Add roxygen2 documentation
   - [ ] Export main function
   - [ ] Keep helpers internal (. prefix)

4. **Add Tests** (tests/testthat/)
   - [ ] Create test-{feature}.R
   - [ ] Test happy path (valid inputs)
   - [ ] Test edge cases (empty, NA, NULL)
   - [ ] Test error conditions
   - [ ] Aim for >80% code coverage

5. **Add UI** (inst/shiny/modules/)
   - [ ] Create mod_{feature}.R if new module
   - [ ] Or modify existing module
   - [ ] Add UI inputs
   - [ ] Add outputs (tables, plots)
   - [ ] Add help text

6. **Update Documentation**
   - [ ] Update CLAUDE.md (Recently Completed section)
   - [ ] Add examples to function documentation
   - [ ] Update README if user-facing feature

7. **Version and Release**
   - [ ] Increment version in DESCRIPTION
   - [ ] Update What's New in mod_home.R
   - [ ] Run devtools::document()
   - [ ] Run devtools::test() (all pass)
   - [ ] Build package: pkgbuild::build()

8. **Commit and PR**
   - [ ] Commit with clear message
   - [ ] Push to GitHub
   - [ ] Create PR with detailed description
   - [ ] Merge when tests pass

9. **Release** (if stable)
   - [ ] Create GitHub release
   - [ ] Attach tarball
   - [ ] Write release notes

## Optimization Tips

### Speed Up Development

**1. Use devtools shortcuts**
```r
# Instead of restarting R and reinstalling
devtools::load_all()  # Fast: loads code into memory

# Instead of building full package
devtools::test()      # Fast: runs tests only

# Instead of full check
devtools::check()     # Use only before release
```

**2. Cache test data**
```r
# In tests/testthat/helper-data.R (runs once per test session)
test_data_klawa <- create_test_klawa_data()
test_data_music <- create_test_music_data()

# Then in tests, reuse:
test_that("something works", {
  result <- my_function(test_data_klawa)
  # ...
})
```

**3. Skip slow tests during development**
```r
test_that("slow integration test", {
  skip_on_cran()  # Skip during rapid iteration
  # ... slow test
})
```

### Keep Code Fast

**DO**:
- ✅ Use dplyr for data manipulation (vectorized, fast)
- ✅ Use data.table for very large datasets (if needed)
- ✅ Cache expensive computations in reactive expressions (Shiny)
- ✅ Use progress indicators for long operations

**DON'T**:
- ❌ Use loops when vectorization possible
- ❌ Recompute same thing multiple times
- ❌ Load entire large files when only need subset
- ❌ Create unnecessary copies of large data frames

## Common Patterns

### Error Handling Pattern

```r
my_function <- function(file) {
  # Validate inputs
  if (is.null(file)) {
    rlang::abort("file cannot be NULL")
  }
  if (!file.exists(file)) {
    rlang::abort(paste0("File not found: ", file))
  }

  # Wrap risky operations
  result <- tryCatch(
    {
      # Main logic here
      data <- readr::read_csv(file)
      # ... process data
      data
    },
    error = function(e) {
      cli::cli_warn("Failed to process {file}: {e$message}")
      return(tibble::tibble())  # Return empty, don't crash
    }
  )

  return(result)
}
```

### Progress Indication Pattern

```r
process_many_files <- function(files) {
  cli::cli_progress_bar("Processing files", total = length(files))

  results <- purrr::map(files, function(f) {
    cli::cli_progress_update()
    process_one_file(f)
  })

  cli::cli_progress_done()
  return(results)
}
```

### Reactive Data Pattern (Shiny)

```r
mod_feature_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Expensive computation - cached until inputs change
    processed_data <- reactive({
      req(input$file)  # Wait for input

      # Only runs when input$file changes
      data <- read_csv(input$file$datapath)
      expensive_processing(data)
    })

    # Use processed_data() multiple times without recomputing
    output$table <- renderDT({
      processed_data()
    })

    output$plot <- renderPlotly({
      plot_practice_curves(processed_data())
    })
  })
}
```

## Summary

**To work efficiently**:
1. Know the structure: Feature → R/feature.R + mod_feature.R + test-feature.R
2. Use devtools::load_all() for fast iteration
3. Write tests for logic, not UI
4. Keep functions pure (no side effects) in R/ folder
5. Put all UI/user interaction in inst/shiny/modules/
6. Use GitHub issues for planning, PRs for implementation
7. Version every time you rebuild

**To find code fast**:
- Feature name? → R/{feature}.R
- UI for feature? → inst/shiny/modules/mod_{feature}.R
- Tests for feature? → tests/testthat/test-{feature}.R
- Recent changes? → CLAUDE.md "Recently Completed"
- Future plans? → CLAUDE.md "Next steps"
- How to use GitHub? → docs/GITHUB_WORKFLOW.md

**When in doubt**:
- Check CLAUDE.md for guidelines
- Check existing code for patterns
- Run devtools::test() before committing
- Increment version before building
- Update What's New in mod_home.R
