# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**musicAnalysis** is an R package for music psychology research data preparation at the University of Graz. It focuses on automated data extraction and preparation from four primary sources:

1. **KLAWA PDFs** - Voice/singing performance metrics
2. **PPPT data** - (To be implemented)
3. **AAT data** - (To be implemented)
4. **Musical Experience** - Practice history and instrument proficiency questionnaires

**Note**: LimeSurvey processing and scale analysis (MDBF, etc.) should NOT be part of this package. Those will be moved to a separate limesurvey-focused package.

## Package Structure

Standard R package layout:
- **R/** - Core package functions (focus on KLAWA, musical experience, PPPT, AAT)
- **inst/shiny/** - Shiny web application for GUI-based data processing
- **tests/testthat/** - Unit tests (108 tests covering core functions)
- **docs/** - Development documentation and guidelines
  - **CLAUDE.md** - This file: AI assistant guidelines, development priorities, version history
  - **GITHUB_WORKFLOW.md** - Complete guide to GitHub CLI project management
  - **PROJECT_STRUCTURE.md** - Code organization, finding code fast, optimization tips
- **DESCRIPTION** - Package metadata and dependencies

**📖 For detailed project structure and workflow**: See `docs/PROJECT_STRUCTURE.md` and `docs/GITHUB_WORKFLOW.md`

## Building and Testing

### Development workflow
```r
# Load package during development
devtools::load_all()        # Load all functions into memory
devtools::document()        # Update documentation from roxygen2 comments
devtools::check()           # Run R CMD check for issues

# Install locally
devtools::install()

# Run Shiny app
library(musicAnalysis)
launch_app()
```

### Versioning System

**CRITICAL: Always increment version number when making changes!**

**⚠️ MANDATORY STEPS - DO NOT SKIP:**

1. **Update version in DESCRIPTION**:
   ```
   Version: 0.0.0.9002  # Increment last number
   ```

2. **ALWAYS UPDATE HOME PAGE** (`inst/shiny/modules/mod_home.R`):
   - Update "What's New" section header with new version number
   - List changes made in this version (Added/Fixed/Improved/Enhanced)
   - Move previous "What's New" entry to "Previous Updates" section
   - **This is MANDATORY for every version release!**

3. **Update timestamp if shown** in home page:
   ```r
   "2025-10-31 14:30"  # Update to current date and time
   ```

**Version numbering scheme**:
- `0.0.0.9xxx` = Development versions
- Increment last digit for each change (9001, 9002, 9003, etc.)
- This creates unique tarball filenames so you know which version you're installing

### Understanding Tarballs vs dist/

**What are tarballs?**
- A tarball is a `.tar.gz` file containing the full R package
- Built using `pkgbuild::build()` or `devtools::build()`
- Filename includes version: `musicAnalysis_0.0.0.9002.tar.gz`
- Located in parent directory after building

**Why tarballs, not dist/?**
- Tarballs are the R standard for package distribution
- The filename tells you exactly which version you're installing
- `dist/` folder is used by other ecosystems (JavaScript/npm) but not R packages
- R CMD build automatically creates versioned tarballs

**How to build a new version**:
```r
# In R session:
devtools::document()  # Update docs
devtools::test()      # Verify tests pass
pkgbuild::build(path = ".")  # Creates ../musicAnalysis_0.0.0.9002.tar.gz
```

This creates: `C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis_0.0.0.9002.tar.gz`

**Push to GitHub** (after building):
```bash
git add .
git commit -m "Bump version to 0.0.0.9024

- Updated DESCRIPTION version
- Updated What's New in mod_home.R
- Added version to version history
- Built and tested tarball"

git push origin master

# Optional: Create GitHub release
gh release create v0.0.0.9024 \
  --title "v0.0.0.9024" \
  --notes "See README.md for changes" \
  musicAnalysis_0.0.0.9024.tar.gz
```

### Manual Testing in Fresh R Session

**IMPORTANT**: After making changes, always test in a completely fresh R session to ensure the updated package works correctly.

```r
# Step 1: Close ALL R sessions (RStudio, RGui, terminals, etc.)

# Step 2: Open a fresh R session and run:

# Remove old version
remove.packages("musicAnalysis")

# Install from specific tarball (RECOMMENDED - you know exactly what version):
install.packages("C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis_0.0.0.9002.tar.gz",
                 repos = NULL, type = "source")

# OR install from source directory (gets whatever is currently in the code):
# devtools::install("C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis")

# Step 3: Load and test
library(musicAnalysis)

# Verify version matches what you installed
packageVersion("musicAnalysis")  # Should show: 0.0.0.9002

# Check home page shows correct timestamp
launch_app()

# Or test functions directly
klawa_data <- klawa_scan("path/to/test/data")
```

**Why this is critical**:
- R caches package files in memory across sessions
- Simply reinstalling in the same session may use cached old files
- The Shiny app UI files (`inst/shiny/modules/*.R`) especially need fresh session to reload
- Version number and timestamp on home page help verify you're using the latest build
- Tarball filename ensures you install the exact version you built

### Testing Requirements

**CRITICAL**: Always write and run unit tests for any new functions or modifications.

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-klawa.R")

# Interactive testing during development
devtools::load_all()
# ... test your functions manually with sample data
```

**When writing tests**:
- Create test files in `tests/testthat/` with pattern `test-*.R`
- Test edge cases: empty inputs, missing values, malformed data
- Test with realistic data samples from actual research
- Verify return types (tibble vs data.frame)
- Check column names and data types in output
- Test error handling and warning messages

### Core dependencies
- **Data manipulation**: dplyr, tidyr, tibble, purrr, stringr
- **File I/O**: readr, fs, pdftools
- **Shiny UI**: shiny, bslib, DT, shinyFiles

## Core Architecture

### 1. KLAWA PDF Extraction (R/klawa.R)

**Purpose**: Extract singing/voice performance metrics from hierarchical folder structure.

**Expected directory structure**:
```
data/KLAWA/
  Computer/
    <PC>/
      Gruppen/
        <GROUP>/
          pre/
            *.pdf
          post/
            *.pdf
```

**Key functions**:
- `klawa_scan(root)` - Main entry point, recursively scans folder and returns tidy tibble
- `klawa_file_info(rel_file)` - Extracts metadata from path/filename (group, measurement, pc, code)
- `klawa_pdf_values(file)` - Extracts numeric values from PDF text
- `extract_and_check_code(x, pattern)` - Validates participant codes, detects conflicts

**Participant code format**: 4 digits + 4 letters (e.g., "0102SICH")
- Regex: `\\d{4}[A-Za-z]{4}`
- Returns `"CODE_CONFLICT"` if multiple different codes found in same filename
- Returns `NA_character_` if no code found

**Known groups**: LEN, LEO, LHG, LLO, MAR, MEL

**Measurements**: "pre" (before intervention), "post" (after intervention)

**PC identifiers**: "Jeki 4", "Jeki 7" (computer workstation labels)

**Extracted metrics** (configurable via `ma_options()`):
- `volume_difference` - Default label regex: "Lautstaerkendifferenz"
- `pitch` - Default label regex: "Tonhöhe"
- `onset_difference` - Default label regex: "Tonbeginn\\s*[-\\u2013]?\\s*Differenz"
- `pitch_duration_difference` - Default label regex: "Laengendifferenz"

**PDF parsing strategy** (robust multi-stage approach):
1. **Normalize PDF quirks**: Convert non-breaking spaces to regular spaces, normalize Unicode dash variants
2. **Same-line search**: Look for label followed by number on the same line
3. **Cross-line fallback**: Search within 240-character window after label (handles values on next line)
4. **Special onset handling**: "Tonbeginn" label often appears on different line than "Differenz" value; custom logic searches up to 3 lines ahead
5. **Returns NA** for missing values (never errors)

**Configuration example**:
```r
# Override label patterns for different PDF formats
set_ma_options(labels = list(
  pitch = "(Tonhöhe|Pitch|F0|Fo|Grundfrequenz)",
  volume = "Lautstaerkendifferenz|Volume.*Diff"
))

# Then scan
klawa_data <- klawa_scan("data/KLAWA")
```

**Output tibble columns**:
- `group` - Group identifier (LEN, LEO, etc.)
- `measurement` - "pre" or "post"
- `pc` - PC identifier
- `code` - Participant code (or NA or "CODE_CONFLICT")
- `volume_difference` - Numeric value or NA
- `pitch` - Numeric value or NA
- `onset_difference` - Numeric value or NA
- `pitch_duration_difference` - Numeric value or NA
- `file` - Relative path to source PDF

### 2. Musical Experience (R/musical_experience*.R)

**Purpose**: Parse and combine musical training history and instrument proficiency data.

**Main function**: `musical_experience(file, ...)` (R/musical_experience.R)
- Combines two sub-parsers: time-based practice data + profile data
- Returns list with `sections`, `wide` (merged tibble), `flags`, `merge_notes`

**Time parser**: `musical_experience_time(file, ...)` (R/musical_experience_time.R)
- Parses complex time string formats: "2d" (days), "1.5w" (weeks), "1/2w" (half week), "3m" (months), "2y" (years)
- Converts to numeric hours (yearly totals)
- Validates practice hours (configurable thresholds, e.g., max 12h/day)
- Flags unrealistic values for manual review
- **NEW (v0.0.0.9009)**: Computes starting age for each instrument/singing/othermusic (minimum age where practice > 0)
- **Output includes**: `instrument1_starting_age`, `instrument2_starting_age`, `singing1_starting_age`, etc.

**Profile parser**: `musical_experience_profile(file, ...)` (R/musical_experience_profile.R)
- Processes instrument names, proficiency rankings
- Handles variations and typos in instrument names
- Multi-dimensional ranking data

**Key parameters**:
- `join` - Merge strategy: "left", "inner", "full"
- `profile_select_regex` - Optional filter to keep only specific profile columns (e.g., `"^(music_status|main_instrument)"`)
- `validate` - If TRUE, computes NA-inflation report to detect merge issues
- `time_args`, `profile_args` - Named lists forwarded to sub-parsers

**Usage example**:
```r
music_exp <- musical_experience(
  file = "data/musical_exp.csv",
  profile_select_regex = "^(music_status|main_instrument)",
  validate = TRUE
)

# Access components
music_exp$wide           # Merged data (use this for analysis)
music_exp$sections$time  # Raw time parsing results
music_exp$flags          # Validation warnings
music_exp$merge_notes    # NA inflation report
```

**Practice History Analysis** (NEW in v0.0.0.9009):

`compute_practice_history(long_data, current_age = NULL, time_windows = c(1, 2, 5, 10))`
- Computes practice hours within flexible retrospective time windows
- Returns wide tibble with columns like `instrument1_hours_1yr`, `instrument1_hours_5yr`, etc.
- Useful for analyzing recent practice patterns vs. lifetime totals

**Interactive Visualization** (NEW in v0.0.0.9009):

`plot_practice_curves(long_data, categories = NULL, category_ids = NULL, codes = NULL, ...)`
- Creates interactive plotly growth curves showing practice hours over age
- **Full customization**:
  - Select specific categories (instrument/singing/othermusic)
  - Select specific category IDs (e.g., only instrument1, instrument2)
  - Filter participants (all, top N, bottom N, random N)
  - Visual options: smoothing, faceting, coloring
- Returns plotly object for interactive exploration in Shiny or standalone

**Example workflow**:
```r
# Parse data
res <- musical_experience_time("data/survey.csv")

# Check starting ages in wide format
View(res$wide)  # Look for *_starting_age columns

# Compute practice history for 1, 2, 5, 10 years
history <- compute_practice_history(res$long, time_windows = c(1, 2, 5, 10))

# Visualize top 10 practitioners (instruments only)
plot_practice_curves(
  res$long,
  categories = "instrument",
  n_participants = 10,
  subset_by = "highest",
  smooth = TRUE
)

# Or use Shiny app: launch_app() -> Musical Experience tab
```

### 3. PPPT Data

**Status**: To be implemented
**Expected location**: R/pppt.R (or similar)
**Function naming**: Follow pattern `pppt_scan()`, `pppt_parse_*()`

### 4. AAT Data

**Status**: To be implemented
**Expected location**: R/aat.R (or similar)
**Function naming**: Follow pattern `aat_scan()`, `aat_parse_*()`

## Configuration System (R/options.R)

**Session-level configuration** for label patterns and thresholds:

```r
# Get current options
ma_options()

# Override specific options (only change what you need)
set_ma_options(
  labels = list(
    pitch = "(Tonhöhe|Pitch|F0|Grundfrequenz)",
    volume = "Lautst.*differenz"
  ),
  thresholds = list(
    daily_max_hours = 16  # For musical experience validation
  )
)

# Internal getter (used in package code)
labels <- ma_get("labels")
```

## Data Merging (R/merge.R)

**merge_by_code(x, y, suffix)** - Safe left join by participant code:
- Validates `code` column exists in both data frames
- Normalizes codes: trim whitespace, convert empty strings to NA
- Warns about duplicates in `y` (will cause row expansion)
- Returns tibble (never base data.frame)

**Usage**:
```r
# Merge KLAWA with musical experience
final_data <- klawa_data %>%
  merge_by_code(music_exp$wide)
```

## Shiny Application (inst/shiny/)

Modular interface with separate modules:
- **mod_home** - Welcome/instructions (inst/shiny/modules/mod_home.R)
- **mod_klawa** - KLAWA PDF processing with folder browser (inst/shiny/modules/mod_klawa.R)
- **mod_mexp** - Musical experience processing (inst/shiny/modules/mod_mexp.R)
- **mod_merge** - Interactive data merging (inst/shiny/modules/mod_merge.R)

**Note**: Modules for LimeSurvey/MDBF should be removed from this package.

Launch: `musicAnalysis::launch_app()` or `launch_app()`

## Code Style and Conventions

### Naming
- **Functions**: snake_case (e.g., `klawa_scan()`)
- **Internal helpers**: prefix with `.` (e.g., `.parse_time_strings()`)
- **Constants**: UPPER_CASE
- **Variables**: snake_case

### Documentation
- All exported functions require roxygen2 documentation with `@export`
- Include `@param`, `@return`, `@examples` sections
- Document expected file formats and column requirements

### Error Handling
- Use `rlang::abort()` for errors with context
- Use `cli::cli_warn()` for warnings
- Use `cli::cli_inform()` for progress/status messages
- Wrap all file I/O in `tryCatch()` to provide helpful error messages
- **Prefer returning NA over erroring** when individual values are missing

### Return Types
- **Always return tibbles** (use `tibble::as_tibble()`, not base data.frames)
- Multi-part results: return **named lists** with clear component names
  - Example: `list(wide = tibble, flags = tibble, sections = list(...))`
- Single data results: return tibble directly

### Character Encoding
- **UTF-8 throughout** - all file reads and writes
- Handle German umlauts (ä, ö, ü, ß) correctly
- Normalize Unicode variants in PDF parsing (e.g., different dash types)

## Common Workflows

### Standard KLAWA + Musical Experience pipeline
```r
library(musicAnalysis)
library(tidyverse)

# 1. Configure label patterns if needed
set_ma_options(labels = list(
  pitch = "(Tonhöhe|Pitch|F0)"
))

# 2. Process KLAWA PDFs
klawa_data <- klawa_scan("data/KLAWA") %>%
  filter(measurement == "post", !is.na(code))

# 3. Process musical experience
music_exp <- musical_experience(
  file = "data/musical_exp.csv",
  profile_select_regex = "^(music_status|main_instrument)",
  validate = TRUE
)

# 4. Check for issues
if (!is.null(music_exp$flags)) {
  print(music_exp$flags)
}

# 5. Merge datasets
final_data <- klawa_data %>%
  merge_by_code(music_exp$wide)

# 6. Check merge quality
if (!is.null(music_exp$merge_notes)) {
  print(music_exp$merge_notes)
}
```

### Testing individual components
```r
# Test KLAWA metadata extraction
klawa_file_info("Computer/Jeki4/Gruppen/LEN/post/0102SICH_post.pdf")

# Test PDF value extraction on single file
klawa_pdf_values("data/KLAWA/Computer/Jeki4/Gruppen/LEN/post/0102SICH_post.pdf")

# Test code extraction
extract_and_check_code("0102SICH_post_080425.pdf")  # Returns "0102SICH"
extract_and_check_code("0102SICH_0103ANDE.pdf")     # Returns "CODE_CONFLICT"
```

## Known Data Challenges

### KLAWA PDFs
- **Label variations**: Same metric labeled differently across PDFs (handle via configurable regex)
- **Multi-line values**: Value appears 1-3 lines after label (robust fallback search)
- **Unicode inconsistencies**: Various dash types (–, —, ‐, -), non-breaking spaces
- **Onset difference**: Unique two-part label ("Tonbeginn" ... "Differenz") requires special logic

### Musical Experience
- **Time format variations**: "2d", "1.5w", "1/2w", "3m", "2y" all valid
- **Instrument name variations**: "Piano" vs "Klavier", typos, capitalization
- **Unrealistic practice hours**: Need validation (e.g., >12h/day should be flagged)
- **Multi-instrument profiles**: Rankings across multiple instruments

## GitHub Project Management

### Repository Setup
**GitHub URL**: https://github.com/saiko-psych/musicAnalysis
**Remote**: `origin` → https://github.com/saiko-psych/musicAnalysis.git

### Creating GitHub Issues from CLAUDE.md Todos

All future tasks from this file have been converted to GitHub issues for better tracking.

**Run once to create all issues**:
```bash
bash create_github_issues.sh
```

This creates 13 issues with proper labels and priorities based on the "Next steps" section below.

**Benefits of using GitHub issues**:
- ✅ Trackable and can be assigned
- ✅ Integrate with PRs and commits (`Closes #123`)
- ✅ Provide clear project board view
- ✅ Easier to manage than text file
- ✅ Can add milestones and due dates
- ✅ Enable GitHub Actions integration

### Standard Development Workflow

1. **Create or pick an issue**:
   ```bash
   gh issue list  # See all open issues
   gh issue view 5  # View details of issue #5
   ```

2. **Create feature branch**:
   ```bash
   gh issue develop 5  # Auto-creates branch like "5-fix-participant-selection"
   ```

3. **Make changes and test**:
   ```r
   devtools::load_all()  # Fast iteration
   devtools::test()      # All tests pass
   ```

4. **Commit and push**:
   ```bash
   git add .
   git commit -m "Implement context-aware ranking

   - Modified plot_practice_curves() ranking logic
   - Added tests for all scenarios
   - Updated UI to show ranking context

   Closes #5"

   git push
   ```

5. **Create Pull Request**:
   ```bash
   gh pr create --title "Fix participant selection" \
     --body "Implements context-aware ranking. Closes #5"
   ```

6. **Merge when ready**:
   ```bash
   gh pr merge 1 --squash --delete-branch
   ```

7. **Update version and push**:
   ```r
   # Update DESCRIPTION, mod_home.R, CLAUDE.md
   devtools::document()
   devtools::test()
   pkgbuild::build(path = ".")
   ```

   ```bash
   git add .
   git commit -m "Bump version to 0.0.0.9024"
   git push origin master

   # Create release
   gh release create v0.0.0.9024 \
     --notes "See README.md for changes" \
     musicAnalysis_0.0.0.9024.tar.gz
   ```

**For detailed GitHub CLI guide**: See `docs/GITHUB_WORKFLOW.md`

## Development Priorities

### Current focus (implemented)
1. ✅ KLAWA PDF extraction with robust parsing
2. ✅ Musical experience time and profile processing
3. ✅ Safe merging by participant code
4. ✅ Configuration system for labels/thresholds

### Next steps (Future enhancements)

#### High Priority
1. ⬜ **KLAWA flexible folder structure with guided setup**
   - Make KLAWA scanning work with non-standard folder structures
   - Add interactive guidance to help users organize their files
   - Optional variables: sometimes no Group needed, sometimes no PC variable
   - Only include variables that are actually present in the structure
   - Goal: More forgiving import process with user assistance

2. ⬜ **Musical Experience variable organization**
   - Sort variables in wide and long format logically
   - Group related variables together (e.g., all instrument1 vars, then instrument2, etc.)
   - Makes data tables easier to read and understand

3. ⬜ **Variable name cleaning**
   - Remove brackets `[]` and other special characters from variable names in long and wide format
   - Ensures clean, R-friendly variable names throughout

4. ⬜ **Variable labels implementation**
   - Add descriptive labels to all variables
   - Makes output more readable and self-documenting
   - Useful for reports and data dictionaries

5. ⬜ **Plot grouping by variables**
   - Allow plots to be organized/colored by grouping variables
   - E.g., compare practice curves by intervention group, school, etc.
   - Applies to both KLAWA and Musical Experience plots

6. ⬜ **Descriptive statistics table for Musical Experience**
   - Add summary statistics display option in Shiny app
   - Include grouping option for stratified statistics
   - Mean, SD, min, max, N for key practice variables

7. ⬜ **Version timestamps**
   - Show date and time when each package version was created
   - Helps track when changes were made
   - Display in home page version history

8. ⬜ **Show R code buttons**
   - Add "Show R Code" button for CSV parsing in Musical Experience module
   - Add "Show R Code" button for KLAWA scanning
   - Helps users understand what the app is doing and reproduce analyses

9. ⬜ **Fix top_N and bottom_N participant selection in Musical Experience plots**
   - Currently selects top/bottom based on total hours across all categories
   - Should be context-aware based on current plot_type and category selection
   - Examples:
     - plot_type = "individual", category = "instrument", category_id = 1 → top N for instrument1 only
     - plot_type = "category_sum", category = "singing" → top N for total singing hours
     - plot_type = "total" → top N for overall musical experience (current behavior is correct)
   - Improves relevance of participant subset in focused analyses

#### Medium Priority
10. ⬜ **Musical Experience - Additional useful variables** (to be designed):
   - Years of formal instruction per instrument
   - Current practice frequency (hours/week currently)
   - Performance experience (concerts, recitals, competitions)
   - Ensemble experience (band, orchestra, choir membership)
   - Music theory training (years)
   - Perfect pitch / relative pitch abilities
   - Primary vs secondary instruments ranking
   - Motivation for playing (intrinsic vs extrinsic)

11. ⬜ PPPT data parser
12. ⬜ AAT data parser
13. ⬜ Further validation improvements based on user feedback

### Recently Completed (GitHub Setup - 2025-11-06)
1. ✅ **Set up GitHub CLI project management**
   - Repository: https://github.com/saiko-psych/musicAnalysis
   - Created comprehensive README.md with installation, usage examples, documentation
   - Created create_github_issues.sh script to convert CLAUDE.md todos to 13 GitHub issues
   - Documented GitHub workflow in CLAUDE.md
   - Set up push workflow for future version releases

2. ✅ **Documentation improvements**
   - Added GitHub Project Management section to CLAUDE.md
   - Documented standard development workflow with GitHub CLI
   - Added push-to-GitHub step in version release process
   - Created issue creation script with proper labels and priorities

### Recently Completed (v0.0.0.9023 - 2025-11-06)
1. ✅ **FINAL FIX for category_sum plot labels and colors**
   - Problem 1: Labels showed "Singing - total CODE_Singing" (duplicate legend entries)
   - Problem 2: All participants in same category had same color (lines overlapped visually)
   - Solution: Changed label format to "CODE - total Category" (R/plot_practice_curves.R:260-262)
   - Result: Each participant gets unique color, proper label format
   - Labels now: "0102SICH - total Instruments", "0103ANDE - total Singing"
   - Both `group_var` and `color_var` use same value to prevent duplicate legend entries

2. ✅ **Restored category_sum plotting behavior to v0.0.0.9018/9019 quality**
   - Each participant has separate line (proper line separation maintained)
   - Each participant has unique color (better visual distinction)
   - Clean legend labels with participant code

### Recently Completed (v0.0.0.9022 - 2025-11-06)
1. ✅ **Fixed category_sum plot labels (initial attempt)**
   - Only join wide_data names for `plot_type == "individual"` (R/plot_practice_curves.R:205)
   - First attempt at aggregated labels (refined in v0.0.0.9023)

2. ✅ **Consolidated version history in home page**
   - Moved all older updates into single collapsible section
   - Cleaner UI with nested collapsible for very old versions

3. ✅ **Updated CLAUDE.md with comprehensive future task roadmap**
   - Added 8 high-priority tasks based on user feedback
   - Organized tasks by priority (High/Medium)
   - Clear descriptions for each enhancement

### Recently Completed (v0.0.0.9021 - 2025-11-06)
1. ✅ **FULLY FIXED individual plot labels in Shiny app**
   - Added `wide_data` parameter to plot call in `inst/shiny/modules/mod_mexp.R:433`
   - Individual plots now correctly show instrument names from `whichinstrument#`, `singingtype#`, `whichothermusic#` columns
   - Tested and confirmed working: "0102SICH - Klavier", "0103ANDE - Geige", etc.

2. ✅ **Implemented clickable module navigation on home page**
   - Updated `inst/shiny/app.R`: Added `id = "main_nav"` and tab values
   - Updated `mod_home_server()`: Accepts parent_session, handles navigation with observeEvent
   - Fixed all onclick handlers to use `Shiny.setInputValue()` instead of querySelector
   - All three module panels (KLAWA, Musical Experience, Merge) now navigate correctly when clicked

3. ✅ **Fixed critical syntax error in mod_home.R**
   - Problem: App wouldn't launch due to unclosed HTML tags (lines 99-143)
   - Solution: Added 3 missing closing parentheses for tags$div, tags$div, and tags$details
   - Collapsible version history now properly renders

4. ✅ **Created collapsible version history structure**
   - Created `version_history_snippet.R` with proper `<details>` and `<summary>` tags
   - Demonstrates clean, nested collapsible sections for version history
   - User can expand/collapse to view specific version details

### Previously Completed (v0.0.0.9020 - 2025-11-06)
1. ✅ **TRULY FIXED plot legend display issues** (after deep debugging)
   - Root cause identified: plotly shows both `color` AND `split` values when they differ (displays as "color<br/>split")
   - Solution: Aligned `group_var` (split parameter) with `category_label_display` (color parameter)
   - Individual plots now correctly show: "0102SICH - Klavier", "0103ANDE - Geige" (actual instrument names)
   - Category_sum plots now correctly show: "0102SICH - Instruments", "0102SICH - Singing" (readable categories)
   - Fixed in `R/plot_practice_curves.R:269-273` by replacing group_var after creating category_label_display
   - Comprehensive tests verify all plot types work correctly

2. ✅ **Enhanced Shiny app home page**
   - Made all module panels clickable - clicking navigates directly to that module
   - Added comprehensive descriptions for each module explaining purpose and use cases
   - Added detailed feature lists for KLAWA, Musical Experience, and Merge modules
   - Included "Click to open" hints on each panel
   - Better user guidance for first-time users

### Previously Completed (v0.0.0.9019 - 2025-11-06)
1. ✅ **Added count variables for musical experiences**
   - `number_of_instruments`: counts number of instruments played per participant (practice hours > 0)
   - `number_of_singing`: counts number of singing experiences per participant
   - `number_of_othermusic`: counts number of other music experiences per participant
   - `nodme` (number of different musical experiences): sum of all three counts
   - All variables automatically computed in `musical_experience_time()` wide output

2. ✅ **Fixed plot legend display issues**
   - Individual plots now correctly show instrument names in legend (e.g., "0102SICH - Klavier")
   - Category_sum plots now show readable category names (e.g., "0102SICH - Instruments")
   - Enhanced `category_label_display` logic with `dplyr::case_when()` for proper name mapping
   - Fixed fallback behavior for cases without instrument name data

### Previously Completed (v0.0.0.9018 - 2025-11-06)
1. ✅ **Fixed KLAWA Data Quality Analysis refresh issue**
   - Fixed duplicate `problems_detailed_rv` reactive declaration in mod_klawa.R
   - Added comprehensive reactive value reset at scan start to ensure fresh data
   - Changed error handling to visible (silent=FALSE) for better debugging
   - Now properly refreshes detailed problem tables on each scan with custom parameters

2. ✅ **Fixed Musical Experience category_sum plotting error**
   - Replaced non-vectorized `.get_category_label()` function call with inline `dplyr::case_when()`
   - Fixed hover_text generation to use `dplyr::if_else()` instead of base `if ()`
   - category_sum plot type now works correctly without errors

3. ✅ **Enhanced plot legends with instrument names**
   - Added `wide_data` parameter to `plot_practice_curves()` function
   - Implemented logic to join instrument/singing/othermusic names from wide format
   - Created `category_label_display` that combines code with instrument name (e.g., "0102SICH - Klavier")
   - Enhanced hover tooltips to show instrument names
   - Legends now display meaningful names instead of generic codes

### Previously Completed (v0.0.0.9011 - 2025-11-05)
1. ✅ **Enhanced practice growth curve plots**
   - Fixed line connections - now properly grouped by participant+instrument using `split` parameter
   - Added support for 3 plot types: "total" (all musical experience), "category_sum" (instruments/singing/other), "individual" (separate lines per instrument)
   - Added per-participant faceting option
   - Improved labels showing actual instrument names (e.g., "instrument1 (Piano)")

2. ✅ **Added total musical experience variables**
   - Computed `instrument_total`, `singing_total`, `othermusic_total` (sum within each category)
   - Computed `total_musical_experience` (grand total across all categories)
   - Variables automatically added to wide format output

3. ✅ **Added IMP (Index of Musical Practice) calculation**
   - Formula: IMP = weekly_hours × years_practiced
   - Computed in long format for each age point
   - Aggregated in wide format as `instrument_imp#`, `singing_imp#`, `othermusic_imp#`

4. ✅ **Improved Shiny UI for Musical Experience**
   - Added comprehensive explanation panel (similar to KLAWA module)
   - Improved category selection with 3 modes: total/category_sum/individual
   - Dynamic instrument labels showing actual names
   - Removed summary tab (cleaned up interface)

5. ✅ **Standardized NA vs 0 handling**
   - Consistently use NA for missing values instead of mixing 0 and NA
   - Proper NA handling in sum calculations

### Previously Completed (v0.0.0.9010 - 2025-11-05)
1. ✅ **Fixed plotly customdata error**
   - Resolved [object Object] error in practice growth curves

### Previously Completed (v0.0.0.9009 - 2025-11-04)
1. ✅ **Added starting age calculation**
   - Computes starting age for each instrument/singing/othermusic (minimum age where practice > 0)
   - Added columns: `instrument#_starting_age`, `singing#_starting_age`, `othermusic#_starting_age`

2. ✅ **Added practice history time windows**
   - New function: `compute_practice_history(long_data, current_age, time_windows)`
   - Computes practice hours within flexible retrospective windows (e.g., last 1, 2, 5, 10 years)

3. ✅ **Added interactive practice growth curves**
   - New function: `plot_practice_curves(long_data, ...)` returns plotly object
   - Full customization: categories, participants, smoothing, faceting, coloring

### Previously Completed (v0.0.0.9007 - 2025-10-31)
1. ✅ **Added date extraction from KLAWA filenames**
   - New `date` column in output (R/klawa.R:373-421)
   - Extracts date from filename and formats as DD/MM/YYYY
   - Smart 2-digit year handling: 00-49 → 20xx, 50-99 → 19xx

2. ✅ **Made date format configurable**
   - Added `date_format` parameter to `klawa_scan()` and `klawa_file_info()`
   - Supports 6 formats: DDMMYY (default), DDMMYYYY, YYMMDD, YYYYMMDD, MMDDYY, MMDDYYYY
   - Automatically parses and standardizes to DD/MM/YYYY output format

3. ✅ **Added date format selector to Shiny UI**
   - Dropdown menu in Advanced Settings (inst/shiny/modules/mod_klawa.R:145-158)
   - Shows example for each format
   - Default: DDMMYY (most common: 080425 → 08/04/2025)

### Previously Completed (v0.0.0.9006 - 2025-10-31)
1. ✅ **Simplified quality reports - removed text, kept only tables**
   - Removed text-based validation and quality summary reports
   - Now shows only detailed problem analysis tables (inst/shiny/modules/mod_klawa.R:510-528)
   - Cleaner, more useful interface - all problems visible at a glance in organized tabs

2. ✅ **Made data table editable**
   - Double-click any cell to edit its value (inst/shiny/modules/mod_klawa.R:477-507)
   - Edits stored automatically in reactive value
   - CSV download includes all edits
   - File column is protected from editing

3. ✅ **Added folder structure diagram and preparation guide**
   - Visual tree diagram showing ideal KLAWA folder organization (inst/shiny/modules/mod_klawa.R:201-264)
   - Explains what KLAWA scan does step-by-step
   - File naming convention clearly documented
   - Located at bottom of KLAWA tab for easy reference

4. ✅ **Fixed folder structure display issue**
   - Added `outputOptions(..., suspendWhenHidden = FALSE)` (inst/shiny/modules/mod_klawa.R:329-330)
   - Folder structure analysis now properly renders in Shiny UI
   - No more console-only output

### Previously Completed (v0.0.0.9005 - 2025-10-31)
1. ✅ **Restored code_pdf column for mismatch detection**
   - Added back `code_pdf` column (R/klawa.R:280-282) - extracts participant code from PDF content
   - Added `code_mismatch` column (R/klawa.R:284-287) - TRUE when filename code differs from PDF code
   - Essential for quality control: detects when files are mislabeled

2. ✅ **Enhanced problem detection for code mismatches**
   - Updated `peek_problems()` function (R/utils.R:173-186) to detect and report code mismatches
   - Code mismatches appear first in problem summary table
   - Detailed tab shows both filename code and PDF code side-by-side for easy comparison

3. ✅ **Improved Shiny UI for problem analysis**
   - Added "Code Mismatches" tab as first tab in detailed problem analysis
   - Shows file, code_filename, and code_pdf columns for quick identification
   - All problem types now properly tracked and displayed

### Previously Completed (v0.0.0.9004 - 2025-10-31)
1. ✅ **Fixed group detection to work with actual folder structure**
   - Completely rewrote detection logic in `klawa_analyze_structure()` (R/klawa.R:45-68)
   - Now detects groups by finding folders that contain measurement subfolders (pre/post)
   - Works with real structure: `<PC>/<GROUP>/<measurement>` or `<GROUP>/<measurement>`
   - Tested with actual data structure: `Rechner Jeki 4/LEN/post/`, `Rechner Jeki 7/LEO/pre/`, etc.

2. ✅ **Removed useless PDF metadata extraction**
   - Removed "both" metadata source option - was generating useless NA columns
   - Removed `klawa_pdf_metadata()` function calls for group/measurement/PC (R/klawa.R:275-309)
   - Simplified to two modes: "path" (recommended) extracts all metadata from folders, "pdf" extracts only codes
   - Updated UI to remove "both" option and clarify what each mode does
   - Removed metadata mismatch handling code (no longer needed)

3. ✅ **Auto-run validation and problem analysis after scan**
   - Removed separate "Validate", "Peek Problems", "Detailed Problems" buttons
   - Validation, problem summary, and detailed analysis now run automatically during scan
   - Progress bar shows each step: scan → validation → quality analysis → detailed problems
   - All reports appear immediately in unified UI (inst/shiny/modules/mod_klawa.R:444-491)
   - Single-click workflow: scan once, see everything

4. ✅ **Improved auto-reports UI**
   - Created unified `auto_reports_ui` that shows all reports in sequence
   - Better visual hierarchy: validation (blue) → problems (orange) → detailed tables (gray)
   - All reports in one scrollable view with proper formatting
   - Removed conditional panels that were causing display issues

### Previously Completed (v0.0.0.9003 and earlier)
- Auto-detection of folder structure, visual folder tree display, customizable patterns, metadata validation, enhanced UI

## Package Philosophy

1. **Data preparation, not analysis**: Focus on extraction and cleaning, not statistical analysis
2. **Robustness over strictness**: Return NA when values missing, don't error
3. **Configuration over hard-coding**: Use options system for patterns that vary
4. **Tidy output**: Always return analysis-ready tibbles
5. **Explicit validation**: Warn about data quality issues but don't block processing
6. **Clear separation**: KLAWA/PPPT/AAT/MusicalExp only; LimeSurvey goes elsewhere
7. **Always use unit testing yourself**
8. **CRITICAL**: After a successful task update this CLAUDE.md file and set the next steps and priorities
9. **CRITICAL**: ALWAYS update inst/shiny/modules/mod_home.R "What's New" section when creating a new version!


