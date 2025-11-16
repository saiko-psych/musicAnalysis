# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**📖 Additional Documentation**:
- **Version History**: See `.claude/memory/VERSION_HISTORY.md` for complete version history
- **GitHub Workflow**: See `docs/GITHUB_WORKFLOW.md` for GitHub CLI project management
- **Project Structure**: See `docs/PROJECT_STRUCTURE.md` for code organization

## Overview

**musicAnalysis** is an R package for music psychology research data preparation at the University of Graz. It focuses on automated data extraction and preparation from four primary sources:

1. **KLAWA PDFs** - Voice/singing performance metrics
2. **Musical Experience** - Practice history and instrument proficiency questionnaires
3. **AAT CSV** - Auditory Ambiguity Test (ambiguous % and control % metrics)
4. **PPPT CSV** - Pitch Perception Proficiency Test (PPP indices across 6 UCF frequency bands)

**Note**: LimeSurvey processing and scale analysis (MDBF, etc.) should NOT be part of this package. Those will be moved to a separate limesurvey-focused package.

## Package Structure

Standard R package layout:
- **R/** - Core package functions (focus on KLAWA, musical experience, PPPT, AAT)
- **inst/shiny/** - Shiny web application for GUI-based data processing
- **tests/testthat/** - Unit tests (139 tests: 60 KLAWA, 31 AAT, 20 merge, 28 utils)
- **.claude/memory/** - Extended documentation (VERSION_HISTORY.md)
- **DESCRIPTION** - Package metadata and dependencies

## Current Status (v0.0.0.9034 - 2025-11-14)

✅ **Recently Improved**:
- PPPT Shiny module UI enhancements
- Fixed data table display issue
- Added "Show R Code" button with download capability
- Enhanced editing and CSV export functionality

## Git Workflow (IMPORTANT!)

### Branch Strategy

**NEVER work directly on `master` branch!** Always use feature branches for development.

```bash
# Current branch structure:
# - master: Production-ready, stable code only
# - dev: Integration branch for completed features
# - feature/*: Individual feature development branches
```

### Standard Development Workflow

1. **Create Feature Branch from `dev`**:
   ```bash
   git checkout dev
   git pull origin dev
   git checkout -b feature/your-feature-name
   ```

2. **Make Changes and Commit**:
   ```bash
   devtools::test()  # Ensure tests pass
   git add -A
   git commit -m "FEAT: Description of changes"
   ```

3. **Push Feature Branch**:
   ```bash
   git push -u origin feature/your-feature-name
   ```

4. **Create Pull Request**:
   ```bash
   gh pr create --base dev --title "Feature: Description" --body "Details..."
   ```

5. **User Reviews PR Manually** (NEVER auto-merge without user approval)

6. **After Approval, Merge to dev**:
   ```bash
   gh pr merge --squash  # Only after user approval!
   ```

7. **When Ready for Release, Merge dev → master**:
   ```bash
   git checkout master
   git pull origin master
   git merge dev
   git push origin master
   ```

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
   Version: 0.0.0.9032  # Increment last number
   ```

2. **ALWAYS UPDATE HOME PAGE** (`inst/shiny/modules/mod_home.R`):
   - Update "What's New" section header with new version number
   - List changes made in this version (Added/Fixed/Improved/Enhanced)
   - Move previous "What's New" entry to "Version History" section
   - Update "Build Date" field with current date
   - **This is MANDATORY for every version release!**

3. **ALWAYS UPDATE README.md**:
   - Update version badge at top
   - Update version history section with new changes
   - Update "Last Updated" and "Version" at bottom
   - Update installation example tarball filename
   - **This is MANDATORY for every version release!**

4. **ALWAYS UPDATE VERSION_HISTORY.md**:
   - Add new version to `.claude/memory/VERSION_HISTORY.md`
   - Document all significant changes
   - **This is MANDATORY for every version release!**

**Version numbering scheme**:
- `0.0.0.9xxx` = Development versions
- Increment last digit for each change (9001, 9002, 9003, etc.)
- This creates unique tarball filenames

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
- `klawa_scan(root)` - Main entry point, recursively scans folder
- `klawa_file_info(rel_file)` - Extracts metadata from path/filename
- `klawa_pdf_values(file)` - Extracts numeric values from PDF text
- `extract_and_check_code(x, pattern)` - Validates participant codes

**Participant code format**: 4 digits + 4 letters (e.g., "1234ABCD")

**Extracted metrics** (configurable via `ma_options()`):
- `volume_difference` - Lautstaerkendifferenz
- `pitch` - Tonhöhe
- `onset_difference` - Tonbeginn Differenz
- `pitch_duration_difference` - Laengendifferenz

### 2. Musical Experience (R/musical_experience*.R)

**Purpose**: Parse and combine musical training history and instrument proficiency data.

**Main function**: `musical_experience(file, ...)` (R/musical_experience.R)
- Combines two sub-parsers: time-based practice data + profile data
- Returns list with `sections`, `wide` (merged tibble), `flags`, `merge_notes`

**Time parser**: `musical_experience_time(file, ...)` (R/musical_experience_time.R)
- Parses complex time string formats: "2d", "1.5w", "1/2w", "3m", "2y"
- Converts to numeric hours (yearly totals)
- Computes starting age for each instrument/singing/othermusic

**Key computed variables**:
- `instrument#_starting_age`, `singing#_starting_age`, `othermusic#_starting_age`
- `number_of_instruments`, `number_of_singing`, `number_of_othermusic`, `nodme`
- `instrument_total`, `singing_total`, `othermusic_total`, `total_musical_experience`
- `instrument_imp#`, `singing_imp#`, `othermusic_imp#` (Index of Musical Practice)

### 3. AAT (Auditory Ambiguity Test) (R/aat.R)

**Purpose**: Extract AAT metrics from CSV response files (*.itl.csv or *.rsl.csv).

**Key functions**:
- `aat_scan(root)` - Main entry point, recursively scans folder for CSV files
- `aat_parse_one(file_path, ...)` - Parses single AAT CSV file
- `aat_analyze_structure(root)` - Analyzes folder organization

**Participant code format**: 4 digits + 4 letters (same as KLAWA)

**Required CSV columns**:
- `Pitch Classification` with codes: 0 (spectral), 1 (f0), 2 (ambivalent), 3 (don't know)
- `Nmin [-]` to identify control vs ambiguous items

**Item type identification** (CRITICAL):
- **Control items**: Nmin has SAME harmonic numbers (e.g., "3 3", "4 4", "5 5")
- **Ambiguous items**: Nmin has DIFFERENT harmonic numbers (e.g., "5 2", "7 3", "9 4")

**Calculated metrics**:
- **Ambiguous (%)**: Percentage of f0-responses (code 1) in ambiguous items
- **Control (%)**: Percentage correct in control items
- **Quality metrics**: Counts of ambivalent (2) and "don't know" (-1) responses

**Calculation logic** (per AAT manual):
- Only codes 0 and 1 are included in percentage calculations
- Codes 2 and -1 are excluded from denominators but counted separately
- Formula: `(count of 1's) / (count of 0's and 1's) * 100`

### 4. PPPT (Pitch Perception Proficiency Test) (R/pppt.R)

**Purpose**: Extract PPP (Pitch Perception Proficiency) indices from PPPT .rsl.csv result files.

**Key functions**:
- `pppt_scan(root)` - Main entry point, recursively scans folder for .rsl.csv files
- `pppt_parse_one(file_path, ...)` - Parses single PPPT CSV file
- `pppt_analyze_structure(root)` - Analyzes folder organization

**Participant code format**: 4 digits + 4 letters (same as KLAWA and AAT)

**Required CSV structure**:
- Must contain "UCF [Hz]" column with values: 294, 523, 932, 1661, 2960, 5274
- Must contain "PPP-Index [-]" column with numeric PPP index values
- Files automatically detected by content (presence of UCF column)

**Extracted metrics**:
- **ppp_294**, **ppp_523**, **ppp_932**, **ppp_1661**, **ppp_2960**, **ppp_5274**: PPP indices for each UCF frequency band
- **ppp_overall**: Overall PPP index across all frequencies
- **date**: Extracted from filename (configurable format)

**Shiny module**: `inst/shiny/modules/mod_pppt.R` provides GUI interface with folder scanning, data editing, and CSV export

## Data Merging (R/merge.R)

**merge_by_code(x, y, suffix)** - Safe left join by participant code:
- Validates `code` column exists in both data frames
- Normalizes codes: trim whitespace, convert empty strings to NA
- Warns about duplicates in `y`
- Returns tibble (never base data.frame)

## Development Priorities

### Current focus (implemented)
1. ✅ KLAWA PDF extraction with robust parsing
2. ✅ Musical experience time and profile processing
3. ✅ Safe merging by participant code
4. ✅ AAT CSV extraction with correct calculations
5. ✅ PPPT CSV extraction with PPP indices for all UCF bands
6. ✅ Configuration system for labels/thresholds

### Next steps (Future enhancements)

#### High Priority
1. ⬜ **KLAWA flexible folder structure with guided setup**
   - Make KLAWA scanning work with non-standard folder structures
   - Add interactive guidance to help users organize their files

2. ⬜ **Musical Experience variable organization**
   - Sort variables in wide and long format logically
   - Group related variables together

3. ⬜ **Variable name cleaning**
   - Remove brackets `[]` and special characters from variable names

4. ⬜ **Variable labels implementation**
   - Add descriptive labels to all variables

5. ⬜ **Plot grouping by variables**
   - Allow plots to be organized/colored by grouping variables

6. ⬜ **Descriptive statistics table for Musical Experience**
   - Add summary statistics display option in Shiny app

7. ⬜ **Version timestamps**
   - Show date and time when each package version was created

8. ⬜ **Show R code buttons**
   - Add "Show R Code" button for CSV parsing and KLAWA scanning

9. ⬜ **Fix top_N and bottom_N participant selection**
   - Should be context-aware based on current plot_type and category selection

#### Medium Priority
10. ⬜ **Musical Experience - Additional useful variables**
11. ⬜ Further validation improvements
12. ⬜ Automatic folder structure maker/file sorting

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
- Wrap all file I/O in `tryCatch()`
- **Prefer returning NA over erroring** when individual values are missing

### Return Types
- **Always return tibbles** (use `tibble::as_tibble()`)
- Multi-part results: return **named lists** with clear component names
- Single data results: return tibble directly

## Package Philosophy

1. **Data preparation, not analysis**: Focus on extraction and cleaning, not statistical analysis
2. **Robustness over strictness**: Return NA when values missing, don't error
3. **Configuration over hard-coding**: Use options system for patterns that vary
4. **Tidy output**: Always return analysis-ready tibbles
5. **Explicit validation**: Warn about data quality issues but don't block processing
6. **Clear separation**: KLAWA/PPPT/AAT/MusicalExp only; LimeSurvey goes elsewhere
7. **Always use unit testing yourself**
8. **CRITICAL**: After a successful task update VERSION_HISTORY.md and set the next steps and priorities
9. **CRITICAL**: ALWAYS update inst/shiny/modules/mod_home.R "What's New" section when creating a new version!

## GitHub Project Management

**Repository**: https://github.com/saiko-psych/musicAnalysis

**Standard workflow**:
1. Create or pick an issue: `gh issue list`
2. Create feature branch: `gh issue develop 5`
3. Make changes and test: `devtools::test()`
4. Commit and push with issue reference: `git commit -m "Fix AAT calculation\n\nCloses #5"`
5. Create PR: `gh pr create --title "Fix AAT calculation" --body "Details..."`
6. Merge when ready: `gh pr merge --squash --delete-branch`

For detailed GitHub CLI guide, see `docs/GITHUB_WORKFLOW.md`.
