# Version History

This file contains the complete version history for the musicAnalysis package.
Main development documentation is in CLAUDE.md.

## Current Version: v0.0.0.9044 (2025-11-14)

### v0.0.0.9044 (2025-11-14)
- **NEW: Modern, musical-themed UI design**
- Added gradient purple/blue musical banner with musical note decorations
- Replaced plain blue header with modern design
- Added GitHub and email contact links with icons
- Implemented responsive grid layout for version info
- Added hover effects on info cards for better interactivity
- Improved spacing and visual hierarchy throughout home page
- Created GitHub issue #16 for R code generation across all modules
- All PPPT features confirmed working (grouping variable selector already implemented)

### v0.0.0.9043 (2025-11-14)
- **FIXED: R code generation now properly escapes Windows paths**
- Added `escape_for_r()` helper function to handle path escaping
- Windows paths now use double backslashes (e.g., `C:\\Users\\David\\...`)
- Regex patterns now properly escaped (e.g., `\\d{4}` instead of `\d{4}`)
- Generated R code is now directly executable without modification
- Cross-platform compatible: works on Windows, Mac, and Linux
- Updated both "Show R Code" and "Download R Code" sections

### v0.0.0.9042 (2025-11-14)
- **CRITICAL FIX: 95% Confidence Interval now works correctly**
- Fixed `.calculate_error()` function to use `ifelse()` instead of `if` for vectorization
- The `if (n > 30)` statement was causing "condition has length > 1" error
- CI95 now calculates correctly using t-distribution: `t_val * SD / sqrt(n)`
- All error bar types (SE, SD, CI95) now work properly in all plot types
- Overall PPP index markers now automatically match line colors via `legendgroup` in overlaid plots
- Fixed vectorization to handle multiple data points simultaneously

### v0.0.0.9041 (2025-11-14)
- **FIXED: Error bars now calculate correctly (were showing ±0)**
- Fixed error bar calculation to properly compute SE, SD, and CI95
- Changed from hardcoded `se` column to dynamic `error` column based on user selection
- Added UI selector for error bar type with 4 options: SE, SD, 95% CI, or None
- Added helpful descriptions for each error bar type in UI
- Standard Error: SE = SD/√n
- Standard Deviation: SD (spread of data)
- 95% Confidence Interval: uses t-distribution for accurate intervals
- Error bars now display correctly in all mean/summary plots
- Updated hover templates to show error type in parentheses
- R code generation includes error_type parameter

### v0.0.0.9040 (2025-11-14)
- **FIXED: PPPT group summary plot now works correctly**
- Fixed rendering of group_summary plot type in Shiny UI
- Added error bars to overall PPP index markers in overlaid plots
- Error bars now display properly for both frequency bands and overall index
- Improved conditional panel logic for color_by selector (includes group_summary)
- Fixed group_summary to render as single plot instead of multiple plots
- Enhanced error bar implementation in `.create_overlaid_profile_plot()`
- All plot types now properly display error bars when applicable

### v0.0.0.9039 (2025-11-14)
- **ENHANCED: PPPT Visualization with flexible grouping and error bars**
- Added selectable grouping variable from data columns (not hardcoded to "group")
- Added "Group summary" plot type showing mean line per group with error bars
- Added error bars (±SE) to all mean/summary plots (all_combined and group_summary)
- Increased overall PPP index symbol size from 10 to 14 for better visibility
- Error bars shown for both frequency bands and overall index
- Dynamic group variable selector that excludes PPP index columns
- Updated R code generation to include group_var parameter
- Improved plot customization and flexibility

### v0.0.0.9038 (2025-11-14)
- **FIXED: PPPT Visualization UI - buttons and plots now visible**
- Removed outer conditional panel that was hiding plot controls
- Changed "Update Plot" button to "Generate Plot" for clarity
- Removed emoji from "Show R Code" button text for consistency
- Added helpful messages in plot area when no data or plot generated
- Improved user experience to match musical experience module style

### v0.0.0.9037 (2025-11-14)
- **FIXED: PPPT Visualization tab data recognition**
- Fixed conditional panel to properly recognize scanned data from Data Scanning tab
- Added CSV upload option for visualization with format validation
- Added data source selector (scanned vs uploaded)
- Added real-time data validation and status display
- Updated R code generation to handle both scanned and uploaded data sources
- Improved reactive data flow between tabs

### v0.0.0.9036 (2025-11-14)
- **NEW: PPPT Frequency Profile Visualization**
- Added `pppt_plot_profile()` function for creating interactive PPPT profile plots
- Frequency bands (294, 523, 932, 1661, 2960, 5274 Hz) shown as connected lines
- Overall PPP index shown as separate diamond marker (not connected)
- Four plot types: all_combined (mean), all_overlaid, individual, by_group
- Color-by options: participant, group, or custom color
- Added visualization tab in PPPT Shiny module with:
  - Plot type selector
  - Color and legend controls
  - Update Plot button
  - Download Plot (HTML) button
  - Show R Code button for reproducibility
  - Download Code (.R) button
- Helper functions: `.pppt_to_long()`, `.create_single_profile_plot()`, `.create_overlaid_profile_plot()`

### v0.0.0.9035 (2025-11-14)
- **IMPROVED: PPPT Scanner with comprehensive enhancements**
- Added row display selector (10, 25, 50, 100, All)
- Enhanced folder structure analysis showing 4 file type counts
- Added duplicate detection and removal with reporting
- Improved code extraction with fallback strategies
- Added optional group extraction from folder paths
- Created `pppt_validate()` function for data quality checks
- Complete Shiny UI overhaul with validation results

### v0.0.0.9034 (2025-11-14)
- **IMPROVED: PPPT Shiny module UI enhancements**
- Fixed data table display to show properly after scanning
- Added "Show R Code" button to display reproducible scan code
- Added download button for R code (.R file)
- Improved data table with better editing capabilities
- Enhanced UI layout with proper action buttons

### v0.0.0.9033 (2025-11-14)
- **NEW: PPPT (Pitch Perception Proficiency Test) Shiny Module**
- Complete Shiny module for PPPT data extraction (inst/shiny/modules/mod_pppt.R)
- Integration with main app navigation and home page
- Extracts PPP indices for all 6 UCF frequency bands (294, 523, 932, 1661, 2960, 5274 Hz)
- Extracts overall PPP index across all frequencies
- Folder structure analysis and recursive file scanning
- Editable data table with CSV export
- Configurable code pattern and date format recognition
- Wide format output: one row per participant

### v0.0.0.9032 (2025-11-07)
- **FEAT: PPPT data parser implementation (backend only)**
- Core PPPT parsing functions in R/pppt.R
- 46 passing tests for PPPT functionality
- Extracts PPP indices from .rsl.csv files

### v0.0.0.9031 (2025-11-13)
- **FIX: AAT calculation using TONE PAIR aggregation**
- Correct implementation verified with item-level .rsl files
- All 6 test participants match exactly

### v0.0.0.9030 (2025-11-13)
- **FIX: AAT calculations reverted to match .rsl item-level format**
- Calculations now verified correct against item-level data

### v0.0.0.9029 (2025-11-07)
- **AAT CRITICAL FIX: Correct identification of control vs ambiguous items**
- Uses Nmin [-] column pattern (SAME harmonic numbers = control, DIFFERENT = ambiguous)
- 100% match verified for all 6 participants with item-level .rsl files
- Updated 28 tests to use correct Nmin pattern

### v0.0.0.9028 (2025-11-07)
- **AAT VERIFICATION: Understanding summary .rsl vs item-level .rsl**
- Summary .rsl files: Pre-calculated percentages from AAT software (cannot reproduce exactly)
- Item-level .rsl files: Raw data that matches .itl calculations perfectly
- My .itl calculations ARE correct (proven by item-level .rsl matches)

### v0.0.0.9027 (2025-11-07)
- **AAT Complete Variable Extraction**
- Added ALL variables from .rsl summary format: `a_tone_pairs`, `c_tone_pairs`, `a_avg_items_per_pair`, `c_avg_items_per_pair`
- Output expanded from 10 columns to 14 columns

### v0.0.0.9026 (2025-11-06)
- **Fixed AAT Parsing for Real Data Structure**
- Fixed .rsl file parsing to correctly extract from summary format
- Fixed .itl file parsing to handle column names with suffixes
- AAT Home Page Integration with purple theme
- Updated All AAT Tests (26 tests passing)

### v0.0.0.9025 (2025-11-06)
- **AAT Module Refinements for Real Data**
- Filename filtering: Only processes files containing "AAT" in filename
- Automatic file type detection: .itl.csv (raw) vs .rsl.csv (computed results)
- Metadata extraction: participant code and date from filenames

### v0.0.0.9024 (2025-11-06)
- **NEW: AAT (Auditory Ambiguity Test) Module Implemented**
- Created R/aat.R with full CSV parsing functionality
- AAT Shiny Module with editable data tables
- GitHub Security Improvements - removed private test data
- GitHub Issue Management - 13 issues created

### v0.0.0.9023 (2025-11-06)
- **FINAL FIX for category_sum plot labels and colors**
- Each participant gets unique color with proper label format
- Labels: "<PARTICIPANT> - total Instruments", "<PARTICIPANT> - total Singing"

### v0.0.0.9022 (2025-11-06)
- **Fixed category_sum plot labels (initial attempt)**
- Consolidated version history in home page
- Updated CLAUDE.md with comprehensive future task roadmap

### v0.0.0.9021 (2025-11-06)
- **FULLY FIXED individual plot labels in Shiny app**
- Implemented clickable module navigation on home page
- Fixed critical syntax error in mod_home.R
- Created collapsible version history structure

### v0.0.0.9020 (2025-11-06)
- **TRULY FIXED plot legend display issues**
- Enhanced Shiny app home page with module descriptions

### v0.0.0.9019 (2025-11-06)
- **Added count variables for musical experiences**
- `number_of_instruments`, `number_of_singing`, `number_of_othermusic`, `nodme`
- Fixed plot legend display issues with instrument names

### v0.0.0.9018 (2025-11-06)
- **Fixed KLAWA Data Quality Analysis refresh issue**
- Fixed Musical Experience category_sum plotting error
- Enhanced plot legends with instrument names

### v0.0.0.9011 (2025-11-05)
- **Enhanced practice growth curve plots**
- Added total musical experience variables
- Added IMP (Index of Musical Practice) calculation
- Improved Shiny UI for Musical Experience
- Standardized NA vs 0 handling

### v0.0.0.9010 (2025-11-05)
- **Fixed plotly customdata error**

### v0.0.0.9009 (2025-11-04)
- **Added starting age calculation**
- Added practice history time windows
- Added interactive practice growth curves

### v0.0.0.9007 (2025-10-31)
- **Added date extraction from KLAWA filenames**
- Made date format configurable (6 formats supported)
- Added date format selector to Shiny UI

### v0.0.0.9006 (2025-10-31)
- **Simplified quality reports - tables only**
- Made data table editable (double-click cells)
- Added folder structure diagram
- Fixed folder structure display issue

### v0.0.0.9005 (2025-10-31)
- **Restored code_pdf column for mismatch detection**
- Enhanced problem detection for code mismatches
- Improved Shiny UI for problem analysis

### v0.0.0.9004 (2025-10-31)
- **Fixed group detection to work with actual folder structure**
- Removed useless PDF metadata extraction
- Auto-run validation and problem analysis after scan
- Improved auto-reports UI

### v0.0.0.9003 and earlier
- Auto-detection of folder structure
- Visual folder tree display
- Customizable patterns
- Metadata validation
- Enhanced UI

## GitHub Setup (2025-11-06)
- Set up GitHub CLI project management
- Repository: https://github.com/saiko-psych/musicAnalysis
- Created comprehensive README.md
- Created create_github_issues.sh script
- Documented GitHub workflow
