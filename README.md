# musicAnalysis

> **Music Psychology Data Preparation for University of Graz**

An R package for automated data extraction and preparation from music psychology research sources, designed for the KF-Graz research team.

[![R](https://img.shields.io/badge/R-%3E%3D4.4.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-0.0.0.9073-green.svg)](https://github.com/saiko-psych/musicAnalysis/releases)

---

## 📋 Table of Contents

- [Overview](#-overview)
- [Features](#-features)
- [Installation](#-installation)
- [Quick Start](#-quick-start)
- [Data Sources](#-data-sources)
- [Usage Examples](#-usage-examples)
- [Shiny Web Application](#-shiny-web-application)
- [Development](#-development)
- [Documentation](#-documentation)
- [Version History](#-version-history)
- [Roadmap](#-roadmap)
- [Contributing](#-contributing)
- [License](#-license)

---

## 🎯 Overview

**musicAnalysis** automates the tedious process of extracting and preparing music psychology research data from multiple sources. It handles:

- 📄 **PDF extraction** from KLAWA voice/singing assessments
- 🎵 **Musical experience questionnaires** with practice history
- 🔗 **Data merging** by participant codes
- 📊 **Interactive visualizations** of practice curves over age
- ✅ **Data quality validation** with detailed reports

The package includes both **programmatic functions** (for R scripts) and a **Shiny web application** (for GUI-based workflows).

---

## ✨ Features

### KLAWA PDF Processing
- ✅ Automatic hierarchical folder scanning (Computer/PC/Group/Measurement)
- ✅ Extracts voice metrics: volume, pitch, onset, pitch duration
- ✅ Robust multi-stage PDF parsing (handles formatting variations)
- ✅ Participant code validation and conflict detection
- ✅ Date extraction from filenames with configurable formats
- ✅ Comprehensive data quality reports

### Musical Experience Analysis
- ✅ Parses complex time formats (days, weeks, months, years)
- ✅ Converts to yearly practice hours with validation
- ✅ Calculates starting ages for each instrument/singing/othermusic
- ✅ Computes IMP (Index of Musical Practice)
- ✅ Tracks practice history within flexible time windows
- ✅ Interactive plotly growth curves with multiple plot types

### Data Management
- ✅ Safe merging by participant codes with duplicate detection
- ✅ Missing value analysis and NA-inflation reports
- ✅ Editable data tables in Shiny interface
- ✅ CSV export with all modifications preserved

### Visualization
- ✅ Three plot types: total, category_sum, individual
- ✅ Context-aware participant selection (top N, bottom N, random N)
- ✅ Interactive hover tooltips with detailed information
- ✅ Customizable color schemes and faceting options
- ✅ Download plots as interactive HTML files

---

## 📦 Installation

### From GitHub (Latest Development Version)

```r
# Install devtools if needed
install.packages("devtools")

# Install from GitHub
devtools::install_github("saiko-psych/musicAnalysis")
```

### From Local Tarball

```r
# Download the latest release tarball
# Then install locally:
install.packages("path/to/musicAnalysis_0.0.0.9073.tar.gz",
                 repos = NULL, type = "source")
```

### Dependencies

The package automatically installs required dependencies:

```r
# Core data manipulation
dplyr, tidyr, tibble, purrr, stringr, magrittr, rlang

# File I/O
readr, fs, pdftools

# Shiny interface
shiny, bslib, DT, shinyFiles, plotly

# Utilities
cli
```

---

## 🚀 Quick Start

```r
library(musicAnalysis)

# Option 1: Launch the Shiny web application (recommended for beginners)
launch_app()

# Option 2: Use programmatic functions in R scripts

# Process KLAWA PDFs
klawa_data <- klawa_scan("data/KLAWA")

# Process Musical Experience questionnaire
music_exp <- musical_experience("data/musical_experience.csv")

# Merge datasets
final_data <- klawa_data %>%
  merge_by_code(music_exp$wide)

# Create interactive practice curves
plot_practice_curves(
  music_exp$long,
  wide_data = music_exp$wide,
  plot_type = "category_sum",
  n_participants = 10
)
```

---

## 📊 Data Sources

### 1. KLAWA (Voice/Singing Performance)

**Expected folder structure**:
```
data/KLAWA/
  Computer/
    <PC>/
      Gruppen/
        <GROUP>/
          <MEASUREMENT>/
            <CODE>_<MEASUREMENT>_<DATE>.pdf
            <CODE>_<MEASUREMENT>_<DATE>.pdf
          <MEASUREMENT>/
            <CODE>_<MEASUREMENT>_<DATE>.pdf
```

**Example**:
```
data/KLAWA/Computer/PC1/Gruppen/GroupA/pre/1234ABCD_pre_080425.pdf
```

**Extracted metrics**:
- Volume difference (Lautstärkendifferenz)
- Pitch (Tonhöhe)
- Onset difference (Tonbeginn-Differenz)
- Pitch duration difference (Längendifferenz)

### 2. Musical Experience (Questionnaires)

**CSV format with columns**:
- `code`: Participant code (e.g., "0102SICH")
- `age`: Current age
- `instrument1_hours_*`, `instrument2_hours_*`, etc.: Practice hours at different ages
- `whichinstrument1`, `whichinstrument2`: Instrument names
- `singing1_hours_*`, `singing2_hours_*`: Singing practice hours
- `singingtype1`, `singingtype2`: Type of singing (choir, solo, etc.)
- `othermusic1_hours_*`: Other musical activities

**Time format support**: `2d`, `1.5w`, `1/2w`, `3m`, `2y` (days, weeks, months, years)

### 3. AAT (Auditory Ambiguity Test)

**Expected files**:
- `.rsl.csv` files: Computed results with ambiguous % and control %
- `.itl.csv` files: Raw response data with pitch classifications

**Extracted metrics**:
- **Ambiguous %**: Percentage of f0-responses in ambiguous items
- **Control %**: Percentage correct in control items
- **Quality metrics**: Ambivalent responses, "don't know" responses

**Shiny module**: `inst/shiny/modules/mod_aat.R` provides GUI interface with folder scanning, data editing, and CSV export

### 4. PPPT (Pitch Perception Proficiency Test)

**Expected files**:
- `.rsl.csv` files containing PPP indices across UCF frequency bands

**Extracted metrics**:
- **PPP indices** for UCF bands: 294 Hz, 523 Hz, 932 Hz, 1661 Hz, 2960 Hz, 5274 Hz
- **Overall PPP index** across all frequencies
- **Date extraction** from filenames (configurable format)

**Shiny module**: `inst/shiny/modules/mod_pppt.R` provides complete GUI with plotting, editing, and export features

---

## 💻 Usage Examples

### Example 1: KLAWA Processing with Quality Checks

```r
library(musicAnalysis)
library(dplyr)

# Scan KLAWA PDFs with custom label patterns
set_ma_options(labels = list(
  pitch = "(Tonhöhe|Pitch|F0|Grundfrequenz)",
  volume = "Lautst.*differenz"
))

klawa_data <- klawa_scan(
  root = "data/KLAWA",
  date_format = "DDMMYY"  # 080425 → 08/04/2025
)

# Filter and inspect
klawa_post <- klawa_data %>%
  filter(measurement == "post", !is.na(code))

# Check for issues
conflicts <- klawa_data %>%
  filter(code == "CODE_CONFLICT")

missing_values <- klawa_data %>%
  filter(is.na(volume_difference) | is.na(pitch))
```

### Example 2: Musical Experience Analysis

```r
# Parse musical experience with validation
music_exp <- musical_experience(
  file = "data/music_survey.csv",
  profile_select_regex = "^(music_status|main_instrument)",
  validate = TRUE
)

# Access components
music_exp$wide           # For statistical analysis
music_exp$long           # For plotting over age
music_exp$flags          # Validation warnings (unrealistic hours, etc.)
music_exp$merge_notes    # NA-inflation report

# Calculate practice history
library(dplyr)
recent_practice <- compute_practice_history(
  music_exp$long,
  time_windows = c(1, 2, 5, 10)  # Last 1, 2, 5, 10 years
)

# Summary statistics
music_exp$wide %>%
  summarise(
    mean_instrument_total = mean(instrument_total, na.rm = TRUE),
    mean_singing_total = mean(singing_total, na.rm = TRUE),
    mean_IMP = mean(IMP_total, na.rm = TRUE)
  )
```

### Example 3: Interactive Visualizations

```r
# Top 10 participants by total musical experience
plot_practice_curves(
  music_exp$long,
  wide_data = music_exp$wide,
  plot_type = "total",
  n_participants = 10,
  subset_by = "highest",
  smooth = TRUE,
  title = "Top 10 Musicians: Total Practice Over Age"
)

# Category comparison (instruments vs singing vs other)
plot_practice_curves(
  music_exp$long,
  wide_data = music_exp$wide,
  plot_type = "category_sum",
  categories = c("instrument", "singing"),
  n_participants = 20
)

# Individual instrument trajectories
plot_practice_curves(
  music_exp$long,
  wide_data = music_exp$wide,
  plot_type = "individual",
  categories = "instrument",
  category_ids = c(1, 2),  # First two instruments
  codes = c("0102SICH", "0103ANDE", "0104TEST")
)
```

### Example 4: Complete Analysis Pipeline

```r
library(musicAnalysis)
library(tidyverse)

# 1. Load all data sources
klawa <- klawa_scan("data/KLAWA")
music <- musical_experience("data/music.csv", validate = TRUE)

# 2. Filter and prepare
klawa_clean <- klawa %>%
  filter(measurement == "post", !is.na(code), code != "CODE_CONFLICT")

# 3. Merge datasets
combined <- klawa_clean %>%
  merge_by_code(music$wide) %>%
  filter(!is.na(pitch), !is.na(instrument_total))

# 4. Analysis
model <- lm(pitch ~ instrument_total + age, data = combined)
summary(model)

# 5. Visualization
combined %>%
  ggplot(aes(x = instrument_total, y = pitch, color = group)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Pitch Performance vs. Musical Training",
       x = "Total Instrument Practice Hours",
       y = "Pitch Score") +
  theme_minimal()

# 6. Export results
write_csv(combined, "results/combined_data.csv")
```

---

## 🖥️ Shiny Web Application

Launch the interactive GUI:

```r
library(musicAnalysis)
launch_app()
```

### Features

**Home Tab**:
- 📌 Version information and what's new
- 📜 Collapsible version history
- 📖 Module descriptions with clickable navigation

**KLAWA Tab**:
- 📁 Interactive folder browser
- ⚙️ Advanced settings (metadata source, date format, label patterns)
- 📊 Real-time progress indicators
- ✅ Automatic validation and quality reports
- ✏️ Editable data table (double-click cells)
- 💾 CSV export with modifications

**Musical Experience Tab**:
- 📂 CSV file upload
- 📈 Interactive practice growth curves (plotly)
- 🎯 Three plot types: total, category_sum, individual
- 🎨 Customizable filters: categories, instruments, participants
- 📊 Context-aware participant selection (top N, bottom N)
- 💾 Download plots as HTML
- 📋 Data table view with summary statistics
- 📝 Show R Code button with copy/download functionality

**AAT Tab**:
- 📁 Interactive folder browser for AAT data
- 🔍 Automatic detection of .rsl.csv and .itl.csv files
- ⚙️ Date format configuration
- ✏️ Editable data table
- 💾 CSV export with modifications
- 📝 Show R Code for reproducible analysis

**PPPT Tab**:
- 📁 Folder scanning for PPPT .rsl.csv files
- 📊 PPP profile plots across UCF frequency bands
- 🎨 Individual and overlaid profile visualizations
- ⚙️ Date format configuration
- ✏️ Editable data table
- 💾 CSV export
- 📝 Show R Code button

**Merge Tab**:
- 🔗 Merge multiple datasets by participant code
- ⚠️ Duplicate detection and warnings
- 📊 Preview merged data
- 💾 Export merged results

---

## 🛠️ Development

### Project Structure

```
musicAnalysis/
├── R/                          # Core functions
│   ├── klawa.R                # KLAWA PDF extraction
│   ├── musical_experience*.R  # Musical experience parsing
│   ├── plot_practice_curves.R # Interactive plotting
│   ├── merge.R                # Data merging
│   ├── options.R              # Configuration
│   └── utils.R                # Helpers
│
├── inst/shiny/                # Shiny application
│   ├── app.R
│   ├── www/                   # Static assets (music sheet background)
│   └── modules/
│       ├── mod_home.R
│       ├── mod_klawa.R
│       ├── mod_mexp.R
│       ├── mod_aat.R
│       ├── mod_pppt.R
│       └── mod_merge.R
│
├── tests/testthat/            # Unit tests (139 tests)
│   ├── test-klawa.R
│   ├── test-aat.R
│   ├── test-merge.R
│   └── test-utils.R
│
└── docs/                      # Documentation
    ├── CLAUDE.md              # Development guidelines
    ├── GITHUB_WORKFLOW.md     # GitHub CLI guide
    └── PROJECT_STRUCTURE.md   # Code organization
```

### Development Workflow

```r
# Load package for development
devtools::load_all()

# Run tests
devtools::test()  # Currently 139 tests: 60 KLAWA, 31 AAT, 20 merge, 28 utils

# Update documentation
devtools::document()

# Check package
devtools::check()

# Build tarball
pkgbuild::build(path = ".")
```

### Running Tests

```bash
# All tests
Rscript -e "devtools::test()"

# Specific test file
Rscript -e "testthat::test_file('tests/testthat/test-klawa.R')"

# With coverage report
Rscript -e "covr::package_coverage()"
```

---

## 📚 Documentation

### For Users
- **This README**: Overview and usage examples
- **Function documentation**: `?klawa_scan`, `?musical_experience`, etc.
- **Shiny app help**: Built-in tooltips and explanations

### For Developers
- **`docs/CLAUDE.md`**: Development guidelines, priorities, version history
- **`docs/GITHUB_WORKFLOW.md`**: Complete GitHub CLI guide for project management
- **`docs/PROJECT_STRUCTURE.md`**: Code organization and optimization tips

### Configuration

```r
# View current settings
ma_options()

# Customize label patterns for KLAWA
set_ma_options(labels = list(
  pitch = "(Tonhöhe|Pitch|F0|Grundfrequenz)",
  volume = "Lautst.*differenz",
  onset = "Tonbeginn.*Diff"
))

# Set validation thresholds for musical experience
set_ma_options(thresholds = list(
  daily_max_hours = 12,  # Max realistic practice per day
  weekly_max_hours = 60
))
```

---

## 📋 Version History

### v0.0.0.9072 (2026-04-13) - Latest
- ✅ **FIXED**: AAT terminology corrected ("ambivalent" for response type)
- ✅ **REFACTORED**: Quality tables use shared reactive expressions
- ✅ **ADDED**: shinytest2 regression test for AAT module
- ✅ Heading hierarchy, spacing, and dead code cleanup

### v0.0.0.9063-9070 (2025-11-17 to 2025-11-19)
- ✅ PPPT module fully implemented with PPP indices across 6 UCF bands
- ✅ AAT folder tree, file type selection, quality categorization
- ✅ Configurable quality thresholds and dynamic descriptions
- ✅ Row display selectors for all data tables

### v0.0.0.9062 (2025-11-16)
- ✅ **HOTFIX**: ASCII art background now properly transparent
- ✅ Enhanced CSS specificity to override Bootstrap 3 defaults

### v0.0.0.9061 (2025-11-16)
- ✅ Attempted ASCII background transparency fix
- ✅ CSS selector improvements

### v0.0.0.9060 (2025-11-16)
- ✅ All links underlined for better visibility
- ✅ Turquoise colors replaced with vintage brown theme (#8B7355)
- ✅ Summary elements ("Advanced Settings") now bold, underlined, clickable
- ✅ Version info styling matches brown theme

### v0.0.0.9058-9059 (2025-11-16)
- ✅ UI polish: emoji removal, button sizing, navbar fixes
- ✅ Font Awesome icons replace all emojis
- ✅ Compact navbar (40px height)

### v0.0.0.9034 (2025-11-14)
- ✅ PPPT Shiny module enhancements
- ✅ Show R Code buttons with download capability
- ✅ Enhanced data table editing

### Earlier Versions
See `.claude/memory/VERSION_HISTORY.md` for complete version history.

---

## 🎯 Roadmap

### High Priority
1. **KLAWA flexible folder structure** - Guided setup for non-standard structures
2. **Musical Experience variable organization** - Logical sorting in wide/long formats
3. **Variable name cleaning** - Remove brackets `[]` and special characters
4. **Variable labels** - Descriptive labels for all variables
5. **Plot grouping** - Allow grouping by variables in visualizations

### Medium Priority
- **Descriptive statistics table** - Summary stats for Musical Experience
- **Version timestamps** - Show date/time for each version
- **Fix top_N/bottom_N selection** - Context-aware based on plot_type
- **Additional useful variables** - Expand Musical Experience metrics

### Completed
- ✅ AAT module with complete workflow
- ✅ PPPT module with profile plots
- ✅ Show R Code buttons across all modules
- ✅ Transparent ASCII art background
- ✅ Brown vintage theme throughout app

See `CLAUDE.md` for detailed development guidelines.

---

## 🤝 Contributing

This package is primarily for internal use at University of Graz (KF-Graz research team). However, suggestions and bug reports are welcome!

### Reporting Issues

```bash
# Using GitHub CLI
gh issue create --title "Bug: Issue description" \
  --label bug \
  --body "Detailed description..."

# Or via web interface
https://github.com/saiko-psych/musicAnalysis/issues
```

### Development Workflow

See `docs/GITHUB_WORKFLOW.md` for complete guide:

```bash
# 1. Create issue
gh issue create --title "Feature description"

# 2. Create feature branch
gh issue develop {issue_number}

# 3. Make changes, test, commit
devtools::test()
git commit -m "Implement feature"

# 4. Create pull request
gh pr create

# 5. Merge when ready
gh pr merge --squash
```

---

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

---

## 👥 Authors

**David Matischek**
Music Psychology Research, University of Graz
📧 david.matischek@uni-graz.at

---

## 🙏 Acknowledgments

- University of Graz (KF-Graz) music psychology research team
- All contributors and testers
- The R community for excellent packages (tidyverse, shiny, plotly)

---

## 📞 Support

For questions, issues, or feature requests:

1. Check the documentation: `docs/` folder
2. Search existing issues: `gh issue list`
3. Create a new issue: `gh issue create`
4. Contact: david.matischek@uni-graz.at

---

**Last Updated**: 2026-04-13
**Version**: 0.0.0.9073
**Status**: Active Development 🚧
