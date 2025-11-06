# Archived Functions for Future LimeSurvey Package

This directory contains functions that were removed from the `musicAnalysis` package to maintain focus on KLAWA, PPPT, AAT, and musical experience data preparation.

These functions are intended to be moved to a separate **limesurvey** package for general survey data processing and psychometric scale analysis.

## Contents

### R Functions

1. **limesurvey_pre.R** - LimeSurvey CSV preprocessing
   - Combines multiple CSV exports
   - Handles column name normalization (brackets removal)
   - Deduplicates by participant code
   - Recodes demographic variables
   - Maps group codes to labels

2. **mdbf_scales.R** - MDBF mood questionnaire analysis
   - Computes Cronbach's alpha for subscales
   - Reverse-scores items
   - Calculates scale scores

3. **mdbf_report.R** - Automated MDBF reporting
   - Generates HTML reports with visualizations
   - Reliability tables
   - Inter-item correlations

### Shiny Modules

- **mod_mexp.R** - Musical experience data interface (if LimeSurvey-specific)
- Any other LimeSurvey-related UI modules

## Migration Notes

When creating the new limesurvey package:

1. Copy these files to the new package's `R/` directory
2. Update package dependencies in DESCRIPTION:
   - psych (for reliability analysis)
   - knitr, rmarkdown (for reports)
   - ggplot2 (for visualizations)
3. Create appropriate roxygen2 documentation
4. Add unit tests in `tests/testthat/`
5. Consider generalizing functions to work with any LimeSurvey export, not just music psychology specific

## Dependencies to Add

```r
Imports:
    psych,
    knitr,
    rmarkdown,
    ggplot2,
    tidyr,
    dplyr,
    stringr,
    readr,
    cli,
    rlang
```
