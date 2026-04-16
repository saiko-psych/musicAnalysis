# musicAnalysis

> Music Psychology Data Preparation for University of Graz

[![R >= 4.4.0](https://img.shields.io/badge/R-%3E%3D4.4.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-0.0.0.9077-green.svg)](https://github.com/saiko-psych/musicAnalysis)
[![Tests](https://img.shields.io/badge/tests-189%20passing-brightgreen.svg)]()
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20macOS%20%7C%20Linux-lightgrey.svg)]()

---

An R package for extracting and preparing music psychology research data. Includes a Shiny web application for interactive workflows.

## Data Sources

| Source | Input | Output |
|--------|-------|--------|
| **KLAWA** | PDF test reports (hierarchical folders) | Volume, pitch, onset, duration metrics |
| **Musical Experience** | LimeSurvey CSV | Practice hours, starting ages, IMP, profile variables |
| **AAT** | .rsl.csv / .itl.csv | Ambiguous %, Control %, quality metrics |
| **PPPT** | .rsl.csv | PPP indices across 6 UCF frequency bands |

## Installation

```r
# From GitHub
devtools::install_github("saiko-psych/musicAnalysis")

# From local tarball
install.packages("musicAnalysis_0.0.0.9077.tar.gz", repos = NULL, type = "source")
```

## Quick Start

```r
library(musicAnalysis)

# Launch the Shiny app (recommended)
launch_app()

# Or use programmatic functions:
klawa_data <- klawa_scan("path/to/KLAWA")
music_exp  <- musical_experience("path/to/survey.csv")
aat_data   <- aat_scan("path/to/AAT")
pppt_data  <- pppt_scan("path/to/PPPT")
```

## Shiny Application

The interactive web app provides GUI-based workflows for all data sources:

- **KLAWA** -- Folder scanning, PDF extraction, quality reports, visualizations
- **Musical Experience** -- CSV parsing, practice growth curves, profile data, descriptive statistics, SPSS/Stata export
- **AAT** -- File type detection, configurable thresholds, quality categorization
- **PPPT** -- PPP profile plots, frequency band visualization
- **Merge** -- Safe joining by participant code with duplicate detection

## Testing

```r
devtools::test()  # 189 tests
```

## Documentation

- Function docs: `?klawa_scan`, `?musical_experience`, `?aat_scan`, `?pppt_scan`
- Version history: `.claude/memory/VERSION_HISTORY.md`
- Development guide: `CLAUDE.md`

## Contributing

Bug reports and feature requests: [GitHub Issues](https://github.com/saiko-psych/musicAnalysis/issues)

## License

MIT -- see [LICENSE](LICENSE.md)

## Author

**David Matischek** -- [david.matischek@uni-graz.at](mailto:david.matischek@uni-graz.at)
University of Graz, Music Psychology Research

---

Version 0.0.0.9077 | Last updated 2026-04-16
