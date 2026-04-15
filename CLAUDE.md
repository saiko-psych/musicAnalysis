# CLAUDE.md

This file provides essential guidance to Claude Code when working with this repository.

**📖 Documentation**:
- **Workflow Rules**: See `.claude/WORKFLOW.md` for mandatory workflow guidelines
- **Version History**: See `.claude/memory/VERSION_HISTORY.md` for complete version history
- **Archived Docs**: See `.claude/archive/` for historical documentation

## Overview

**musicAnalysis** is an R package for music psychology research data preparation at the University of Graz.

**Data Sources**:
1. **KLAWA PDFs** - Voice/singing performance metrics
2. **Musical Experience** - Practice history and instrument proficiency questionnaires
3. **AAT CSV** - Auditory Ambiguity Test (ambiguous % and control % metrics)
4. **PPPT CSV** - Pitch Perception Proficiency Test (PPP indices across 6 UCF frequency bands)

**Current Version**: 0.0.0.9076 (2026-04-14)

## Git Workflow

**Work directly on `master` branch** - No feature branches unless multi-week complex work.

```bash
# Standard workflow
git add -A
git commit -m "TYPE: Description"
git push origin master

# Commit types: FEAT, FIX, HOTFIX, DOCS, VERSION, REFACTOR
```

## Versioning System (CRITICAL!)

**⚠️ MANDATORY STEPS - DO NOT SKIP:**

1. **Update DESCRIPTION**:
   ```
   Version: 0.0.0.9XXX  # Increment XXX
   ```

2. **Update mod_home.R "What's New"**:
   - Update header with new version number
   - List changes (Added/Fixed/Improved/Enhanced)
   - Move previous "What's New" to "Version History" section
   - Update "Build Date" field

3. **Update README.md**:
   - Version badge at top
   - Version history section
   - Installation tarball filename
   - "Last Updated" and "Version" at bottom

4. **Update VERSION_HISTORY.md**:
   - Add new version entry
   - Document all significant changes

4b. **Update CLAUDE.md**:
    - Update "Current Version" field

5. **Build Package**:
   ```r
   devtools::build()
   ```

## Environment

- **R executable**: `"C:/Program Files/R/R-4.4.1/bin/Rscript.exe"` (not in PATH)
- **Shiny app starten**: `nohup Rscript tests/manual/test_start_app.R > /tmp/shiny_app.log 2>&1 &` (Port 7654)
- **WICHTIG**: `devtools::install('.', quick=TRUE)` VOR Shiny-Tests — `app.R` nutzt `system.file()` (installiertes Paket)
- **Playwright**: Kann nur Dateien innerhalb des Projektordners hochladen — Testdaten aus `tests/testdata_*` verwenden

## Shiny Gotchas

- **`renderUI`/`renderDT` in conditionalPanel oder hidden Tabs**: IMMER `outputOptions(output, "id", suspendWhenHidden = FALSE)` setzen, sonst bleibt Output leer
- **AAT Terminologie**: "ambiguous" = Item-Typ (Stimulus), "ambivalent" = Antwort-Typ (code=2) — NICHT verwechseln
- **Musical Experience**: `musical_experience()` (nicht `_time()`) liefert merged Time+Profile; Shiny nutzt `$wide`, `$sections$time$long`, `$sections$profile`, `$flags`

## Core Architecture

### 1. KLAWA (R/klawa.R)
- Scans hierarchical folders: `Computer/<PC>/Gruppen/<GROUP>/<MEASUREMENT>/*.pdf`
- Extracts: volume_difference, pitch, onset_difference, pitch_duration_difference
- Participant code format: 4 digits + 4 letters (e.g., "1234ABCD")

### 2. Musical Experience (R/musical_experience*.R)
- Parses time formats: "2d", "1.5w", "3m", "2y"
- Computes: starting ages, IMP (Index of Musical Practice), total hours
- Returns: `wide` (analysis), `long` (plotting), `flags`, `merge_notes`

### 3. AAT (R/aat.R)
- Scans `.rsl.csv` and `.itl.csv` files
- Extracts: Ambiguous %, Control %, quality metrics
- Item type detection: Same Nmin = control, Different Nmin = ambiguous

### 4. PPPT (R/pppt.R)
- Scans `.rsl.csv` files
- Extracts PPP indices for UCF bands: 294, 523, 932, 1661, 2960, 5274 Hz
- Calculates overall PPP index

## Development Priorities

### Current Focus
1. ✅ KLAWA, Musical Experience, AAT, PPPT extraction working
2. ✅ Shiny modules for all data sources
3. ✅ Show R Code buttons implemented
4. ✅ UI polish complete (brown theme, transparent ASCII art)

### Status
All planned features through v0.0.0.9075 are complete.
See `.claude/memory/VERSION_HISTORY.md` for detailed history.

## Code Style

- **Functions**: snake_case
- **Internal helpers**: prefix with `.`
- **Always return tibbles** (not data.frames)
- **Use rlang::abort()** for errors
- **Prefer NA over errors** for missing values
- **Document with roxygen2**
- **Write unit tests** for all functions

## Testing

```r
# Run all tests (189 tests)
devtools::test()

# Quick visual test
source("tests/manual/test_app_visual.R")

# Build verification
source("tests/manual/test_package_build.R")
```

## Package Philosophy

1. **Data preparation, not analysis** - Extract and clean, don't analyze
2. **Robustness over strictness** - Return NA when missing, don't error
3. **Configuration over hard-coding** - Use options system
4. **Tidy output** - Always return analysis-ready tibbles
5. **Explicit validation** - Warn about issues, don't block
6. **Clear separation** - KLAWA/PPPT/AAT/MusicalExp only
7. **Always test yourself** - Use test scripts
8. **Update docs after tasks** - Keep VERSION_HISTORY.md current

## Critical Rules for Claude Code

1. **ALWAYS use TodoWrite** for non-trivial tasks
2. **ALWAYS launch app** to verify UI changes visually
3. **ALWAYS increment version** after completing tasks
4. **ALWAYS update mod_home.R** when creating new versions
5. **ALWAYS build package** and deliver .tar.gz
6. **NEVER assume** - Verify everything works
7. **NO SHITTY WORK** - Quality over speed

See `.claude/WORKFLOW.md` for detailed workflow rules.
