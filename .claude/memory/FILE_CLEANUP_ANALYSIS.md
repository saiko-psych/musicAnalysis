# File Cleanup Analysis - musicAnalysis Package

**Date**: 2025-11-13
**Analyzed by**: Claude Code

## Summary

Found several redundant/outdated files that can be cleaned up:

### 🗑️ FILES TO DELETE (Safe to remove)

#### Root Test Files (Development artifacts - NOT needed)
All these are temporary debugging scripts that should be deleted:
- `test_aat_formula.R` - AAT debugging script (outdated)
- `test_all_aat_files.R` - AAT testing script (outdated)
- `test_debug.R` - General debugging script
- `test_debug2.R` - General debugging script
- `test_debug_filtering.R` - Filtering debug script
- `test_final_aat.R` - AAT testing script (outdated)
- `test_manual_aat.R` - AAT manual testing script
- `test_rsl_calc.R` - RSL calculation testing script
- `test_single_file.R` - Single file testing script
- `test_installed.R` - Installation test script

**Why delete**: These are development debugging scripts. Real tests are in `tests/testthat/`

#### Root Documentation File
- `shiny_ui_guide.md` - Shiny UI tips (contains navigation fix notes)

**Action**: Move useful content to `docs/` or delete if outdated

### 📁 FILES TO CONSOLIDATE

#### docs/DEVELOPMENT_GUIDELINES.md → CLAUDE.md
**Content**: Code standards, naming conventions, function design
**Status**: REDUNDANT - Already in CLAUDE.md "Code Style and Conventions" section
**Action**: DELETE (content already in CLAUDE.md)

#### docs/QUICK_START.md → README.md
**Content**: Setup instructions, basic workflow examples
**Status**: PARTIALLY REDUNDANT - Better as part of README.md
**Action**: MERGE into README.md or DELETE if outdated

#### docs/OVERFLOW.md
**Content**: Project overview, core components description
**Status**: OUTDATED - Describes old structure, AAT as "to be implemented" (now complete)
**Action**: DELETE (information outdated and covered in CLAUDE.md)

#### docs/TEMPLATE_R_CLAUDE.md
**Content**: GitHub CLI commands, build/test commands
**Status**: REDUNDANT - Commands already documented in GITHUB_WORKFLOW.md
**Action**: DELETE (covered by GITHUB_WORKFLOW.md)

### ✅ FILES TO KEEP (Current and necessary)

#### Core Documentation
- `CLAUDE.md` - Main AI assistant guidelines ✅
- `README.md` - Package overview for users ✅
- `LICENSE.md` - License information ✅

#### Extended Documentation (.claude/memory/)
- `.claude/memory/VERSION_HISTORY.md` - Version history ✅

#### Project Documentation (docs/)
- `docs/GITHUB_WORKFLOW.md` - Complete GitHub CLI guide ✅
- `docs/PROJECT_STRUCTURE.md` - Code organization guide ✅
- `docs/GOALS_AND_REQUIREMENTS.md` - Project goals ✅
- `docs/AAT_manual.txt` - AAT reference manual ✅
- `docs/PPPT_manual.txt` - PPPT reference manual ✅
- `docs/nn1530.pdf` - Research paper reference ✅
- `docs/sensors-21-03998.pdf` - Research paper reference ✅

#### Test Files (Keep - these are real tests)
- `tests/testthat/test-aat.R` ✅
- `tests/testthat/test-klawa.R` ✅
- `tests/testthat/test-merge.R` ✅
- `tests/testthat/test-utils.R` ✅
- `tests/testthat.R` ✅

## Recommended Actions

### Phase 1: Delete Root Test Files (Safe)
```bash
cd "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis"
rm test_*.R
```
**Impact**: None - these are temporary debugging scripts

### Phase 2: Clean Up docs/ Folder
```bash
cd docs/
rm DEVELOPMENT_GUIDELINES.md  # Already in CLAUDE.md
rm OVERFLOW.md                 # Outdated
rm TEMPLATE_R_CLAUDE.md        # Covered by GITHUB_WORKFLOW.md
```
**Impact**: None - content preserved elsewhere

### Phase 3: Review shiny_ui_guide.md
- Check if navigation fix is documented elsewhere
- If yes: DELETE
- If no: MOVE useful parts to PROJECT_STRUCTURE.md

### Phase 4: Consider QUICK_START.md
- Review content
- Either merge into README.md or delete if redundant

## File Count Summary

**Before cleanup**: ~40 files (including redundant)
**After cleanup**: ~25 files (clean, organized)

**Space saved**: ~15 unnecessary files removed
**Maintenance benefit**: Clear documentation structure, no confusion about which file is current

## Documentation Structure After Cleanup

```
musicAnalysis/
├── CLAUDE.md                          # Main AI guidelines
├── README.md                          # User-facing overview
├── LICENSE.md                         # License
├── .claude/memory/
│   ├── VERSION_HISTORY.md            # Complete version history
│   └── FILE_CLEANUP_ANALYSIS.md      # This file
└── docs/
    ├── GITHUB_WORKFLOW.md             # GitHub CLI guide
    ├── PROJECT_STRUCTURE.md           # Code organization
    ├── GOALS_AND_REQUIREMENTS.md      # Project goals
    ├── AAT_manual.txt                 # AAT reference
    ├── PPPT_manual.txt                # PPPT reference
    └── *.pdf                          # Research papers
```

Clean, minimal, no redundancy!
