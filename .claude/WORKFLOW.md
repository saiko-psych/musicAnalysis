# Claude Code Workflow Guidelines

## Mandatory Workflow Rules

### 1. ALWAYS Use Todo Lists
- Create todos at the start of ANY non-trivial task
- Mark tasks as `in_progress` BEFORE starting work
- Mark as `completed` IMMEDIATELY after finishing
- Only ONE task should be `in_progress` at a time

### 2. Visual Verification Required
- For ANY Shiny UI changes:
  - Launch the app with `devtools::load_all(); launch_app()`
  - Visually inspect the changes in the browser
  - Check ALL affected modules, not just one
  - Verify changes match user requirements EXACTLY
- NEVER assume CSS/UI changes work without visual confirmation

### 3. Quality Control
- **NO SHITTY WORK**: Verify everything works before delivering
- Test changes with actual data when possible
- Use test scripts to save tokens (see Test Scripts section)
- Double-check all file paths, function calls, and syntax

### 4. Version Management
- ALWAYS increment version after completing a task
- Update DESCRIPTION file
- Update inst/shiny/modules/mod_home.R "What's New" section
- Build package: `devtools::build()`
- Deliver working .tar.gz file to user

### 5. Documentation Updates
- After each task, update `.claude/memory/` with learnings
- Keep CLAUDE.md concise - move old content to archive
- Update VERSION_HISTORY.md with changes
- Clean up project - remove temporary files, backup files

## Test Scripts for Visual Verification

### Shiny App Quick Launch Script
Location: `tests/manual/test_app_visual.R`

```r
# Quick visual test of Shiny app
devtools::load_all()
launch_app()
# Manual checklist:
# [ ] ASCII art background transparent?
# [ ] Links underlined and brown?
# [ ] No turquoise colors anywhere?
# [ ] Contact menu in navbar right side?
# [ ] Advanced Settings clickable/underlined?
```

### CSS Verification Script
Location: `tests/manual/test_css.R`

```r
# Verify CSS is applied correctly
library(rvest)
app_url <- "http://127.0.0.1:XXXX"  # Update with actual port
html <- read_html(app_url)

# Check wellPanel with transparent background
wells <- html_nodes(html, ".well[style*='transparent']")
cat("Transparent wellPanels found:", length(wells), "\n")

# Check link styling
links <- html_nodes(html, "a")
cat("Total links:", length(links), "\n")
```

## Token-Saving Strategies

### 1. Use Grep Instead of Reading Full Files
```r
# BAD (uses many tokens)
Read entire file, scan for issue

# GOOD (saves tokens)
Grep for specific pattern, only read relevant lines
```

### 2. Batch Git Operations
```r
# BAD
git add file1
git commit
git add file2
git commit

# GOOD
git add -A
git commit -m "message"
```

### 3. Use Test Scripts
- Create reusable test scripts in `tests/manual/`
- Run scripts instead of manually checking
- Save token-heavy verification patterns

## Project Cleanup Checklist

After each task:
- [ ] Remove `.backup` files
- [ ] Remove temporary test files
- [ ] Clean `.claude/settings.local.json` uncommitted changes
- [ ] Archive old VERSION_HISTORY entries if too large
- [ ] Remove redundant documentation

## Git Workflow

1. **Work on master** (no feature branches unless complex multi-week work)
2. **Commit frequently** with clear messages
3. **Push to GitHub** after each completed task
4. **Use conventional commits**:
   - `FEAT:` New features
   - `FIX:` Bug fixes
   - `HOTFIX:` Critical fixes
   - `DOCS:` Documentation only
   - `VERSION:` Version bumps
   - `REFACTOR:` Code restructuring

## Version Numbering

Format: `0.0.0.9XXX`

- Increment XXX for each change
- Update DESCRIPTION
- Update mod_home.R "What's New"
- Move previous version to "Version History"
- Build tarball

## Error Recovery Protocol

If user reports issues:

1. **Stop and assess** - Don't make assumptions
2. **Launch app yourself** - Visually verify the issue
3. **Test scripts** - Run relevant test scripts
4. **Fix properly** - Not quick hacks
5. **Verify again** - Launch app and check ALL affected areas
6. **Deliver working version** - Build and test tarball

## Communication Rules

- Be concise but thorough
- Admit mistakes immediately
- Ask for clarification if requirements unclear
- Show what you verified, not just what you changed
