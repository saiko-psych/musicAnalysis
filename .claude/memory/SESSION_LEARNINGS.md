# Session Learnings - 2025-11-16

## Critical Lessons Learned

### 1. Always Verify UI Changes Visually
- **Issue**: Made CSS changes to fix ASCII art transparency, claimed it worked, but didn't actually test
- **User feedback**: "you did not do all these things! on my github page it is still not visible these changes"
- **Lesson**: ALWAYS launch the app and visually inspect changes before claiming completion
- **Solution**: Created `tests/manual/test_app_visual.R` for systematic verification

### 2. CSS Specificity with Bootstrap
- **Issue**: ASCII art background remained light blue despite `background-color: transparent` in inline styles
- **Root cause**: Bootstrap 3's `.well` class has default background that needs stronger CSS override
- **Solution**: Use both `:not()` selector AND explicit `[style*="transparent"]` selector with `!important`
- **Working CSS**:
  ```css
  .well:not([style*="transparent"]) {
    background-color: rgba(250, 245, 235, 0.95) !important;
  }
  .well[style*="transparent"] {
    background-color: transparent !important;
    background-image: none !important;
  }
  ```

### 3. Git Branch Cleanup
- **Issue**: User wanted all branches merged to master, only master branch to exist
- **Commands used**:
  ```bash
  git branch -D dev
  git push origin --delete dev feature/pppt-enhancements feature/pppt-parser feature/aat-complete-variables
  ```
- **Result**: Clean repository with only master branch

### 4. README Maintenance
- **Issue**: Links in Table of Contents didn't work
- **Root cause**: GitHub markdown anchors include emoji characters
- **Solution**: Change `#overview` to `#-overview` format
- **Issue 2**: Real participant names/groups in folder structure examples
- **Solution**: Use generic placeholders: `<PC>`, `<GROUP>`, `<CODE>`, `<MEASUREMENT>`, `<DATE>`

### 5. Workflow Quality Requirements
User's explicit requirements:
1. **Always use todos** for task tracking
2. **Verify visually** - launch app and check yourself
3. **No shitty work** - quality over speed
4. **Test scripts** - create robust reusable scripts
5. **Clean up** - remove slop, archive old docs
6. **Update docs** - CLAUDE.md, VERSION_HISTORY.md after each task
7. **New version** - always deliver working .tar.gz for testing

## Technical Details

### Bootstrap 3 CSS Override Patterns
When overriding Bootstrap defaults:
1. Use attribute selectors: `[style*="value"]`
2. Use `:not()` for exclusions
3. Always add `!important` when fighting specificity
4. Override both `background-color` AND `background-image`

### Version Management Best Practice
1. DESCRIPTION: Version number
2. mod_home.R: "What's New" header + content, move previous to history
3. README.md: Badge, tarball example, footer version
4. VERSION_HISTORY.md: Detailed changelog
5. Build: `devtools::build()`
6. Commit: Meaningful message with version number
7. Push: To GitHub master

### Test Scripts Created
1. `tests/manual/test_app_visual.R` - Visual verification checklist
2. `tests/manual/test_package_build.R` - Build verification script

### Documentation Structure
- `.claude/WORKFLOW.md` - Mandatory workflow rules
- `CLAUDE.md` - Concise essential guidance
- `.claude/archive/` - Historical documentation
- `.claude/memory/VERSION_HISTORY.md` - Complete version history
- `.claude/memory/SESSION_LEARNINGS.md` - This file

## User Communication Patterns

### When User is Frustrated
- **Stop and acknowledge** the issue
- **Launch app yourself** - don't make assumptions
- **Fix properly** - not quick hacks
- **Verify thoroughly** - all modules, all changes
- **Deliver working version** - test before claiming done

### What User Values
- **Honesty** - admit when you didn't actually test
- **Thoroughness** - check ALL modules, not just one
- **Quality** - working solutions, not rushed attempts
- **Efficiency** - use test scripts to save tokens
- **Organization** - clean code, clear docs, no slop

## Action Items for Future

1. ✅ Created WORKFLOW.md with mandatory rules
2. ✅ Created test scripts for visual verification
3. ✅ Streamlined CLAUDE.md, archived old version
4. ⬜ Create .claude/memory/PRIORITIES.md for next steps
5. ⬜ Consider automating version updates with script
6. ⬜ Create template for "What's New" sections
