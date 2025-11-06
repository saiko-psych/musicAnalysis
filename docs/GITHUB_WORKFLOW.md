# GitHub CLI Project Management for musicAnalysis

## Overview

This guide explains how to use GitHub CLI (`gh`) for managing the musicAnalysis R package development. GitHub CLI provides a command-line interface for all GitHub features: issues, pull requests, releases, and more.

## Prerequisites

### 1. Install GitHub CLI

**Windows (using winget)**:
```bash
winget install --id GitHub.cli
```

**Or download from**: https://cli.github.com/

### 2. Authenticate with GitHub

```bash
gh auth login
```

Follow the prompts:
- Choose GitHub.com
- Choose HTTPS
- Authenticate via web browser
- This creates a token stored securely on your machine

### 3. Initialize Git Repository (if not already done)

```bash
cd C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis
git init
git add .
git commit -m "Initial commit: musicAnalysis v0.0.0.9023"
```

### 4. Create GitHub Repository

```bash
# Create a new repository on GitHub
gh repo create musicAnalysis --private --source=. --remote=origin

# Or if you want it public:
gh repo create musicAnalysis --public --source=. --remote=origin --push
```

This creates the repository and pushes your code to GitHub.

## Development Workflow with GitHub CLI

### Phase 1: Issue-Based Development

#### 1.1 Create Issues for Tasks

Issues are the foundation of project management. Each feature, bug, or enhancement gets an issue.

```bash
# Create an issue manually
gh issue create --title "Fix top_N and bottom_N participant selection" \
  --body "Currently selects based on total hours. Should be context-aware based on plot_type and category selection."

# Create issue interactively (opens editor)
gh issue create
```

**Best practices**:
- One issue per feature/bug
- Use clear, descriptive titles
- Add details in the body: what, why, how
- Use labels: `bug`, `enhancement`, `documentation`, `priority-high`

```bash
# Create with labels
gh issue create --title "Add descriptive statistics table" \
  --label enhancement,priority-high \
  --body "Add summary statistics display option in Musical Experience Shiny module"
```

#### 1.2 List and View Issues

```bash
# List all open issues
gh issue list

# List with filters
gh issue list --label bug
gh issue list --label enhancement
gh issue list --state all  # Including closed issues

# View specific issue details
gh issue view 1
gh issue view 80

# View in browser
gh issue view 80 --web
```

#### 1.3 Assign Issues

```bash
# Assign to yourself
gh issue edit 80 --add-assignee @me

# Add labels
gh issue edit 80 --add-label priority-high,enhancement
```

### Phase 2: Branch-Based Development

GitHub CLI can automatically create feature branches from issues.

#### 2.1 Create Feature Branch from Issue

```bash
# Create and checkout branch for issue #80
gh issue develop 80

# This creates a branch named like: 80-fix-top-n-bottom-n-participant-selection
# And automatically checks it out
```

**Manual alternative**:
```bash
git checkout -b 80-fix-participant-selection
```

#### 2.2 Work on the Feature

Make your changes as normal:

```bash
# Edit files
# inst/shiny/modules/mod_mexp.R
# R/plot_practice_curves.R

# Test your changes
devtools::test()

# Commit incrementally
git add R/plot_practice_curves.R
git commit -m "Add context-aware participant selection logic"

git add inst/shiny/modules/mod_mexp.R
git commit -m "Update UI to show selection context"

git add tests/testthat/test-musical-experience.R
git commit -m "Add tests for context-aware selection"
```

**Commit message best practices**:
- Use present tense: "Add feature" not "Added feature"
- Be specific: "Fix category_sum label format" not "Fix bug"
- Reference issue: "Fix participant selection (closes #80)"

#### 2.3 Push Branch to GitHub

```bash
# First time pushing this branch
git push -u origin 80-fix-participant-selection

# Subsequent pushes
git push
```

### Phase 3: Pull Requests

#### 3.1 Create Pull Request

```bash
# Create PR from current branch
gh pr create --title "Fix top_N and bottom_N participant selection" \
  --body "Implements context-aware selection based on plot_type and category.

## Changes
- Modified plot_practice_curves() to calculate top/bottom based on current context
- Updated Shiny UI to show which context is used
- Added tests for all scenarios

Closes #80"

# Create PR interactively (opens editor)
gh pr create

# Create PR that auto-fills from issue
gh pr create --title "Fix top_N participant selection" \
  --body "Implements #80" \
  --assignee @me
```

**PR body structure** (recommended):
```markdown
## Summary
Brief description of what this PR does

## Changes
- Bullet list of specific changes
- What files were modified
- What functionality was added/fixed

## Testing
- How was this tested?
- What test cases were added?

## Related Issues
Closes #80
Refs #75
```

#### 3.2 View and Manage Pull Requests

```bash
# List all open PRs
gh pr list

# View specific PR
gh pr view 5

# View in browser
gh pr view 5 --web

# Check PR status (CI checks, reviews)
gh pr checks 5
```

#### 3.3 Review Changes

```bash
# View diff
gh pr diff 5

# View specific files changed
gh pr view 5 --json files

# Comment on PR
gh pr comment 5 --body "LGTM! Tests all pass."
```

#### 3.4 Merge Pull Request

```bash
# Merge PR (creates merge commit)
gh pr merge 5

# Squash and merge (recommended for clean history)
gh pr merge 5 --squash --delete-branch

# Merge and auto-delete branch
gh pr merge 5 --merge --delete-branch

# Interactive merge (choose method)
gh pr merge 5
```

**When to merge**:
- All tests pass (`devtools::test()` shows 108 pass, 0 fail)
- Code review is complete (if working with others)
- Changes are documented in CLAUDE.md
- Version number incremented in DESCRIPTION
- What's New section updated in mod_home.R

### Phase 4: Release Management

#### 4.1 Create a Release

After merging several PRs and reaching a stable state:

```bash
# Create a release
gh release create v0.0.0.9023 \
  --title "v0.0.0.9023 - Category_sum Plot Fixes" \
  --notes "## What's New
- Fixed category_sum plot labels and colors
- Each participant gets unique color
- Proper label format: CODE - total Category

## Full Changelog
See CLAUDE.md for detailed changes."

# Attach the tarball
gh release create v0.0.0.9023 \
  --title "v0.0.0.9023" \
  --notes-file docs/RELEASE_NOTES.md \
  musicAnalysis_0.0.0.9023.tar.gz
```

#### 4.2 List and View Releases

```bash
# List all releases
gh release list

# View specific release
gh release view v0.0.0.9023

# Download release assets
gh release download v0.0.0.9023
```

## Recommended Project Structure for GitHub

### Labels to Create

```bash
# Bug tracking
gh label create bug --color d73a4a --description "Something isn't working"
gh label create critical --color b60205 --description "Urgent fix needed"

# Feature development
gh label create enhancement --color a2eeef --description "New feature or request"
gh label create refactor --color 0e8a16 --description "Code improvement without functionality change"

# Priorities
gh label create priority-high --color d93f0b --description "High priority"
gh label create priority-medium --color fbca04 --description "Medium priority"
gh label create priority-low --color 0e8a16 --description "Low priority"

# Modules
gh label create module-klawa --color 1d76db --description "Related to KLAWA module"
gh label create module-mexp --color 1d76db --description "Related to Musical Experience module"
gh label create module-merge --color 1d76db --description "Related to Merge module"

# Status
gh label create in-progress --color ededed --description "Currently being worked on"
gh label create blocked --color e99695 --description "Blocked by another issue"
gh label create needs-testing --color bfd4f2 --description "Needs manual testing"
```

### Milestones for Planning

```bash
# Create milestone for major version
gh api repos/:owner/:repo/milestones -f title="v1.0.0 - Public Release" \
  -f description="First stable public release" \
  -f due_on="2025-12-31T00:00:00Z"

# Assign issue to milestone
gh issue edit 80 --milestone "v1.0.0 - Public Release"
```

## Example: Complete Workflow

Let's walk through implementing task #9 (Fix top_N and bottom_N).

### Step 1: Create Issue

```bash
gh issue create --title "Fix top_N and bottom_N participant selection" \
  --label enhancement,priority-high,module-mexp \
  --body "## Problem
Currently participant selection (top N, bottom N) uses total hours across all categories, which doesn't make sense when viewing specific instrument or category plots.

## Proposed Solution
Make selection context-aware:
- For individual plots with specific category_id: rank by that specific instrument/singing/othermusic
- For category_sum with specific category: rank by that category total
- For total plots: rank by overall total (current behavior)

## Acceptance Criteria
- [ ] Ranking logic updated in plot_practice_curves()
- [ ] Tests added for each scenario
- [ ] UI shows which context is used for selection
- [ ] Documentation updated in CLAUDE.md

## Related
Part of future enhancements #9"
```

**Output**: "Created issue #81"

### Step 2: Create Feature Branch

```bash
gh issue develop 81
# Creates and checks out: 81-fix-top-n-bottom-n-participant-selection
```

### Step 3: Implement Changes

```bash
# Edit R/plot_practice_curves.R
# Add context-aware ranking logic

# Edit tests/testthat/test-musical-experience.R
# Add test cases

# Test
devtools::test()  # All 108+ tests pass

# Commit
git add R/plot_practice_curves.R tests/testthat/test-musical-experience.R
git commit -m "Implement context-aware participant ranking

- Added ranking_context parameter to determine ranking method
- Ranking now considers plot_type, categories, and category_ids
- Added tests for individual, category_sum, and total contexts

Closes #81"

git push -u origin 81-fix-top-n-bottom-n-participant-selection
```

### Step 4: Create Pull Request

```bash
gh pr create --title "Fix top_N and bottom_N participant selection" \
  --body "Implements context-aware participant ranking for Musical Experience plots.

## Summary
Participant selection (top N, bottom N, random N) is now context-aware based on the current plot view.

## Changes
- Modified \`plot_practice_curves()\` ranking logic (R/plot_practice_curves.R:176-200)
- Added \`ranking_context\` parameter that considers plot_type + categories + category_ids
- Updated tests with 3 new scenarios (tests/testthat/test-musical-experience.R:+45 lines)

## Testing
✅ All 111 tests pass
✅ Tested manually with various plot configurations

## Examples
- \`plot_type='individual', category_id=1\` → ranks by instrument1 hours only
- \`plot_type='category_sum', category='singing'\` → ranks by total singing hours
- \`plot_type='total'\` → ranks by overall musical experience (unchanged)

Closes #81" \
  --assignee @me
```

### Step 5: Review and Merge

```bash
# Check PR status
gh pr view 1

# If all looks good, merge
gh pr merge 1 --squash --delete-branch

# This:
# - Squashes all commits into one clean commit
# - Merges to main branch
# - Deletes the feature branch
# - Automatically closes issue #81
```

### Step 6: Update Version and Release

```bash
# On main branch now
git checkout main
git pull

# Increment version
# Edit DESCRIPTION: 0.0.0.9023 → 0.0.0.9024
# Edit mod_home.R: Update What's New
# Edit CLAUDE.md: Add to Recently Completed

git add DESCRIPTION inst/shiny/modules/mod_home.R CLAUDE.md
git commit -m "Bump version to 0.0.0.9024"
git push

# Build package
devtools::document()
devtools::test()
pkgbuild::build(path = ".")

# Create release
gh release create v0.0.0.9024 \
  --title "v0.0.0.9024 - Context-aware participant selection" \
  --notes "## What's New
- Fixed top_N and bottom_N participant selection to be context-aware
- Selection now considers current plot_type and category filters
- Improves relevance in focused analyses

## Installation
\`\`\`r
install.packages('musicAnalysis_0.0.0.9024.tar.gz', repos = NULL, type = 'source')
\`\`\`" \
  musicAnalysis_0.0.0.9024.tar.gz
```

## GitHub Project Boards (Advanced)

For visual task management:

```bash
# Create project board
gh project create --title "musicAnalysis Development" \
  --body "Main development board for tracking issues and PRs"

# Link issues to project
# (Currently requires web interface or GitHub API calls)
```

## Useful Aliases

Add these to your `.gitconfig`:

```bash
[alias]
  # Quick status
  st = status -sb

  # Pretty log
  lg = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit

  # Undo last commit (keep changes)
  undo = reset HEAD~1 --soft

  # Quick commit all changes
  ca = commit -am

  # Push and create PR
  pr = "!f() { git push -u origin HEAD && gh pr create; }; f"
```

## Best Practices Summary

### For Issues
✅ **DO**:
- Create issue before coding
- Use clear, descriptive titles
- Add relevant labels and milestones
- Include acceptance criteria
- Reference related issues

❌ **DON'T**:
- Create vague issues like "Fix bugs"
- Leave issues open after fixing
- Skip documentation in issue body

### For Branches
✅ **DO**:
- One branch per issue
- Use descriptive branch names: `81-fix-participant-selection`
- Keep branches short-lived (merge within days)
- Regularly pull from main

❌ **DON'T**:
- Work on multiple unrelated issues in same branch
- Let branches diverge too far from main
- Use generic names like `fix` or `dev`

### For Commits
✅ **DO**:
- Commit frequently with clear messages
- Use present tense: "Add" not "Added"
- Reference issues in commit messages
- Keep commits focused (one logical change)

❌ **DON'T**:
- Make giant commits with mixed changes
- Use vague messages like "Update files"
- Commit broken code

### For Pull Requests
✅ **DO**:
- Write detailed PR descriptions
- Include testing notes
- Reference closing issues
- Request review if working with others
- Ensure all tests pass before merging

❌ **DON'T**:
- Merge without testing
- Leave stale PRs open
- Skip version updates

### For Releases
✅ **DO**:
- Create release after merging related PRs
- Include tarball as release asset
- Write clear release notes
- Use semantic versioning
- Tag releases: `v0.0.0.9024`

❌ **DON'T**:
- Release untested code
- Skip release notes
- Release without incrementing version

## Integration with R Package Development

### Automated Checks with GitHub Actions

Create `.github/workflows/R-CMD-check.yml`:

```yaml
name: R-CMD-check

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  R-CMD-check:
    runs-on: windows-latest

    steps:
      - uses: actions/checkout@v3

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.4.3'

      - name: Install dependencies
        run: |
          install.packages(c("remotes", "devtools"))
          remotes::install_deps(dependencies = TRUE)
        shell: Rscript {0}

      - name: Check
        run: devtools::check()
        shell: Rscript {0}

      - name: Test
        run: devtools::test()
        shell: Rscript {0}
```

This automatically runs tests on every push and PR!

## Quick Reference Card

```bash
# ISSUES
gh issue create                    # Create new issue
gh issue list                      # List open issues
gh issue view 80                   # View issue details
gh issue edit 80 --add-label bug   # Add label
gh issue close 80                  # Close issue

# BRANCHES
gh issue develop 80                # Create branch from issue
git checkout main                  # Switch to main
git pull                          # Update main

# COMMITS
git add .                         # Stage all changes
git commit -m "message"           # Commit
git push                          # Push to GitHub

# PULL REQUESTS
gh pr create                      # Create PR
gh pr list                        # List PRs
gh pr view 5                      # View PR
gh pr merge 5 --squash            # Merge PR
gh pr checks                      # View CI status

# RELEASES
gh release create v0.0.0.9024     # Create release
gh release list                   # List releases
gh release download v0.0.0.9024   # Download release

# REPOSITORY
gh repo view --web                # Open in browser
gh repo sync                      # Sync fork with upstream
```

## Next Steps

1. **Initialize your repository**: `gh repo create musicAnalysis --private --source=. --push`
2. **Create initial issues**: Convert your CLAUDE.md future tasks (1-13) into GitHub issues
3. **Start with one issue**: Pick task #9 (top_N fix) as your first GitHub-managed task
4. **Set up GitHub Actions**: Add automated testing to your workflow
5. **Establish rhythm**: Issue → Branch → Code → PR → Merge → Release

This workflow will make your development more organized, trackable, and professional!
