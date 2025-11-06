#!/bin/bash

# Script to create GitHub issues from CLAUDE.md Future Tasks
# Run this once to populate your GitHub issues

cd "$(dirname "$0")"

echo "Creating GitHub issues from CLAUDE.md..."
echo ""

# High Priority Issues

gh issue create \
  --title "KLAWA: Flexible folder structure with guided setup" \
  --label enhancement,priority-high,module-klawa \
  --body "## Problem
Currently KLAWA scanning expects a rigid folder structure. Need to make it more flexible and guide users when structure doesn't match.

## Proposed Solution
- Make KLAWA scanning work with non-standard folder structures
- Add interactive guidance to help users organize their files
- Optional variables: sometimes no Group needed, sometimes no PC variable
- Only include variables that are actually present in the structure

## Acceptance Criteria
- [ ] Scanning works with various folder structures
- [ ] UI shows detected structure
- [ ] User can confirm or adjust variable mappings
- [ ] Help text guides users on organizing files
- [ ] Only relevant columns appear in output

## Related
High priority task #1 from CLAUDE.md"

echo "✓ Created issue: KLAWA flexible folder structure"

gh issue create \
  --title "Musical Experience: Organize variables in wide and long format" \
  --label enhancement,priority-high,module-mexp \
  --body "## Problem
Variables in wide and long format output are not logically organized, making data tables hard to read.

## Proposed Solution
- Sort variables in wide format logically
- Group related variables together (e.g., all instrument1 vars, then instrument2, etc.)
- Order: code, age, instrument1 vars, instrument2 vars, singing1 vars, etc.

## Acceptance Criteria
- [ ] Wide format columns ordered logically
- [ ] Related variables grouped together
- [ ] Order documented in function documentation
- [ ] Tests verify column order

## Related
High priority task #2 from CLAUDE.md"

echo "✓ Created issue: Variable organization"

gh issue create \
  --title "Clean variable names: Remove brackets and special characters" \
  --label enhancement,priority-high,module-mexp \
  --body "## Problem
Variable names contain brackets `[]` and other special characters, which are not R-friendly.

## Example
Current: \`instrument1_hours[5]\`, \`singing1_type[choir]\`
Desired: \`instrument1_hours_5\`, \`singing1_type_choir\`

## Proposed Solution
- Remove brackets from all variable names
- Replace with underscores or remove entirely
- Ensure all names are valid R identifiers
- Apply to both wide and long format

## Acceptance Criteria
- [ ] All variable names are valid R identifiers
- [ ] No brackets or special characters
- [ ] Tests verify name format
- [ ] Backward compatibility maintained (or documented breaking change)

## Related
High priority task #3 from CLAUDE.md"

echo "✓ Created issue: Variable name cleaning"

gh issue create \
  --title "Implement variable labels for all variables" \
  --label enhancement,priority-high,documentation \
  --body "## Problem
Variables lack descriptive labels, making output less self-documenting.

## Proposed Solution
- Add labels attribute to all variables
- Use descriptive, human-readable text
- Useful for reports and data dictionaries

## Example
\`\`\`r
# Variable: instrument1_total
# Label: \"Total practice hours for first instrument across lifespan\"

# Variable: IMP_total
# Label: \"Index of Musical Practice (weekly hours × years practiced)\"
\`\`\`

## Acceptance Criteria
- [ ] All key variables have labels
- [ ] Labels accessible via attributes
- [ ] Function to display data dictionary
- [ ] Labels shown in Shiny app where appropriate

## Related
High priority task #4 from CLAUDE.md"

echo "✓ Created issue: Variable labels"

gh issue create \
  --title "Add plot grouping by variables (intervention group, school, etc.)" \
  --label enhancement,priority-high,module-mexp,module-klawa \
  --body "## Problem
Cannot compare plots by grouping variables like intervention group, school, cohort, etc.

## Proposed Solution
- Allow plots to be organized/colored by grouping variables
- Add grouping parameter to plot_practice_curves()
- Support faceting by group
- Works for both KLAWA and Musical Experience plots

## Example Use Cases
- Compare practice curves by intervention group
- Compare KLAWA results by school
- Facet plots by measurement time (pre/post)

## Acceptance Criteria
- [ ] grouping parameter added to plot functions
- [ ] Faceting option implemented
- [ ] Color schemes distinguish groups clearly
- [ ] Works with all plot types (total, category_sum, individual)
- [ ] Tests for grouped plotting

## Related
High priority task #5 from CLAUDE.md"

echo "✓ Created issue: Plot grouping"

gh issue create \
  --title "Add descriptive statistics table for Musical Experience" \
  --label enhancement,priority-high,module-mexp \
  --body "## Problem
No easy way to view summary statistics in Shiny app.

## Proposed Solution
- Add summary statistics display option in Musical Experience module
- Include grouping option for stratified statistics
- Show: Mean, SD, min, max, N for key practice variables

## Variables to Summarize
- instrument_total, singing_total, othermusic_total
- total_musical_experience
- IMP_total, IMP_instrument, IMP_singing
- Starting ages
- number_of_instruments, number_of_singing, nodme

## Acceptance Criteria
- [ ] New tab or panel in mod_mexp showing statistics
- [ ] Option to stratify by grouping variable
- [ ] Download statistics as CSV
- [ ] Formatted table with proper column names
- [ ] Option to select which variables to summarize

## Related
High priority task #6 from CLAUDE.md"

echo "✓ Created issue: Descriptive statistics"

gh issue create \
  --title "Add version timestamps to version history" \
  --label enhancement,priority-medium,documentation \
  --body "## Problem
Version history shows dates but not exact times when versions were created.

## Proposed Solution
- Show date and time when each package version was created
- Helps track when changes were made
- Display in home page version history

## Example
Current: v0.0.0.9023 (2025-11-06)
Desired: v0.0.0.9023 (2025-11-06 14:30)

## Acceptance Criteria
- [ ] Timestamps shown in mod_home.R version history
- [ ] Format: YYYY-MM-DD HH:MM
- [ ] Auto-generated (not manually updated)

## Related
High priority task #7 from CLAUDE.md"

echo "✓ Created issue: Version timestamps"

gh issue create \
  --title "Add 'Show R Code' buttons for reproducibility" \
  --label enhancement,priority-high,module-mexp,module-klawa \
  --body "## Problem
Users can't see what R code is being executed, making it hard to reproduce analyses outside the Shiny app.

## Proposed Solution
- Add \"Show R Code\" button for CSV parsing in Musical Experience module
- Add \"Show R Code\" button for KLAWA scanning
- Display actual R code that would reproduce the current operation
- Include downloadable .R script

## Example
When user clicks \"Show R Code\" after scanning KLAWA:
\`\`\`r
# This code reproduces your KLAWA scan
library(musicAnalysis)

klawa_data <- klawa_scan(
  root = \"path/to/KLAWA\",
  date_format = \"DDMMYY\",
  metadata_source = \"path\"
)

View(klawa_data)
\`\`\`

## Acceptance Criteria
- [ ] Button in KLAWA module showing scan code
- [ ] Button in Musical Experience module showing parse code
- [ ] Code includes all current settings/parameters
- [ ] Download as .R file option
- [ ] Code is copy-pasteable and runs independently

## Related
High priority task #8 from CLAUDE.md"

echo "✓ Created issue: Show R code buttons"

gh issue create \
  --title "Fix top_N and bottom_N participant selection to be context-aware" \
  --label enhancement,priority-high,module-mexp \
  --body "## Problem
Currently participant selection (top N, bottom N) uses total hours across all categories, which doesn't make sense when viewing specific instrument or category plots.

## Proposed Solution
Make selection context-aware based on current plot_type and category selection:

### Examples
- plot_type = \"individual\", category = \"instrument\", category_id = 1 → top N for instrument1 only
- plot_type = \"category_sum\", category = \"singing\" → top N for total singing hours
- plot_type = \"total\" → top N for overall musical experience (current behavior is correct)

## Implementation
Modify ranking logic in plot_practice_curves() to consider:
1. Current plot_type
2. Selected categories
3. Selected category_ids

## Acceptance Criteria
- [ ] Ranking considers current plot context
- [ ] UI shows which context is used (e.g., \"Showing top 10 by Instrument 1 hours\")
- [ ] Tests for each scenario
- [ ] Works with all plot types

## Related
High priority task #9 from CLAUDE.md"

echo "✓ Created issue: Context-aware participant selection"

# Medium Priority Issues

gh issue create \
  --title "Musical Experience: Add additional useful variables" \
  --label enhancement,priority-medium,module-mexp \
  --body "## Proposed Variables

To be designed and discussed:

1. **Years of formal instruction per instrument**
   - Lessons, classes, conservatory training

2. **Current practice frequency**
   - Hours per week currently (not lifetime)

3. **Performance experience**
   - Number of concerts, recitals, competitions

4. **Ensemble experience**
   - Band, orchestra, choir membership duration

5. **Music theory training**
   - Years of formal theory instruction

6. **Perfect pitch / relative pitch abilities**
   - Self-reported or tested

7. **Primary vs secondary instruments ranking**
   - Proficiency levels

8. **Motivation for playing**
   - Intrinsic vs extrinsic factors

## Next Steps
1. Discuss with research team which variables are priorities
2. Design questionnaire items if needed
3. Implement parsing logic
4. Add to output

## Related
Medium priority task #10 from CLAUDE.md"

echo "✓ Created issue: Additional musical experience variables"

gh issue create \
  --title "Implement PPPT data parser" \
  --label enhancement,priority-medium,new-feature \
  --body "## Overview
Add parser for PPPT (Pitch Perception Proficiency Test) data.

## Expected Structure
To be determined based on actual data format.

## Deliverables
- [ ] R/pppt.R with parsing functions
- [ ] tests/testthat/test-pppt.R with tests
- [ ] inst/shiny/modules/mod_pppt.R Shiny interface
- [ ] Integration with merge workflow
- [ ] Documentation

## Related
Medium priority task #11 from CLAUDE.md"

echo "✓ Created issue: PPPT parser"

gh issue create \
  --title "Implement AAT data parser" \
  --label enhancement,priority-medium,new-feature \
  --body "## Overview
Add parser for AAT (Audio Aptitude Test) data.

## Expected Structure
To be determined based on actual data format.

## Deliverables
- [ ] R/aat.R with parsing functions
- [ ] tests/testthat/test-aat.R with tests
- [ ] inst/shiny/modules/mod_aat.R Shiny interface
- [ ] Integration with merge workflow
- [ ] Documentation

## Related
Medium priority task #12 from CLAUDE.md"

echo "✓ Created issue: AAT parser"

echo ""
echo "================================================"
echo "✓ All 13 issues created successfully!"
echo "================================================"
echo ""
echo "View all issues:"
echo "  gh issue list"
echo ""
echo "Start working on an issue:"
echo "  gh issue develop <number>"
echo ""
echo "Open in browser:"
echo "  gh issue list --web"
