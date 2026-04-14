# Musical Experience Profile Shiny Integration — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace `musical_experience_time()` with `musical_experience()` in the Shiny module so profile variables are available for viewing, export, and plot grouping.

**Architecture:** The parse button calls `musical_experience()` instead of `musical_experience_time()`. The result structure changes from `$wide/$long/$flags` to `$wide/$sections$time$long/$sections$profile/$flags/$merge_notes`. A new "Profile" tab displays profile-only data. Existing Wide tab shows the merged (time+profile) data. Profile variables become available as grouping options in plots.

**Tech Stack:** R/Shiny, musicAnalysis package, DT, haven, plotly

---

### Task 1: Change parse call from `musical_experience_time()` to `musical_experience()`

**Files:**
- Modify: `inst/shiny/modules/mod_mexp.R:338-352`

- [ ] **Step 1: Update the parse call**

Replace lines 342-346:
```r
        out <- try({
          musicAnalysis::musical_experience_time(
            file = input$csv$datapath,
            check_instruments = isTRUE(input$instrument_checks)
          )
        }, silent = TRUE)
```

With:
```r
        out <- try({
          musicAnalysis::musical_experience(
            file = input$csv$datapath,
            time_args = list(check_instruments = isTRUE(input$instrument_checks)),
            verbose = TRUE
          )
        }, silent = TRUE)
```

- [ ] **Step 2: Update all `res_rv()` accessors**

The result structure changes. Update these references throughout the server function:

| Old accessor | New accessor | Used for |
|---|---|---|
| `res_rv()$wide` | `res_rv()$wide` | No change — merged wide output |
| `res_rv()$long` | `res_rv()$sections$time$long` | Long format for plots and downloads |
| `res_rv()$flags` | `res_rv()$flags` | No change — flags preserved |

Search for all `res_rv()$long` and replace with `res_rv()$sections$time$long`. There are approximately 6 occurrences:
- Line ~457: `long_data <- res_rv()$long` (category_ids_simple renderUI)
- Line ~515: `long_data <- res_rv()$long` (update_graph observeEvent)
- Line ~795: `long_data <- res_rv()$long` (history computation)
- Line ~867: long_tbl renderDT
- Line ~891: dl_long downloadHandler

- [ ] **Step 3: Verify the app starts without errors**

```bash
# Reinstall and start
Rscript -e "devtools::install('.', quick=TRUE, upgrade='never')"
Rscript tests/manual/test_start_app.R
```

- [ ] **Step 4: Commit**

```bash
git add inst/shiny/modules/mod_mexp.R
git commit -m "REFACTOR: Switch from musical_experience_time() to musical_experience()"
```

---

### Task 2: Add "Profile" tab to the UI

**Files:**
- Modify: `inst/shiny/modules/mod_mexp.R` (UI section, lines ~52-115)

- [ ] **Step 1: Add Profile tabPanel after the Flags tab**

Insert after line 115 (closing `)` of the Flags tabPanel):
```r
      tabPanel(
        "Profile",
        br(),
        fluidRow(
          column(
            width = 6,
            p("Musical background profile: demographics, preferences, inner hearing, rankings")
          ),
          column(
            width = 6,
            selectInput(
              ns("rows_to_display_profile"),
              "Rows to display:",
              choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
              selected = 25,
              width = "150px"
            )
          )
        ),
        DTOutput(ns("profile_tbl"))
      ),
```

- [ ] **Step 2: Add Profile download buttons in the export section**

Update the download buttons fluidRow (around line 316-328). Add to the CSV column:
```r
downloadButton(ns("dl_profile"), "Download PROFILE CSV", icon = icon("download")),
```

Add to the SPSS/Stata column:
```r
downloadButton(ns("dl_profile_sav"), "Download PROFILE .sav (SPSS)", icon = icon("download")),
```

- [ ] **Step 3: Commit**

```bash
git add inst/shiny/modules/mod_mexp.R
git commit -m "feat: Add Profile tab and download buttons to Musical Experience UI"
```

---

### Task 3: Add Profile server-side rendering and download handlers

**Files:**
- Modify: `inst/shiny/modules/mod_mexp.R` (server section)

- [ ] **Step 1: Add profile table render**

After the `flags_tbl` renderDT block (~line 883), add:
```r
    output$profile_tbl <- DT::renderDT({
      req(res_rv())
      req(res_rv()$sections$profile)

      rows_display <- as.integer(input$rows_to_display_profile)
      if (is.na(rows_display)) rows_display <- 25

      DT::datatable(res_rv()$sections$profile,
                    options = list(scrollX = TRUE, pageLength = rows_display),
                    filter = "top")
    })
```

- [ ] **Step 2: Add profile CSV download handler**

After the existing dl_flags handler:
```r
    output$dl_profile <- downloadHandler(
      filename = function() paste0("musical_experience_profile_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
      content = function(file) { req(res_rv()); readr::write_csv(res_rv()$sections$profile, file) }
    )
```

- [ ] **Step 3: Add profile SPSS download handler**

After the existing dl_wide_sav handler:
```r
    output$dl_profile_sav <- downloadHandler(
      filename = function() paste0("musical_experience_profile_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".sav"),
      content = function(file) {
        req(res_rv())
        haven::write_sav(res_rv()$sections$profile, file)
      }
    )
```

- [ ] **Step 4: Commit**

```bash
git add inst/shiny/modules/mod_mexp.R
git commit -m "feat: Add Profile table rendering and download handlers"
```

---

### Task 4: Test with real data via Playwright

**Files:**
- No file changes — verification only

- [ ] **Step 1: Reinstall package and start app**

```bash
Rscript -e "devtools::install('.', quick=TRUE, upgrade='never')"
nohup Rscript tests/manual/test_start_app.R > /tmp/shiny_app.log 2>&1 &
```

- [ ] **Step 2: Upload test CSV and parse**

Use Playwright to:
1. Navigate to http://127.0.0.1:7654/
2. Click Musical Experience tab
3. Upload `tests/testdata_musical_experience/music_13042025.csv`
4. Click "Parse CSV"

- [ ] **Step 3: Verify Profile tab**

1. Click "Profile" tab
2. Verify table renders with profile columns (music_status, handedness, main_instrument, etc.)
3. Take screenshot

- [ ] **Step 4: Verify plot grouping with profile variable**

1. Click "Practice Growth Curves" tab
2. Set facet_by to "group" via selectize
3. Select a profile variable as group_var (e.g., "music_status")
4. Click "Update Graph"
5. Verify separate plots appear per music_status value
6. Take screenshot

- [ ] **Step 5: Verify merge notes**

Check for any merge quality issues in the Shiny log or notifications.

- [ ] **Step 6: Kill app**

```bash
taskkill //F //PID <pid>
```

---

### Task 5: Version bump, docs, final commit

**Files:**
- Modify: `DESCRIPTION` (version → 0.0.0.9075)
- Modify: `CLAUDE.md` (current version, move to completed)
- Modify: `README.md` (version badge, tarball, footer)
- Modify: `.claude/memory/VERSION_HISTORY.md` (new entry)
- Modify: `inst/shiny/modules/mod_home.R` (What's New, Build Date)

- [ ] **Step 1: Update DESCRIPTION version to 0.0.0.9075**
- [ ] **Step 2: Update CLAUDE.md — mark "Musical Experience profile Shiny integration" as completed**
- [ ] **Step 3: Update README.md — version badge, tarball filename, footer**
- [ ] **Step 4: Update VERSION_HISTORY.md — add v0.0.0.9075 entry**
- [ ] **Step 5: Update mod_home.R — What's New section**
- [ ] **Step 6: Run full test suite**

```bash
Rscript -e "devtools::test('.')"
```

Expected: FAIL 0

- [ ] **Step 7: Build package**

```bash
Rscript -e "devtools::build('.')"
```

- [ ] **Step 8: Commit and push**

```bash
git add -A
git commit -m "FEAT: Musical Experience Profile integration in Shiny (v0.0.0.9075)"
git push origin master
```
