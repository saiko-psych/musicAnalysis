# TRACE: Step-by-step comparison of ITL calculation vs RSL Summary
# Goal: Find exactly WHERE the calculation diverges
library(readr)
library(dplyr)
library(stringr)

test_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT"

# Pick a participant where we have BOTH itl raw AND rsl summary: 0112CLPE
# Summary says: 50/90
# Our ITL computed: 54/90 (control matches, ambiguous off)

itl_file <- file.path(test_dir, "bis_05062025_rechner_bei_betti/LLO/pre/0112CLPE_raw_020425_AAT_Default_70dBSPL_FM2_~Calib_response.itl.csv")
rsl_file <- file.path(test_dir, "bis_05062025_rechner_bei_betti/LLO/pre/0112CLPE_Ergebnisse_020425_AAT_Default_70dBSPL_FM2_~Calib.rsl.csv")

cat("=== Reading RSL Summary (ground truth) ===\n")
rsl <- read_csv(rsl_file, show_col_types = FALSE)
print(rsl)

cat("\n=== Reading ITL Raw ===\n")
itl <- read_csv(itl_file, show_col_types = FALSE)
cat("Columns:", paste(names(itl), collapse = ", "), "\n")
cat("Rows:", nrow(itl), "\n\n")

# Find the actual column names (they have suffixes)
pitch_col <- names(itl)[str_detect(names(itl), "^Pitch Classification")]
response_col <- names(itl)[str_detect(names(itl), "^Response")]
nmin_col <- "Nmin [-]"

cat("Pitch Classification column:", pitch_col, "\n")
cat("Response column:", response_col, "\n\n")

# Step 1: Filter skipped trials
responses <- as.numeric(itl[[response_col]])
pitch <- as.numeric(itl[[pitch_col]])
cat("=== Step 1: Response distribution (before filtering) ===\n")
cat("Response values:\n")
print(table(responses, useNA = "ifany"))
cat("Pitch Classification values:\n")
print(table(pitch, useNA = "ifany"))

# Filter out Response == -1
skip_idx <- which(is.na(responses) | responses == -1)
cat("\nSkipped trials (Response=-1):", length(skip_idx), "\n")
itl_filtered <- itl
if (length(skip_idx) > 0) itl_filtered <- itl[-skip_idx, ]
cat("Rows after filtering:", nrow(itl_filtered), "\n\n")

pitch_filtered <- as.numeric(itl_filtered[[pitch_col]])

# Step 2: Classify as Control vs Ambiguous via Nmin
nmin_vals <- itl_filtered[[nmin_col]]
is_control <- str_detect(nmin_vals, "^(\\d+)\\s+\\1$")
cat("=== Step 2: Control vs Ambiguous classification ===\n")
cat("Control items:", sum(is_control), "\n")
cat("Ambiguous items:", sum(!is_control), "\n\n")

# Step 3: Create tone pair keys
pair_keys <- paste(
  itl_filtered[["Reference F0 [Hz]"]],
  itl_filtered[["F0 Difference [%]"]],
  itl_filtered[[nmin_col]],
  itl_filtered[["Phase [Cos;Alt;Rnd]"]],
  sep = "||"
)
cat("Unique tone pairs:", length(unique(pair_keys)), "\n\n")

# Step 4: Aggregate per tone pair — TEST BOTH hypotheses
cat("=== Step 4: Two calculation methods ===\n\n")

# Method A: F0 = code 1 ONLY (hypothesis: this matches AAT software)
is_f0_only1 <- (pitch_filtered == 1)

# Method B: F0 = codes 1 AND 2 (current implementation)
is_f0_1and2 <- (pitch_filtered %in% c(1, 2))

itl_work <- tibble(
  pair_key = pair_keys,
  type = ifelse(is_control, "Control", "Ambiguous"),
  is_f0_method_a = is_f0_only1,
  is_f0_method_b = is_f0_1and2
)

# Method A aggregation
pairs_a <- itl_work %>%
  group_by(pair_key, type) %>%
  summarise(n_items = n(), n_f0 = sum(is_f0_method_a), .groups = "drop")

summary_a <- pairs_a %>%
  group_by(type) %>%
  summarise(
    n_pairs = n(),
    avg_items = mean(n_items),
    score = 100 * sum(n_f0) / sum(n_items),
    .groups = "drop"
  )

cat("METHOD A (F0 = code 1 only):\n")
print(summary_a)

# Method B aggregation
pairs_b <- itl_work %>%
  group_by(pair_key, type) %>%
  summarise(n_items = n(), n_f0 = sum(is_f0_method_b), .groups = "drop")

summary_b <- pairs_b %>%
  group_by(type) %>%
  summarise(
    n_pairs = n(),
    avg_items = mean(n_items),
    score = 100 * sum(n_f0) / sum(n_items),
    .groups = "drop"
  )

cat("\nMETHOD B (F0 = codes 1+2, current implementation):\n")
print(summary_b)

cat("\n=== COMPARISON ===\n")
cat(sprintf("RSL Summary:  Ambiguous=%.2f%%, Control=%.2f%%\n",
            as.numeric(rsl[rsl[["Type of Pair"]]=="Ambiguous", "AAT Score [%]"]),
            as.numeric(rsl[rsl[["Type of Pair"]]=="Control", "AAT Score [%]"])))

ambig_a <- summary_a$score[summary_a$type == "Ambiguous"]
ctrl_a <- summary_a$score[summary_a$type == "Control"]
ambig_b <- summary_b$score[summary_b$type == "Ambiguous"]
ctrl_b <- summary_b$score[summary_b$type == "Control"]

cat(sprintf("Method A:     Ambiguous=%.2f%%, Control=%.2f%%  %s\n",
            ambig_a, ctrl_a,
            if (abs(ambig_a - 50) < 0.5 && abs(ctrl_a - 90) < 0.5) "MATCH!" else ""))
cat(sprintf("Method B:     Ambiguous=%.2f%%, Control=%.2f%%  %s\n",
            ambig_b, ctrl_b,
            if (abs(ambig_b - 50) < 0.5 && abs(ctrl_b - 90) < 0.5) "MATCH!" else ""))

# Also check: does it matter if we count ambivalent?
cat("\n=== Pitch Classification in Ambiguous items ===\n")
ambig_pitch <- pitch_filtered[!is_control]
cat("Distribution in ambiguous items:\n")
print(table(ambig_pitch, useNA = "ifany"))
cat(sprintf("Code 0 (spectral): %d, Code 1 (f0): %d, Code 2 (ambivalent): %d, Code -1 (don't know): %d\n",
            sum(ambig_pitch == 0, na.rm=TRUE), sum(ambig_pitch == 1, na.rm=TRUE),
            sum(ambig_pitch == 2, na.rm=TRUE), sum(ambig_pitch == -1, na.rm=TRUE)))

cat("\n=== Pitch Classification in Control items ===\n")
ctrl_pitch <- pitch_filtered[is_control]
cat("Distribution in control items:\n")
print(table(ctrl_pitch, useNA = "ifany"))

# Now test with MORE participants
cat("\n\n=== BATCH TEST: Method A vs Method B for all matched pairs ===\n")
all_csv <- fs::dir_ls(test_dir, recurse = TRUE, regexp = "\\.csv$", type = "file")
aat_files <- all_csv[str_detect(basename(all_csv), "AAT")]

# Build file index
file_idx <- tibble(
  file = aat_files,
  bn = basename(aat_files),
  code = str_extract(bn, "\\d{4}[A-Za-z]{4}"),
  is_itl = str_detect(bn, "\\.itl\\.csv$"),
  is_rsl_summary = FALSE
)

# Check which RSL files are summary format
for (i in which(!file_idx$is_itl)) {
  d <- tryCatch(read_csv(file_idx$file[i], show_col_types = FALSE, n_max = 1), error = function(e) NULL)
  if (!is.null(d) && "Type of Pair" %in% names(d)) file_idx$is_rsl_summary[i] <- TRUE
}

# Find matched pairs
matched <- file_idx %>%
  filter(!is.na(code)) %>%
  group_by(code) %>%
  summarise(has_itl = any(is_itl), has_summary = any(is_rsl_summary)) %>%
  filter(has_itl & has_summary)

cat("Matched ITL+Summary pairs:", nrow(matched), "\n\n")

results <- list()
for (mc in matched$code) {
  itl_f <- file_idx$file[file_idx$code == mc & file_idx$is_itl][1]
  rsl_f <- file_idx$file[file_idx$code == mc & file_idx$is_rsl_summary][1]

  rsl_d <- tryCatch(read_csv(rsl_f, show_col_types = FALSE), error = function(e) NULL)
  itl_d <- tryCatch(read_csv(itl_f, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(rsl_d) || is.null(itl_d)) next

  rsl_ambig <- as.numeric(rsl_d[rsl_d[["Type of Pair"]]=="Ambiguous", "AAT Score [%]"])
  rsl_ctrl <- as.numeric(rsl_d[rsl_d[["Type of Pair"]]=="Control", "AAT Score [%]"])

  # Parse ITL
  pc <- names(itl_d)[str_detect(names(itl_d), "^Pitch Classification")][1]
  rc <- names(itl_d)[str_detect(names(itl_d), "^Response")][1]
  if (is.na(pc) || is.na(rc)) next

  resp <- as.numeric(itl_d[[rc]])
  skip <- which(is.na(resp) | resp == -1)
  if (length(skip) > 0) itl_d <- itl_d[-skip, ]

  pitch_v <- as.numeric(itl_d[[pc]])
  nmin_v <- itl_d[["Nmin [-]"]]
  is_ctrl <- str_detect(nmin_v, "^(\\d+)\\s+\\1$")

  pk <- paste(itl_d[["Reference F0 [Hz]"]], itl_d[["F0 Difference [%]"]], nmin_v, itl_d[["Phase [Cos;Alt;Rnd]"]], sep="||")

  for (method in c("A", "B")) {
    is_f0 <- if (method == "A") (pitch_v == 1) else (pitch_v %in% c(1, 2))
    work <- tibble(pk = pk, type = ifelse(is_ctrl, "Control", "Ambiguous"), f0 = is_f0)
    pairs <- work %>% group_by(pk, type) %>% summarise(ni = n(), nf = sum(f0), .groups = "drop")
    sm <- pairs %>% group_by(type) %>% summarise(score = 100 * sum(nf) / sum(ni), .groups = "drop")

    calc_ambig <- sm$score[sm$type == "Ambiguous"]
    calc_ctrl <- sm$score[sm$type == "Control"]
    if (length(calc_ambig) == 0) calc_ambig <- NA
    if (length(calc_ctrl) == 0) calc_ctrl <- NA

    results[[length(results) + 1]] <- tibble(
      code = mc, method = method,
      rsl_ambig = rsl_ambig, rsl_ctrl = rsl_ctrl,
      calc_ambig = round(calc_ambig, 2), calc_ctrl = round(calc_ctrl, 2),
      ambig_diff = round(abs(calc_ambig - rsl_ambig), 2),
      ctrl_diff = round(abs(calc_ctrl - rsl_ctrl), 2)
    )
  }
}

batch <- bind_rows(results)
cat("Method A (code 1 only) — Mean absolute difference:\n")
a <- batch %>% filter(method == "A")
cat(sprintf("  Ambiguous: %.2f%%   Control: %.2f%%\n", mean(a$ambig_diff, na.rm=TRUE), mean(a$ctrl_diff, na.rm=TRUE)))

cat("Method B (codes 1+2) — Mean absolute difference:\n")
b <- batch %>% filter(method == "B")
cat(sprintf("  Ambiguous: %.2f%%   Control: %.2f%%\n", mean(b$ambig_diff, na.rm=TRUE), mean(b$ctrl_diff, na.rm=TRUE)))

cat("\nPer-participant comparison (Method A):\n")
for (i in seq_len(nrow(a))) {
  r <- a[i, ]
  status <- if (r$ambig_diff < 0.5 && r$ctrl_diff < 0.5) "MATCH" else "MISMATCH"
  cat(sprintf("  %s: rsl=%s/%s  calc=%s/%s  diff=%s/%s  %s\n",
              r$code, r$rsl_ambig, r$rsl_ctrl, r$calc_ambig, r$calc_ctrl,
              r$ambig_diff, r$ctrl_diff, status))
}
