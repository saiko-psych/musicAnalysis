# TEST: Method C — only evaluable items (code 0+1) as denominator, code 1 as F0
library(readr)
library(dplyr)
library(stringr)

test_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/tests/testdata_AAT_PPPT"

# Build file index
all_csv <- fs::dir_ls(test_dir, recurse = TRUE, regexp = "\\.csv$", type = "file")
aat_files <- all_csv[str_detect(basename(all_csv), "AAT")]

file_idx <- tibble(
  file = aat_files, bn = basename(aat_files),
  code = str_extract(bn, "\\d{4}[A-Za-z]{4}"),
  is_itl = str_detect(bn, "\\.itl\\.csv$"), is_rsl_summary = FALSE
)
for (i in which(!file_idx$is_itl)) {
  d <- tryCatch(read_csv(file_idx$file[i], show_col_types = FALSE, n_max = 1), error = function(e) NULL)
  if (!is.null(d) && "Type of Pair" %in% names(d)) file_idx$is_rsl_summary[i] <- TRUE
}

matched <- file_idx %>%
  filter(!is.na(code)) %>%
  group_by(code) %>%
  summarise(has_itl = any(is_itl), has_summary = any(is_rsl_summary)) %>%
  filter(has_itl & has_summary)

cat("Matched pairs:", nrow(matched), "\n\n")

results <- list()
for (mc in matched$code) {
  itl_f <- file_idx$file[file_idx$code == mc & file_idx$is_itl][1]
  rsl_f <- file_idx$file[file_idx$code == mc & file_idx$is_rsl_summary][1]

  rsl_d <- tryCatch(read_csv(rsl_f, show_col_types = FALSE), error = function(e) NULL)
  itl_d <- tryCatch(read_csv(itl_f, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(rsl_d) || is.null(itl_d)) next

  rsl_ambig <- as.numeric(rsl_d[rsl_d[["Type of Pair"]] == "Ambiguous", "AAT Score [%]"])
  rsl_ctrl <- as.numeric(rsl_d[rsl_d[["Type of Pair"]] == "Control", "AAT Score [%]"])
  rsl_a_pairs <- as.numeric(rsl_d[rsl_d[["Type of Pair"]] == "Ambiguous", "# Tone Pairs"])
  rsl_a_avg <- as.numeric(rsl_d[rsl_d[["Type of Pair"]] == "Ambiguous", "Avg. # Items/Pair"])

  pc <- names(itl_d)[str_detect(names(itl_d), "^Pitch Classification")][1]
  rc <- names(itl_d)[str_detect(names(itl_d), "^Response")][1]
  if (is.na(pc) || is.na(rc)) next

  resp <- as.numeric(itl_d[[rc]])
  skip <- which(is.na(resp) | resp == -1)
  if (length(skip) > 0) itl_d <- itl_d[-skip, ]

  pitch_v <- as.numeric(itl_d[[pc]])
  nmin_v <- itl_d[["Nmin [-]"]]
  is_ctrl <- str_detect(nmin_v, "^(\\d+)\\s+\\1$")
  pk <- paste(itl_d[["Reference F0 [Hz]"]], itl_d[["F0 Difference [%]"]],
              nmin_v, itl_d[["Phase [Cos;Alt;Rnd]"]], sep = "||")

  # METHOD C: Only evaluable items (code 0 or 1) count. F0 = code 1 only.
  is_evaluable <- (pitch_v %in% c(0, 1))
  is_f0 <- (pitch_v == 1)

  work <- tibble(pk = pk, type = ifelse(is_ctrl, "Control", "Ambiguous"),
                 evaluable = is_evaluable, f0 = is_f0)

  # Only count evaluable items
  work_eval <- work %>% filter(evaluable)

  pairs <- work_eval %>%
    group_by(pk, type) %>%
    summarise(ni = n(), nf = sum(f0), .groups = "drop")

  sm <- pairs %>%
    group_by(type) %>%
    summarise(
      n_pairs = n(),
      avg_items = round(mean(ni), 2),
      score = round(100 * sum(nf) / sum(ni), 2),
      .groups = "drop"
    )

  calc_ambig <- sm$score[sm$type == "Ambiguous"]
  calc_ctrl <- sm$score[sm$type == "Control"]
  calc_a_pairs <- sm$n_pairs[sm$type == "Ambiguous"]
  calc_a_avg <- sm$avg_items[sm$type == "Ambiguous"]
  if (length(calc_ambig) == 0) calc_ambig <- NA
  if (length(calc_ctrl) == 0) calc_ctrl <- NA

  results[[length(results) + 1]] <- tibble(
    code = mc,
    rsl_ambig = rsl_ambig, rsl_ctrl = rsl_ctrl,
    calc_ambig = calc_ambig, calc_ctrl = calc_ctrl,
    ambig_diff = round(abs(calc_ambig - rsl_ambig), 2),
    ctrl_diff = round(abs(calc_ctrl - rsl_ctrl), 2),
    rsl_pairs = rsl_a_pairs, calc_pairs = calc_a_pairs,
    rsl_avg = rsl_a_avg, calc_avg = calc_a_avg
  )
}

batch <- bind_rows(results)
cat("METHOD C: Evaluable items only (code 0+1), F0 = code 1\n")
cat(sprintf("Mean absolute difference: Ambiguous=%.2f%%  Control=%.2f%%\n\n",
            mean(batch$ambig_diff, na.rm = TRUE), mean(batch$ctrl_diff, na.rm = TRUE)))

cat("Per-participant:\n")
for (i in seq_len(nrow(batch))) {
  r <- batch[i, ]
  status <- if (r$ambig_diff < 0.2 && r$ctrl_diff < 0.2) "EXACT MATCH"
            else if (r$ambig_diff < 1.0 && r$ctrl_diff < 1.0) "CLOSE"
            else "MISMATCH"
  cat(sprintf("  %s: rsl=%.2f/%.2f  calc=%.2f/%.2f  diff=%.2f/%.2f  pairs=%s/%s  avg=%.2f/%.2f  %s\n",
              r$code, r$rsl_ambig, r$rsl_ctrl, r$calc_ambig, r$calc_ctrl,
              r$ambig_diff, r$ctrl_diff,
              r$rsl_pairs, r$calc_pairs, r$rsl_avg, r$calc_avg, status))
}

n_exact <- sum(batch$ambig_diff < 0.2 & batch$ctrl_diff < 0.2, na.rm = TRUE)
n_close <- sum(batch$ambig_diff < 1.0 & batch$ctrl_diff < 1.0, na.rm = TRUE)
cat(sprintf("\nExact matches: %d/%d  Close: %d/%d\n", n_exact, nrow(batch), n_close, nrow(batch)))
