#' Combine and preprocess LimeSurvey pre-survey CSV exports
#'
#' @description
#' Reads all CSV files from a directory, harmonizes columns, filters to completed cases,
#' deduplicates rows per `code` (default: latest timestamp), drops administrative fields,
#' normalizes names, maps numeric `group` to labels, and applies label-aware recoding
#' for selected variables. Original raw variables are dropped in favor of clear,
#' analysis-ready pairs: numeric (`*_num`) + factor (`*_f`).
#'
#' @param root Character. Directory with CSVs.
#' @param pattern Regex for files. Default `"\\.csv$"`.
#' @param recursive Search subdirs. Default `TRUE`.
#' @param lastpage_required Integer. Keep only rows with this `lastpage`. Default `10`.
#' @param code_col Name of participant code column. Default `"code"`.
#' @param drop_cols Columns to drop after preprocessing (applied **after** dedup). Default
#'   `c("id","submitdate","lastpage","startlanguage","seed","token","startdate","datestamp")`.
#' @param output_file Optional path to write combined CSV.
#' @param deduplicate Strategy for duplicate `code`s. Default `"latest"`. Options:
#'   - `"latest"`: keep row with the **latest timestamp** (see `timestamp_cols`)
#'   - `"none"`: keep all
#'   - `"first"`: keep the **first** occurrence (file/row order)
#'   - `"most_complete"`: keep row with **most non-missing/non-empty fields**
#' @param timestamp_cols Character vector of timestamp columns (priority order) used by `"latest"`.
#'   Default `c("datestamp","submitdate","startdate")`.
#'
#' @return A tibble.
#' @export
limesurvey_pre <- function(
    root,
    pattern = "\\.csv$",
    recursive = TRUE,
    lastpage_required = 10,
    code_col = "code",
    drop_cols = c("id","submitdate","lastpage","startlanguage","seed","token","startdate","datestamp"),
    output_file = NULL,
    deduplicate = c("latest","none","first","most_complete"),
    timestamp_cols = c("datestamp","submitdate","startdate")
) {
  # ---- defaults / checks ----
  deduplicate <- match.arg(deduplicate)

  stopifnot(is.character(root), length(root) == 1L)
  if (!dir.exists(root)) {
    rlang::abort(paste0("Directory does not exist: ", root))
  }

  files <- list.files(root, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (length(files) == 0L) {
    cli::cli_warn("No CSV files found in {.path {root}} with pattern {.val {pattern}}.")
    return(dplyr::tibble())
  }

  cli::cli_inform("Reading {length(files)} file{?s} from {root} ...")

  # ---- I/O helper ----
  read_one <- function(path) {
    first <- tryCatch(readr::read_lines(path, n_max = 1), error = function(e) "")
    semi <- stringr::str_count(first, ";")
    comma <- stringr::str_count(first, ",")
    delim <- if (!is.na(semi) && !is.na(comma) && semi > comma) ";" else ","

    suppressWarnings(
      readr::read_delim(
        file = path,
        delim = delim,
        col_types = readr::cols(.default = readr::col_character()),
        locale = readr::locale(decimal_mark = ".", grouping_mark = ",", encoding = "UTF-8"),
        na = c("", "NA", "N/A", "null", "Null", "NULL")
      )
    ) |>
      dplyr::mutate(.file = basename(path),
                    .row_id = dplyr::row_number())
  }

  dfs <- lapply(files, read_one)

  has_code <- vapply(dfs, function(x) code_col %in% names(x), logical(1))
  if (!all(has_code)) {
    bad <- basename(files[!has_code])
    rlang::abort(c(
      "Missing code column in one or more files.",
      x = paste("Files missing column", sQuote(code_col), ":", paste(bad, collapse = ", "))
    ))
  }

  dat_raw <- dplyr::bind_rows(dfs, .id = ".src_idx")

  # ---- normalize names (remove [ ]) ----
  new_names <- names(dat_raw) |>
    stringr::str_replace_all("\\[|\\]", "") |>
    make.unique(sep = "_")
  names(dat_raw) <- new_names

  # ---- filter on lastpage before dropping admin ----
  if (!"lastpage" %in% names(dat_raw)) {
    rlang::abort("Required column 'lastpage' not found after reading.")
  }

  dat_raw <- dat_raw |>
    dplyr::mutate(lastpage_num = suppressWarnings(as.integer(lastpage)))

  kept <- dat_raw |>
    dplyr::filter(.data$lastpage_num == as.integer(lastpage_required))

  dropped_n <- nrow(dat_raw) - nrow(kept)
  cli::cli_inform("{dropped_n} row{?s} dropped because lastpage != {lastpage_required} or missing.")

  # ---- DEDUP helpers ----
  parse_dt <- function(x) suppressWarnings(readr::parse_datetime(x, na = c("", "NA")))

  # ensure timestamp cols exist
  for (tc in timestamp_cols) if (!tc %in% names(kept)) kept[[tc]] <- NA_character_

  kept <- kept |>
    dplyr::mutate(
      .ts_primary = parse_dt(kept[[timestamp_cols[1]]])
    )

  if (length(timestamp_cols) > 1L) {
    for (i in 2:length(timestamp_cols)) {
      col <- timestamp_cols[i]
      kept <- kept |>
        dplyr::mutate(
          .ts_primary = dplyr::coalesce(.ts_primary, parse_dt(kept[[col]]))
        )
    }
  }

  # count non-missing/non-empty (type-safe), exclude tech cols
  tech_cols <- c(".file",".row_id",".src_idx","lastpage_num",".ts_primary")
  non_tech <- setdiff(names(kept), tech_cols)
  nonempty_mat <- as.data.frame(lapply(kept[non_tech], function(v) {
    if (is.character(v)) {
      !is.na(v) & nzchar(v)
    } else {
      !is.na(v)
    }
  }), stringsAsFactors = FALSE)

  kept <- kept |>
    dplyr::mutate(
      .nonempty = if (ncol(nonempty_mat) == 0L) 0L else rowSums(nonempty_mat, na.rm = TRUE)
    )

  # Only dedup codes that are present
  has_code_val <- !is.na(kept[[code_col]]) & kept[[code_col]] != ""
  dedup_block <- kept[has_code_val, , drop = FALSE]
  rest_block  <- kept[!has_code_val, , drop = FALSE]

  if (nrow(dedup_block) > 0 && deduplicate != "none") {
    if (deduplicate == "latest") {
      dedup_block <- dedup_block |>
        dplyr::group_by(.data[[code_col]]) |>
        dplyr::arrange(dplyr::desc(.ts_primary), dplyr::desc(.nonempty), .src_idx, .row_id, .by_group = TRUE) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    } else if (deduplicate == "first") {
      dedup_block <- dedup_block |>
        dplyr::group_by(.data[[code_col]]) |>
        dplyr::arrange(.src_idx, .row_id, .by_group = TRUE) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    } else if (deduplicate == "most_complete") {
      dedup_block <- dedup_block |>
        dplyr::group_by(.data[[code_col]]) |>
        dplyr::arrange(dplyr::desc(.nonempty), dplyr::desc(.ts_primary), .src_idx, .row_id, .by_group = TRUE) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    }
    removed <- sum(has_code_val) - nrow(dedup_block)
    cli::cli_inform("{removed} duplicate row{?s} removed via deduplicate = '{deduplicate}'.")
  } else {
    if (deduplicate == "none") {
      dups <- dedup_block |>
        dplyr::count(.data[[code_col]]) |>
        dplyr::filter(.data$n > 1)
      if (nrow(dups) > 0) {
        cli::cli_warn("{nrow(dups)} duplicate code value{?s} found; rows are retained (deduplicate = 'none').")
      }
    }
  }

  dat <- dplyr::bind_rows(dedup_block, rest_block)

  # ---- drop admin cols AFTER dedup ----
  drop_cols2 <- unique(c(drop_cols, "lastpage_num", ".ts_primary", ".nonempty", ".row_id", ".src_idx", ".file"))
  present_drop <- intersect(drop_cols2, names(dat))
  dat <- dplyr::select(dat, -dplyr::all_of(present_drop))

  # ---- standardize mis-typed columns (gneder1 -> gender1) ----
  if ("gneder1" %in% names(dat) && !"gender1" %in% names(dat)) {
    dat <- dplyr::rename(dat, gender1 = "gneder1")
  }

  # ---- group mapping ----
  if ("group" %in% names(dat)) {
    grp_map <- c("1" = "LEN","2" = "LEO","3" = "MAR","4" = "MEL","5" = "LLO","6" = "LHG","7" = "HGM")
    dat <- dat |>
      dplyr::mutate(
        group_clean = stringr::str_trim(group),
        group_name  = dplyr::recode(group_clean, !!!grp_map, .default = NA_character_),
        group_name  = factor(group_name, levels = unname(grp_map))
      ) |>
      dplyr::relocate(dplyr::any_of(c(code_col, "group", "group_name")), .before = dplyr::everything())

    unknown_grp <- dat |> dplyr::filter(!is.na(group), is.na(group_name)) |> nrow()
    if (unknown_grp > 0) {
      cli::cli_warn("{unknown_grp} row{?s} with unknown 'group' code (not in 1..7).")
    }
  } else {
    cli::cli_inform("Column 'group' not found; skipping group label mapping.")
  }

  # ==============================
  # LABEL-AWARE NUMERIC + FACTOR
  # ==============================

  # helpers
  to_int <- function(x) suppressWarnings(as.integer(stringr::str_trim(as.character(x))))
  make_factor_from_num <- function(num_vec, code_to_label, ordered = FALSE, levels_order = NULL) {
    lab <- dplyr::recode(as.character(num_vec), !!!code_to_label, .default = NA_character_)
    if (is.null(levels_order)) {
      factor(lab, levels = unname(code_to_label), ordered = ordered)
    } else {
      factor(lab, levels = levels_order, ordered = ordered)
    }
  }
  to_code_from_label <- function(x, label_to_code) {
    x_chr <- stringr::str_trim(as.character(x))
    out <- suppressWarnings(as.integer(x_chr))
    need_map <- is.na(out) & nzchar(x_chr)
    if (any(need_map)) {
      mapped <- label_to_code[tolower(x_chr[need_map])]
      mapped <- suppressWarnings(as.integer(mapped))
      out[need_map] <- mapped
    }
    out
  }
  reverse_map <- function(code_to_label) {
    stats::setNames(names(code_to_label), tolower(unname(code_to_label)))
  }

  # --- Recodes (create new, clear names), then drop originals ---

  # gender -> gender_current_*
  if ("gender" %in% names(dat)) {
    map <- c("0"="weiblich","1"="männlich","2"="divers")
    revm <- reverse_map(map)
    num <- to_code_from_label(dat$gender, revm)
    dat$gender_current_num <- num
    dat$gender_current_f   <- make_factor_from_num(num, map, ordered = FALSE)
  }

  # sex -> sex_assigned_at_birth_*
  if ("sex" %in% names(dat)) {
    map <- c("0"="weiblich","1"="männlich")
    revm <- reverse_map(map)
    num <- to_code_from_label(dat$sex, revm)
    dat$sex_assigned_at_birth_num <- num
    dat$sex_assigned_at_birth_f   <- make_factor_from_num(num, map, ordered = FALSE)
  }

  # gender1 -> gender_identity_*  (after gneder1 fix above)
  if ("gender1" %in% names(dat)) {
    map <- c("0"="weiblich","1"="männlich","2"="divers","3"="keine Angabe")
    revm <- reverse_map(map)
    num <- to_code_from_label(dat$gender1, revm)
    dat$gender_identity_num <- num
    dat$gender_identity_f   <- make_factor_from_num(num, map, ordered = FALSE)
  }

  # gender2 -> gender_transition_status_*
  if ("gender2" %in% names(dat)) {
    map <- c("0"="Ja","1"="Nein","2"="möchte ich nicht sagen")
    revm <- reverse_map(map)
    num <- to_code_from_label(dat$gender2, revm)
    dat$gender_transition_status_num <- num
    dat$gender_transition_status_f   <- make_factor_from_num(num, map, ordered = FALSE)
  }

  # gender31 -> voice_gender_perception_*
  if ("gender31" %in% names(dat)) {
    num <- to_int(dat$gender31)
    lvls <- c("sehr männlich","männlich","eher männlich","neutral",
              "eher weiblich","weiblich","sehr weiblich")
    dat$voice_gender_perception_num <- num
    dat$voice_gender_perception_f <- factor(
      dplyr::case_when(
        num == 1 ~ "sehr männlich",
        num == 2 ~ "männlich",
        num == 3 ~ "eher männlich",
        num == 4 ~ "neutral",
        num == 5 ~ "eher weiblich",
        num == 6 ~ "weiblich",
        num == 7 ~ "sehr weiblich",
        TRUE ~ NA_character_
      ),
      levels = lvls, ordered = TRUE
    )
  }

  # gender41 -> appearance_gender_perception_*
  if ("gender41" %in% names(dat)) {
    num <- to_int(dat$gender41)
    lvls <- c("sehr maskulin","maskulin","eher maskulin","neutral",
              "eher feminin","feminin","sehr feminin")
    dat$appearance_gender_perception_num <- num
    dat$appearance_gender_perception_f <- factor(
      dplyr::case_when(
        num == 1 ~ "sehr maskulin",
        num == 2 ~ "maskulin",
        num == 3 ~ "eher maskulin",
        num == 4 ~ "neutral",
        num == 5 ~ "eher feminin",
        num == 6 ~ "feminin",
        num == 7 ~ "sehr feminin",
        TRUE ~ NA_character_
      ),
      levels = lvls, ordered = TRUE
    )
  }

  # education -> education_*
  if ("education" %in% names(dat)) {
    map <- c(
      "1" = "Kein Abschluss",
      "2" = "Pflichtschule",
      "3" = "Berufsschule (LAP)",
      "4" = "Matura/Abitur",
      "5" = "Fachhochschule",
      "6" = "Universität"
    )
    revm <- reverse_map(map)
    num <- to_code_from_label(dat$education, revm)
    dat$education_num <- num
    dat$education_f   <- make_factor_from_num(num, map, ordered = TRUE, levels_order = unname(map))
  }

  # ---- DROP original raw variables (keep only new *_num / *_f) ----
  raw_to_drop <- c("gender","sex","gender1","gneder1","gender2","gender31","gender41","education")
  present_raw <- intersect(raw_to_drop, names(dat))
  if (length(present_raw)) {
    dat <- dplyr::select(dat, -dplyr::all_of(present_raw))
  }

  # ---- tidy column order: put id/code/group up front, then new *_num/_f ----
  front <- c(code_col, "group", "group_name",
             "gender_current_num","gender_current_f",
             "sex_assigned_at_birth_num","sex_assigned_at_birth_f",
             "gender_identity_num","gender_identity_f",
             "gender_transition_status_num","gender_transition_status_f",
             "voice_gender_perception_num","voice_gender_perception_f",
             "appearance_gender_perception_num","appearance_gender_perception_f",
             "education_num","education_f")
  dat <- dat |>
    dplyr::relocate(dplyr::any_of(front), .before = dplyr::everything())

  # ---- write-out optional ----
  if (!is.null(output_file)) {
    readr::write_csv(dat, output_file)
    cli::cli_inform("Combined preprocessed CSV written to {.path {output_file}}.")
  }

  dat
}
