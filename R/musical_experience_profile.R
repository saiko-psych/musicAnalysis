#' Parse LimeSurvey musical experience (profile: clean fields only)
#'
#' @description
#' Defensively loads a LimeSurvey CSV, ensures an ID column `code`, optionally
#' filters by `lastpage`, and returns a clean, wide tibble with **no raw columns**.
#' Field names follow a stable English scheme; value **labels** are provided as
#' parallel `*_label` columns where useful.
#'
#' Included blocks and fields:
#' - Motivation & status:
#'   - `practice_motivation_early` (0..10) + `practice_motivation_early_label`
#'   - `music_status` (1..6) + `music_status_label`
#'   - `main_instrument` (character)
#'   - `vocal_knowledge` (1/2) + `vocal_knowledge_label`
#' - Voice & hearing:
#'   - `estimated_vocal` (1..10) + `estimated_vocal_label` (1 = sehr tief, 10 = sehr hoch)
#'   - `vocal_range` (1..10 \u2192 Bass/Tenor/Alt/Sopran) + `vocal_range_label`
#'   - `handedness` (-5..5; L..R) + `handedness_label`
#'   - `absolute_hearing` (1..3) + `absolute_hearing_label`
#' - Early musical exposure (formerly `music2[...]`), renamed:
#'   - `parents_sang`, `listened_children_music`, `liked_singing`,
#'     `parents_musicians`, `parents_non_musicians` (0/1) + each `*_label`
#'   - free text `early_experience_other`
#'   - `parents_musicians_contradiction_flag` = 1 if both parents_musicians==1 and
#'     parents_non_musicians==1 (else 0; NA if undetermined)
#' - Inner hearing & relative pitch:
#'   - `innerhearing_melody_memory`, `innerhearing_earworms_frequency`,
#'     `innerhearing_imagine_memorized_music`, `innerhearing_imagine_from_notation`
#'     (each -10..10) + `*_label`
#'   - `relative_hearing` (-10..10) + `relative_hearing_label`
#' - Ranking blocks (**column names without brackets; value = rank**):
#'   - Preferred way of making music (`preferedmusicmaking1..4`, ranks 1..4) + `*_label` = "Rang <n>"
#'   - Learning music by mental imagery (`musiclearing1..4`, ranks 1..4) + `*_label` = "Rang <n>"
#'   - Preferred genres (`preferedmusic1..7`, ranks 1..7) + `*_label` = "Rang <n>"
#' - Free-text top/least three genres:
#'   - `best_music1..3` + `best_music_concat` (keeps input order)
#'   - `worst_music1..3` + `worst_music_concat` (keeps input order)
#'
#' @param file Path to the LimeSurvey CSV export.
#' @param min_lastpage Keep rows where `lastpage == min_lastpage` when column exists (default: 4).
#' @param id_col Optional column name to be used as `code`. If `NULL`, uses existing `code`,
#'   otherwise creates a synthetic code.
#' @param anchor_regex Only used to warn if the expected block (e.g. `firstyears[1]`) is missing.
#' @param verbose If `TRUE`, prints a short processing summary.
#'
#' @return A tibble with exactly one row per `code` containing only clean fields.
#' @export
musical_experience_profile <- function(
    file,
    min_lastpage = 4,
    id_col = NULL,
    anchor_regex = "(?i)^firstyears\\[1\\]$",
    verbose = TRUE
) {
  # ---------- helpers ----------
  # Case-insensitive exact match ("name" == column name)
  ci_pick <- function(df, name) {
    nms <- names(df); low <- tolower(nms)
    hit <- which(low == tolower(name))
    if (length(hit) == 1L) nms[hit] else NA_character_
  }
  # Robust match for indexed names: accepts base[idx] OR base_idx OR baseidx
  pick_var <- function(df, base, idx) {
    nms <- names(df); low <- tolower(nms)
    cands <- tolower(c(
      sprintf("%s[%d]", base, idx),
      sprintf("%s_%d", base, idx),
      sprintf("%s%d",  base, idx)
    ))
    m <- match(cands, low, nomatch = 0L)
    if (any(m > 0L)) nms[m[m > 0L][1]] else NA_character_
  }

  # mehrere Basisnamen zulassen (z.B. "musiclearing" ODER "musiclearning")
  pick_var_any <- function(df, bases, idx) {
    nms <- names(df); low <- tolower(nms)
    for (b in bases) {
      cands <- tolower(c(
        sprintf("%s[%d]", b, idx),
        sprintf("%s_%d", b, idx),
        sprintf("%s%d",  b, idx)
      ))
      m <- match(cands, low, nomatch = 0L)
      if (any(m > 0L)) return(nms[m[m > 0L][1]])
    }
    NA_character_
  }

  # Rang -> Label per Mapping (labels[rank]); NA bleibt NA
  rank_to_label <- function(rank_vec, labels) {
    ifelse(is.na(rank_vec), NA_character_, labels[rank_vec])
  }

  # Create binary label (ja/nein)
  binary_label <- function(int_vec) {
    dplyr::if_else(int_vec == 1L, "ja",
                   dplyr::if_else(int_vec == 0L, "nein", NA_character_))
  }



  # Robust match for "other": base[other] OR base_other OR baseother
  pick_var_other <- function(df, base, other = "other") {
    nms <- names(df); low <- tolower(nms)
    cands <- tolower(c(
      sprintf("%s[%s]", base, other),
      sprintf("%s_%s", base, other),
      sprintf("%s%s",  base, other)
    ))
    m <- match(cands, low, nomatch = 0L)
    if (any(m > 0L)) nms[m[m > 0L][1]] else NA_character_
  }
  # Extract numeric (handles Unicode minus; comma/point decimals); clamp to [minv,maxv]
  parse_num_range <- function(x, minv, maxv) {
    x <- stringr::str_replace_all(x, "\u2212|\u2013|\u2014", "-")           # unicode minus/dashes -> "-"
    num <- stringr::str_extract(x, "-?\\d+(?:[\\.,]\\d+)?")        # first number
    num <- stringr::str_replace_all(num, ",", ".")
    v <- suppressWarnings(as.numeric(num))
    v[is.na(v) | v < minv | v > maxv] <- NA_real_
    v
  }
  # Extract integer (handles Unicode minus); clamp
  parse_int_range <- function(x, minv, maxv) {
    x <- stringr::str_replace_all(x, "\u2212|\u2013|\u2014", "-")
    d <- stringr::str_extract(x, "-?\\d+")
    v <- suppressWarnings(as.integer(d))
    v[is.na(v) | v < minv | v > maxv] <- NA_integer_
    v
  }
  # Map 0/1-ish answers (ja/yes/true -> 1; nein/no/false -> 0)
  parse_binary01 <- function(x) {
    y <- stringr::str_trim(tolower(x))
    dplyr::case_when(
      y %in% c("1","ja","yes","y","true","wahr")  ~ 1L,
      y %in% c("0","nein","no","n","false","falsch") ~ 0L,
      TRUE ~ NA_integer_
    )
  }
  norm_text <- function(x) {
    y <- stringr::str_squish(x)
    y[y %in% c("", "NA", "na")] <- NA_character_
    y
  }
  # Label for rank values (in German, as requested)
  rank_label_vec <- function(r, max_rank) {
    dplyr::case_when(
      is.na(r) ~ NA_character_,
      r >= 1 & r <= max_rank ~ paste0("Rang ", r),
      TRUE ~ NA_character_
    )
  }

  # ---------- read defensively ----------
  raw <- readr::read_csv(
    file,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character())
  )
  if ("lastpage" %in% names(raw)) {
    raw <- dplyr::filter(raw, .data$lastpage == as.character(min_lastpage))
  }
  if (!is.null(id_col) && id_col %in% names(raw)) {
    raw <- dplyr::rename(raw, code = dplyr::all_of(id_col))
  } else if (!("code" %in% names(raw))) {
    raw <- dplyr::mutate(raw, code = sprintf("SYNTH-%04d", dplyr::row_number()))
  }
  if (!any(stringr::str_detect(names(raw), anchor_regex))) {
    cli::cli_warn("Anchor column 'firstyears[1]' not found. Proceeding with profile parsing.")
  }
  n <- nrow(raw)

  # ---------- Motivation & status ----------
  # practice_motivation_early (firstyears[1]) 0..10
  col_firstyears1 <- pick_var(raw, "firstyears", 1)
  v <- if (!is.na(col_firstyears1)) parse_num_range(raw[[col_firstyears1]], 0, 10) else rep(NA_real_, n)
  practice_motivation_early <- v
  practice_motivation_early_label <- dplyr::case_when(
    is.na(v) ~ NA_character_,
    v == 0   ~ "Mit \u00E4u\u00DFerer Anregung musiziert (Fremdimpuls)",
    v == 10  ~ "Aus eigenem Antrieb musiziert (Eigenimpuls)",
    TRUE     ~ "zwischen"
  )

  # music_status (music1) 1..6
  col_music1 <- ci_pick(raw, "music1")
  ms <- if (!is.na(col_music1)) parse_int_range(raw[[col_music1]], 1, 6) else rep(NA_integer_, n)
  music_status <- ms
  music_status_label <- dplyr::case_when(
    ms == 1L ~ "MusikstudentIn",
    ms == 2L ~ "gerade abgeschlossenes Musikstudium",
    ms == 3L ~ "BerufsmusikerIn",
    ms == 4L ~ "DozentIn an einer Musik(hoch)schule",
    ms == 5L ~ "Hobbymusiker",
    ms == 6L ~ "Nichtmusiker",
    TRUE ~ NA_character_
  )

  # main_instrument (maininstrument / mainistrument)
  col_main <- ci_pick(raw, "maininstrument"); if (is.na(col_main)) col_main <- ci_pick(raw, "mainistrument")
  main_instrument <- if (!is.na(col_main)) norm_text(raw[[col_main]]) else rep(NA_character_, n)

  # vocal_knowledge (vocalknowledge) 1/2
  col_vocal <- ci_pick(raw, "vocalknowledge")
  vk <- if (!is.na(col_vocal)) dplyr::case_when(
    stringr::str_detect(raw[[col_vocal]], "^(?i)(1|yes|y|ja)$")  ~ 1L,
    stringr::str_detect(raw[[col_vocal]], "^(?i)(2|no|n|nein)$") ~ 2L,
    TRUE ~ NA_integer_
  ) else rep(NA_integer_, n)
  vocal_knowledge <- vk
  vocal_knowledge_label <- dplyr::case_when(
    vk == 1L ~ "yes",
    vk == 2L ~ "no",
    TRUE ~ NA_character_
  )

  # ---------- Voice & hearing ----------
  # estimated_vocal (estimatedvocal[1]) 1..10
  col_ev <- pick_var(raw, "estimatedvocal", 1)
  ev <- if (!is.na(col_ev)) parse_num_range(raw[[col_ev]], 1, 10) else rep(NA_real_, n)
  estimated_vocal <- ev
  estimated_vocal_label <- dplyr::case_when(
    is.na(ev) ~ NA_character_,
    ev == 1   ~ "sehr tief",
    ev == 10  ~ "sehr hoch",
    TRUE      ~ "zwischen"
  )

  # vocal_range (vocalrange[1]) 1..10 -> Bass/Tenor/Alt/Sopran
  col_vr <- pick_var(raw, "vocalrange", 1)
  vr <- if (!is.na(col_vr)) parse_int_range(raw[[col_vr]], 1, 10) else rep(NA_integer_, n)
  vocal_range <- vr
  vocal_range_label <- dplyr::case_when(
    vr %in% 1:3         ~ "Bass",
    vr == 4L            ~ "Tenor",
    vr %in% c(5L,6L,7L) ~ "Alt",
    vr %in% 8:10        ~ "Sopran",
    TRUE ~ NA_character_
  )

  # handedness (hand[1]) -5..5
  col_hand <- pick_var(raw, "hand", 1)
  h <- if (!is.na(col_hand)) parse_num_range(raw[[col_hand]], -5, 5) else rep(NA_real_, n)
  handedness <- h
  handedness_label <- dplyr::case_when(
    is.na(h) ~ NA_character_,
    h < 0    ~ "L",
    h == 0   ~ "neutral",
    h > 0    ~ "R"
  )

  # absolute_hearing (absolutehearing) 1..3
  col_ah <- ci_pick(raw, "absolutehearing")
  ah <- if (!is.na(col_ah)) parse_int_range(raw[[col_ah]], 1, 3) else rep(NA_integer_, n)
  absolute_hearing <- ah
  absolute_hearing_label <- dplyr::case_when(
    ah == 1L ~ "absolutes Geh\u00F6r",
    ah == 2L ~ "teilweise absolutes Geh\u00F6r",
    ah == 3L ~ "kein absolutes Geh\u00F6r",
    TRUE ~ NA_character_
  )

  # ---------- Early musical exposure (former music2) ----------
  col_m21 <- pick_var(raw, "music2", 1)
  parents_sang <- if (!is.na(col_m21)) parse_binary01(raw[[col_m21]]) else rep(NA_integer_, n)
  parents_sang_label <- binary_label(parents_sang)

  col_m22 <- pick_var(raw, "music2", 2)
  listened_children_music <- if (!is.na(col_m22)) parse_binary01(raw[[col_m22]]) else rep(NA_integer_, n)
  listened_children_music_label <- binary_label(listened_children_music)

  col_m23 <- pick_var(raw, "music2", 3)
  liked_singing <- if (!is.na(col_m23)) parse_binary01(raw[[col_m23]]) else rep(NA_integer_, n)
  liked_singing_label <- binary_label(liked_singing)

  col_m24 <- pick_var(raw, "music2", 4)
  parents_musicians <- if (!is.na(col_m24)) parse_binary01(raw[[col_m24]]) else rep(NA_integer_, n)
  parents_musicians_label <- binary_label(parents_musicians)

  col_m25 <- pick_var(raw, "music2", 5)
  parents_non_musicians <- if (!is.na(col_m25)) parse_binary01(raw[[col_m25]]) else rep(NA_integer_, n)
  parents_non_musicians_label <- binary_label(parents_non_musicians)

  col_m2o <- pick_var_other(raw, "music2", "other")
  early_experience_other <- if (!is.na(col_m2o)) norm_text(raw[[col_m2o]]) else rep(NA_character_, n)

  parents_musicians_contradiction_flag <- dplyr::if_else(
    parents_musicians == 1L & parents_non_musicians == 1L, 1L,
    dplyr::if_else(is.na(parents_musicians) | is.na(parents_non_musicians), NA_integer_, 0L)
  )

  # ---------- Inner hearing & relative hearing ----------
  # innerhearing[1..4]  (-10..10)
  col_ih1 <- pick_var(raw, "innerhearing", 1)
  ih1 <- if (!is.na(col_ih1)) parse_num_range(raw[[col_ih1]], -10, 10) else rep(NA_real_, n)
  innerhearing_melody_memory <- ih1
  innerhearing_melody_memory_label <- dplyr::case_when(
    is.na(ih1) ~ NA_character_,
    ih1 == -10 ~ "schlecht",
    ih1 ==  10 ~ "gut",
    TRUE ~ "zwischen"
  )

  col_ih2 <- pick_var(raw, "innerhearing", 2)
  ih2 <- if (!is.na(col_ih2)) parse_num_range(raw[[col_ih2]], -10, 10) else rep(NA_real_, n)
  innerhearing_earworms_frequency <- ih2
  innerhearing_earworms_frequency_label <- dplyr::case_when(
    is.na(ih2) ~ NA_character_,
    ih2 == -10 ~ "selten",
    ih2 ==  10 ~ "oft",
    TRUE ~ "zwischen"
  )

  col_ih3 <- pick_var(raw, "innerhearing", 3)
  ih3 <- if (!is.na(col_ih3)) parse_num_range(raw[[col_ih3]], -10, 10) else rep(NA_real_, n)
  innerhearing_imagine_memorized_music <- ih3
  innerhearing_imagine_memorized_music_label <- dplyr::case_when(
    is.na(ih3) ~ NA_character_,
    ih3 == -10 ~ "schlecht",
    ih3 ==  10 ~ "gut",
    TRUE ~ "zwischen"
  )

  col_ih4 <- pick_var(raw, "innerhearing", 4)
  ih4 <- if (!is.na(col_ih4)) parse_num_range(raw[[col_ih4]], -10, 10) else rep(NA_real_, n)
  innerhearing_imagine_from_notation <- ih4
  innerhearing_imagine_from_notation_label <- dplyr::case_when(
    is.na(ih4) ~ NA_character_,
    ih4 == -10 ~ "schlecht",
    ih4 ==  10 ~ "gut",
    TRUE ~ "zwischen"
  )

  # relativehearing[1] (-10..10)
  col_rh1 <- pick_var(raw, "relativehearing", 1)
  rh1 <- if (!is.na(col_rh1)) parse_num_range(raw[[col_rh1]], -10, 10) else rep(NA_real_, n)
  relative_hearing <- rh1
  relative_hearing_label <- dplyr::case_when(
    is.na(rh1) ~ NA_character_,
    rh1 == -10 ~ "schlecht",
    rh1 ==  10 ~ "gut",
    TRUE ~ "zwischen"
  )

  # ---------- Ranking blocks (names without brackets; value = rank) ----------
  # preferedmusicmaking[1..4] -> preferedmusicmaking1..4 (1..4) + *_label (Label gem\u00E4\u00DF Wert)
  mm_labels <- c(
    "Auswendig",
    "aus Noten (ge\u00FCbt)",
    "Improvisiert",
    "Prima Vista (unvorbereitet/ ohne vorherige Kenntnis)"
  )
  mm_cols <- vapply(1:4, function(i)
    pick_var_any(raw, bases = c("preferedmusicmaking","preferredmusicmaking"), idx = i),
    FUN.VALUE = character(1)
  )
  preferedmusicmaking1 <- if (!is.na(mm_cols[1])) parse_int_range(raw[[mm_cols[1]]], 1, 4) else rep(NA_integer_, n)
  preferedmusicmaking2 <- if (!is.na(mm_cols[2])) parse_int_range(raw[[mm_cols[2]]], 1, 4) else rep(NA_integer_, n)
  preferedmusicmaking3 <- if (!is.na(mm_cols[3])) parse_int_range(raw[[mm_cols[3]]], 1, 4) else rep(NA_integer_, n)
  preferedmusicmaking4 <- if (!is.na(mm_cols[4])) parse_int_range(raw[[mm_cols[4]]], 1, 4) else rep(NA_integer_, n)

  # NEU: dynamische Labels \u2013 basierend auf dem Rang-Wert in der jeweiligen Spalte
  preferedmusicmaking1_label <- rank_to_label(preferedmusicmaking1, mm_labels)
  preferedmusicmaking2_label <- rank_to_label(preferedmusicmaking2, mm_labels)
  preferedmusicmaking3_label <- rank_to_label(preferedmusicmaking3, mm_labels)
  preferedmusicmaking4_label <- rank_to_label(preferedmusicmaking4, mm_labels)



  # musiclearing[1..4] / musiclearning[1..4] -> musiclearing1..4 (1..4) + *_label (Label gem\u00E4\u00DF Wert)
  ml_labels <- c(
    "Auditiv (durch Klangvorstellung)",
    "Visuell (durch Vorstellung von Notenbild o.\u00E4.)",
    "Motorisch (Vorstellung von Bewegungsabl\u00E4ufen)",
    "Logisch/assoziativ (Struktur)"
  )
  ml_cols <- vapply(1:4, function(i)
    pick_var_any(raw, bases = c("musiclearing","musiclearning"), idx = i),
    FUN.VALUE = character(1)
  )
  musiclearing1 <- if (!is.na(ml_cols[1])) parse_int_range(raw[[ml_cols[1]]], 1, 4) else rep(NA_integer_, n)
  musiclearing2 <- if (!is.na(ml_cols[2])) parse_int_range(raw[[ml_cols[2]]], 1, 4) else rep(NA_integer_, n)
  musiclearing3 <- if (!is.na(ml_cols[3])) parse_int_range(raw[[ml_cols[3]]], 1, 4) else rep(NA_integer_, n)
  musiclearing4 <- if (!is.na(ml_cols[4])) parse_int_range(raw[[ml_cols[4]]], 1, 4) else rep(NA_integer_, n)

  # NEU: dynamische Labels \u2013 pro Zelle aus dem Rang ableiten
  musiclearing1_label <- rank_to_label(musiclearing1, ml_labels)
  musiclearing2_label <- rank_to_label(musiclearing2, ml_labels)
  musiclearing3_label <- rank_to_label(musiclearing3, ml_labels)
  musiclearing4_label <- rank_to_label(musiclearing4, ml_labels)



  # preferedmusic[1..7] / preferredmusic[1..7] -> preferedmusic1..7 (1..7) + *_label (Label gem\u00E4\u00DF Wert)
  pm_labels <- c(
    "Alte Musik",
    "Wiener Klassik",
    "Romantik",
    "Impressionismus",
    "Zw\u00F6lftonmusik / serielle Musik",
    "Jazz",
    "Pop/rock"
  )
  pm_cols <- vapply(1:7, function(i)
    pick_var_any(raw, bases = c("preferedmusic","preferredmusic"), idx = i),
    FUN.VALUE = character(1)
  )
  preferedmusic1 <- if (!is.na(pm_cols[1])) parse_int_range(raw[[pm_cols[1]]], 1, 7) else rep(NA_integer_, n)
  preferedmusic2 <- if (!is.na(pm_cols[2])) parse_int_range(raw[[pm_cols[2]]], 1, 7) else rep(NA_integer_, n)
  preferedmusic3 <- if (!is.na(pm_cols[3])) parse_int_range(raw[[pm_cols[3]]], 1, 7) else rep(NA_integer_, n)
  preferedmusic4 <- if (!is.na(pm_cols[4])) parse_int_range(raw[[pm_cols[4]]], 1, 7) else rep(NA_integer_, n)
  preferedmusic5 <- if (!is.na(pm_cols[5])) parse_int_range(raw[[pm_cols[5]]], 1, 7) else rep(NA_integer_, n)
  preferedmusic6 <- if (!is.na(pm_cols[6])) parse_int_range(raw[[pm_cols[6]]], 1, 7) else rep(NA_integer_, n)
  preferedmusic7 <- if (!is.na(pm_cols[7])) parse_int_range(raw[[pm_cols[7]]], 1, 7) else rep(NA_integer_, n)

  # NEU: dynamische Labels
  preferedmusic1_label <- rank_to_label(preferedmusic1, pm_labels)
  preferedmusic2_label <- rank_to_label(preferedmusic2, pm_labels)
  preferedmusic3_label <- rank_to_label(preferedmusic3, pm_labels)
  preferedmusic4_label <- rank_to_label(preferedmusic4, pm_labels)
  preferedmusic5_label <- rank_to_label(preferedmusic5, pm_labels)
  preferedmusic6_label <- rank_to_label(preferedmusic6, pm_labels)
  preferedmusic7_label <- rank_to_label(preferedmusic7, pm_labels)



  # ---------- Free-text top/least three genres ----------

  bm_cols <- vapply(1:3, function(i) pick_var(raw, "bestmusic", i), FUN.VALUE = character(1))
  best_music1 <- if (!is.na(bm_cols[1])) norm_text(raw[[bm_cols[1]]]) else rep(NA_character_, n)
  best_music2 <- if (!is.na(bm_cols[2])) norm_text(raw[[bm_cols[2]]]) else rep(NA_character_, n)
  best_music3 <- if (!is.na(bm_cols[3])) norm_text(raw[[bm_cols[3]]]) else rep(NA_character_, n)
  best_music_concat <- paste(
    ifelse(is.na(best_music1), "", best_music1),
    ifelse(is.na(best_music2), "", best_music2),
    ifelse(is.na(best_music3), "", best_music3),
    sep = "; "
  )
  best_music_concat <- stringr::str_replace_all(best_music_concat, "^(;\\s*)+|(;\\s*)+$", "")
  best_music_concat[best_music_concat == ""] <- NA_character_

  wm_cols <- vapply(1:3, function(i) pick_var(raw, "worstmusic", i), FUN.VALUE = character(1))
  worst_music1 <- if (!is.na(wm_cols[1])) norm_text(raw[[wm_cols[1]]]) else rep(NA_character_, n)
  worst_music2 <- if (!is.na(wm_cols[2])) norm_text(raw[[wm_cols[2]]]) else rep(NA_character_, n)
  worst_music3 <- if (!is.na(wm_cols[3])) norm_text(raw[[wm_cols[3]]]) else rep(NA_character_, n)
  worst_music_concat <- paste(
    ifelse(is.na(worst_music1), "", worst_music1),
    ifelse(is.na(worst_music2), "", worst_music2),
    ifelse(is.na(worst_music3), "", worst_music3),
    sep = "; "
  )
  worst_music_concat <- stringr::str_replace_all(worst_music_concat, "^(;\\s*)+|(;\\s*)+$", "")
  worst_music_concat[worst_music_concat == ""] <- NA_character_

  # ---------- output ----------
  out <- tibble::tibble(
    code = raw$code,
    practice_motivation_early, practice_motivation_early_label,
    music_status, music_status_label,
    main_instrument,
    vocal_knowledge, vocal_knowledge_label,
    estimated_vocal, estimated_vocal_label,
    vocal_range, vocal_range_label,
    handedness, handedness_label,
    absolute_hearing, absolute_hearing_label,

    # early experience (renamed):
    parents_sang, parents_sang_label,
    listened_children_music, listened_children_music_label,
    liked_singing, liked_singing_label,
    parents_musicians, parents_musicians_label,
    parents_non_musicians, parents_non_musicians_label,
    early_experience_other,
    parents_musicians_contradiction_flag,

    # inner hearing + relative hearing:
    innerhearing_melody_memory, innerhearing_melody_memory_label,
    innerhearing_earworms_frequency, innerhearing_earworms_frequency_label,
    innerhearing_imagine_memorized_music, innerhearing_imagine_memorized_music_label,
    innerhearing_imagine_from_notation, innerhearing_imagine_from_notation_label,
    relative_hearing, relative_hearing_label,

    # rankings (value = rank; labels = "Rang n"):
    preferedmusicmaking1, preferedmusicmaking1_label,
    preferedmusicmaking2, preferedmusicmaking2_label,
    preferedmusicmaking3, preferedmusicmaking3_label,
    preferedmusicmaking4, preferedmusicmaking4_label,

    musiclearing1, musiclearing1_label,
    musiclearing2, musiclearing2_label,
    musiclearing3, musiclearing3_label,
    musiclearing4, musiclearing4_label,

    preferedmusic1, preferedmusic1_label,
    preferedmusic2, preferedmusic2_label,
    preferedmusic3, preferedmusic3_label,
    preferedmusic4, preferedmusic4_label,
    preferedmusic5, preferedmusic5_label,
    preferedmusic6, preferedmusic6_label,
    preferedmusic7, preferedmusic7_label,

    # free-text:
    best_music1, best_music2, best_music3, best_music_concat,
    worst_music1, worst_music2, worst_music3, worst_music_concat
  )

  if (anyDuplicated(out$code)) {
    cli::cli_warn("Duplicate participant `code` values detected in profile output.")
  }
  if (verbose) {
    cli::cli_inform(c("v" = sprintf("Profile parsed (clean only): %d rows, %d columns.", nrow(out), ncol(out))))
  }
  out
}
