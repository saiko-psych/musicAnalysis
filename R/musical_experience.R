#' Combine LimeSurvey musical experience: time + profile
#'
#' @description
#' Runs the time parser [musical_experience_time()] and the profile parser
#' [musical_experience_profile()] on the same CSV and safely merges them by `code`.
#' Allows forwarding arguments to each sub-parser and basic post-merge validation.
#'
#' @param file Path to the LimeSurvey CSV export.
#' @param join One of `"left"`, `"inner"`, `"full"`; default `"left"`.
#' @param suffix Character vector of length 2 with name suffixes applied on collisions
#'   (`c(".time", ".profile")` by default).
#' @param time_args Named list of extra arguments forwarded to
#'   [musical_experience_time()]. Example: `list(min_lastpage = 4, weekly_max_hours = 84)`.
#' @param profile_args Named list of extra arguments forwarded to
#'   [musical_experience_profile()]. Example: `list(min_lastpage = 4)`.
#' @param profile_select_regex Optional regex; if provided, only profile columns whose
#'   names match this pattern (plus `code`) are kept before the merge.
#'   Useful to keep the merged table lean, e.g. `profile_select_regex = "^(main_instrument|music_status)"`.
#' @param validate If `TRUE`, computes a small NA-inflation report for columns that
#'   existed already in the time table.
#' @param verbose If `TRUE`, prints concise progress messages.
#'
#' @return A list with:
#' \itemize{
#'   \item `sections`: list with the raw section results (`time`, `profile`).
#'   \item `wide`: merged tibble by `code`.
#'   \item `flags`: forwarded flags from `musical_experience_time()` (if any).
#'   \item `merge_notes`: tibble with NA-inflation report (or `NULL`).
#' }
#' @export
musical_experience <- function(
    file,
    join = c("left", "inner", "full"),
    suffix = c(".time", ".profile"),
    time_args = list(),
    profile_args = list(),
    profile_select_regex = NULL,
    validate = TRUE,
    verbose = TRUE
) {
  join <- match.arg(join)

  # --- call sub-parsers with forwarded args ---
  time_call <- utils::modifyList(list(file = file, verbose = verbose), time_args)
  prof_call <- utils::modifyList(list(file = file, verbose = verbose), profile_args)

  time_out  <- do.call(musical_experience_time, time_call)
  profile_w <- do.call(musical_experience_profile, prof_call)

  if (!is.list(time_out) || is.null(time_out$wide)) {
    stop("`musical_experience_time()` must return a list containing a `wide` element.")
  }
  if (!"code" %in% names(time_out$wide))  stop("`time$wide` has no `code` column.")
  if (!"code" %in% names(profile_w))      stop("`profile` has no `code` column.")

  # --- optionally keep only a subset of profile columns ---
  if (!is.null(profile_select_regex) && is.character(profile_select_regex) && nzchar(profile_select_regex)) {
    keep <- c("code", grep(profile_select_regex, names(profile_w), value = TRUE))
    keep <- unique(keep)
    profile_w <- dplyr::select(profile_w, dplyr::all_of(keep))
    if (verbose) cli::cli_inform(c("i" = sprintf("Profile subset kept: %d columns.", ncol(profile_w))))
  }

  # --- join (safe) ---
  base <- time_out$wide

  # Safe join helper with duplication checks
  .safe_join <- function(x, y, by = "code", how = c("left","inner","full"), suffix = c(".x",".y")) {
    how <- match.arg(how)
    if (anyDuplicated(x[[by]])) stop("Left table has duplicated IDs in `", by, "`.")
    if (anyDuplicated(y[[by]])) stop("Right table has duplicated IDs in `", by, "`.")
    overlap <- intersect(setdiff(names(x), by), setdiff(names(y), by))
    if (length(overlap) > 0) {
      names(y)[match(overlap, names(y))] <- paste0(overlap, suffix[2])
    }
    switch(how,
           left  = dplyr::left_join(x, y, by = by, multiple = "error"),
           inner = dplyr::inner_join(x, y, by = by, multiple = "error"),
           full  = dplyr::full_join(x, y, by = by, multiple = "error"))
  }

  wide <- .safe_join(base, profile_w, by = "code", how = join, suffix = suffix)

  # --- NA inflation validation ---
  merge_notes <- NULL
  if (validate) {
    common <- intersect(names(base), names(wide))
    na_before <- vapply(base[common], function(v) mean(is.na(v)), numeric(1))
    na_after  <- vapply(wide[common], function(v) mean(is.na(v)), numeric(1))
    changed   <- which(abs(na_before - na_after) > .Machine$double.eps)
    if (length(changed)) {
      merge_notes <- tibble::tibble(
        column    = names(na_before)[changed],
        na_before = unname(na_before[changed]),
        na_after  = unname(na_after[changed])
      )
      if (verbose) cli::cli_inform(c("i" = sprintf("NA-inflation: %d column(s) changed.", length(changed))))
    } else if (verbose) {
      cli::cli_inform(c("i" = "NA-inflation: no change."))
    }
  }

  # --- forward flags from time parser (if present) ---
  flags <- if ("flags" %in% names(time_out)) time_out$flags else NULL

  if (verbose) {
    cli::cli_inform(c(
      "v" = sprintf("Merged result: %d rows, %d columns.", nrow(wide), ncol(wide))
    ))
  }

  structure(
    list(
      sections    = list(time = time_out, profile = profile_w),
      wide        = wide,
      flags       = flags,
      merge_notes = merge_notes
    ),
    class = "musical_experience_result"
  )
}
