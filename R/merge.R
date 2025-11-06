#' Merge two datasets by validated `code`
#'
#' Safely merges (left join) two data frames by `code`. Validates column presence and uniqueness.
#'
#' @param x,y Data frames containing a `code` column.
#' @param suffix Character vector of length 2 passed to dplyr::left_join suffix argument.
#' @return A tibble with rows of `x` and matched columns from `y`.
#' @examples
#' # merge_by_code(klawa_tbl, survey_tbl)
#' @export
merge_by_code <- function(x, y, suffix = c(".x", ".y")) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  if (!"code" %in% names(x)) stop("`x` has no `code` column.", call. = FALSE)
  if (!"code" %in% names(y)) stop("`y` has no `code` column.", call. = FALSE)

  # Defensive: trim & normalize code
  norm_code <- function(v) {
    v <- trimws(v)
    v[v == ""] <- NA_character_
    v
  }
  x$code <- norm_code(x$code)
  y$code <- norm_code(y$code)

  dup_x <- anyDuplicated(x$code[!is.na(x$code)]) > 0
  dup_y <- anyDuplicated(y$code[!is.na(y$code)]) > 0
  if (dup_y) cli::cli_warn("{sum(duplicated(y$code, incomparables = NA))} duplicated codes in `y` will cause row expansion.")

  dplyr::left_join(tibble::as_tibble(x), tibble::as_tibble(y), by = "code", suffix = suffix)
}
