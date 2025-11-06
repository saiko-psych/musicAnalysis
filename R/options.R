#' Get default musicAnalysis options
#'
#' Returns a list of default options used by parsers (currently only label
#' regexes). Defaults are ASCII for portability. You can override them at
#' runtime with [set_ma_options()].
#'
#' @return A named list, e.g. `list(labels = list(...))`.
#' @examples
#' # Show current defaults
#' ma_options()
#'
#' # Accept several alternative pitch terms (for this R session only):
#' set_ma_options(labels = list(
#'   pitch = "(Tonh\u00F6he|Pitch|F0|Fo|Grundfrequenz|Fundamentalfrequenz)"
#' ))
#' @export
ma_options <- function() {
  list(
    labels = list(
      volume = "Lautstaerkendifferenz",
      pitch  = "Tonh\u00F6he",
      # use ASCII hyphen and \u2013 (en dash) to stay portable in code
      onset  = "Tonbeginn\\s*[-\\u2013]?\\s*Differenz",
      durdif = "Laengendifferenz"
    )
  )
}

#' Override musicAnalysis options for the current session
#'
#' Use this to adjust label regexes without editing package code.
#' Only provide the entries you want to change; all other options remain at
#' their defaults.
#'
#' @param ... Named elements to override, e.g. `labels = list(pitch = "\u2026")`.
#' @return (Invisibly) the updated options list.
#' @examples
#' # Override only the pitch label (keep all others at defaults)
#' set_ma_options(labels = list(
#'   pitch = "(Tonh\u00F6he|Pitch|F0|Fo|Grundfrequenz)"
#' ))
#' @export
set_ma_options <- function(...) {
  current <- getOption("musicAnalysis.options", default = ma_options())
  updated <- modifyList(current, list(...), keep.null = TRUE)
  options("musicAnalysis.options" = updated)
  invisible(updated)
}

# Internal getter used by parsers (not exported)
#' @keywords internal
#' @noRd
ma_get <- function(key) {
  getOption("musicAnalysis.options", default = ma_options())[[key]]
}
