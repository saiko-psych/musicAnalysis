#' musicAnalysis: tools for robust processing of music psychology data
#'
#' The package provides parsers and cleaners for experimental test data
#' (e.g., KLAWA), returning merge-friendly tibbles with consistent schemas.
#'
#' @section Configuration (labels and behavior):
#' By default, parsers look for ASCII spellings of German labels
#' (e.g., "Lautstaerkendifferenz", "Tonhöhe"). If your PDFs use different
#' wording (e.g., "Pitch", "F0", "Grundfrequenz") you can override the patterns
#' **without changing code**:
#'
#' \preformatted{
#'   # override only the pitch label for this R session
#'   set_ma_options(labels = list(
#'     pitch = "(Tonhöhe|Pitch|F0|Fo|Grundfrequenz|Fundamentalfrequenz)"
#'   ))
#' }
#'
#' See \code{\link{ma_options}} and \code{\link{set_ma_options}}.
#'
#' @section Typical workflow:
#' \preformatted{
#'   library(musicAnalysis)
#'   df <- klawa_scan("data/KLAWA")
#'   validate_dataset(df)   # check required columns
#'   peek_problems(df)      # quick diagnostics
#' }
#'
#' @docType package
#' @name musicAnalysis
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils head modifyList
#' @importFrom rlang .data
#' @importFrom magrittr %>%
## usethis namespace: end
NULL


