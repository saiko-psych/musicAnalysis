#' Scan folder for PPPT data files
#'
#' Recursively scans a folder structure for PPPT .rsl.csv files and extracts
#' PPP (Pitch Perception Proficiency) indices per participant.
#'
#' @param root Path to root folder containing PPPT data
#' @param code_pattern Regex pattern for participant codes (default: 4 digits + 4 letters)
#' @param date_format Date format in filenames (default: "DDMMYY")
#' @return Tibble with columns: code, ppp_index_overall, ppp_index_294, ppp_index_523,
#'   ppp_index_932, ppp_index_1661, ppp_index_2960, ppp_index_5274, date, file
#' @export
#' @examples
#' \dontrun{
#' pppt_data <- pppt_scan("data/PPPT")
#' }
pppt_scan <- function(root,
                      code_pattern = "\\d{4}[A-Za-z]{4}",
                      date_format = "DDMMYY") {

  # Validate root directory
  if (!fs::dir_exists(root)) {
    cli::cli_abort("Directory {.path {root}} does not exist.")
  }

  # Find all .rsl.csv files (will filter by content later if needed)
  # Some PPPT files don't have "PPPT" in filename, so we need to check content
  all_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                          regexp = "\\.rsl\\.csv$")

  if (length(all_files) == 0) {
    cli::cli_warn("No PPPT .rsl.csv files found under {.path {root}}.")
    return(tibble::tibble(
      code = character(),
      ppp_index_overall = numeric(),
      ppp_index_294 = numeric(),
      ppp_index_523 = numeric(),
      ppp_index_932 = numeric(),
      ppp_index_1661 = numeric(),
      ppp_index_2960 = numeric(),
      ppp_index_5274 = numeric(),
      date = character(),
      file = character()
    ))
  }

  cli::cli_inform("Found {length(all_files)} PPPT file{?s} to process.")

  # Process each file
  results <- purrr::map_dfr(all_files, function(file_path) {
    tryCatch({
      pppt_parse_one(file_path, code_pattern = code_pattern, date_format = date_format)
    }, error = function(e) {
      cli::cli_warn("Error processing {.path {basename(file_path)}}: {e$message}")
      NULL
    })
  })

  # Make file paths relative to root
  results$file <- fs::path_rel(results$file, start = root)

  tibble::as_tibble(results)
}

#' Parse a single PPPT .rsl.csv file
#'
#' @param file_path Path to PPPT .rsl.csv file
#' @param code_pattern Regex pattern for participant codes
#' @param date_format Date format in filename
#' @return Tibble with one row containing extracted data
#' @keywords internal
pppt_parse_one <- function(file_path, code_pattern, date_format) {

  # Extract metadata from filename
  filename <- basename(file_path)

  # Extract participant code
  code <- extract_and_check_code(filename, pattern = code_pattern)

  # Extract date
  date <- .extract_date_from_filename(filename, date_format)

  # Read CSV file
  data <- readr::read_csv(
    file_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  # Check if required columns exist (filter out non-PPPT files)
  required_cols <- c("UCF", "PPP Index")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    # This is not a PPPT file, skip it silently
    return(NULL)
  }

  # Extract PPP indices for each UCF frequency
  ppp_294 <- .extract_ppp_for_ucf(data, "294")
  ppp_523 <- .extract_ppp_for_ucf(data, "523")
  ppp_932 <- .extract_ppp_for_ucf(data, "932")
  ppp_1661 <- .extract_ppp_for_ucf(data, "1661")
  ppp_2960 <- .extract_ppp_for_ucf(data, "2960")
  ppp_5274 <- .extract_ppp_for_ucf(data, "5274")

  # Extract overall PPP index
  overall_row <- data[data$UCF == "Overall", ]
  ppp_overall <- if (nrow(overall_row) > 0) {
    as.numeric(overall_row$`PPP Index`[1])
  } else {
    NA_real_
  }

  tibble::tibble(
    code = code,
    ppp_index_overall = ppp_overall,
    ppp_index_294 = ppp_294,
    ppp_index_523 = ppp_523,
    ppp_index_932 = ppp_932,
    ppp_index_1661 = ppp_1661,
    ppp_index_2960 = ppp_2960,
    ppp_index_5274 = ppp_5274,
    date = date,
    file = as.character(file_path)
  )
}

#' Extract PPP index for a specific UCF frequency
#'
#' @param data Data frame with UCF and PPP Index columns
#' @param ucf_value UCF value to extract (as string)
#' @return Numeric PPP index value or NA
#' @keywords internal
.extract_ppp_for_ucf <- function(data, ucf_value) {
  row <- data[data$UCF == ucf_value, ]
  if (nrow(row) > 0) {
    as.numeric(row$`PPP Index`[1])
  } else {
    NA_real_
  }
}

#' Analyze PPPT folder structure
#'
#' Scans folder to identify PPPT files and their organization
#'
#' @param root Path to root folder
#' @return List with structure information
#' @export
pppt_analyze_structure <- function(root) {
  if (!fs::dir_exists(root)) {
    cli::cli_abort("Directory {.path {root}} does not exist.")
  }

  all_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                          regexp = "PPPT.*\\.rsl\\.csv$")

  itl_files <- fs::dir_ls(root, recurse = TRUE, type = "file",
                          regexp = "PPPT.*\\.itl\\.csv$")

  list(
    n_rsl_files = length(all_files),
    n_itl_files = length(itl_files),
    rsl_files = basename(all_files),
    itl_files = basename(itl_files),
    folder_structure = fs::path_rel(dirname(all_files), start = root)
  )
}
