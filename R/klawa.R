#' Analyze KLAWA folder structure and auto-detect metadata patterns
#'
#' Scans the folder hierarchy to automatically detect:
#' - Group names (folders under "Gruppen" or similar)
#' - Measurement types (typically "pre" and "post" folders)
#' - PC identifiers (folders containing "Jeki", "PC", "Computer", etc.)
#'
#' @param root Path to the KLAWA root folder.
#' @return A list with:
#'   \itemize{
#'     \item \code{groups} - Character vector of detected group names
#'     \item \code{measurements} - Character vector of detected measurement types
#'     \item \code{pcs} - Character vector of detected PC identifiers
#'     \item \code{tree} - Nested list representing folder structure
#'     \item \code{tree_text} - Character vector with formatted tree display
#'   }
#' @export
#' @examples
#' \dontrun{
#' structure <- klawa_analyze_structure("data/KLAWA")
#' cat(structure$tree_text, sep = "\n")
#' }
klawa_analyze_structure <- function(root) {
  if (!dir.exists(root)) {
    stop("Directory does not exist: ", root, call. = FALSE)
  }

  # Get all directories recursively
  all_dirs <- fs::dir_ls(root, recurse = TRUE, type = "directory")
  rel_dirs <- fs::path_rel(all_dirs, start = root)

  # Measurement patterns to exclude from group detection
  measurement_patterns <- c("pre", "post", "vorher", "nachher", "vor", "nach", "baseline", "followup")

  # Detect groups - look at folder structure depth
  groups <- character(0)

  # Strategy: Groups are typically at depth 2 (after PC folder) in structure: <PC>/<GROUP>/<measurement>
  # Or at depth 1 if no PC folder: <GROUP>/<measurement>
  # We'll look for folders that have measurement subfolders

  split_paths <- strsplit(rel_dirs, "/")
  depths <- sapply(split_paths, length)

  # Check each directory to see if it contains measurement subfolders
  for (dir in rel_dirs) {
    dir_path <- fs::path(root, dir)
    if (!dir.exists(dir_path)) next

    subdirs <- list.dirs(dir_path, full.names = FALSE, recursive = FALSE)
    if (length(subdirs) == 0) next

    # Check if this directory contains measurement folders
    subdirs_lower <- tolower(subdirs)
    has_measurements <- any(subdirs_lower %in% tolower(measurement_patterns))

    if (has_measurements) {
      # This directory contains measurements, so it's likely a group folder
      # Extract the group name (last component of the path)
      group_name <- basename(dir)
      # Make sure it's not itself a measurement folder
      if (!tolower(group_name) %in% tolower(measurement_patterns)) {
        groups <- c(groups, group_name)
      }
    }
  }

  groups <- unique(groups)

  # Detect measurements (typically "pre", "post", "vorher", "nachher", etc.)
  measurements <- character(0)
  measurement_patterns <- c("pre", "post", "vorher", "nachher", "vor", "nach", "baseline", "followup")
  for (pattern in measurement_patterns) {
    matches <- stringr::str_subset(basename(rel_dirs), stringr::regex(paste0("^", pattern, "$"), ignore_case = TRUE))
    if (length(matches) > 0) {
      measurements <- c(measurements, unique(tolower(matches)))
    }
  }

  # Detect PC identifiers
  pcs <- character(0)
  pc_pattern <- "(?i)(jeki|pc|computer|rechner)\\s*[0-9]+"
  pc_matches <- stringr::str_extract_all(rel_dirs, pc_pattern)
  pc_matches <- unlist(pc_matches)
  if (length(pc_matches) > 0) {
    pcs <- unique(pc_matches)
  }

  # Build tree structure
  tree <- .build_tree_structure(root, max_depth = 4)

  # Format tree as text
  tree_text <- .format_tree_text(tree)

  list(
    groups = groups,
    measurements = measurements,
    pcs = pcs,
    tree = tree,
    tree_text = tree_text,
    n_pdfs = length(fs::dir_ls(root, recurse = TRUE, type = "file", glob = "*.pdf"))
  )
}

#' Build nested tree structure from directory
#' @keywords internal
#' @noRd
.build_tree_structure <- function(path, current_depth = 0, max_depth = 4) {
  if (current_depth >= max_depth || !dir.exists(path)) {
    return(NULL)
  }

  items <- list.files(path, full.names = TRUE)
  dirs <- items[file.info(items)$isdir]
  files <- items[!file.info(items)$isdir]

  # Count PDFs
  pdf_files <- grep("\\.pdf$", files, ignore.case = TRUE, value = TRUE)

  structure <- list(
    name = basename(path),
    type = "directory",
    n_pdfs = length(pdf_files),
    children = list()
  )

  # Add subdirectories
  for (d in dirs) {
    child <- .build_tree_structure(d, current_depth + 1, max_depth)
    if (!is.null(child)) {
      structure$children[[basename(d)]] <- child
    }
  }

  structure
}

#' Format tree structure as text
#' @keywords internal
#' @noRd
.format_tree_text <- function(tree, prefix = "", is_last = TRUE) {
  if (is.null(tree)) return(character(0))

  connector <- if (is_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
  pdf_info <- if (tree$n_pdfs > 0) sprintf(" (%d PDFs)", tree$n_pdfs) else ""

  lines <- paste0(prefix, connector, tree$name, pdf_info)

  if (length(tree$children) > 0) {
    child_names <- names(tree$children)
    for (i in seq_along(child_names)) {
      child <- tree$children[[child_names[i]]]
      is_last_child <- (i == length(child_names))
      new_prefix <- paste0(prefix, if (is_last) "    " else "\u2502   ")
      child_lines <- .format_tree_text(child, new_prefix, is_last_child)
      lines <- c(lines, child_lines)
    }
  }

  lines
}

#' KLAWA: scan a folder hierarchy and extract values
#'
#' Recursively searches \code{root} for KLAWA PDFs and returns a tidy tibble.
#' Metadata can be extracted from path/filename, PDF content, or both (with mismatch detection).
#'
#' @details
#' Label regexes are taken from \code{\link{ma_options}} (session-configurable).
#' If your PDFs use different terms, override just the needed label(s) before
#' scanning, e.g.:
#' \preformatted{
#'   set_ma_options(labels = list(
#'     pitch = "(Tonhoehe|Pitch|F0|Fo|Grundfrequenz)"
#'   ))
#'   df <- klawa_scan("data/KLAWA")
#' }
#'
#' **Metadata Sources:**
#' - \code{"path"}: Extract from folder structure and filename (default, recommended)
#'   - Detects PC, group, and measurement from folder hierarchy
#'   - Extracts participant codes from filenames
#' - \code{"pdf"}: Extract codes from PDF content only (for unorganized folders)
#'   - Note: PDFs typically only contain participant codes, not folder metadata
#'   - Group/measurement/PC information cannot be reliably extracted from PDFs
#'
#' @param root Path to the KLAWA root folder (default: "data/KLAWA").
#' @param metadata_source Character: "path" (default), "pdf", or "both". See Details.
#' @param groups Character vector of group names to recognize, or NULL for auto-detection.
#' @param measurements Character vector of measurement types to recognize, or NULL for auto-detection.
#' @param pcs Character vector of PC identifiers to recognize, or NULL for auto-detection.
#' @param code_pattern Regex pattern for participant codes. Default: "(\\\\d{4}[A-Za-z\\u00C4\\u00D6\\u00DC\\u00E4\\u00F6\\u00FC\\u00DF]{4})" (includes German umlauts).
#' @param date_format Character: date format in filenames. Options: "DDMMYY" (default), "DDMMYYYY", "YYMMDD", "YYYYMMDD", "MMDDYY", "MMDDYYYY".
#' @param auto_detect If TRUE (default) and groups/measurements/pcs are NULL, automatically detect from folder structure.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{group}, \code{measurement}, \code{pc}, \code{code}, \code{date} - Metadata from folder structure/filename
#'     \item \code{code_pdf} - Participant code extracted from PDF content
#'     \item \code{code_mismatch} - Logical flag: TRUE if filename code differs from PDF code
#'     \item \code{volume_difference}, \code{pitch}, \code{onset_difference},
#'           \code{pitch_duration_difference} - Numeric values from PDF
#'     \item \code{file} - Path relative to \code{root}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Standard usage (metadata from path)
#' df <- klawa_scan("data/KLAWA")
#'
#' # PDFs in flat folder (extract codes only, no folder metadata)
#' df <- klawa_scan("data/PDFs", metadata_source = "pdf")
#' }
klawa_scan <- function(root = "data/KLAWA",
                       metadata_source = c("path", "pdf"),
                       groups = NULL,
                       measurements = NULL,
                       pcs = NULL,
                       code_pattern = "(\\d{4}[A-Za-z\u00C4\u00D6\u00DC\u00E4\u00F6\u00FC\u00DF]{4})",
                       date_format = "DDMMYY",
                       auto_detect = TRUE) {
  metadata_source <- match.arg(metadata_source)

  files <- fs::dir_ls(root, recurse = TRUE, type = "file", glob = "*.pdf")
  if (length(files) == 0L) {
    cli::cli_warn("No PDF files found under {.path {root}}.")
    return(tibble::tibble())
  }

  # Auto-detect groups, measurements, PCs if not provided
  if (auto_detect && (is.null(groups) || is.null(measurements) || is.null(pcs))) {
    cli::cli_inform("Auto-detecting folder structure...")
    structure <- klawa_analyze_structure(root)

    if (is.null(groups) && length(structure$groups) > 0) {
      groups <- structure$groups
      cli::cli_inform("Detected {length(groups)} group(s): {paste(groups, collapse = ', ')}")
    }

    if (is.null(measurements) && length(structure$measurements) > 0) {
      measurements <- structure$measurements
      cli::cli_inform("Detected {length(measurements)} measurement type(s): {paste(measurements, collapse = ', ')}")
    }

    if (is.null(pcs) && length(structure$pcs) > 0) {
      pcs <- structure$pcs
      cli::cli_inform("Detected {length(pcs)} PC(s): {paste(pcs, collapse = ', ')}")
    }
  }

  # Set defaults if still NULL
  if (is.null(groups)) groups <- character(0)
  if (is.null(measurements)) measurements <- c("pre", "post")
  if (is.null(pcs)) pcs <- character(0)

  if (metadata_source == "pdf") {
    cli::cli_inform("Extracting participant codes from PDF content...")
    cli::cli_warn("Note: Group, measurement, and PC metadata cannot be extracted from PDFs. Consider using metadata_source='path' if files are organized in folders.")
  }

  rel <- fs::path_rel(files, start = root)
  purrr::map_dfr(rel, ~ klawa_parse_one(
    .x,
    root = root,
    metadata_source = metadata_source,
    groups = groups,
    measurements = measurements,
    pcs = pcs,
    code_pattern = code_pattern,
    date_format = date_format
  ))
}

#' @keywords internal
#' @noRd
klawa_parse_one <- function(rel_file, root, metadata_source = "path", groups = NULL, measurements = NULL, pcs = NULL, code_pattern = "(\\d{4}[A-Za-z]{4})", date_format = "DDMMYY") {
  full_path <- fs::path(root, rel_file)
  vals <- klawa_pdf_values(full_path)

  if (metadata_source == "path") {
    # Extract metadata from path/filename (recommended)
    info <- klawa_file_info(rel_file, groups = groups, measurements = measurements, pcs = pcs, code_pattern = code_pattern, date_format = date_format)

    # ALWAYS extract code from PDF for validation/mismatch detection
    pdf_text <- paste(pdftools::pdf_text(full_path), collapse = "\n")
    code_pdf <- extract_and_check_code(pdf_text, code_pattern)

    # Check for mismatch between filename code and PDF code
    code_mismatch <- !is.na(info$code) && !is.na(code_pdf) &&
      info$code != "CODE_CONFLICT" && code_pdf != "CODE_CONFLICT" &&
      info$code != code_pdf

    return(tibble::tibble(
      group = info$group,
      measurement = info$measurement,
      pc = info$pc,
      code = info$code,
      date = info$date,
      code_pdf = code_pdf,
      code_mismatch = code_mismatch,
      volume_difference = vals$volume_difference,
      pitch = vals$pitch,
      onset_difference = vals$onset_difference,
      pitch_duration_difference = vals$pitch_duration_difference,
      file = rel_file
    ))
  } else {
    # metadata_source == "pdf" - extract participant code from PDF only
    # Note: PDFs don't reliably contain group/measurement/PC info
    pdf_text <- paste(pdftools::pdf_text(full_path), collapse = "\n")
    code <- extract_and_check_code(pdf_text, code_pattern)

    return(tibble::tibble(
      group = NA_character_,
      measurement = NA_character_,
      pc = NA_character_,
      code = code,
      date = NA_character_,  # Can't extract date without filename parsing
      code_pdf = code,  # Same since we're reading from PDF
      code_mismatch = FALSE,  # No filename to compare against
      volume_difference = vals$volume_difference,
      pitch = vals$pitch,
      onset_difference = vals$onset_difference,
      pitch_duration_difference = vals$pitch_duration_difference,
      file = rel_file
    ))
  }
}

# Helper: Extract date from filename based on format
# @noRd
.extract_date_from_filename <- function(filename, date_format) {
  # Define regex patterns for different date formats
  date_patterns <- list(
    DDMMYY   = "(\\d{2})(\\d{2})(\\d{2})",
    DDMMYYYY = "(\\d{2})(\\d{2})(\\d{4})",
    YYMMDD   = "(\\d{2})(\\d{2})(\\d{2})",
    YYYYMMDD = "(\\d{4})(\\d{2})(\\d{2})",
    MMDDYY   = "(\\d{2})(\\d{2})(\\d{2})",
    MMDDYYYY = "(\\d{2})(\\d{2})(\\d{4})"
  )

  if (!date_format %in% names(date_patterns)) {
    return(NA_character_)
  }

  pattern <- date_patterns[[date_format]]
  matches <- stringr::str_match_all(filename, pattern)[[1]]

  if (nrow(matches) == 0) {
    return(NA_character_)
  }

  # Take the last match (most likely to be the date, not part of code)
  date_parts <- matches[nrow(matches), -1]  # Exclude full match, keep groups

  # Convert 2-digit year to 4-digit (00-49 -> 20xx, 50-99 -> 19xx)
  .expand_year <- function(yy) {
    year <- as.numeric(yy)
    ifelse(year < 50, 2000 + year, 1900 + year)
  }

  # Format based on date_format
  switch(date_format,
    DDMMYY = sprintf("%02d/%02d/%04d",
                     as.numeric(date_parts[1]),
                     as.numeric(date_parts[2]),
                     .expand_year(date_parts[3])),
    DDMMYYYY = sprintf("%02d/%02d/%04d",
                       as.numeric(date_parts[1]),
                       as.numeric(date_parts[2]),
                       as.numeric(date_parts[3])),
    YYMMDD = sprintf("%02d/%02d/%04d",
                     as.numeric(date_parts[3]),
                     as.numeric(date_parts[2]),
                     .expand_year(date_parts[1])),
    YYYYMMDD = sprintf("%02d/%02d/%04d",
                       as.numeric(date_parts[3]),
                       as.numeric(date_parts[2]),
                       as.numeric(date_parts[1])),
    MMDDYY = sprintf("%02d/%02d/%04d",
                     as.numeric(date_parts[2]),
                     as.numeric(date_parts[1]),
                     .expand_year(date_parts[3])),
    MMDDYYYY = sprintf("%02d/%02d/%04d",
                       as.numeric(date_parts[2]),
                       as.numeric(date_parts[1]),
                       as.numeric(date_parts[3])),
    NA_character_
  )
}

#' Parse KLAWA metadata from a path/filename
#'
#' Detects group, measurement, PC, participant \code{code}, and date from the relative path.
#' @param rel_file File path relative to the KLAWA root.
#' @param groups Character vector of group names to recognize, or NULL to skip group detection.
#' @param measurements Character vector of measurement types to recognize, or NULL to use defaults.
#' @param pcs Character vector of PC identifiers to recognize, or NULL to skip PC detection.
#' @param code_pattern Regex pattern for participant codes.
#' @param date_format Character: date format in filename. Options: "DDMMYY" (default), "DDMMYYYY", "YYMMDD", "YYYYMMDD", "MMDDYY", "MMDDYYYY".
#' @return A list with elements \code{group}, \code{measurement}, \code{pc}, \code{code}, \code{date}.
#' @export
klawa_file_info <- function(rel_file, groups = NULL, measurements = NULL, pcs = NULL, code_pattern = "(\\d{4}[A-Za-z]{4})", date_format = "DDMMYY") {
  # Extract group
  group <- NA_character_
  if (!is.null(groups) && length(groups) > 0) {
    g_hit <- stringr::str_extract(rel_file,
                                  stringr::regex(paste0("\\b(", paste(groups, collapse="|"), ")\\b"), ignore_case = TRUE))
    if (!is.na(g_hit)) group <- g_hit
  }

  # Extract measurement
  measurement <- NA_character_
  if (is.null(measurements) || length(measurements) == 0) {
    measurements <- c("pre", "post", "vorher", "nachher", "vor", "nach", "baseline", "followup")
  }
  for (m in measurements) {
    pattern <- paste0("(/|_)", m, "(/|_|\\b)")
    if (stringr::str_detect(rel_file, stringr::regex(pattern, ignore_case = TRUE))) {
      measurement <- m
      break
    }
  }

  # Extract PC
  pc <- NA_character_
  if (!is.null(pcs) && length(pcs) > 0) {
    for (p in pcs) {
      # Escape special regex characters in PC name
      p_escaped <- stringr::str_replace_all(p, "([.|()\\^{}+$*?\\[\\]])", "\\\\\\1")
      if (stringr::str_detect(rel_file, stringr::regex(p_escaped, ignore_case = TRUE))) {
        pc <- p
        break
      }
    }
  }

  # Extract code
  code <- extract_and_check_code(rel_file, code_pattern)

  # Extract date based on format
  date <- .extract_date_from_filename(basename(rel_file), date_format)

  list(group = group, measurement = measurement, pc = pc, code = code, date = date)
}

#' Extract a unique participant code and detect conflicts
#' @param x String (path/filename).
#' @param pattern Regex for the code (default: 4 digits + 4 letters, including German umlauts).
#' @return Single code (character), \code{NA}, or \code{"CODE_CONFLICT"}.
#' @export
extract_and_check_code <- function(x, pattern = "(\\d{4}[A-Za-z\u00C4\u00D6\u00DC\u00E4\u00F6\u00FC\u00DF]{4})") {
  matches <- stringr::str_extract_all(x, pattern)[[1]]
  if (length(matches) == 0) return(NA_character_)
  matches <- unique(matches)
  if (length(matches) == 1) return(matches[1])
  "CODE_CONFLICT"
}

#' Extract metadata from PDF content
#'
#' Attempts to extract group, measurement, PC, and participant code from the PDF text content.
#' This is useful when the folder structure doesn't follow the expected hierarchy.
#'
#' @param file Absolute path to the PDF file.
#' @param groups Character vector of group names to search for, or NULL to skip.
#' @param measurements Character vector of measurement types to search for, or NULL to use defaults.
#' @param pcs Character vector of PC identifiers to search for, or NULL to skip.
#' @param code_pattern Regex pattern for participant codes.
#' @return Named list with \code{group}, \code{measurement}, \code{pc}, \code{code} (all may be NA).
#' @keywords internal
#' @noRd
klawa_pdf_metadata <- function(file, groups = NULL, measurements = NULL, pcs = NULL, code_pattern = "(\\d{4}[A-Za-z]{4})") {
  txt <- pdftools::pdf_text(file)
  full_text <- paste(txt, collapse = "\n")

  # Normalize text
  full_text <- gsub("\u00A0", " ", full_text)
  full_text <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", full_text)

  # Extract group
  group <- NA_character_
  if (!is.null(groups) && length(groups) > 0) {
    group_match <- stringr::str_extract(
      full_text,
      stringr::regex(paste0("\\b(", paste(groups, collapse = "|"), ")\\b"), ignore_case = TRUE)
    )
    if (!is.na(group_match)) {
      group <- group_match
    }
  }

  # Extract measurement
  measurement <- NA_character_
  if (is.null(measurements) || length(measurements) == 0) {
    measurements <- c("pre", "post", "vorher", "nachher", "vor", "nach", "baseline", "followup")
  }
  for (m in measurements) {
    if (stringr::str_detect(full_text, stringr::regex(paste0("\\b", m, "\\b"), ignore_case = TRUE))) {
      measurement <- m
      break
    }
  }

  # Extract PC
  pc <- NA_character_
  if (!is.null(pcs) && length(pcs) > 0) {
    for (p in pcs) {
      # Escape special regex characters
      p_escaped <- stringr::str_replace_all(p, "([.|()\\^{}+$*?\\[\\]])", "\\\\\\1")
      pc_match <- stringr::str_extract(
        full_text,
        stringr::regex(p_escaped, ignore_case = TRUE)
      )
      if (!is.na(pc_match)) {
        pc <- p  # Use original (non-escaped) value
        break
      }
    }
  }

  # Extract participant code
  code <- extract_and_check_code(full_text, code_pattern)

  list(
    group = group,
    measurement = measurement,
    pc = pc,
    code = code
  )
}

#' Extract numeric KLAWA values from a PDF (robust page/line; onset handles line breaks)
#' @param file Absolute path to the PDF file.
#' @return Named list with numeric values (NA if not found).
#' @export
klawa_pdf_values <- function(file) {
  txt <- pdftools::pdf_text(file)

  # --- normalize some PDF quirks ---
  norm <- function(x) {
    x <- gsub("\u00A0", " ", x)                                   # NBSP -> space
    x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", x)   # various dashes -> "-"
    x
  }
  pages <- lapply(txt, norm)

  lab <- ma_get("labels")                     # list(volume, pitch, onset, durdif) as regex
  num_re <- "(-?\\d+[\\.,]\\d+|-?\\d+)"       # decimal or integer, comma or dot
  to_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))

  # ---- generic finder for a single label (no reliance on capture indices) ----
  find_by_label <- function(label_regex, win = 240L) {
    for (pg in pages) {
      lines <- unlist(strsplit(pg, "\n", fixed = TRUE))
      if (!length(lines)) next

      # 1) Same-line: locate label, then extract number on THAT line (right side)
      hit_idx <- stringr::str_which(lines, stringr::regex(label_regex, ignore_case = TRUE))
      if (length(hit_idx)) {
        for (i in hit_idx) {
          # take substring from first label match to EOL and extract the first number there
          mloc <- stringr::str_locate(lines[i], stringr::regex(label_regex, ignore_case = TRUE))[1, , drop = TRUE]
          if (any(is.na(mloc))) next
          snip <- substr(lines[i], mloc[2] + 1L, nchar(lines[i]))
          num <- stringr::str_extract(snip, num_re)
          if (!is.na(num)) return(to_num(num))
        }
      }

      # 2) Fallback: on full page, take first occurrence; then small window after label
      locs <- stringr::str_locate_all(pg, stringr::regex(label_regex, ignore_case = TRUE))[[1]]
      if (!is.null(dim(locs)) && nrow(locs) > 0) {
        for (k in seq_len(nrow(locs))) {
          end <- locs[k, 2]
          snip <- substr(pg, end + 1L, min(nchar(pg), end + win))
          num  <- stringr::str_extract(snip, num_re)
          if (!is.na(num)) return(to_num(num))
        }
      }
    }
    NA_real_
  }

  # ---- special onset finder: Tonbeginn on one line, Differenz in ms on a later line ----
  find_onset <- function() {
    for (pg in pages) {
      lines <- unlist(strsplit(pg, "\n", fixed = TRUE))
      if (!length(lines)) next

      tb_idx <- which(stringr::str_detect(lines, stringr::regex("\\bTonbeginn\\b", ignore_case = TRUE)))
      if (!length(tb_idx)) next

      for (i in tb_idx) {
        # look up to 3 lines ahead for "Differenz" (with/without dash elsewhere)
        j_range <- i:min(i + 3L, length(lines))
        j_hit <- NA_integer_
        for (j in j_range) {
          if (stringr::str_detect(lines[j], stringr::regex("\\bDifferenz\\b", ignore_case = TRUE))) {
            j_hit <- j; break
          }
        }
        if (is.na(j_hit)) next

        # Try number on the Differenz line
        num <- stringr::str_extract(lines[j_hit], num_re)
        if (!is.na(num)) return(to_num(num))

        # Or on the next up to 2 lines (tables often wrap)
        look_ahead <- (j_hit + 1L):min(j_hit + 2L, length(lines))
        for (k in look_ahead) {
          num2 <- stringr::str_extract(lines[k], num_re)
          if (!is.na(num2)) return(to_num(num2))
        }

        # Last resort on this page: small block around Differenz line
        block <- paste(lines[j_hit:min(j_hit + 3L, length(lines))], collapse = " ")
        num3 <- stringr::str_extract(block, num_re)
        if (!is.na(num3)) return(to_num(num3))
      }
    }

    # Absolute fallback occasionally present:
    # "Tonbeginn in ms ... Mittel: <num>"
    alt <- "Tonbeginn\\s*in\\s*ms\\s*:?\\s*Mittel\\s*:?\\s*(-?\\d+[\\.,]\\d+|-?\\d+)"
    for (pg in pages) {
      num <- stringr::str_match(pg, stringr::regex(alt, ignore_case = TRUE))[, 2]
      if (!is.na(num[1])) return(to_num(num[1]))
    }
    NA_real_
  }

  list(
    volume_difference         = find_by_label(lab$volume),
    pitch                     = find_by_label(lab$pitch),
    onset_difference          = find_onset(),
    pitch_duration_difference = find_by_label(lab$durdif)
  )
}
