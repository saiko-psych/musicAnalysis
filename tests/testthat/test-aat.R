# Tests for AAT (Auditory Ambiguity Test) module

test_that("aat_scan handles missing directory", {
  expect_error(aat_scan("nonexistent_path"), "Directory not found")
})

test_that("aat_scan returns empty tibble for directory with no CSV files", {
  # Create temporary empty directory
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_aat_test")
  if (dir.exists(empty_dir)) unlink(empty_dir, recursive = TRUE)
  dir.create(empty_dir)

  result <- suppressWarnings(aat_scan(empty_dir))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("code", "ambiguous_pct", "control_pct", "n_ambivalent",
                         "n_dont_know", "n_evaluable", "n_total", "file"))

  unlink(empty_dir, recursive = TRUE)
})

test_that("aat_parse_one extracts participant code from filename", {
  # Create temporary CSV file with minimal data
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "AAT_response_0102SICH_20250611.itl.csv")

  # Create test data
  test_data <- data.frame(
    `Pitch Classification` = c(0, 1, 1, 0, 1, 2, -1),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_parse_one(test_file, basename(test_file),
                          code_pattern = "(\\d{4}[A-Za-z]{4})",
                          ambiguous_items = 1:7,
                          control_items = integer(0))

  expect_equal(result$code, "0102SICH")
  expect_equal(result$n_total, 7)
  expect_equal(result$n_ambivalent, 1)
  expect_equal(result$n_dont_know, 1)
  expect_equal(result$n_evaluable, 5)  # Only 0 and 1 codes

  unlink(test_file)
})

test_that("aat_parse_one calculates ambiguous percentage correctly", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "AAT_0211BAMA.csv")

  # Test data: 10 ambiguous items, 3 are f0 (code 1), 5 are spectral (code 0), 2 are ambivalent (code 2)
  test_data <- data.frame(
    `Pitch Classification` = c(1, 0, 1, 0, 1, 0, 0, 0, 2, 2),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_parse_one(test_file, basename(test_file),
                          code_pattern = "(\\d{4}[A-Za-z]{4})",
                          ambiguous_items = 1:10,
                          control_items = integer(0))

  # Evaluable responses: 1,0,1,0,1,0,0,0 (8 total, 3 are f0)
  # Expected: 3/8 * 100 = 37.5%
  expect_equal(result$ambiguous_pct, 37.5)
  expect_equal(result$n_ambivalent, 2)

  unlink(test_file)
})

test_that("aat_parse_one handles control items correctly", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "AAT_0403CLJO.csv")

  # 5 control items, 3 ambiguous items
  test_data <- data.frame(
    `Pitch Classification` = c(1, 0, 1, 1, 1, 0, 1, 0),  # Items 1-5 control, 6-8 ambiguous
    `Correct Answer` = c(1, 0, 1, 0, 1, NA, NA, NA),  # Correct answers for control items
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_parse_one(test_file, basename(test_file),
                          code_pattern = "(\\d{4}[A-Za-z]{4})",
                          ambiguous_items = 6:8,
                          control_items = 1:5)

  # Control: 4 out of 5 correct (items 1,3,5 match; item 2,4 don't match)
  # Item responses: 1,0,1,1,1; Correct: 1,0,1,0,1
  # Matches: positions 1,2,3,5 = 4 out of 5
  expect_equal(result$control_pct, 80.0)  # 4/5 * 100

  # Ambiguous: items 6-8 are [0,1,0], so 1 f0 out of 3 = 33.3%
  expect_equal(result$ambiguous_pct, 33.3)

  unlink(test_file)
})

test_that("aat_parse_one excludes ambivalent and dont_know from percentage calculations", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "AAT_0501TEST.csv")

  # Mix of response types: 5 evaluable (0,1), 2 ambivalent (2), 2 don't know (-1)
  test_data <- data.frame(
    `Pitch Classification` = c(1, 0, 1, 2, -1, 0, 2, 1, -1, 0),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_parse_one(test_file, basename(test_file),
                          code_pattern = "(\\d{4}[A-Za-z]{4})",
                          ambiguous_items = 1:10,
                          control_items = integer(0))

  # Evaluable responses: positions 1,2,3,6,8,10 = [1,0,1,0,1,0] = 3 f0s out of 6
  expect_equal(result$n_evaluable, 6)
  expect_equal(result$ambiguous_pct, 50.0)  # 3/6 * 100
  expect_equal(result$n_ambivalent, 2)
  expect_equal(result$n_dont_know, 2)

  unlink(test_file)
})

test_that("aat_analyze_structure detects flat structure", {
  temp_dir <- tempdir()
  flat_dir <- file.path(temp_dir, "aat_flat_test")
  if (dir.exists(flat_dir)) unlink(flat_dir, recursive = TRUE)
  dir.create(flat_dir)

  # Create files directly in root
  file.create(file.path(flat_dir, "AAT_0102SICH.csv"))
  file.create(file.path(flat_dir, "AAT_0211BAMA.csv"))

  result <- aat_analyze_structure(flat_dir)

  expect_equal(result$structure, "flat")
  expect_equal(result$n_files, 2)

  unlink(flat_dir, recursive = TRUE)
})

test_that("aat_analyze_structure detects one-level structure", {
  temp_dir <- tempdir()
  nested_dir <- file.path(temp_dir, "aat_nested_test")
  if (dir.exists(nested_dir)) unlink(nested_dir, recursive = TRUE)
  dir.create(nested_dir)
  dir.create(file.path(nested_dir, "group_A"))
  dir.create(file.path(nested_dir, "group_B"))

  # Create files in subfolders
  file.create(file.path(nested_dir, "group_A", "AAT_0102SICH.csv"))
  file.create(file.path(nested_dir, "group_A", "AAT_0211BAMA.csv"))
  file.create(file.path(nested_dir, "group_B", "AAT_0403CLJO.csv"))

  result <- aat_analyze_structure(nested_dir)

  expect_equal(result$structure, "one-level")
  expect_equal(result$n_files, 3)
  expect_equal(nrow(result$subfolder_summary), 2)

  unlink(nested_dir, recursive = TRUE)
})

test_that("aat_scan processes multiple files correctly", {
  temp_dir <- tempdir()
  multi_dir <- file.path(temp_dir, "aat_multi_test")
  if (dir.exists(multi_dir)) unlink(multi_dir, recursive = TRUE)
  dir.create(multi_dir)

  # Create test files
  file1 <- file.path(multi_dir, "AAT_0102SICH.csv")
  file2 <- file.path(multi_dir, "AAT_0211BAMA.csv")

  # File 1: 60% f0 responses
  test_data1 <- data.frame(
    `Pitch Classification` = c(1, 1, 1, 0, 0),
    check.names = FALSE
  )
  readr::write_csv(test_data1, file1)

  # File 2: 40% f0 responses
  test_data2 <- data.frame(
    `Pitch Classification` = c(1, 0, 1, 0, 0),
    check.names = FALSE
  )
  readr::write_csv(test_data2, file2)

  result <- aat_scan(multi_dir)

  expect_equal(nrow(result), 2)
  expect_true(all(c("0102SICH", "0211BAMA") %in% result$code))

  # Check calculations
  sich_row <- result[result$code == "0102SICH", ]
  expect_equal(sich_row$ambiguous_pct, 60.0)

  bama_row <- result[result$code == "0211BAMA", ]
  expect_equal(bama_row$ambiguous_pct, 40.0)

  unlink(multi_dir, recursive = TRUE)
})

test_that("aat_parse_one returns NA for ambiguous_pct when no evaluable responses", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "AAT_0601NONA.csv")

  # Only non-evaluable responses
  test_data <- data.frame(
    `Pitch Classification` = c(2, -1, 2, -1),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_parse_one(test_file, basename(test_file),
                          code_pattern = "(\\d{4}[A-Za-z]{4})",
                          ambiguous_items = 1:4,
                          control_items = integer(0))

  expect_true(is.na(result$ambiguous_pct))
  expect_equal(result$n_evaluable, 0)
  expect_equal(result$n_ambivalent, 2)
  expect_equal(result$n_dont_know, 2)

  unlink(test_file)
})

test_that("aat_parse_one handles CODE_CONFLICT in filename", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "AAT_0102SICH_0211BAMA.csv")

  test_data <- data.frame(
    `Pitch Classification` = c(1, 0, 1),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_parse_one(test_file, basename(test_file),
                          code_pattern = "(\\d{4}[A-Za-z]{4})",
                          ambiguous_items = 1:3,
                          control_items = integer(0))

  expect_equal(result$code, "CODE_CONFLICT")

  unlink(test_file)
})
