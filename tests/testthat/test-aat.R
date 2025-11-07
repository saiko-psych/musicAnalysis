# Tests for AAT (Auditory Ambiguity Test) module

test_that("aat_scan handles missing directory", {
  expect_error(aat_scan("nonexistent_path"), "Directory not found")
})

test_that("aat_scan returns empty tibble for directory with no AAT files", {
  # Create temporary empty directory
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_aat_test")
  if (dir.exists(empty_dir)) unlink(empty_dir, recursive = TRUE)
  dir.create(empty_dir)

  result <- suppressWarnings(aat_scan(empty_dir))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("code", "date", "file_type", "ambiguous_pct", "control_pct",
                         "a_tone_pairs", "c_tone_pairs", "a_avg_items_per_pair", "c_avg_items_per_pair",
                         "n_ambivalent", "n_dont_know", "n_evaluable", "n_total", "file"))

  unlink(empty_dir, recursive = TRUE)
})

test_that(".rsl file parsing works with summary format (Type of Pair)", {
  # This tests the Format 1 .rsl files with "Type of Pair" and "AAT Score [%]" columns
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_summary.rsl.csv")

  # Create test data in summary format
  test_data <- data.frame(
    `Type of Pair` = c("Ambiguous", "Control"),
    `# Tone Pairs` = c(50, 5),
    `Avg. # Items/Pair` = c(2, 2),
    `AAT Score [%]` = c(79, 100),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- .aat_parse_rsl(test_file, "test.csv", "0102SICH", "17/03/25")

  expect_equal(result$ambiguous_pct, 79)
  expect_equal(result$control_pct, 100)
  expect_equal(result$file_type, "rsl")

  unlink(test_file)
})

test_that(".rsl file parsing calculates from item-level format", {
  # This tests the Format 2 .rsl files with "% F0" column
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_itemlevel.rsl.csv")

  # Create test data in item-level format
  # Row 1-2: Ambiguous (F0 diff != 12.5), Row 3: Control (F0 diff == 12.5)
  test_data <- data.frame(
    Index = c(1, 2, 3),
    `Reference F0 [Hz]` = c(100, 200, 300),
    `F0 Difference [%]` = c(50, 25, 12.5),
    `# Items` = c(2, 2, 2),  # Total items per pair
    `# F0` = c(2, 1, 0),    # F0 responses per pair
    `% F0` = c(100, 50, 0),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- .aat_parse_rsl(test_file, "test.csv", "0102SICH", "17/03/25")

  # Item-level format now properly calculates using F0 Difference
  # Ambiguous (rows 1-2): (2+1) f0 / (2+2) items = 3/4 = 75%
  expect_equal(result$ambiguous_pct, 75.0)
  # Control (row 3): 0 f0 / 2 items = 0%
  expect_equal(result$control_pct, 0.0)
  expect_equal(result$file_type, "rsl")

  # Check tone pair counts
  expect_equal(result$a_tone_pairs, 2)  # 2 ambiguous pairs
  expect_equal(result$c_tone_pairs, 1)  # 1 control pair

  unlink(test_file)
})

test_that(".itl file parsing extracts quality metrics", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test.itl.csv")

  # Create test data with Pitch Classification column
  test_data <- data.frame(
    Index = c("1 *", "2 *", "3 *", "4 *", "5 *", "6 *", "7 *"),
    `Pitch Classification [-1;0;1;2]` = c(0, 1, 1, 0, 1, 2, -1),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- .aat_parse_itl(test_file, "test.csv", "0102SICH", "17/03/25", NULL, NULL)

  expect_equal(result$n_total, 7)
  expect_equal(result$n_ambivalent, 1)  # One code "2"
  expect_equal(result$n_dont_know, 1)    # One code "-1"
  expect_equal(result$n_evaluable, 5)    # Five codes "0" or "1"
  expect_equal(result$file_type, "itl")

  # Without F0 Difference column, fallback calculates across ALL items (including non-evaluable)
  # (3 f0 responses / 7 total items = 42.9%)
  expect_equal(result$ambiguous_pct, 42.9)
  # control_pct remains NA since we can't calculate it without F0 Difference column
  expect_true(is.na(result$control_pct))

  unlink(test_file)
})

test_that(".itl file parsing calculates percentages when ambiguous_items specified", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test.itl.csv")

  # Create test data
  test_data <- data.frame(
    Index = c("1 *", "2 *", "3 *", "4 *", "5 *", "6 *", "7 *", "8 *"),
    `Pitch Classification [-1;0;1;2]` = c(0, 1, 1, 0, 1, 1, 1, 0),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- .aat_parse_itl(test_file, "test.csv", "0102SICH", "17/03/25",
                           ambiguous_items = 1:8, control_items = NULL)

  expect_equal(result$n_total, 8)
  expect_equal(result$n_evaluable, 8)
  # 5 out of 8 are "1" (f0 responses)
  expect_equal(result$ambiguous_pct, round((5/8) * 100, 1))

  unlink(test_file)
})

test_that("aat_scan extracts code and date from filename", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "0102SICH_170325_AAT_Default.rsl.csv")

  # Create minimal test data
  test_data <- data.frame(
    `Type of Pair` = c("Ambiguous"),
    `# Tone Pairs` = c(50),
    `Avg. # Items/Pair` = c(2),
    `AAT Score [%]` = c(75),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_scan(temp_dir, date_format = "DDMMYY")

  expect_equal(result$code, "0102SICH")
  expect_equal(result$date, "17/03/2025")  # Date is formatted as DD/MM/YYYY
  expect_equal(result$ambiguous_pct, 75)

  unlink(test_file)
})

test_that("aat_scan filters for AAT in filename", {
  temp_dir <- tempdir()

  # Create one AAT file and one non-AAT file
  aat_file <- file.path(temp_dir, "0102SICH_AAT_test.rsl.csv")
  non_aat_file <- file.path(temp_dir, "0102SICH_other_test.rsl.csv")

  test_data <- data.frame(
    `Type of Pair` = c("Ambiguous"),
    `# Tone Pairs` = c(50),
    `Avg. # Items/Pair` = c(2),
    `AAT Score [%]` = c(75),
    check.names = FALSE
  )

  readr::write_csv(test_data, aat_file)
  readr::write_csv(test_data, non_aat_file)

  result <- aat_scan(temp_dir)

  # Should only find the AAT file
  expect_equal(nrow(result), 1)
  expect_true(stringr::str_detect(result$file, "AAT"))

  unlink(aat_file)
  unlink(non_aat_file)
})

test_that("aat_scan handles CODE_CONFLICT", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "0102SICH_0103ANDE_AAT_test.rsl.csv")

  test_data <- data.frame(
    `Type of Pair` = c("Ambiguous"),
    `# Tone Pairs` = c(50),
    `Avg. # Items/Pair` = c(2),
    `AAT Score [%]` = c(75),
    check.names = FALSE
  )
  readr::write_csv(test_data, test_file)

  result <- aat_scan(temp_dir)

  expect_equal(result$code, "CODE_CONFLICT")

  unlink(test_file)
})
