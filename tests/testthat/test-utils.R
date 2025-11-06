# Tests for utility functions (validate_dataset, peek_problems)

test_that("validate_dataset with return_type='logical' works", {
  df <- tibble::tibble(
    group = c("LEN", "LEO"),
    measurement = c("pre", "post"),
    pc = c("Jeki 4", "Jeki 7"),
    code = c("0102SICH", "0103ANDE"),
    date = c("01/01/2025", "02/01/2025"),
    volume_difference = c(1.5, 2.0),
    pitch = c(100, 110),
    onset_difference = c(5, 10),
    pitch_duration_difference = c(2, 3),
    file = c("file1.pdf", "file2.pdf")
  )

  expect_true(validate_dataset(df, return_type = "logical"))
})

test_that("validate_dataset detects missing columns", {
  df <- tibble::tibble(
    group = c("LEN"),
    measurement = c("pre"),
    file = c("file1.pdf")
    # Missing: pc, code, and all numeric columns
  )

  expect_false(validate_dataset(df, return_type = "logical"))
})

test_that("validate_dataset return_type='text' provides formatted output", {
  df <- tibble::tibble(
    group = c("LEN", "LEO"),
    measurement = c("pre", "post"),
    pc = c("Jeki 4", "Jeki 7"),
    code = c("0102SICH", NA),
    volume_difference = c(1.5, 2.0),
    pitch = c(100, 110),
    onset_difference = c(NA, 10),
    pitch_duration_difference = c(2, 3),
    file = c("file1.pdf", "file2.pdf")
  )

  result <- validate_dataset(df, return_type = "text")

  expect_type(result, "character")
  expect_match(result, "Dataset Validation Report")
  expect_match(result, "Data Completeness")
})

test_that("validate_dataset with return_type='stop' throws error on missing columns", {
  df <- tibble::tibble(
    group = c("LEN"),
    file = c("file1.pdf")
  )

  expect_error(
    validate_dataset(df, return_type = "stop"),
    "Missing required columns"
  )
})

test_that("validate_dataset with return_type='stop' returns invisibly on success", {
  df <- tibble::tibble(
    group = c("LEN"),
    measurement = c("pre"),
    pc = c("Jeki 4"),
    code = c("0102SICH"),
    date = c("01/01/2025"),
    volume_difference = 1.5,
    pitch = 100,
    onset_difference = 5,
    pitch_duration_difference = 2,
    file = c("file1.pdf")
  )

  result <- validate_dataset(df, return_type = "stop")
  expect_identical(result, df)
})

test_that("peek_problems with return_type='summary' returns text", {
  df <- tibble::tibble(
    group = c("LEN", "LEO", NA),
    measurement = c("pre", "post", "pre"),
    pc = c("Jeki 4", NA, "Jeki 7"),
    code = c("0102SICH", "CODE_CONFLICT", NA),
    volume_difference = c(1.5, NA, 3.0),
    pitch = c(100, 110, NA),
    onset_difference = c(5, 10, 15),
    pitch_duration_difference = c(2, 3, 4),
    file = c("file1.pdf", "file2.pdf", "file3.pdf")
  )

  result <- peek_problems(df, return_type = "summary")

  expect_type(result, "character")
  expect_match(result, "KLAWA Data Quality Report")
  expect_match(result, "Problem Summary")
  expect_match(result, "Code Conflicts")
  expect_match(result, "Missing Codes")
})

test_that("peek_problems with return_type='detailed' returns list with tibbles", {
  df <- tibble::tibble(
    group = c("LEN", "LEO"),
    measurement = c("pre", "post"),
    pc = c("Jeki 4", "Jeki 7"),
    code = c("0102SICH", "CODE_CONFLICT"),
    volume_difference = c(1.5, NA),
    pitch = c(100, 110),
    onset_difference = c(5, 10),
    pitch_duration_difference = c(2, 3),
    file = c("file1.pdf", "file2.pdf")
  )

  result <- peek_problems(df, return_type = "detailed")

  expect_type(result, "list")
  expect_named(result, c("summary", "code_mismatches", "code_conflicts", "missing_codes", "missing_metadata", "missing_values"))
  expect_s3_class(result$summary, "tbl_df")
  expect_s3_class(result$code_mismatches, "tbl_df")
  expect_s3_class(result$code_conflicts, "tbl_df")
  expect_s3_class(result$missing_codes, "tbl_df")
  expect_s3_class(result$missing_metadata, "tbl_df")
  expect_s3_class(result$missing_values, "tbl_df")
})

test_that("peek_problems detects code conflicts correctly", {
  df <- tibble::tibble(
    code = c("0102SICH", "CODE_CONFLICT", "0103ANDE"),
    file = c("file1.pdf", "file2.pdf", "file3.pdf")
  )

  result <- peek_problems(df, return_type = "detailed")

  expect_equal(nrow(result$code_conflicts), 1)
  expect_equal(result$code_conflicts$file[1], "file2.pdf")
})

test_that("peek_problems detects missing codes correctly", {
  df <- tibble::tibble(
    code = c("0102SICH", NA, "0103ANDE"),
    file = c("file1.pdf", "file2.pdf", "file3.pdf")
  )

  result <- peek_problems(df, return_type = "detailed")

  expect_equal(nrow(result$missing_codes), 1)
  expect_equal(result$missing_codes$file[1], "file2.pdf")
})

test_that("peek_problems detects missing metadata correctly", {
  df <- tibble::tibble(
    group = c("LEN", NA, "LEO"),
    measurement = c("pre", "post", NA),
    pc = c("Jeki 4", "Jeki 7", "Jeki 4"),
    file = c("file1.pdf", "file2.pdf", "file3.pdf")
  )

  result <- peek_problems(df, return_type = "detailed")

  expect_gte(nrow(result$missing_metadata), 2)  # At least 2 missing metadata entries
})

test_that("peek_problems detects missing numeric values correctly", {
  df <- tibble::tibble(
    volume_difference = c(1.5, NA, 3.0),
    pitch = c(100, 110, NA),
    file = c("file1.pdf", "file2.pdf", "file3.pdf")
  )

  result <- peek_problems(df, return_type = "detailed")

  expect_gte(nrow(result$missing_values), 2)  # 2 missing values total
})

test_that("peek_problems returns 'no problems' message when data is clean", {
  df <- tibble::tibble(
    group = c("LEN", "LEO"),
    measurement = c("pre", "post"),
    pc = c("Jeki 4", "Jeki 7"),
    code = c("0102SICH", "0103ANDE"),
    volume_difference = c(1.5, 2.0),
    pitch = c(100, 110),
    onset_difference = c(5, 10),
    pitch_duration_difference = c(2, 3),
    file = c("file1.pdf", "file2.pdf")
  )

  result <- peek_problems(df, return_type = "summary")

  expect_match(result, "No problems detected")
})

test_that("peek_problems requires 'file' column", {
  df <- tibble::tibble(
    code = c("0102SICH", "0103ANDE"),
    value = c(1, 2)
  )

  expect_error(
    peek_problems(df),
    "Column 'file' is required"
  )
})
