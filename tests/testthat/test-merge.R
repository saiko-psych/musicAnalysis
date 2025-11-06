# Tests for merge_by_code function

test_that("merge_by_code performs basic left join", {
  df1 <- tibble::tibble(
    code = c("0102SICH", "0103ANDE", "0104TEST"),
    value1 = c(10, 20, 30)
  )

  df2 <- tibble::tibble(
    code = c("0102SICH", "0103ANDE"),
    value2 = c(100, 200)
  )

  result <- merge_by_code(df1, df2)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_equal(result$value2[1], 100)
  expect_equal(result$value2[2], 200)
  expect_true(is.na(result$value2[3]))
})

test_that("merge_by_code normalizes codes", {
  df1 <- tibble::tibble(
    code = c(" 0102SICH ", "0103ANDE  "),
    value1 = c(10, 20)
  )

  df2 <- tibble::tibble(
    code = c("0102SICH", "  0103ANDE"),
    value2 = c(100, 200)
  )

  result <- merge_by_code(df1, df2)

  expect_equal(nrow(result), 2)
  expect_equal(result$value2[1], 100)
  expect_equal(result$value2[2], 200)
})

test_that("merge_by_code converts empty strings to NA", {
  df1 <- tibble::tibble(
    code = c("0102SICH", ""),
    value1 = c(10, 20)
  )

  df2 <- tibble::tibble(
    code = c("0102SICH"),
    value2 = c(100)
  )

  result <- merge_by_code(df1, df2)

  expect_true(is.na(result$code[2]))
  expect_equal(nrow(result), 2)
})

test_that("merge_by_code warns about duplicates in y", {
  df1 <- tibble::tibble(
    code = c("0102SICH", "0103ANDE"),
    value1 = c(10, 20)
  )

  df2 <- tibble::tibble(
    code = c("0102SICH", "0102SICH"),  # Duplicate
    value2 = c(100, 101)
  )

  expect_warning(
    result <- merge_by_code(df1, df2),
    "duplicated codes"
  )

  # Should create row expansion
  expect_equal(nrow(result), 3)  # 2 rows for 0102SICH, 1 for 0103ANDE
})

test_that("merge_by_code errors without code column in x", {
  df1 <- tibble::tibble(
    id = c("a", "b"),
    value1 = c(10, 20)
  )

  df2 <- tibble::tibble(
    code = c("0102SICH"),
    value2 = c(100)
  )

  expect_error(merge_by_code(df1, df2), "has no `code` column")
})

test_that("merge_by_code errors without code column in y", {
  df1 <- tibble::tibble(
    code = c("0102SICH", "0103ANDE"),
    value1 = c(10, 20)
  )

  df2 <- tibble::tibble(
    id = c("a", "b"),
    value2 = c(100, 200)
  )

  expect_error(merge_by_code(df1, df2), "has no `code` column")
})

test_that("merge_by_code handles suffix for overlapping columns", {
  df1 <- tibble::tibble(
    code = c("0102SICH", "0103ANDE"),
    value = c(10, 20)
  )

  df2 <- tibble::tibble(
    code = c("0102SICH", "0103ANDE"),
    value = c(100, 200)
  )

  result <- merge_by_code(df1, df2, suffix = c(".left", ".right"))

  expect_true("value.left" %in% names(result))
  expect_true("value.right" %in% names(result))
  expect_equal(result$value.left[1], 10)
  expect_equal(result$value.right[1], 100)
})

test_that("merge_by_code returns tibble not data.frame", {
  df1 <- data.frame(
    code = c("0102SICH"),
    value1 = c(10)
  )

  df2 <- data.frame(
    code = c("0102SICH"),
    value2 = c(100)
  )

  result <- merge_by_code(df1, df2)

  expect_s3_class(result, "tbl_df")
})
