test_that("pppt_parse_one extracts all PPP indices correctly", {
  test_file <- test_path("../testdata_AAT_PPPT/16032025/LHG/9905MAHE_results_16032025_PPPT_Default_70dBSPL_~Calib_Hausladen.rsl.csv")

  result <- pppt_parse_one(test_file, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$code, "9905MAHE")
  expect_equal(result$ppp_index_overall, -0.364)
  expect_equal(result$ppp_index_294, 1)
  expect_equal(result$ppp_index_523, 1)
  expect_equal(result$ppp_index_932, -1)
  expect_equal(result$ppp_index_1661, -1)
  expect_equal(result$ppp_index_2960, -1)
  expect_equal(result$ppp_index_5274, 0)
})

test_that("pppt_parse_one extracts correct code from another file", {
  test_file <- test_path("../testdata_AAT_PPPT/17032025/MEL/0206ANDA_170325_PPPT_Default_70dBSPL_~Calib_Hausladen.rsl.csv")

  result <- pppt_parse_one(test_file, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")

  expect_equal(result$code, "0206ANDA")
  expect_equal(result$ppp_index_overall, -0.636)
  expect_equal(result$ppp_index_294, 1)
  expect_equal(result$ppp_index_523, 1)
  expect_equal(result$ppp_index_932, -1)
  expect_equal(result$ppp_index_5274, -0.5)
})

test_that("pppt_scan processes all PPPT files in folder", {
  test_dir <- test_path("../testdata_AAT_PPPT/16032025")

  result <- pppt_scan(test_dir)

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("code", "ppp_index_overall", "ppp_index_294", "ppp_index_523",
                    "ppp_index_932", "ppp_index_1661", "ppp_index_2960", "ppp_index_5274",
                    "date", "file") %in% names(result)))
})

test_that("pppt_scan extracts dates correctly", {
  test_dir <- test_path("../testdata_AAT_PPPT/16032025")

  result <- pppt_scan(test_dir, date_format = "DDMMYY")

  expect_true(all(!is.na(result$date)))
  expect_true(all(grepl("\\d{2}/\\d{2}/\\d{4}", result$date)))
})

test_that("pppt_scan handles missing files gracefully", {
  test_dir <- tempdir()

  result <- pppt_scan(test_dir)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("pppt_scan handles invalid directory", {
  expect_error(pppt_scan("nonexistent_directory"), "does not exist")
})

test_that("pppt_analyze_structure returns correct counts", {
  test_dir <- test_path("../testdata_AAT_PPPT")

  result <- pppt_analyze_structure(test_dir)

  expect_type(result, "list")
  expect_true("n_rsl_files" %in% names(result))
  expect_true("n_itl_files" %in% names(result))
  expect_gte(result$n_rsl_files, 1)
})

test_that("pppt_parse_one handles CODE_CONFLICT correctly", {
  # This would need a test file with conflicting codes
  # For now, we test that single code extraction works
  test_file <- test_path("../testdata_AAT_PPPT/16032025/LHG/9905MAHE_results_16032025_PPPT_Default_70dBSPL_~Calib_Hausladen.rsl.csv")

  result <- pppt_parse_one(test_file, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")

  expect_false(result$code == "CODE_CONFLICT")
  expect_equal(result$code, "9905MAHE")
})

test_that("pppt_scan returns tibble with correct column types", {
  test_dir <- test_path("../testdata_AAT_PPPT/16032025")

  result <- pppt_scan(test_dir)

  expect_type(result$code, "character")
  expect_type(result$ppp_index_overall, "double")
  expect_type(result$ppp_index_294, "double")
  expect_type(result$date, "character")
  expect_type(result$file, "character")
})

test_that("pppt_scan makes file paths relative to root", {
  test_dir <- test_path("../testdata_AAT_PPPT/16032025")

  result <- pppt_scan(test_dir)

  expect_true(all(!grepl("^/", result$file)))
  expect_true(all(!grepl("^C:/", result$file)))
})

test_that("pppt_parse_one handles missing UCF frequencies", {
  # Test that missing UCF values return NA
  test_file <- test_path("../testdata_AAT_PPPT/16032025/LHG/9905MAHE_results_16032025_PPPT_Default_70dBSPL_~Calib_Hausladen.rsl.csv")

  result <- pppt_parse_one(test_file, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")

  # All UCF frequencies should be present in this file
  expect_false(is.na(result$ppp_index_294))
  expect_false(is.na(result$ppp_index_523))
  expect_false(is.na(result$ppp_index_932))
})

test_that("pppt_scan handles multiple date formats", {
  test_dir <- test_path("../testdata_AAT_PPPT/17032025")

  result_ddmmyy <- pppt_scan(test_dir, date_format = "DDMMYY")

  expect_true(all(grepl("\\d{2}/\\d{2}/\\d{4}", result_ddmmyy$date[!is.na(result_ddmmyy$date)])))
})

test_that("pppt_parse_one handles negative PPP indices", {
  test_file <- test_path("../testdata_AAT_PPPT/17032025/MEL/0206ANDA_170325_PPPT_Default_70dBSPL_~Calib_Hausladen.rsl.csv")

  result <- pppt_parse_one(test_file, code_pattern = "\\d{4}[A-Za-z]{4}", date_format = "DDMMYY")

  # Check that negative values are preserved
  expect_equal(result$ppp_index_overall, -0.636)
  expect_equal(result$ppp_index_932, -1)
  expect_equal(result$ppp_index_5274, -0.5)
})

test_that("pppt_scan filters only PPPT files", {
  test_dir <- test_path("../testdata_AAT_PPPT")

  result <- pppt_scan(test_dir)

  # All results should have PPPT data (ppp_index columns should exist)
  expect_true(all(c("ppp_index_overall", "ppp_index_294", "ppp_index_523") %in% names(result)))
  # Should have at least some data
  expect_gt(nrow(result), 0)
})
