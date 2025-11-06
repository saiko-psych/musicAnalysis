# Tests for KLAWA functions

test_that("extract_and_check_code extracts valid codes", {
  # Valid code
  expect_equal(extract_and_check_code("0102SICH_post.pdf"), "0102SICH")
  expect_equal(extract_and_check_code("test_0102SICH_pre_date.pdf"), "0102SICH")
  expect_equal(extract_and_check_code("1234ABCD.pdf"), "1234ABCD")
})

test_that("extract_and_check_code handles missing codes", {
  # No code present
  expect_true(is.na(extract_and_check_code("no_code_here.pdf")))
  expect_true(is.na(extract_and_check_code("123ABC.pdf")))  # Too short
  expect_true(is.na(extract_and_check_code("")))
})

test_that("extract_and_check_code detects conflicts", {
  # Multiple different codes
  expect_equal(extract_and_check_code("0102SICH_0103ANDE.pdf"), "CODE_CONFLICT")
  expect_equal(extract_and_check_code("1234ABCD_5678EFGH.pdf"), "CODE_CONFLICT")
})

test_that("extract_and_check_code handles duplicate codes", {
  # Same code multiple times is not a conflict
  expect_equal(extract_and_check_code("0102SICH_0102SICH.pdf"), "0102SICH")
})

test_that("klawa_file_info extracts metadata correctly with default groups", {
  # Use the default groups and PCs
  # Note: Path must have "Jeki 4" with space to match PC pattern
  default_groups <- c("LEN", "LEO", "MAR", "MEL", "LLO", "LHG")
  default_pcs <- c("Jeki 4", "Jeki 7")
  info <- klawa_file_info("Computer/Jeki 4/Gruppen/LEN/post/0102SICH_post.pdf",
                          groups = default_groups, pcs = default_pcs)

  expect_equal(info$group, "LEN")
  expect_equal(info$measurement, "post")
  expect_equal(info$pc, "Jeki 4")
  expect_equal(info$code, "0102SICH")
})

test_that("klawa_file_info handles different group names", {
  default_groups <- c("LEN", "LEO", "MAR", "MEL", "LLO", "LHG")
  expect_equal(klawa_file_info("Gruppen/LEO/pre/test.pdf", groups = default_groups)$group, "LEO")
  expect_equal(klawa_file_info("Gruppen/MAR/post/test.pdf", groups = default_groups)$group, "MAR")
  expect_equal(klawa_file_info("Gruppen/MEL/pre/test.pdf", groups = default_groups)$group, "MEL")
  expect_equal(klawa_file_info("Gruppen/LLO/pre/test.pdf", groups = default_groups)$group, "LLO")
  expect_equal(klawa_file_info("Gruppen/LHG/pre/test.pdf", groups = default_groups)$group, "LHG")
})

test_that("klawa_file_info handles pre vs post measurement", {
  expect_equal(klawa_file_info("test/pre/file.pdf")$measurement, "pre")
  expect_equal(klawa_file_info("test/post/file.pdf")$measurement, "post")
  expect_equal(klawa_file_info("test_pre.pdf")$measurement, "pre")
  expect_equal(klawa_file_info("test_post.pdf")$measurement, "post")
})

test_that("klawa_file_info handles PC identifiers", {
  default_pcs <- c("Jeki 4", "Jeki 7")
  expect_equal(klawa_file_info("Rechner Jeki 4/test.pdf", pcs = default_pcs)$pc, "Jeki 4")
  expect_equal(klawa_file_info("Rechner Jeki 7/test.pdf", pcs = default_pcs)$pc, "Jeki 7")
  expect_equal(klawa_file_info("Jeki 4/test.pdf", pcs = default_pcs)$pc, "Jeki 4")
  expect_equal(klawa_file_info("Jeki 7/test.pdf", pcs = default_pcs)$pc, "Jeki 7")
})

test_that("klawa_file_info returns NA for missing components", {
  info <- klawa_file_info("unknown/path/file.pdf")
  expect_true(is.na(info$group))
  expect_true(is.na(info$measurement))
  expect_true(is.na(info$pc))
  expect_true(is.na(info$code))
})

test_that("klawa_scan returns empty tibble for no files", {
  # Create temp directory with no PDFs
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_klawa_empty")
  dir.create(test_dir, showWarnings = FALSE)

  result <- klawa_scan(test_dir)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

# ==== Auto-detection tests ====

test_that("klawa_analyze_structure detects groups from Gruppen folders", {
  # Create temp directory with Gruppen structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_klawa_gruppen")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  # Create Gruppen/LEN/pre structure
  dir.create(file.path(test_dir, "Computer", "Jeki4", "Gruppen", "LEN", "pre"),
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(test_dir, "Computer", "Jeki4", "Gruppen", "LEO", "post"),
             showWarnings = FALSE, recursive = TRUE)

  # Create dummy PDF files
  file.create(file.path(test_dir, "Computer", "Jeki4", "Gruppen", "LEN", "pre", "test1.pdf"))
  file.create(file.path(test_dir, "Computer", "Jeki4", "Gruppen", "LEO", "post", "test2.pdf"))

  result <- klawa_analyze_structure(test_dir)

  expect_type(result, "list")
  expect_named(result, c("groups", "measurements", "pcs", "tree", "tree_text", "n_pdfs"))
  expect_true("LEN" %in% result$groups)
  expect_true("LEO" %in% result$groups)
  expect_equal(result$n_pdfs, 2)

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("klawa_analyze_structure detects measurements", {
  # Create temp directory with pre/post structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_klawa_measurements")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  dir.create(file.path(test_dir, "Gruppen", "TestGroup", "pre"),
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(test_dir, "Gruppen", "TestGroup", "post"),
             showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(test_dir, "Gruppen", "TestGroup", "pre", "test1.pdf"))
  file.create(file.path(test_dir, "Gruppen", "TestGroup", "post", "test2.pdf"))

  result <- klawa_analyze_structure(test_dir)

  expect_true("pre" %in% result$measurements)
  expect_true("post" %in% result$measurements)

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("klawa_analyze_structure detects PC identifiers", {
  # Create temp directory with Jeki structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_klawa_pcs")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  dir.create(file.path(test_dir, "Jeki 4", "Gruppen", "LEN", "pre"),
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(test_dir, "Jeki 7", "Gruppen", "LEO", "post"),
             showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(test_dir, "Jeki 4", "Gruppen", "LEN", "pre", "test1.pdf"))
  file.create(file.path(test_dir, "Jeki 7", "Gruppen", "LEO", "post", "test2.pdf"))

  result <- klawa_analyze_structure(test_dir)

  expect_true("Jeki 4" %in% result$pcs)
  expect_true("Jeki 7" %in% result$pcs)

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("klawa_analyze_structure builds tree structure", {
  # Create temp directory
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_klawa_tree")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  dir.create(file.path(test_dir, "Gruppen", "LEN", "pre"),
             showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(test_dir, "Gruppen", "LEN", "pre", "test.pdf"))

  result <- klawa_analyze_structure(test_dir)

  expect_type(result$tree, "list")
  expect_type(result$tree_text, "character")
  # tree_text is a character vector where each element is a line
  # Collapse to single string for testing
  tree_full <- paste(result$tree_text, collapse = "\n")
  expect_true(grepl("Gruppen", tree_full, fixed = TRUE))
  expect_true(grepl("LEN", tree_full, fixed = TRUE))
  expect_true(grepl("pre", tree_full, fixed = TRUE))
  expect_true(grepl("1 PDFs", tree_full, fixed = TRUE))

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("klawa_analyze_structure handles empty directory", {
  # Create temp directory with no PDFs
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_klawa_empty_structure")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  result <- klawa_analyze_structure(test_dir)

  expect_equal(result$n_pdfs, 0)
  expect_length(result$groups, 0)
  expect_length(result$measurements, 0)
  expect_length(result$pcs, 0)

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("klawa_file_info with custom groups only matches specified groups", {
  # Test with custom group that doesn't match path
  info1 <- klawa_file_info("Gruppen/LEN/pre/test.pdf", groups = c("CustomGroup"))
  expect_true(is.na(info1$group))

  # Test with custom group that does match
  info2 <- klawa_file_info("Gruppen/CustomGroup/pre/test.pdf", groups = c("CustomGroup"))
  expect_equal(info2$group, "CustomGroup")
})

test_that("klawa_file_info with custom measurements extracts correctly", {
  # Test with custom measurement
  info <- klawa_file_info("data/baseline/test.pdf", measurements = c("baseline", "followup"))
  expect_equal(info$measurement, "baseline")

  # Test that default measurements don't match when custom specified
  info2 <- klawa_file_info("data/pre/test.pdf", measurements = c("baseline", "followup"))
  expect_true(is.na(info2$measurement))
})

test_that("klawa_file_info with custom code pattern extracts correctly", {
  # Test with custom code pattern for "ID-123" format
  info <- klawa_file_info("data/ID-123.pdf", code_pattern = "ID-(\\d+)")
  expect_equal(info$code, "ID-123")

  # Test with custom code pattern for "SUBJ_999" format
  info2 <- klawa_file_info("data/SUBJ_999.pdf", code_pattern = "SUBJ_(\\d+)")
  expect_equal(info2$code, "SUBJ_999")
})

test_that("klawa_file_info with custom PC identifiers extracts correctly", {
  # Test with custom PC identifiers
  info <- klawa_file_info("PC-A/data/test.pdf", pcs = c("PC-A", "PC-B"))
  expect_equal(info$pc, "PC-A")

  # Test with another PC
  info2 <- klawa_file_info("folder/PC-B/test.pdf", pcs = c("PC-A", "PC-B"))
  expect_equal(info2$pc, "PC-B")

  # Test that non-matching PC returns NA
  info3 <- klawa_file_info("folder/PC-C/test.pdf", pcs = c("PC-A", "PC-B"))
  expect_true(is.na(info3$pc))
})
