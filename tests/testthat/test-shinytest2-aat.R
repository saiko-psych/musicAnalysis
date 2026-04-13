# Shinytest2 tests for the AAT module
# Tests the AAT tab of the musicAnalysis Shiny app

library(shinytest2)

# Determine the app directory
app_dir <- "C:/Users/David/Nextcloud2/Documents/R/musicpsychology/musicAnalysis/inst/shiny"

test_that("AAT tab loads correctly", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "aat-tab-loads",
    timeout = 30000,
    wait = TRUE
  )

  on.exit(app$stop(), add = TRUE)

  # Navigate to the AAT tab by setting the main_nav input
  app$set_inputs(main_nav = "aat", wait_ = TRUE, timeout_ = 10000)

  # Verify the AAT heading is present
  html <- app$get_html("body")
  expect_true(
    grepl("AAT Scanner", html, fixed = TRUE),
    info = "Expected 'AAT Scanner' heading to be present on AAT tab"
  )
})

test_that("AAT threshold inputs exist with correct labels", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "aat-threshold-inputs",
    timeout = 30000,
    wait = TRUE
  )

  on.exit(app$stop(), add = TRUE)

  # Navigate to AAT tab
  app$set_inputs(main_nav = "aat", wait_ = TRUE, timeout_ = 10000)

  # Get page HTML to check for threshold inputs
  html <- app$get_html("body")

  # Check that threshold input labels are present
  expect_true(
    grepl("Low Control Score Threshold", html, fixed = TRUE),
    info = "Expected 'Low Control Score Threshold' label for threshold_control input"
  )
  expect_true(
    grepl("High Ambivalent Count Threshold", html, fixed = TRUE),
    info = "Expected 'High Ambivalent Count Threshold' label for threshold_ambiguous input"
  )

  # Verify input values exist via app values
  values <- app$get_values(input = c("aat-threshold_control", "aat-threshold_ambiguous"))

  # Default values should be 80 and 5
  expect_equal(
    values$input[["aat-threshold_control"]], 80,
    info = "Default threshold_control should be 80"
  )
  expect_equal(
    values$input[["aat-threshold_ambiguous"]], 5,
    info = "Default threshold_ambiguous should be 5"
  )
})

test_that("AAT folder path text input can be set", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "aat-folder-path",
    timeout = 30000,
    wait = TRUE
  )

  on.exit(app$stop(), add = TRUE)

  # Navigate to AAT tab
  app$set_inputs(main_nav = "aat", wait_ = TRUE, timeout_ = 10000)

  # Set a folder path in the manual text input
  test_path <- "C:/test/aat/data"
  app$set_inputs(`aat-root_manual` = test_path, wait_ = TRUE, timeout_ = 5000)

  # Verify the value was set
  values <- app$get_values(input = "aat-root_manual")
  expect_equal(
    values$input[["aat-root_manual"]], test_path,
    info = "Folder path text input should hold the value we set"
  )
})

test_that("AAT tab screenshot for visual verification", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "aat-screenshot",
    variant = shinytest2::platform_variant(),
    timeout = 30000,
    wait = TRUE
  )

  on.exit(app$stop(), add = TRUE)

  # Navigate to AAT tab
  app$set_inputs(main_nav = "aat", wait_ = TRUE, timeout_ = 10000)

  # Take a screenshot - saved to tests/testthat/_snaps/ for visual verification
  # Uses platform_variant so the snapshot is platform-aware
  app$expect_screenshot()
})
