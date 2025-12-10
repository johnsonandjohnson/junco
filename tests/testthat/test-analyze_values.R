library(rtables)

test_that("analyze_values correctly modifies layout with proper formats", {
  # Create a simple layout
  lyt <- basic_table(round_type = "sas") %>%
    split_cols_by("ARM")

  # Define variables to analyze
  vars <- c("AGE", "WEIGHT")

  # Define formats
  formats <- list(
    mean_sd = "xx.x (xx.x)",
    median = "xx.x",
    range = "xx.x - xx.x"
  )

  # Apply analyze_values
  modified_lyt <- analyze_values(
    lyt = lyt,
    vars = vars,
    formats = formats
  )

  # Check that the layout was modified correctly
  expect_s4_class(modified_lyt, "PreDataTableLayouts")

  # Create a mock dataframe to test the layout's behavior
  mock_data <- data.frame(
    ARM = rep(c("A", "B"), each = 5),
    AGE = c(25, 30, 35, 40, 45, 22, 28, 32, 38, 42),
    WEIGHT = c(70, 75, 80, 85, 90, 68, 72, 78, 82, 88)
  )

  # Build the table
  tbl <- build_table(modified_lyt, mock_data)

  # Check that the table has the expected structure
  expect_true("AGE" %in% row.names(tbl))
  expect_true("WEIGHT" %in% row.names(tbl))

  # Extract all row names to check for statistics
  all_row_names <- unlist(row.names(tbl))

  # Check that all statistics are present in the table (exact matches)
  expect_true(any(grepl("^N$", all_row_names, fixed = FALSE)))
  expect_true(any(grepl("Mean \\(SD\\)", all_row_names, fixed = FALSE)))
  expect_true(any(grepl("^Median$", all_row_names, fixed = FALSE)))
  expect_true(any(grepl("Min, Max", all_row_names, fixed = FALSE)))
})
