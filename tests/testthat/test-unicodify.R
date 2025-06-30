library(testthat)

test_that("unicodify replaces patterns with Unicode characters", {
  # Test input strings
  input <- c(
    "Value >= 10",
    "Value <= 5",
    "Values 5 <= x <= 10"
  )

  # Expected output with Unicode replacements
  expected <- c(
    paste0("Value ", intToUtf8(strtoi("2265", base = 16)), " 10"),
    paste0("Value ", intToUtf8(strtoi("2264", base = 16)), " 5"),
    paste0(
      "Values 5 ",
      intToUtf8(strtoi("2264", base = 16)),
      " x ",
      intToUtf8(strtoi("2264", base = 16)),
      " 10"
    )
  )

  # Create a test map
  test_map <- tibble::tribble(
    ~pattern,
    ~unicode,
    ">=",
    "2265",
    "<=",
    "2264"
  )

  # Run the function
  result <- unicodify(input, test_map)

  # Check results
  expect_equal(result[1], expected[1])
  expect_equal(result[2], expected[2])
  expect_equal(result[3], expected[3])

  # Test with empty input
  expect_equal(unicodify(character(0), test_map), character(0))

  # Test with no matches
  expect_equal(
    unicodify("No special characters here", test_map),
    "No special characters here"
  )
})

test_that("unicodify works with the default jj_uc_map", {
  # Test with the default map
  input <- c("Value >= 10", "Value <= 5")
  result <- unicodify(input, jj_uc_map)

  # Check that replacements were made
  expect_false(grepl(">=", result[1], fixed = TRUE))
  expect_false(grepl("<=", result[2], fixed = TRUE))

  # Verify the specific Unicode characters were inserted
  expect_true(grepl(
    intToUtf8(strtoi("2265", base = 16)),
    result[1],
    fixed = TRUE
  ))
  expect_true(grepl(
    intToUtf8(strtoi("2264", base = 16)),
    result[2],
    fixed = TRUE
  ))
})
