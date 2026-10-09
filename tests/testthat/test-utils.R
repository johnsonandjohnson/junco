test_that("safe_as_logical works as expected", {
  expect_identical(
    safe_as_logical(c(TRUE, FALSE, TRUE)),
    c(TRUE, FALSE, TRUE)
  )
  expect_error(
    safe_as_logical(c(TRUE, FALSE, "a")),
    "Conversion to logical introduced unexpected NAs."
  )
  expect_identical(
    safe_as_logical(c(1, 0, 1)),
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(
    safe_as_logical(c("TRUE", "F", "false")),
    c(TRUE, FALSE, FALSE)
  )
})
