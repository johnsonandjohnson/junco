test_that("response tables accept integer-like counts with the requested dimensions", {
  tbl <- matrix(c(0, 1, 2, 3), nrow = 2)
  strata <- array(as.integer(0:7), dim = c(2, 2, 2))

  expect_identical(assert_response_table(tbl), NULL)
  expect_identical(assert_response_table(strata, stratified = TRUE), NULL)
  expect_error(assert_response_table(tbl, stratified = NA), "stratified")
  expect_error(assert_response_table(tbl, stratified = TRUE))
  expect_error(assert_response_table(strata))
  expect_error(assert_response_table(matrix(1, 2, 3)))
})

test_that("response tables reject invalid counts", {
  tbl <- matrix(1, 2, 2)
  for (value in list(-1, 0.5, NA_real_, Inf, NaN)) {
    invalid <- tbl
    invalid[1, 1] <- value
    expect_error(assert_response_table(invalid))
  }
  expect_error(assert_response_table(matrix("1", 2, 2)))
})
