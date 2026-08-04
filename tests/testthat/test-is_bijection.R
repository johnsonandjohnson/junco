test_that("is_bijection() works as expected for input without missing values", {
  res <- is_bijection(c("A", "A", "B"), c(1, 1, 2))
  expect_true(res)

  res <- is_bijection(c("A", "A", "B", "B", "C"), c(3, 3, 1, 1, 2))
  expect_true(res)

  res <- is_bijection(c("A", "B"), c(1, 1))
  expect_false(res)

  # "B" maps to multiple integers.
  res <- is_bijection(c("A", "A", "B", "B", "C"), c(3, 3, 1, 3, 2))
  expect_false(res)

  res <- is_bijection(c("A", "A", "B", "B", "C"), c(3, 3, 1, 10, 2))
  expect_false(res)

  # Integer 3 maps to multiple categories.
  res <- is_bijection(c("A", "C", "B", "B", "C"), c(3, 3, 1, 1, 2))
  expect_false(res)

  res <- is_bijection(c("A", "D", "B", "B", "C"), c(3, 3, 1, 1, 2))
  expect_false(res)
})

test_that("is_bijection() works as expected for input of different types without missing values", {
  res <- is_bijection(c("A", "A", "B"), c("z", "z", "w"))
  expect_true(res)

  res <- is_bijection(c(1, 1, 2), c(11, 11, 12))
  expect_true(res)

  res <- is_bijection(c("A", "A", "B"), c(1, 1, 2))
  expect_true(res)

  res <- is_bijection(c(1, 1, 2), c("A", "A", "B"))
  expect_true(res)
})

test_that("is_bijection() works as expected for input with missing values", {
  res <- is_bijection(c("A", "A", NA, "B"), c(1, 1, NA, 2))
  expect_true(res)

  res <- is_bijection(c(NA, "A", NA, "A", "B", "B", "C"), c(NA, 3, NA, 3, 1, 1, 2))
  expect_true(res)

  res <- is_bijection(c(NA, "A", "B"), c(1, NA, 1))
  expect_false(res)

  res <- is_bijection(c(1, 1, NA), c(10, 10, NA))
  expect_true(res)

  # NA values match in one position but not in another.
  res <- is_bijection(c(NA, "A", "B", NA), c(1, NA, 1, NA))
  expect_false(res)

  # "B" maps to multiple integers.
  res <- is_bijection(c("A", "A", "B", "B", "C", NA), c(3, 3, 1, 3, 2, NA))
  expect_false(res)

  res <- is_bijection(c("A", "A", "B", NA, "B", "C"), c(NA, 3, 3, 1, 10, 2))
  expect_false(res)

  # Integer 3 maps to multiple categories.
  res <- is_bijection(c("A", "C", "B", "B", "C", NA), c(3, 3, 1, 1, 2, NA))
  expect_false(res)

  res <- is_bijection(c("A", "D", "B", NA, "B", "C"), c(NA, 3, 3, 1, 1, 2))
  expect_false(res)
})

test_that("is_bijection() returns TRUE for empty vectors", {
  expect_identical(is_bijection(character(0), integer(0)), TRUE)
})

test_that("is_bijection() throws an error for unequal length x and y", {
  expect_error(
    is_bijection(c("A", "A", "B"), c(1, 1, 2, 4)), "length"
  )
})

test_that("is_bijection() rejects unsupported types", {
  expect_error(
    is_bijection(factor(c("A", "A", "B")), c(1, 1, 2))
  )

  expect_error(
    is_bijection(list("A", "B"), c(1, 2))
  )

  expect_error(
    is_bijection(c(TRUE, FALSE), c(1, 2))
  )
})
