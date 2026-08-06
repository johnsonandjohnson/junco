expect_error_match_na <- function(x, y, msg = NULL) {
  expect_error(is_bijection(x, y), msg)
  expect_error(is_bijection(x, y, TRUE), msg)
}

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

test_that("is_bijection() supports character and numeric vectors", {
  res <- is_bijection(c("A", "A", "B"), c("z", "z", "w"))
  expect_true(res)

  res <- is_bijection(c(1, 1, 2), c(11, 11, 12))
  expect_true(res)

  res <- is_bijection(c("A", "A", "B"), c(1, 1, 2))
  expect_true(res)

  res <- is_bijection(c(1, 1, 2), c("A", "A", "B"))
  expect_true(res)
})

test_that("is_bijection() works for length-one vectors", {
  expect_true(is_bijection("A", 1))
  expect_error(is_bijection(NA_character_, NA_real_), )
  expect_true(is_bijection(NA_character_, NA_real_, TRUE), "NA.*match_na")
})

test_that("is_bijection() returns TRUE for empty vectors", {
  expect_identical(is_bijection(character(0), integer(0)), TRUE)
})

test_that("is_bijection() throws an error for inputs with NAs in non-matching positions", {
  x <- c("A", "A", NA, "B")
  y <- c(1, 1, 3, 2)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c("A", "A", "B", "B")
  y <- c(1, NA, 3, 3)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c(NA, "A", "G", "A", "B", "B", "C")
  y <- c(4, 3, NA, 3, 1, 1, 2)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c(NA, "A", "B", "B")
  y <- c(NA, 1, 2, NA)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c(NA, "A", NA, "B")
  y <- c(NA, 1, 4, 2)
  expect_error_match_na(x, y, "NA.*positions")
})

test_that("is_bijection() handles NAs on matching positions correctly", {
  # 1 NA
  x <- c(NA, "A", "B", "B")
  y <- c(NA, 1, 2, 2)
  expect_error(is_bijection(x, y), "NA.*match_na")
  expect_true(is_bijection(x, y, TRUE))

  x <- c(NA, "A", "B", "B")
  y <- c(NA, 1, 2, 3)
  expect_error(is_bijection(x, y), "NA.*match_na")
  expect_false(is_bijection(x, y, TRUE))

  x <- c(NA, "A", "B", "C")
  y <- c(NA, 1, 2, 2)
  expect_error(is_bijection(x, y), "NA.*match_na")
  expect_false(is_bijection(x, y, TRUE))

  # > 1 NA
  x <- c(NA, "A", "B", NA, "B")
  y <- c(NA, 1, 2, NA, 2)
  expect_error(is_bijection(x, y), "NA.*match_na")
  expect_true(is_bijection(x, y, TRUE))

  x <- c(NA, "A", "B", NA, "B")
  y <- c(NA, 1, 2, NA, 3)
  expect_error(is_bijection(x, y), "NA.*match_na")
  expect_false(is_bijection(x, y, TRUE))

  x <- c(NA, "A", "B", NA, "C")
  y <- c(NA, 1, 2, NA, 2)
  expect_error(is_bijection(x, y), "NA.*match_na")
  expect_false(is_bijection(x, y, TRUE))

  x <- c(NA, NA, NA)
  expect_error(is_bijection(x, x), "NA.*match_na")
  expect_true(is_bijection(x, x, TRUE))
})

test_that("is_bijection() - NA position mismatches take precedence over bijection checking", {
  # Non-missing values form a bijection.
  x <- c(NA, "A", "B", "B", "C")
  y <- c(15, 1, 2, 2, 3)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c("O", "A", "B", "B", "C")
  y <- c(15, NA, 2, 2, 3)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c(NA, "A", "B", "B", "C")
  y <- c(15, NA, 2, 2, 3)
  expect_error_match_na(x, y, "NA.*positions")

  # Non-missing values do not form a bijection.
  y <- c(15, NA, 2, 5, 3)
  expect_error_match_na(x, y, "NA.*positions")

  x <- c("A", NA, "B", "G", "C")
  y <- c(NA, 15, 2, 2, 3)
  expect_error_match_na(x, y, "NA.*positions")
})

test_that("is_bijection() - matched NA positions take precedence over bijection checking", {
  # Non-missing values form a bijection.
  x <- c(NA, "A", "B", "B", "C")
  y <- c(NA, 1, 2, 2, 3)
  expect_error(is_bijection(x, y), "NA.*match_na")

  # Non-missing values do not form a bijection.
  y <- c(NA, 1, 2, 2, 2)
  expect_error(is_bijection(x, y), "NA.*match_na")

  x <- c(NA, "A", "B", "G", "C")
  y <- c(NA, 1, 2, 2, 3)
  expect_error(is_bijection(x, y), "NA.*match_na")
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
