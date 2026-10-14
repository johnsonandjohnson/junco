test_that("factor_by_order() works as expected for input without missing values", {
  x <- c("A", "A", "B")
  y <- c(1, 1, 2)
  y2 <- c(2, 2, 1)

  # Ordered by y.
  res <- factor_by_order(x, y)
  exp <- factor(x)
  expect_identical(res, exp)

  res <- factor_by_order(x, y, ordered = TRUE)
  exp <- ordered(x)
  expect_identical(res, exp)

  # Ordered by y2.
  res <- factor_by_order(x, y2)
  exp <- factor(x, levels = c("B", "A"))
  expect_identical(res, exp)

  res <- factor_by_order(x, y2, ordered = TRUE)
  exp <- ordered(x, levels = c("B", "A"))
  expect_identical(res, exp)
})

test_that("factor_by_order() works as expected when repeated values are not adjacent", {
  x <- factor(c("B", "A", "B"))
  y <- c(1, 2, 1)

  res <- factor_by_order(x, y)
  exp <- factor(x, levels = c("B", "A"))
  expect_identical(res, exp)

  res <- factor_by_order(x, y, ordered = TRUE)
  exp <- ordered(x, levels = c("B", "A"))
  expect_identical(res, exp)
})


test_that("factor_by_order() works as expected for input with missing values", {
  x <- c("A", NA, "A", "B", NA)
  y <- c(2, NA, 2, 1, NA)

  res <- factor_by_order(x, y)
  exp <- factor(x, levels = c("B", "A"))
  expect_identical(res, exp)

  res <- factor_by_order(x, y, ordered = TRUE)
  exp <- ordered(x, levels = c("B", "A"))
  expect_identical(res, exp)
})

test_that("factor_by_order() works as expected for input with all missing values", {
  x <- NA_character_
  y <- NA_integer_

  res <- factor_by_order(x, y)
  expect_identical(res, factor(x))

  res <- factor_by_order(x, y, ordered = TRUE)
  expect_identical(res, ordered(x))
})

test_that("factor_by_order() works as expected for empty vectors", {
  x <- character(0)
  y <- integer(0)

  res <- factor_by_order(x, y)
  expect_identical(res, factor(x))

  res <- factor_by_order(x, y, ordered = TRUE)
  expect_identical(res, ordered(x))
})

test_that("factor_by_order() works as expected for factors", {
  x <- factor(c("A", "A", "B"))
  y <- c(2, 2, 1)

  res <- factor_by_order(x, y)
  exp <- factor(x, levels = c("B", "A"))
  expect_identical(res, exp)

  res <- factor_by_order(x, y, ordered = TRUE)
  exp <- ordered(x, levels = c("B", "A"))
  expect_identical(res, exp)
})

test_that("factor_by_order() works as expected for factors with unobserved levels", {
  x <- factor(c("A", "A", "B"), levels = c("A", "B", "C"))
  y <- c(2, 2, 1)

  res <- factor_by_order(x, y)
  exp <- factor(x, levels = c("B", "A"))
  expect_identical(res, exp)

  res <- factor_by_order(x, y, ordered = TRUE)
  exp <- ordered(x, levels = c("B", "A"))
  expect_identical(res, exp)
})

test_that("factor_by_order() preserves attributes of x", {
  x <- factor(c("A", "A", "B"), levels = c("A", "B", "C"))
  attr(x, "label") <- "X"
  y <- c(2, 2, 1)

  res <- factor_by_order(x, y)
  exp <- factor(x, levels = c("B", "A"))
  attr(exp, "label") <- "X"
  expect_identical(res, exp)

  res <- factor_by_order(x, y, ordered = TRUE)
  exp <- ordered(x, levels = c("B", "A"))
  attr(exp, "label") <- "X"
  expect_identical(res, exp)
})

test_that("factor_by_order() throws an error for unequal length x and y", {
  expect_error(
    factor_by_order(c("A", "A", "B"), c(1, 1, 2, 4)), "length"
  )
})

test_that("factor_by_order() throws an error for non-integer y", {
  expect_error(
    factor_by_order(c("A", "A", "B"), c("1", "1", "2", "4")), "integer"
  )
})

test_that("factor_by_order() throws an error when x and y do not define a bijection", {
  expect_error(
    factor_by_order(c("A", "A", "B"), c(1, 2, 2)), "bijection"
  )
})

test_that("factor_by_order() throws an error when NAs do not correspond", {
  expect_error(
    factor_by_order(c("A", NA), c(1, 2)), "positions"
  )
})
