test_that("safe_t_test() is identical to t.test() for regular data (x only)", {
  # Intentionally named xx (not x) because we want to check whether
  # safe_t_test() preserves the variable name
  xx <- c(41, 40, 13, 71, 3, 96)
  result <- expect_silent(safe_t_test(xx))
  expected <- t.test(xx)
  expect_identical(result, expected)

  result_a <- expect_silent(safe_t_test(xx, alternative = "less"))
  expected_a <- t.test(xx, alternative = "less")
  expect_identical(result_a, expected_a)

  result_mu <- expect_silent(safe_t_test(xx, mu = 4))
  expected_mu <- t.test(xx, mu = 4)
  expect_identical(result_mu, expected_mu)

  result_var <- expect_silent(safe_t_test(xx, var.equal = TRUE))
  expected_var <- t.test(xx, var.equal = TRUE)
  expect_identical(result_var, expected_var)

  result_cl <- expect_silent(safe_t_test(xx, conf.level = 0.5))
  expected_cl <- t.test(xx, conf.level = 0.5)
  expect_identical(result_cl, expected_cl)
})

test_that("safe_t_test() is identical to t.test() for regular data", {
  # Intentionally named xx and yy (not x and y) because we want to check
  # whether safe_t_test() preserves the variable names
  xx <- c(41, 40, 13, 71, 3, 96)
  yy <- c(1, 50, 43, 76, 94, 77)

  result <- expect_silent(safe_t_test(xx, yy))
  expected <- t.test(xx, yy)
  expect_identical(result, expected)

  result_a <- expect_silent(safe_t_test(xx, yy, alternative = "less"))
  expected_a <- t.test(xx, yy, alternative = "less")
  expect_identical(result_a, expected_a)

  result_mu <- expect_silent(safe_t_test(xx, yy, mu = 4))
  expected_mu <- t.test(xx, yy, mu = 4)
  expect_identical(result_mu, expected_mu)

  result_pair <- expect_silent(safe_t_test(xx, yy, paired = TRUE))
  expected_pair <- t.test(xx, yy, paired = TRUE)
  expect_identical(result_pair, expected_pair)

  result_var <- expect_silent(safe_t_test(xx, yy, var.equal = TRUE))
  expected_var <- t.test(xx, yy, var.equal = TRUE)
  expect_identical(result_var, expected_var)

  result_cl <- expect_silent(safe_t_test(xx, yy, conf.level = 0.5))
  expected_cl <- t.test(xx, yy, conf.level = 0.5)
  expect_identical(result_cl, expected_cl)
})

test_that("safe_t_test() preserves the argument order of t.test()", {
  x <- c(41, 40, 13, 71, 3, 96)
  y <- c(1, 50, 43, 76, 94, 77)

  result <- expect_silent(
    safe_t_test(x, y, "greater", 4, TRUE, TRUE, 0.5)
  )
  expected <- t.test(x, y, "greater", 4, TRUE, TRUE, 0.5)
  expect_identical(result, expected)
})

test_that("safe_t_test() returns NaN for constant data (paired)", {
  x <- rep(2, 5)
  result <- expect_silent(
    safe_t_test(x, x, paired = TRUE)
  )
  expected <- t.test(x, x, paired = TRUE)
  expect_identical(result, expected)
})

test_that("safe_t_test() returns NaN for constant data (paired, var.equal)", {
  x <- rep(2, 5)
  result <- expect_silent(
    safe_t_test(x, x, paired = TRUE, var.equal = TRUE)
  )
  expected <- t.test(x, x, paired = TRUE, var.equal = TRUE)
  expect_identical(result, expected)
})

test_that("safe_t_test() warns for constant paired samples of different lengths", {
  x <- c(2, NA, NA, 2, 2, NA, 2)
  y <- c(NA, 2, 2, 2, 2, NA)
  expect_warning(
    safe_t_test(x, y, paired = TRUE, conf.level = 0.5)
  )
})

test_that("safe_t_test() does not fail for nearly constant data", {
  x <- c(1.709999999999999964473, rep(1.710000000000000186517, 4))
  expect_silent(safe_t_test(x, x))
})

test_that("safe_t_test() returns NA for constant data (x only)", {
  x <- rep(2, 5)
  result <- expect_silent(
    safe_t_test(x, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(2, "mean of x"),
    null.value = setNames(0, "mean"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = "One Sample t-test (failed: data are essentially constant)",
    data.name = "x"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for constant data", {
  x <- rep(2, 5)
  result <- expect_silent(
    safe_t_test(x, x, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(2, 2), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = "Welch Two Sample t-test (failed: data are essentially constant)",
    data.name = "x and x"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for constant data (x only, many NAs)", {
  x <- c(2, NA, NA, 2, 2, NA, 2)
  result <- expect_silent(
    safe_t_test(x, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(2, "mean of x"),
    null.value = setNames(0, "mean"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = "One Sample t-test (failed: data are essentially constant)",
    data.name = "x"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for constant data (many NAs)", {
  x <- c(2, NA, NA, 2, 2, NA, 2)
  y <- c(NA, 2, 2, 2, 2, NA)
  result <- expect_silent(
    safe_t_test(x, x, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(2, 2), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = "Welch Two Sample t-test (failed: data are essentially constant)",
    data.name = "x and x"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NaN for constant data (many NAs, paired)", {
  x <- c(2, NA, NA, 2, 2, 2)
  y <- c(NA, 2, 2, 2, 2, NA)
  result <- expect_silent(
    safe_t_test(x, y, paired = TRUE, conf.level = 0.5)
  )

  expected <- t.test(x, y, paired = TRUE, conf.level = 0.5)
  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for constant data (var.equal)", {
  x <- rep(2, 5)
  result <- expect_silent(
    safe_t_test(x, x, var.equal = TRUE, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(2, 2), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = " Two Sample t-test (failed: data are essentially constant)",
    data.name = "x and x"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for not enough x observations", {
  x <- numeric(0)
  y <- 3
  result <- expect_silent(
    safe_t_test(x, y, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(NA, 3), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = "Welch Two Sample t-test (failed: not enough 'x' observations)",
    data.name = "x and y"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for not enough y observations", {
  x <- 1
  y <- numeric(0)
  result <- expect_silent(
    safe_t_test(x, y, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(1, NA), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = "Welch Two Sample t-test (failed: not enough 'x' observations)",
    data.name = "x and y"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for not enough observations", {
  x <- 1
  y <- 1
  result <- expect_silent(
    safe_t_test(x, y, var.equal = TRUE, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(1, 1), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = " Two Sample t-test (failed: not enough observations)",
    data.name = "x and y"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})

test_that("safe_t_test() returns NA for not enough observations (NA)", {
  x <- c(1, NA, NA)
  y <- c(2, NA, NA)
  result <- expect_silent(
    safe_t_test(x, y, var.equal = TRUE, conf.level = 0.5)
  )

  expected <- list(
    statistic = setNames(NA_real_, "t"),
    parameter = setNames(NA_real_, "df"),
    p.value = NA_real_,
    conf.int = c(NA_real_, NA_real_),
    estimate = setNames(c(1, 2), c("mean of x", "mean of y")),
    null.value = setNames(0, "difference in means"),
    stderr = NA_real_,
    alternative = "two.sided",
    method = " Two Sample t-test (failed: not enough observations)",
    data.name = "x and y"
  )
  attr(expected$conf.int, "conf.level") <- 0.5
  class(expected) <- "htest"

  expect_identical(result, expected)
})
