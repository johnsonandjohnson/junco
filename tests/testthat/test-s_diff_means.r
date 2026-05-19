diff_means_stat_names <- paste0(
  "diff_means_", c("n1", "n2", "est", "se", "est_se", "ci", "est_ci")
)

# Start of tests ----

testthat::test_that("s_diff_means works for unpaired data", {
  df1 <- data.frame(x = c(1, 2, NA, 3))
  df2 <- data.frame(x = c(4, 5))

  res <- s_diff_means(df1, df2, "x", conf.level = 0.8)

  est <- mean(df1$x, na.rm = TRUE) - mean(df2$x)
  se <- sqrt(var(c(1, 2, 3)) / 3 + var(df2$x) / 2)
  ci <- t.test(c(1, 2, 3), df2$x, conf.level = 0.8)$conf.int

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 2L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, est, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, se, ignore_attr = TRUE, tolerance = 10^-12)
  expect_equal(res$diff_means_est_se, c(est, se), ignore_attr = TRUE, tolerance = 10^-12)
  expect_identical(res$diff_means_ci, ci, ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(est, ci), ignore_attr = TRUE)
})

testthat::test_that("s_diff_means works for paired data", {
  df1 <- data.frame(id = c("A", "B", "C", "D", "E"), x = c(1, 2, 3, NA, 5))
  df2 <- data.frame(id = c("D", "C", "B", "A"), x = c(12, 31, 3, 5))

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id", conf.level = 0.7)

  x <- 1:3
  y <- c(5, 3, 31)
  est <- mean(x) - mean(y)
  se <- sd(x - y) / sqrt(3)
  ci <- t.test(x, y, paired = TRUE, conf.level = 0.7)$conf.int

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, est, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, se, ignore_attr = TRUE, tolerance = 10^-12)
  expect_equal(res$diff_means_est_se, c(est, se), ignore_attr = TRUE, tolerance = 10^-12)
  expect_identical(res$diff_means_ci, ci, ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(est, ci), ignore_attr = TRUE)
})

testthat::test_that("s_diff_means handles single observation groups", {
  df1 <- data.frame(x = 10)
  df2 <- data.frame(x = 5)

  res <- s_diff_means(df1, df2, "x")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 1L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 1L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, 5, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE, tolerance = 10^-12)
  expect_equal(res$diff_means_est_se, c(5, NA_real_), ignore_attr = TRUE, tolerance = 10^-12)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(5, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})
