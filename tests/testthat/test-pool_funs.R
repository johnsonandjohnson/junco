test_that("pool_rubin_scalar applies Rubin's rules", {
  q <- c(1.2, 1.5, 0.9)
  u <- c(0.04, 0.09, 0.01)
  pooled <- pool_rubin_scalar(q, u)

  expected_var <- mean(u) + (1 + 1 / length(q)) * stats::var(q)

  expect_named(pooled, c("est", "se", "var", "m"))
  expect_equal(pooled$est, mean(q))
  expect_equal(pooled$var, expected_var)
  expect_equal(pooled$se, sqrt(expected_var))
  expect_equal(pooled$m, length(q))
})

test_that("pool_z_stat combines z statistics and calculates a two-sided p-value", {
  z_stat_vals <- c(0.5, 1, 1.5)
  pooled <- pool_z_stat(z_stat_vals)

  pooled_var <- 1 + (1 + 1 / length(z_stat_vals)) * stats::var(z_stat_vals)
  expected_z <- mean(z_stat_vals) / sqrt(pooled_var)
  expected_p <- 2 * stats::pnorm(abs(expected_z), lower.tail = FALSE)

  expect_named(pooled, c("z", "p"))
  expect_equal(pooled$z, expected_z)
  expect_equal(pooled$p, expected_p)
})
