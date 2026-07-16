# inputs as in Table 11 from https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.2335
inputs <- data.frame(
  x1 = c(11, 6, 4, 7, 6, 2, 10, 22, 5, 0, 3, 1, 10, 1, 1),
  n1 = c(309.5, 309.2, 308.9, 307.3, 307.0, 309.4, 305.7, 303.7, 308.7, 309.9, 309.6, 309.8, 306.4, 309.4, 309.9),
  x2 = c(25, 5, 4, 6, 9, 4, 8, 22, 12, 4, 4, 4, 9, 4, 4),
  n2 = c(295.3, 295.9, 295.6, 291.9, 292.7, 294.3, 293.5, 288.6, 294.1, 294.2, 295.3, 294.6, 292.8, 295, 294.6)
)

test_that("Check h_s_eair_diff for MN method snapshot", {
  res <- round(
    100 *
      h_s_eair_diff(
        x1 = inputs$x1,
        n1 = inputs$n1,
        x2 = inputs$x2,
        n2 = inputs$n2,
        conf_type = "mn",
        conf_level = 0.95
      ),
    digits = 2
  )

  res <- cbind(inputs, res)

  expect_snapshot(cran = TRUE, res)
})

test_that("Check h_s_eair_diff for Wald method without CC", {
  res <- round(
    100 *
      h_s_eair_diff(
        x1 = inputs$x1,
        n1 = inputs$n1,
        x2 = inputs$x2,
        n2 = inputs$n2,
        conf_type = "wald",
        conf_level = 0.95
      ),
    digits = 4
  )
  names(res) <- c("est", "lower", "upper")
  res <- cbind(inputs, res)

  expect_snapshot(cran = TRUE, res)
})

test_that("Check h_s_eair_diff for Wald method with CC", {
  res <- round(
    100 *
      h_s_eair_diff(
        x1 = inputs$x1,
        n1 = inputs$n1,
        x2 = inputs$x2,
        n2 = inputs$n2,
        conf_type = "waldcc",
        conf_level = 0.95
      ),
    digits = 4
  )
  names(res) <- c("est", "lower", "upper")
  res <- cbind(inputs, res)

  expect_snapshot(cran = TRUE, res)
})

test_that("Check h_s_eair_diff for SCAS method", {
  res <- round(
    100 *
      h_s_eair_diff(
        x1 = inputs$x1,
        n1 = inputs$n1,
        x2 = inputs$x2,
        n2 = inputs$n2,
        conf_type = "scas",
        conf_level = 0.95
      ),
    digits = 4
  )

  names(res) <- c("est", "lower", "upper")
  res <- cbind(inputs, res)

  expect_snapshot(cran = TRUE, res)
})

test_that("Check diff in binomial for MN method", {
  # NOTE : these CI intervals do NOT match crude percentage MN column for Table 11 in the paper
  # this test does confirm agreement between tern and ratesci though
  # see test file test-a_eair_diff_ratesci.R

  inputs2 <- inputs
  inputs2$n1 <- 352
  inputs2$n2 <- 370

  res <- round(100 * tern:::desctools_binom(
    x1 = inputs2$x1,
    n1 = inputs2$n1,
    x2 = inputs2$x2,
    n2 = inputs2$n2,
    conf.level = 0.95, # nolint
    sides = "two.sided",
    method = "mn"
  ), 3)
  res <- as.data.frame(res)
  names(res) <- c("est", "lower", "upper")
  row.names(res) <- NULL
  res <- cbind(inputs2, res)

  expect_snapshot(cran = TRUE, res)
})
