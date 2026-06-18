# inputs as in Table 11 from https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.2335
inputs <- data.frame(
  x1 = c(11, 6, 4, 7, 6, 2, 10, 22, 5, 0, 3, 1, 10, 1, 1),
  n1 = c(309.5, 309.2, 308.9, 307.3, 307.0, 309.4, 305.7, 303.7, 308.7, 309.9, 309.6, 309.8, 306.4, 309.4, 309.9),
  x2 = c(25, 5, 4, 6, 9, 4, 8, 22, 12, 4, 4, 4, 9, 4, 4),
  n2 = c(295.3, 295.9, 295.6, 291.9, 292.7, 294.3, 293.5, 288.6, 294.1, 294.2, 295.3, 294.6, 292.8, 295, 294.6)
)
ratesci_pkg_version <- "1.0.0.9000"

# current CRAN version "1.0.0"
# CRAN version 1.0.0 does not yet include function ratesci::rdci
# test that utilize ratesci::rdci require dev version
# rdci does include WALD method for ci, this method is not available in CRAN version 1.0.0

test_that("Check h_s_eair_diff for MN method ratesci function scoreci", {
  skip_if_not_installed("ratesci")

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
    digits = 4
  )
  names(res) <- c("est", "lower", "upper")
  res <- cbind(inputs, res)

  res_ratesci <- t(sapply(seq_len(nrow(inputs)), FUN = function(i) {
    x <- ratesci::scoreci(
      x1 = inputs$x1[i],
      n1 = inputs$n1[i],
      x2 = inputs$x2[i],
      n2 = inputs$n2[i],
      distrib = "poi",
      contrast = "RD",
      level = 0.95,
      skew = FALSE
    )$estimates[, c(1:3), drop = FALSE]


    x <- x[1, c("est", "lower", "upper")]
    x <- round(100 * x, 4)
  }, simplify = TRUE))
  res_ratesci <- as.data.frame(res_ratesci)
  expect_equal(
    res[, c("est", "lower", "upper")],
    res_ratesci,
    tolerance = 1e-4,
    ignore_attr = FALSE
  )
})

test_that("Check h_s_eair_diff for SCAS method against ratesci function scoreci", {
  skip_if_not_installed("ratesci")

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

  res_ratesci <- t(sapply(seq_len(nrow(inputs)), FUN = function(i) {
    x1 <- inputs$x1[i]
    n1 <- inputs$n1[i]
    x2 <- inputs$x2[i]
    n2 <- inputs$n2[i]
    crude_est <- (x1 / n1) - (x2 / n2)

    x <- ratesci::scoreci(
      x1 = x1,
      n1 = n1,
      x2 = x2,
      n2 = n2,
      distrib = "poi",
      contrast = "RD",
      level = 0.95,
      skew = TRUE,
      simpleskew = FALSE
    )$estimates[, c(1:3), drop = FALSE]
    x <- x[1, c("est", "lower", "upper")]

    x["est"] <- crude_est
    x <- round(100 * x, 4)
  }, simplify = TRUE))
  res_ratesci <- as.data.frame(res_ratesci)
  expect_equal(
    res[, c("est", "lower", "upper")],
    res_ratesci,
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
})

test_that("Check diff in binomial for MN method against ratesci function scoreci", {
  skip_if_not_installed("ratesci")

  # NOTE : these CI intervals do NOT match crude percentage MN column for Table 11 in the paper
  # this test does confirm agreement between tern and ratesci though

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

  res_ratesci <- t(sapply(seq_len(nrow(inputs)), FUN = function(i) {
    x <- ratesci::scoreci(
      x1 = inputs2$x1[i],
      n1 = inputs2$n1[i],
      x2 = inputs2$x2[i],
      n2 = inputs2$n2[i],
      distrib = "bin",
      contrast = "RD",
      level = 0.95,
      skew = FALSE,
      bcf = TRUE,
      or_bias = FALSE
    )$estimates[, c(1:3), drop = FALSE]
    x <- x[1, c("est", "lower", "upper")]
    x <- round(100 * x, 3)
  }, simplify = TRUE))
  res_ratesci <- as.data.frame(res_ratesci)
  row.names(res_ratesci) <- NULL
  expect_equal(
    res[, c("est", "lower", "upper")],
    res_ratesci,
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
})

# from here onwards ratesci::rdci - specific version required ----

test_that("Check h_s_eair_diff for SCAS method ratesci::rdci", {
  # same test setting as
  # test: Check h_s_eair_diff for SCAS method against ratesci function scoreci
  # now using the wrapper function rdci instead

  skip_if_not_installed("ratesci")
  skip_if(packageVersion("ratesci") < ratesci_pkg_version)

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

  res_ratesci <- t(sapply(seq_len(nrow(inputs)), FUN = function(i) {
    x1 <- inputs$x1[i]
    n1 <- inputs$n1[i]
    x2 <- inputs$x2[i]
    n2 <- inputs$n2[i]
    crude_est <- (x1 / n1) - (x2 / n2)

    x <- ratesci::rdci(
      x1 = x1,
      n1 = n1,
      x2 = x2,
      n2 = n2,
      distrib = "poi",
      level = 0.95,
      cc = FALSE
    )$estimates
    x <- x["SCAS", c("est", "lower", "upper"), ]
    x <- round(100 * x, 4)
  }, simplify = TRUE))
  res_ratesci <- as.data.frame(res_ratesci)
  expect_equal(
    res[, c("est", "lower", "upper")],
    res_ratesci,
    tolerance = 1e-4,
    ignore_attr = FALSE
  )
})

test_that("Check h_s_eair_diff for Wald method without CC against ratesci function rdci", {
  skip_if_not_installed("ratesci")
  skip_if(packageVersion("ratesci") < ratesci_pkg_version)

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

  res_ratesci <- t(sapply(seq_len(nrow(inputs)), FUN = function(i) {
    x <- ratesci::rdci(
      x1 = inputs$x1[i],
      n1 = inputs$n1[i],
      x2 = inputs$x2[i],
      n2 = inputs$n2[i],
      distrib = "poi",
      level = 0.95
    )$estimates
    x <- x["Approximate Normal", c("est", "lower", "upper"), ]
    x <- round(100 * x, 4)
  }, simplify = TRUE))
  res_ratesci <- as.data.frame(res_ratesci)
  expect_equal(
    res[, c("est", "lower", "upper")],
    res_ratesci,
    tolerance = 1e-4,
    ignore_attr = FALSE
  )
})

test_that("Check h_s_eair_diff for Wald method with CC against ratesci  function rdci", {
  skip_if_not_installed("ratesci")
  skip_if(packageVersion("ratesci") < ratesci_pkg_version)

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

  res_ratesci <- t(sapply(seq_len(nrow(inputs)), FUN = function(i) {
    x <- ratesci::rdci(
      x1 = inputs$x1[i],
      n1 = inputs$n1[i],
      x2 = inputs$x2[i],
      n2 = inputs$n2[i],
      distrib = "poi",
      level = 0.95,
      cc = TRUE
    )$estimates
    x <- x["Continuity adjusted Approximate Normal", c("est", "lower", "upper"), ]
    x <- round(100 * x, 4)
  }, simplify = TRUE))
  res_ratesci <- as.data.frame(res_ratesci)
  expect_equal(
    res[, c("est", "lower", "upper")],
    res_ratesci,
    tolerance = 1e-4,
    ignore_attr = FALSE
  )
})
