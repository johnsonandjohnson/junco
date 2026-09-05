test_that("returns structure identical to s_proportion style", {
  rsp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE) # n=12, n_rsp=7
  
  out <- s_cond_proportion_j(rsp)
  
  expect_type(out, "list")
  expect_named(out, c("n_prop", "prop_ci"))
  # n_prop is a 2-length numeric with a label
  expect_equal(as.numeric(out$n_prop), c(7, 7 / 12))
  expect_identical(attr(out$n_prop, "label"), "Responders")
  # prop_ci is numeric length 2 with label
  expect_equal(length(out$prop_ci), 2L)
  expect_true(is.numeric(out$prop_ci))
  expect_true(is.character(attr(out$prop_ci, "label")))
})

test_that("uses Wald when not near boundaries and n_obs >= denom_limit", {
  set.seed(1)
  # n = 12 >= default denom_limit 10, not extreme successes
  rsp <- c(rep(TRUE, 8), rep(FALSE, 4)) # n_rsp = 8
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_wald(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("uses exact when zero responders", {
  rsp <- rep(FALSE, 12) # n_rsp = 0 -> exact
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_clopper_pearson(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("uses exact when all responders", {
  rsp <- rep(TRUE, 12) # n_rsp = n_obs -> exact
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_clopper_pearson(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("uses exact when n_obs < denom_limit", {
  # default denom_limit = 10; here n_obs = 9
  rsp <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_clopper_pearson(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("num_limit controls boundary exactness (lower boundary)", {
  # n_obs = 20; num_limit = 1 => exact if n_rsp <= 1 or n_rsp >= 19
  rsp <- c(rep(TRUE, 1), rep(FALSE, 19))
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, num_limit = 1, denom = "n")
  expected_ci <- 100 * prop_clopper_pearson(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("num_limit controls boundary exactness (upper boundary)", {
  rsp <- c(rep(TRUE, 19), rep(FALSE, 1))
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, num_limit = 1, denom = "n")
  expected_ci <- 100 * tern::prop_clopper_pearson(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("num_limit not exceeded -> Wald when n_obs >= denom_limit", {
  # n_obs = 20; num_limit = 1; n_rsp = 2 (not within <=1 or >=19)
  rsp <- c(rep(TRUE, 2), rep(FALSE, 18))
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, num_limit = 1, denom = "n")
  expected_ci <- 100 * tern::prop_wald(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("denom = 'N_col' uses provided .N_col for p_hat and CI", {
  rsp <- c(rep(TRUE, 7), rep(FALSE, 5)) # n_obs = 12, n_rsp = 7
  out <- s_cond_proportion_j(rsp, denom = "N_col", .N_col = 30)
  # p_hat should be 7 / 30
  n_prop <- as.numeric(out$n_prop)
  expect_equal(n_prop[1], 7)
  expect_equal(n_prop[2], 7 / 30)
  # CI computed with n = 30 using the same helper as in s_proportion
  # The method should be Wald in this configuration (n_obs = 12 >= 10, not extreme)
  expected_ci <- 100 * tern::prop_wald(rsp, n = 30, conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("denom = 'N_row' uses provided .N_row for p_hat and CI", {
  rsp <- c(rep(TRUE, 6), rep(FALSE, 6)) # n_obs = 12, n_rsp = 6
  out <- s_cond_proportion_j(rsp, denom = "N_row", .N_row = 15)
  # p_hat should be 6 / 15
  expect_equal(as.numeric(out$n_prop)[1], 6)
  expect_equal(as.numeric(out$n_prop)[2], 6 / 15)
  # Wald expected given non-extreme and n_obs >= denom_limit
  expected_ci <- 100 * tern::prop_wald(rsp, n = 15, conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("missing .N_col/.N_row raises error when requested by denom", {
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  expect_error(s_cond_proportion_j(rsp, denom = "N_col"), "argument.*missing|object.*not found", ignore.case = TRUE)
  expect_error(s_cond_proportion_j(rsp, denom = "N_row"), "argument.*missing|object.*not found", ignore.case = TRUE)
})

test_that("conf_level is respected in CI calculation", {
  rsp <- c(rep(TRUE, 8), rep(FALSE, 4)) # n=12, not extreme
  out_90 <- s_cond_proportion_j(rsp, conf_level = 0.90, denom = "n")
  out_95 <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_90 <- 100 * tern::prop_wald(rsp, n = length(rsp), conf_level = 0.90)
  expected_95 <- 100 * tern::prop_wald(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out_90$prop_ci), as.numeric(expected_90), tolerance = 1e-12)
  expect_equal(as.numeric(out_95$prop_ci), as.numeric(expected_95), tolerance = 1e-12)
})

test_that("label is set via d_cond_proportion_j", {
  rsp <- c(rep(TRUE, 8), rep(FALSE, 4))
  out <- s_cond_proportion_j(rsp, conf_level = 0.90, long = TRUE)
  expect_true(is.character(attr(out$prop_ci, "label")))
  # If d_cond_proportion_j is available, label should match exactly
  if (exists("d_cond_proportion_j")) {
    expected_label <- d_cond_proportion_j(conf_level = 0.90, long = TRUE, num_limit = 0, denom_limit = 10)
    expect_identical(attr(out$prop_ci, "label"), expected_label)
  }
})

test_that("d_cond_proportion_j long label looks as expected", {
  result <- d_cond_proportion_j(conf_level = 0.7, long = TRUE, num_limit = 1, denom_limit = 8)
  expected <- "70% CI for Response Rates (Wald if n >= 8 and x > 1, else Clopper-Pearson)"
  expect_identical(result, expected)
  
  result <- d_cond_proportion_j(conf_level = 0.7, long = FALSE)
  expected <- "70% CI (Wald / Clopper-Pearson)"
  expect_identical(result, expected)
})
