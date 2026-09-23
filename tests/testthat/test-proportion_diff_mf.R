# s_proportion_diff_mf() ----

test_that("s_proportion_diff_mf() chooses the CMH method", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), n, replace = TRUE),
    grp = sample(c("Placebo", "X"), n, replace = TRUE),
    strata = factor(sample(LETTERS[1:4], n, replace = TRUE))
  )

  expect_silent(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    )
  )

  expected <- list(
    diff = c(diff_cmh = -0.6902026),
    diff_ci = c(diff_ci_cmh_l = -19.76883, diff_ci_cmh_u = 18.38843),
    diff_est_ci = NA,
    executed_method = "cmh"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)

  label_prefix <- "Difference in Response rate (%)"
  method <- "(CMH, without correction / Unconditional exact)"
  attr(expected$diff, "label") <- paste(label_prefix, method)
  attr(expected$diff_ci, "label") <- paste(label_prefix, "95% CI", method)
  attr(expected$diff_est_ci, "label") <- paste(label_prefix, "and 95% CI", method)

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_proportion_diff_mf() chooses the exact method", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "Placebo", "X", "X", "Placebo"),
    strata = factor(c("A", "A", "B", "B", "B", "A"), levels = c("A", "B", "Z"))
  )

  expect_silent(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    )
  )

  expected <- list(
    diff = c(diff_uncond_exact_diff = 25),
    diff_ci = c(diff_ci_uncond_exact_diff_l = -64.20027, diff_ci_uncond_exact_diff_u = 89.00002),
    diff_est_ci = NA,
    executed_method = "uncond_exact_diff"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)

  label_prefix <- "Difference in Response rate (%)"
  method <- "(CMH, without correction / Unconditional exact)"
  attr(expected$diff, "label") <- paste(label_prefix, method)
  attr(expected$diff_ci, "label") <- paste(label_prefix, "95% CI", method)
  attr(expected$diff_est_ci, "label") <- paste(label_prefix, "and 95% CI", method)

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_proportion_diff_mf() chooses the exact method when strata are not specified", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "Placebo", "X", "X", "Placebo")
  )

  expect_warning(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE
    ),
    "strata variables"
  )

  expected <- list(
    diff = c(diff_uncond_exact_diff = 25),
    diff_ci = c(diff_ci_uncond_exact_diff_l = -64.20027, diff_ci_uncond_exact_diff_u = 89.00002),
    diff_est_ci = NA,
    executed_method = "uncond_exact_diff"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)

  label_prefix <- "Difference in Response rate (%)"
  method <- "(CMH, without correction / Unconditional exact)"
  attr(expected$diff, "label") <- paste(label_prefix, method)
  attr(expected$diff_ci, "label") <- paste(label_prefix, "95% CI", method)
  attr(expected$diff_est_ci, "label") <- paste(label_prefix, "and 95% CI", method)

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_proportion_diff_mf() errors when NAs are present", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  expect_error(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    ),
    "missing"
  )
})

test_that("s_proportion_diff_mf() removes NAs and warns when na.rm = TRUE", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  # expect_snapshot() captures warnings.
  expect_snapshot(
    s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      na.rm = TRUE,
      variables = list(strata = "strata")
    )
  )
})

test_that("s_proportion_diff_mf() uses custom conf_level", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "Placebo", "X", "X", "Placebo"),
    strata = factor(c("A", "A", "B", "B", "B", "A"), levels = c("A", "B", "Z"))
  )

  expect_silent(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata"),
      conf_level = 0.9
    )
  )

  expected <- list(
    diff = c(diff_uncond_exact_diff = 25),
    diff_ci = c(diff_ci_uncond_exact_diff_l = -54.40462, diff_ci_uncond_exact_diff_u = 84.07958),
    diff_est_ci = NA,
    executed_method = "uncond_exact_diff"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)

  label_prefix <- "Difference in Response rate (%)"
  method <- "(CMH, without correction / Unconditional exact)"
  attr(expected$diff, "label") <- paste(label_prefix, method)
  attr(expected$diff_ci, "label") <- paste(label_prefix, "90% CI", method)
  attr(expected$diff_est_ci, "label") <- paste(label_prefix, "and 90% CI", method)

  expect_equal(result, expected, tolerance = 1e-6)
})

# a_proportion_diff_mf() ----

test_that("a_proportion_diff_mf() works in full table build", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S1", "S2", "S1", "S2"), levels = c("S1", "S2", "XXX"))
  )

  lyt <- basic_table() |>
    split_cols_by(var = "grp", ref_group = "Placebo") |>
    analyze(
      vars = "rsp",
      afun = a_proportion_diff_mf,
      extra_args = list(
        variables = list(strata = c("strata_1", "strata_2")),
        conf_level = 0.90,
        mf_method = "cmh_mn",
        .stats = "diff_est_ci"
      )
    )

  tbl <- rtables::build_table(lyt, data)
  vals <- rtables::cell_values(tbl)

  stat_names <- c("diff_uncond_exact_diff", "diff_ci_uncond_exact_diff_l", "diff_ci_uncond_exact_diff_u")
  label <- "Difference in Response rate (%) and 90% CI (CMH, Miettinen and Nurminen / Unconditional exact)"
  expect_true(!is.null(tbl))
  expect_named(vals, c("X", "Placebo"))
  expect_named(vals$X, stat_names)
  expect_equal(
    unname(vals$X[stat_names]), c(16.66667, -61.04420, 82.13047),
    tolerance = 1e-6
  )
  expect_identical(vals$Placebo, formatters::with_label(numeric(0), label))
})
