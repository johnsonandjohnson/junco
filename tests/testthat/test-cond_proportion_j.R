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
  # n is 12 here and no extreme successes.
  rsp <- c(rep(TRUE, 8), rep(FALSE, 4))
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_wald(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("uses exact when zero responders", {
  rsp <- rep(FALSE, 12) # No responses therefore use exact method.
  out <- s_cond_proportion_j(rsp, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_clopper_pearson(rsp, n = length(rsp), conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("uses exact when all responders", {
  rsp <- rep(TRUE, 12) # All responses therefore use exact method.
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

test_that("denom = 'N_row' derives its denominator from .df_row", {
  rsp <- c(rep(TRUE, 6), rep(FALSE, 6)) # n_obs = 12, n_rsp = 6
  df_row <- data.frame(rsp = c(rsp, FALSE, FALSE, FALSE))
  out <- s_cond_proportion_j(rsp, .var = "rsp", denom = "N_row", .df_row = df_row)
  # p_hat should be 6 / 15
  expect_equal(as.numeric(out$n_prop)[1], 6)
  expect_equal(as.numeric(out$n_prop)[2], 6 / 15)
  # Wald expected given non-extreme and n_obs >= denom_limit
  expected_ci <- 100 * tern::prop_wald(rsp, n = 15, conf_level = 0.95)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
})

test_that("missing row denominator input raises an error when requested", {
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  expect_error(s_cond_proportion_j(rsp, denom = "N_row"), "df.*data.frame", ignore.case = TRUE)
})

test_that("data-frame response edge cases have the expected outcomes", {
  all_false <- data.frame(rsp = rep(FALSE, 12))
  all_true <- data.frame(rsp = rep(TRUE, 12))

  false_out <- s_cond_proportion_j(all_false, .var = "rsp")
  true_out <- s_cond_proportion_j(all_true, .var = "rsp")

  expect_equal(as.numeric(false_out$n_prop), c(0, 0))
  expect_equal(as.numeric(true_out$n_prop), c(12, 1))
  expect_equal(
    as.numeric(false_out$prop_ci),
    as.numeric(100 * tern::prop_clopper_pearson(all_false$rsp, n = 12, conf_level = 0.95))
  )
  expect_equal(
    as.numeric(true_out$prop_ci),
    as.numeric(100 * tern::prop_clopper_pearson(all_true$rsp, n = 12, conf_level = 0.95))
  )

  expect_error(
    s_cond_proportion_j(data.frame(rsp = logical()), .var = "rsp"),
    "n.*positive integer",
    ignore.case = TRUE
  )
  expect_error(
    s_cond_proportion_j(data.frame(rsp = c(NA, NA)), .var = "rsp", na.rm = TRUE),
    "n.*positive integer",
    ignore.case = TRUE
  )
  expect_error(
    s_cond_proportion_j(data.frame(rsp = c(NA, NA)), .var = "rsp", na.rm = FALSE),
    "Missing values detected in response and `na.rm = FALSE`.",
    fixed = TRUE
  )
})

test_that("row-derived denominators exclude missing .df_row responses", {
  rsp <- data.frame(rsp = c(TRUE, FALSE))
  df_row <- data.frame(rsp = c(TRUE, FALSE, NA, NA))

  out <- s_cond_proportion_j(
    rsp,
    .var = "rsp",
    denom = "N_row",
    .df_row = df_row,
    na.rm = TRUE
  )

  expect_equal(as.numeric(out$n_prop), c(1, 1 / 2))
  expect_equal(
    as.numeric(out$prop_ci),
    as.numeric(100 * tern::prop_clopper_pearson(rsp$rsp, n = 2, conf_level = 0.95))
  )
})

test_that("na.rm = TRUE removes missing responses before analysis", {
  rsp_na <- c(TRUE, NA, FALSE, TRUE, NA, FALSE)
  rsp_clean <- c(TRUE, FALSE, TRUE, FALSE)

  out_na <- s_cond_proportion_j(rsp_na, na.rm = TRUE, conf_level = 0.95, denom = "n")
  out_clean <- s_cond_proportion_j(rsp_clean, na.rm = TRUE, conf_level = 0.95, denom = "n")

  expect_equal(as.numeric(out_na$n_prop), as.numeric(out_clean$n_prop), tolerance = 1e-12)
  expect_equal(as.numeric(out_na$prop_ci), as.numeric(out_clean$prop_ci), tolerance = 1e-12)
})

test_that("na.rm = FALSE errors when missing responses are present", {
  rsp <- c(TRUE, NA, FALSE)

  expect_error(
    s_cond_proportion_j(rsp, na.rm = FALSE),
    "Missing values detected in response and `na.rm = FALSE`.",
    fixed = TRUE
  )
})

test_that("na.rm = FALSE works when no missing responses are present", {
  dta <- data.frame(rsp = c(TRUE, FALSE, TRUE, FALSE))

  out <- s_cond_proportion_j(dta, .var = "rsp", na.rm = FALSE, conf_level = 0.95, denom = "n")
  expected_ci <- 100 * tern::prop_clopper_pearson(dta$rsp, n = nrow(dta), conf_level = 0.95)

  expect_equal(as.numeric(out$n_prop), c(2, 0.5), tolerance = 1e-12)
  expect_equal(as.numeric(out$prop_ci), as.numeric(expected_ci), tolerance = 1e-12)
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
  expected <- "70% CI for Response Rates (Wald if n >= 8, x > 1, x < n - 1; else Clopper-Pearson)"
  expect_identical(result, expected)

  result <- d_cond_proportion_j(conf_level = 0.7, long = FALSE)
  expected <- "70% CI (Wald / Clopper-Pearson)"
  expect_identical(result, expected)
})

test_that("d_cond_proportion_j explains a selected long-label method", {
  result <- d_cond_proportion_j(
    conf_level = 0.95,
    long = TRUE,
    num_limit = 1,
    denom_limit = 10,
    method = "clopper-pearson",
    method_denom = 12,
    method_rsp = 11
  )
  expect_identical(
    result,
    "95% CI for Response Rates (Clopper-Pearson because x >= n - 1)"
  )

  result <- d_cond_proportion_j(
    conf_level = 0.95,
    long = TRUE,
    method = "wald",
    reason = "n >= 10, x = 8"
  )
  expect_identical(
    result,
    "95% CI for Response Rates (Wald because n >= 10, x = 8)"
  )

  result <- d_cond_proportion_j(
    conf_level = 0.95,
    long = TRUE,
    method = "clopper-pearson",
    reason = "x = 0"
  )
  expect_identical(
    result,
    "95% CI for Response Rates (Clopper-Pearson because x = 0)"
  )
})

test_that("d_cond_proportion_j uses concise labels for selected methods", {
  expect_identical(
    d_cond_proportion_j(
      conf_level = 0.95,
      method = "wald",
      reason = "n >= 10, x = 8"
    ),
    "95% CI (Wald)"
  )
  expect_identical(
    d_cond_proportion_j(
      conf_level = 0.95,
      method = "clopper-pearson",
      reason = "x = 0"
    ),
    "95% CI (Clopper-Pearson)"
  )
})

test_that("a_cond_proportion_j returns formatted section consistent with s_cond_proportion_j", {
  rsp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE) # n=12, n_rsp=7
  dta <- data.frame(rsp = rsp)

  out <- a_cond_proportion_j(dta, .var = "rsp", conf_level = 0.95)
  expected <- s_cond_proportion_j(dta, .var = "rsp", conf_level = 0.95)

  expect_s3_class(out, "RowsVerticalSection")
  expect_named(out, c("n_prop", "prop_ci"))

  expect_equal(as.numeric(out$n_prop[[1]]), as.numeric(expected$n_prop), tolerance = 1e-12)
  expect_equal(as.numeric(out$prop_ci[[1]]), as.numeric(expected$prop_ci), tolerance = 1e-12)
  expect_identical(attr(out$n_prop, "label"), attr(expected$n_prop, "label"))
  expect_identical(attr(out$prop_ci, "label"), attr(expected$prop_ci, "label"))
})

test_that("a_cond_proportion_j works in full table build", {
  rsp <- c(rep(TRUE, 7), rep(FALSE, 5))
  dta <- data.frame(rsp = rsp)

  lyt <- rtables::basic_table() |>
    rtables::analyze("rsp", afun = a_cond_proportion_j)

  tbl <- rtables::build_table(lyt, dta)
  vals <- rtables::cell_values(tbl)

  expect_true(!is.null(tbl))
  expect_named(vals, c("n_prop", "prop_ci"))
  expect_equal(as.numeric(vals$n_prop[["all obs"]]), c(7, 7 / 12), tolerance = 1e-12)
  expect_equal(
    as.numeric(vals$prop_ci[["all obs"]]),
    as.numeric(100 * tern::prop_wald(rsp, n = length(rsp), conf_level = 0.95)),
    tolerance = 1e-12
  )
})

test_that("table workflow uses row count as denominator when requested", {
  rsp <- c(rep(TRUE, 6), rep(FALSE, 6))
  dta <- data.frame(grp = rep(c("A", "B"), each = 12), rsp = rep(rsp, 2))

  lyt <- rtables::basic_table() |>
    rtables::split_cols_by("grp") |>
    rtables::analyze("rsp", afun = a_cond_proportion_j, extra_args = list(denom = "N_row"))
  vals <- rtables::cell_values(rtables::build_table(lyt, dta))

  for (grp in c("A", "B")) {
    expect_equal(as.numeric(vals$n_prop[[grp]]), c(6, 6 / 24))
    expect_equal(
      as.numeric(vals$prop_ci[[grp]]),
      as.numeric(100 * tern::prop_wald(rsp, n = 24, conf_level = 0.95)),
      tolerance = 1e-12
    )
  }
})

test_that("row-level method uses exact CIs in every column with row-wise method scope", {
  rsp_a <- c(TRUE, TRUE, FALSE, FALSE)
  rsp_b <- rep(FALSE, 4)
  dta <- data.frame(
    grp = rep(c("A", "B"), each = 4),
    rsp = c(rsp_a, rsp_b)
  )

  lyt <- rtables::basic_table() |>
    rtables::split_cols_by("grp") |>
    rtables::analyze(
      "rsp",
      afun = a_cond_proportion_j,
      extra_args = list(denom_limit = 10, long = TRUE, method_scope = "row")
    )
  tbl <- rtables::build_table(lyt, dta)
  vals <- rtables::cell_values(tbl)$prop_ci

  expect_identical(
    rtables::make_row_df(tbl)$label[2],
    "95% CI for Response Rates (Clopper-Pearson because n < 10)"
  )

  expect_equal(
    as.numeric(vals[["A"]]),
    as.numeric(100 * tern::prop_clopper_pearson(rsp_a, n = 4, conf_level = 0.95)),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(vals[["B"]]),
    as.numeric(100 * tern::prop_clopper_pearson(rsp_b, n = 4, conf_level = 0.95)),
    tolerance = 1e-12
  )
})

test_that("row-level method uses Wald CIs in every column with row-wise method scope", {
  rsp_a <- c(rep(TRUE, 8), rep(FALSE, 4))
  rsp_b <- rep(FALSE, 12)
  dta <- data.frame(
    grp = rep(c("A", "B"), each = 12),
    rsp = c(rsp_a, rsp_b)
  )

  lyt <- rtables::basic_table() |>
    rtables::split_cols_by("grp") |>
    rtables::analyze(
      "rsp",
      afun = a_cond_proportion_j,
      extra_args = list(denom_limit = 20, long = TRUE, method_scope = "row")
    )
  tbl <- rtables::build_table(lyt, dta)
  vals <- rtables::cell_values(tbl)$prop_ci

  expect_identical(
    rtables::make_row_df(tbl)$label[2],
    "95% CI for Response Rates (Wald because n >= 20, x = 8)"
  )

  expect_equal(
    as.numeric(vals[["A"]]),
    as.numeric(100 * tern::prop_wald(rsp_a, n = 12, conf_level = 0.95)),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(vals[["B"]]),
    as.numeric(100 * tern::prop_wald(rsp_b, n = 12, conf_level = 0.95)),
    tolerance = 1e-12
  )
})

test_that("row-level numerator limit selects exact CIs across columns with row-wise method scope", {
  rsp_a <- c(TRUE, rep(FALSE, 11))
  rsp_b <- rep(FALSE, 12)
  dta <- data.frame(grp = rep(c("A", "B"), each = 12), rsp = c(rsp_a, rsp_b))

  lyt <- rtables::basic_table() |>
    rtables::split_cols_by("grp") |>
    rtables::analyze(
      "rsp",
      afun = a_cond_proportion_j,
      extra_args = list(method_scope = "row", long = TRUE, num_limit = 1)
    )
  tbl <- rtables::build_table(lyt, dta)
  vals <- rtables::cell_values(tbl)$prop_ci

  expect_identical(
    rtables::make_row_df(tbl)$label[2],
    "95% CI for Response Rates (Clopper-Pearson because x <= 1)"
  )
  expect_equal(
    as.numeric(vals[["A"]]),
    as.numeric(100 * tern::prop_clopper_pearson(rsp_a, n = 12, conf_level = 0.95)),
    tolerance = 1e-12
  )
})

test_that("row-wise method selection handles missing values according to na.rm", {
  rsp <- c(TRUE, FALSE, TRUE)
  row_dta <- data.frame(rsp = c(TRUE, FALSE, NA))

  out <- s_cond_proportion_j(
    rsp,
    .var = "rsp",
    method_scope = "row",
    .df_row = row_dta,
    denom_limit = 2,
    na.rm = TRUE,
    long = TRUE
  )
  expect_identical(
    attr(out$prop_ci, "label"),
    "95% CI for Response Rates (Wald because n >= 2, x = 1)"
  )

  expect_error(
    s_cond_proportion_j(
      rsp,
      .var = "rsp",
      method_scope = "row",
      .df_row = row_dta,
      denom_limit = 2,
      na.rm = FALSE
    ),
    "Missing values detected in response and `na.rm = FALSE`.",
    fixed = TRUE
  )
})

test_that("row-wise labels distinguish upper-boundary exact-method reasons", {
  all_responders <- rep(TRUE, 12)
  out <- s_cond_proportion_j(
    all_responders,
    .var = "rsp",
    method_scope = "row",
    .df_row = data.frame(rsp = all_responders),
    long = TRUE
  )
  expect_identical(
    attr(out$prop_ci, "label"),
    "95% CI for Response Rates (Clopper-Pearson because x = n)"
  )

  near_all_responders <- c(rep(TRUE, 11), FALSE)
  out <- s_cond_proportion_j(
    near_all_responders,
    .var = "rsp",
    method_scope = "row",
    .df_row = data.frame(rsp = near_all_responders),
    num_limit = 1,
    long = TRUE
  )
  expect_identical(
    attr(out$prop_ci, "label"),
    "95% CI for Response Rates (Clopper-Pearson because x >= n - 1)"
  )
})
