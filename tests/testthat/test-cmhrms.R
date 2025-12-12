test_that("s_cmhrms_j works as expected with and without strata", {
  set.seed(12)
  nobs <- 100
  dta <- data.frame(
    rsp = c(rep(FALSE, nobs / 2), rep(TRUE, nobs / 2)),
    grp = factor(sample(c("A", "B"), nobs, TRUE), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), nobs, TRUE))
  )

  result_strata <- s_cmhrms_j(
    df = subset(dta, grp == "B"),
    .df_row = dta,
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp", strata = "strata")
  )
  expected_strata <- list(pval = formatters::with_label(0.5241419, "p-value"))
  expect_equal(result_strata, expected_strata, tolerance = 1e-6)

  result_no_strata <- expect_silent(s_cmhrms_j(
    df = subset(dta, grp == "B"),
    .df_row = dta,
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp")
  ))
  expected_no_strata <- list(pval = formatters::with_label(0.686809, "p-value"))
  expect_equal(result_no_strata, expected_no_strata, tolerance = 1e-6)
})

test_that("s_cmhrms_j works as expected with empty data set or in reference column", {
  dta <- data.frame(
    rsp = logical(0),
    grp = factor(character(0), levels = c("A", "B"))
  )

  result <- expect_silent(s_cmhrms_j(
    df = subset(dta, grp == "B"),
    .df_row = dta,
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp")
  ))
  expected <- list(pval = formatters::with_label(numeric(0), "p-value"))
  expect_equal(result, expected)

  result_refcol <- expect_silent(s_cmhrms_j(
    df = subset(dta, grp == "B"),
    .df_row = dta,
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = TRUE,
    variables = list(arm = "grp")
  ))
  expected_refcol <- list(pval = formatters::with_label(numeric(0), "p-value"))
  expect_equal(result_refcol, expected_refcol)
})

test_that("a_cmhrms_j works in a table layout as expected", {
  set.seed(12)
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  lyt <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      "rsp",
      afun = a_cmhrms_j,
      show_labels = "hidden",
      extra_args = list(
        variables = list(arm = "grp", strata = "strata"),
        ref_path = c("grp", "A")
      )
    )
  res <- expect_silent(build_table(lyt, df = dta))
  expect_snapshot(res)
})

test_that("a_cmhrms_j_with_exclude works in a table layout as expected", {
  set.seed(12)
  dta <- data.frame(
    visit = factor(rep(c("Baseline", "Week 1"), length.out = 100)),
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  lyt <- basic_table() |>
    split_cols_by(var = "grp") |>
    split_rows_by(var = "visit") |>
    analyze(
      "rsp",
      afun = a_cmhrms_j_with_exclude,
      show_labels = "hidden",
      extra_args = list(
        variables = list(arm = "grp", strata = "strata"),
        ref_path = c("grp", "A"),
        exclude_levels = list(visit = "Baseline")
      )
    )
  res <- expect_silent(build_table(lyt, df = dta)) |>
    safe_prune_table(prune_func = tern::keep_rows(keep_non_null_rows))
  expect_snapshot(res)
})
