# s_test_proportion_diff_mf() ----

test_that("s_test_proportion_diff_mf() chooses the CMH method", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), n, replace = TRUE),
    grp = sample(c("Placebo", "X"), n, replace = TRUE),
    strata = factor(sample(LETTERS[1:4], n, replace = TRUE))
  )

  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(0.9452043, label),
    executed_method = "cmh"
  )
  attr(expected$pval, "z_stat") <- -0.06873026

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_test_proportion_diff_mf() chooses custom cmh_sato method", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), n, replace = TRUE),
    grp = sample(c("Placebo", "X"), n, replace = TRUE),
    strata = factor(sample(LETTERS[1:4], n, replace = TRUE))
  )

  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata"),
      mf_method = "cmh_sato"
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test with Sato Variance Estimator)"
  expected <- list(
    pval = formatters::with_label(0.9441739, label),
    executed_method = "cmh_sato"
  )
  attr(expected$pval, "z_stat") <- -0.07002487

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_test_proportion_diff_mf() chooses the exact method", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
    grp = c("Placebo", "Placebo", "X", "Placebo", "Placebo", "X", "X", "Placebo", "X"),
    strata = factor(c("A", "A", "A", "A", "B", "B", "B", "B", "B"), levels = c("A", "B", "Z"))
  )

  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(0.5238095, label),
    executed_method = "fisher"
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_test_proportion_diff_mf() chooses the exact method when strata are not specified", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "Placebo", "X", "X", "Placebo")
  )

  expect_warning(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE
    ),
    "strata variables"
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(1, label),
    executed_method = "fisher"
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_test_proportion_diff_mf() returns empty results when .in_ref_col is NULL or TRUE", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  # .in_ref_col = NULL # nolintr
  expect_silent(
    result_null <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = NULL,
      variables = list(strata = "strata")
    )
  )

  # .in_ref_col = TRUE # nolintr
  expect_silent(
    result_true <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = TRUE,
      variables = list(strata = "strata")
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(numeric(), label),
    executed_method = NA_character_
  )

  expect_identical(result_null, expected)
  expect_identical(result_true, expected)
})

test_that("s_test_proportion_diff_mf() errors when .ref_group is NULL", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  expect_error(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = NULL,
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    ),
    ".ref_group"
  )
})

test_that("s_test_proportion_diff_mf() works with unused strata levels", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(1, label),
    executed_method = "fisher"
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_test_proportion_diff_mf() works with empty df or .ref_group", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  # Both df and .ref_group empty.
  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "not_exist"),
      .var = "rsp",
      .ref_group = subset(data, grp == "not_exist"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  # Only .ref_group empty.
  expect_silent(
    result_ref_empty <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "not_exist"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  # Only df empty.
  expect_silent(
    result_df_empty <- s_test_proportion_diff_mf(
      df = subset(data, grp == "not_exist"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(1, label),
    executed_method = "fisher"
  )

  expect_identical(result, expected)
  expect_identical(result_ref_empty, expected)
  expect_identical(result_df_empty, expected)
})

test_that("s_test_proportion_diff_mf() works with string val", {
  data <- data.frame(
    rsp = c("Y", "Y", "Y", "Y", "N", "N"),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .in_ref_col = FALSE,
      .ref_group = subset(data, grp == "Placebo"),
      val = "Y",
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
  expected <- list(
    pval = formatters::with_label(0.4, label),
    executed_method = "fisher"
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_test_proportion_diff_mf() errors when val is incompatible", {
  data <- data.frame(
    rsp = c("Y", "Y", "Y", "Y", "N", "N"),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  expect_error(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .in_ref_col = FALSE,
      .ref_group = subset(data, grp == "Placebo"),
      val = TRUE,
      variables = list(strata = c("strata_1", "strata_2"))
    ),
    "val"
  )
})

test_that("s_test_proportion_diff_mf() errors when NAs are present", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  expect_error(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    ),
    "missing"
  )
})

test_that("s_test_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE", {
  data <- data.frame(
    rsp = c(TRUE, NA, TRUE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2")),
    some_var_with_NAs = c("v1", "v2", "v2", "v3", "v4", NA)
  )

  # expect_snapshot() captures 2 warnings.
  expect_snapshot(
    s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      na.rm = TRUE,
      variables = list(strata = "strata")
    )
  )
})

test_that("s_test_proportion_diff_mf() uses custom alternative", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "Placebo", "X", "X", "Placebo"),
    strata = factor(c("A", "A", "B", "B", "B", "A"), levels = c("A", "B", "Z"))
  )

  expect_silent(
    result <- s_test_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = "strata"),
      alternative = "greater"
    )
  )

  label <- "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test, 1-sided, direction greater)"
  expected <- list(
    pval = formatters::with_label(0.9333333, label),
    executed_method = "fisher"
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

# a_test_proportion_diff_mf() ----

test_that("a_test_proportion_diff_mf() works in full table build (large sample)", {
  set.seed(1)
  n <- 100
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), n, replace = TRUE),
    grp = sample(c("Placebo", "X"), n, replace = TRUE),
    strata = factor(sample(LETTERS[1:4], n, replace = TRUE))
  )

  tbl <- expect_silent(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(variables = list(strata = "strata"))
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_test_proportion_diff_mf() works in full table build", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S1", "S2", "S1", "S2"), levels = c("S1", "S2", "XXX"))
  )

  tbl <- expect_silent(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(variables = list(strata = c("strata_1", "strata_2")))
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_test_proportion_diff_mf() respects custom settings (CMH method)", {
  set.seed(1)
  n <- 100
  data <- data.frame(
    rsp = sample(c("Y", "N"), n, replace = TRUE),
    grp = sample(c("Placebo", "X"), n, replace = TRUE),
    strata = factor(sample(LETTERS[1:4], n, replace = TRUE))
  )

  tbl <- expect_silent(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(
          variables = list(strata = "strata"),
          val = "Y",
          alternative = "less",
          mf_method = "cmh_wh",
          .stats = "pval",
          .labels = c(pval = "my_label"),
          .formats = list(pval = "xx.xxxx"),
          .indent_mods = c(pval = 2L)
        )
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_test_proportion_diff_mf() respects custom settings", {
  data <- data.frame(
    rsp = c("Y", "Y", "N", "Y", "N"),
    grp = c("X", "X", "X", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S1", "S2", "S1", "S2"), levels = c("S1", "S2", "XXX"))
  )

  tbl <- expect_silent(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(
          variables = list(strata = c("strata_1", "strata_2")),
          val = "Y",
          alternative = "less",
          mf_method = "cmh_wh",
          .stats = "pval",
          .labels = c(pval = "my_label"),
          .formats = list(pval = "xx.xxxx"),
          .indent_mods = c(pval = 2L)
        )
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_test_proportion_diff_mf() respects custom exact_footnote", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S1", "S2", "S1", "S2"), levels = c("S1", "S2", "XXX"))
  )

  tbl <- expect_silent(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(
          variables = list(strata = c("strata_1", "strata_2")),
          exact_footnote = "This was the Fisher's exact test"
        )
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_test_proportion_diff_mf() errors when NAs are present", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  expect_error(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(variables = list(strata = "strata"))
      ) |>
      build_table(data),
    "missing"
  )
})

test_that("a_test_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2")),
    some_var_with_NAs = c("v1", "v2", "v2", "v3", "v4", NA)
  )

  # expect_snapshot() captures 2 warnings.
  expect_snapshot(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_test_proportion_diff_mf,
        extra_args = list(variables = list(strata = "strata"), na.rm = TRUE)
      ) |>
      build_table(data)
  )
})
