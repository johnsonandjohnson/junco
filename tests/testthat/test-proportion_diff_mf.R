# h_set_labels_prop_diff_mf() ----

test_that("h_set_labels_prop_diff_mf() sets labels with default methods", {
  y <- list(diff = 2, diff_ci = c(1, 3), diff_est_ci = c(2, 1, 3))

  result <- h_set_labels_prop_diff_mf(y = y, mf_method = "cmh", conf_level = 0.95)

  expect_identical(
    attr(result$diff, "label"),
    "Difference in Response rate (%) (CMH, without correction / Unconditional exact)"
  )
  expect_identical(
    attr(result$diff_ci, "label"),
    "Difference in Response rate (%) 95% CI (CMH, without correction / Unconditional exact)"
  )
  expect_identical(
    attr(result$diff_est_ci, "label"),
    "Difference in Response rate (%) and 95% CI (CMH, without correction / Unconditional exact)"
  )
})

test_that("h_set_labels_prop_diff_mf() uses the specified MF method and conf_level", {
  y <- list(diff = 2, diff_ci = c(1, 3), diff_est_ci = c(2, 1, 3))

  result <- h_set_labels_prop_diff_mf(y = y, mf_method = "cmh_mn", conf_level = 0.90)

  expect_equal(
    attr(result$diff, "label"),
    "Difference in Response rate (%) (CMH, Miettinen and Nurminen / Unconditional exact)"
  )
  expect_equal(
    attr(result$diff_ci, "label"),
    "Difference in Response rate (%) 90% CI (CMH, Miettinen and Nurminen / Unconditional exact)"
  )
})

test_that("h_set_labels_prop_diff_mf() uses the specified non-MF method", {
  y <- list(diff = 2, diff_ci = c(1, 3), diff_est_ci = c(2, 1, 3))

  result <- h_set_labels_prop_diff_mf(
    y = y, mf_method = "cmh_sato", non_mf_method = "wald", conf_level = 0.91
  )

  expect_equal(
    attr(result$diff, "label"),
    "Difference in Response rate (%) (CMH, Sato variance estimator / Wald, without correction)"
  )
  expect_equal(
    attr(result$diff_ci, "label"),
    "Difference in Response rate (%) 91% CI (CMH, Sato variance estimator / Wald, without correction)"
  )
  expect_equal(
    attr(result$diff_est_ci, "label"),
    "Difference in Response rate (%) and 91% CI (CMH, Sato variance estimator / Wald, without correction)"
  )
})

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
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_proportion_diff_mf() chooses custom cmh_sato method", {
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
      variables = list(strata = "strata"),
      mf_method = "cmh_sato"
    )
  )

  expected <- list(
    diff = c(diff_cmh_sato = -0.6902026),
    diff_ci = c(diff_ci_cmh_sato_l = -20.0086550, diff_ci_cmh_sato_u = 18.6282499),
    diff_est_ci = NA,
    executed_method = "cmh_sato"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh_sato", conf_level = 0.95)

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
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

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
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_proportion_diff_mf() returns empty results when .in_ref_col is NULL or TRUE", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  # .in_ref_col = NULL # nolintr
  expect_silent(
    result_null <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = NULL,
      variables = list(strata = "strata")
    )
  )

  # .in_ref_col = TRUE # nolintr
  expect_silent(
    result_true <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = TRUE,
      variables = list(strata = "strata")
    )
  )

  expected <- list(
    diff = numeric(0),
    diff_ci = numeric(0),
    diff_est_ci = numeric(0),
    executed_method = NA_character_
  )
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

  expect_identical(result_null, expected)
  expect_identical(result_true, expected)
})

test_that("s_proportion_diff_mf() errors when .ref_group is NULL", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2"))
  )

  expect_error(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = NULL,
      .in_ref_col = FALSE,
      variables = list(strata = "strata")
    ),
    ".ref_group"
  )
})

test_that("s_proportion_diff_mf() works with unused strata levels", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  expect_silent(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  expected <- list(
    diff = c(diff_uncond_exact_diff = 33.33333),
    diff_ci = c(diff_ci_uncond_exact_diff_l = -55.44439, diff_ci_uncond_exact_diff_u = 90.94305),
    diff_est_ci = NA,
    executed_method = "uncond_exact_diff"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("s_proportion_diff_mf() works with empty df or .ref_group", {
  data <- data.frame(
    rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  # Both df and .ref_group empty.
  expect_silent(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "not_exist"),
      .var = "rsp",
      .ref_group = subset(data, grp == "not_exist"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  # Only .ref_group empty.
  expect_silent(
    result_ref_empty <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .ref_group = subset(data, grp == "not_exist"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  # Only df empty.
  expect_silent(
    result_df_empty <- s_proportion_diff_mf(
      df = subset(data, grp == "not_exist"),
      .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"),
      .in_ref_col = FALSE,
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  expected <- list(
    diff = c(diff_uncond_exact_diff = NaN),
    diff_ci = c(diff_ci_uncond_exact_diff_l = NaN, diff_ci_uncond_exact_diff_u = NaN),
    diff_est_ci = NA,
    executed_method = "uncond_exact_diff"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

  expect_identical(result, expected)
  expect_identical(result_ref_empty, expected)
  expect_identical(result_df_empty, expected)
})

test_that("s_proportion_diff_mf() works with string val", {
  data <- data.frame(
    rsp = c("Y", "Y", "N", "Y", "N", "N"),
    grp = c("X", "X", "X", "Placebo", "Placebo", "Placebo"),
    strata_1 = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B", "Z")),
    strata_2 = factor(c("S1", "S2", "S2", "S1", "S2", "S1"), levels = c("S1", "S2", "XXX"))
  )

  expect_silent(
    result <- s_proportion_diff_mf(
      df = subset(data, grp == "X"),
      .var = "rsp",
      .in_ref_col = FALSE,
      .ref_group = subset(data, grp == "Placebo"),
      val = "Y",
      variables = list(strata = c("strata_1", "strata_2"))
    )
  )

  expected <- list(
    diff = c(diff_uncond_exact_diff = 33.33333),
    diff_ci = c(diff_ci_uncond_exact_diff_l = -55.44439, diff_ci_uncond_exact_diff_u = 90.94305),
    diff_est_ci = NA,
    executed_method = "uncond_exact_diff"
  )
  expected$diff_est_ci <- c(expected$diff, expected$diff_ci)
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.95)

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

test_that("s_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE", {
  data <- data.frame(
    rsp = c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    grp = factor(c("X", "X", "X", "Placebo", "Placebo", "Placebo")),
    strata = factor(c("S1", "S1", NA, "S1", "S2", "S2")),
    some_var_with_NAs = c("v1", "v2", "v2", "v3", "v4", NA)
  )

  # expect_snapshot() captures 2 warnings.
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
  expected <- h_set_labels_prop_diff_mf(expected, mf_method = "cmh", conf_level = 0.9)

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

  tbl <- expect_silent(
    basic_table() |>
      split_cols_by(var = "grp", ref_group = "Placebo") |>
      analyze(
        vars = "rsp",
        afun = a_proportion_diff_mf,
        extra_args = list(variables = list(strata = c("strata_1", "strata_2")))
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_proportion_diff_mf() respects custom settings", {
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
        afun = a_proportion_diff_mf,
        extra_args = list(
          variables = list(strata = c("strata_1", "strata_2")),
          val = "Y",
          conf_level = 0.90,
          mf_method = "cmh_mn",
          .stats = "diff_est_ci",
          .labels = c(diff_est_ci = "my_label"),
          .formats = list(diff_est_ci = "xx.xx (xx.xx - xx.xx)"),
          .indent_mods = c(diff_est_ci = 2L)
        )
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_proportion_diff_mf() respects custom exact_footnote", {
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
        afun = a_proportion_diff_mf,
        extra_args = list(
          variables = list(strata = c("strata_1", "strata_2")),
          exact_footnote = "This was the exact method"
        )
      ) |>
      build_table(data)
  )

  expect_snapshot(tbl)
})

test_that("a_proportion_diff_mf() errors when NAs are present", {
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
        afun = a_proportion_diff_mf,
        extra_args = list(variables = list(strata = "strata"))
      ) |>
      build_table(data),
    "missing"
  )
})

test_that("a_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE", {
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
        afun = a_proportion_diff_mf,
        extra_args = list(variables = list(strata = "strata"), na.rm = TRUE)
      ) |>
      build_table(data)
  )
})
