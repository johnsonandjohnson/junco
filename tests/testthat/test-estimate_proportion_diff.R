test_that("s_proportion_diff_j works as expected", {
  set.seed(3534, kind = "Mersenne-Twister")

  nex <- 100
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    stringsAsFactors = TRUE
  )

  result <- expect_silent(s_proportion_diff_j(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    conf_level = 0.90,
    method = "ha"
  ))
  expect_snapshot_value(result, tolerance = 1e-3, style = "deparse")

  # CMH with strata.
  result <- expect_silent(s_proportion_diff_j(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    variables = list(strata = c("f1", "f2")),
    conf_level = 0.90,
    method = "cmh"
  ))
  expect_snapshot_value(result, tolerance = 1e-3, style = "deparse")
})

test_that("a_proportion_diff_j works as expected in a table layout", {
  set.seed(3534, kind = "Mersenne-Twister")

  nex <- 100
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    stringsAsFactors = TRUE
  )

  l <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      vars = "rsp",
      afun = a_proportion_diff_j,
      show_labels = "hidden",
      na_str = default_na_str(),
      extra_args = list(
        conf_level = 0.9,
        method = "ha",
        ref_path = c("grp", "B"),
        .stats = "diff_est_ci"
      )
    )

  result <- expect_silent(build_table(l, df = dta))
  expect_snapshot(result)
})
