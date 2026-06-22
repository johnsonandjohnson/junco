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
  expect_snapshot(cran = TRUE, result)
})

test_that("a_proportion_diff_j works as expected in a table layout with uncond_exact_diff method", {
  dta <- data.frame(
    rsp = c(rep(TRUE, 5), rep(FALSE, 12), rep(TRUE, 40), rep(FALSE, 38)),
    grp = c(rep("B", 17), rep("A", 78)),
    stringsAsFactors = FALSE
  )

  result <- s_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    conf_level = 0.95,
    method = "uncond_exact_diff"
  )

  expect_equal(as.numeric(result$diff), 21.87, tolerance = 1e-2)
  expect_equal(as.numeric(result$diff_ci), c(-4.66, 46.76), tolerance = 1e-2)
  expect_identical(attr(result$diff_ci, "label"), "95% CI (Unconditional exact)")

  l <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      vars = "rsp",
      afun = a_proportion_diff_j,
      show_labels = "hidden",
      na_str = default_na_str(),
      extra_args = list(
        conf_level = 0.95,
        method = "uncond_exact_diff",
        ref_path = c("grp", "B"),
        .stats = "diff_est_ci",
        .formats = c("diff_est_ci" = "xx.xx (xx.xx - xx.xx)")
      )
    )

  result <- expect_silent(build_table(l, df = dta))
  fv <- format_value(cell_values(result[1, 2])[[1]], format = "xx.xx (xx.xx - xx.xx)")

  expect_equal(fv, c("21.87 (-4.66 - 46.76)"))
})
