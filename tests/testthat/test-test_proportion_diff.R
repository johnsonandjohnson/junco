test_that("a_test_proportion_diff works as expected in table layout", {
  set.seed(123, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  l <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      vars = "rsp",
      afun = a_test_proportion_diff,
      show_labels = "hidden",
      extra_args = list(
        method = "cmh",
        variables = list(strata = "strata"),
        ref_path = c("grp", "B")
      )
    )

  result <- expect_silent(build_table(l, df = dta))
  expect_snapshot(result)
})
