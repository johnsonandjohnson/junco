test_that("a_summary_j_with_exclude returns tern summary when split is not excluded", {
  dta <- data.frame(
    visit = factor(rep(c("Baseline", "Week 1"), length.out = 8)),
    aval = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
  )

  spl_context <- data.frame(
    split = "visit",
    value = "Week 1"
  )

  res <- a_summary_j_with_exclude(
    df = dta[dta$visit == "Week 1", ],
    .var = "aval",
    exclude_levels = list(visit = "Baseline"),
    .spl_context = spl_context
  )
  exp <- tern::a_summary(dta[dta$visit == "Week 1", "aval"])

  expect_identical(res, exp)
})

test_that("a_summary_j_with_exclude returns NULL when split is excluded", {
  dta <- data.frame(
    visit = factor(rep(c("Baseline", "Week 1"), length.out = 8)),
    aval = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
  )

  spl_context <- data.frame(
    split = "visit",
    value = "Baseline"
  )

  res <- a_summary_j_with_exclude(
    df = dta[dta$visit == "Baseline", ],
    .var = "aval",
    exclude_levels = list(visit = "Baseline"),
    .spl_context = spl_context
  )

  expect_null(res)
})

test_that("a_summary_j_with_exclude allows row split levels to be excluded from a table layout", {
  dta <- data.frame(
    id = 1:100,
    visit = factor(rep(c("Baseline", "Week 1"), length.out = 100)),
    aval = seq_len(100),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )

  lyt <- basic_table() |>
    split_cols_by("grp") |>
    split_rows_by("visit") |>
    analyze(
      "aval",
      afun = a_summary_j_with_exclude,
      extra_args = list(
        exclude_levels = list(visit = "Baseline"),
        .stats = c("n", "mean")
      )
    )

  result <- build_table(lyt, dta) |>
    safe_prune_table(prune_func = tern::keep_rows(keep_non_null_rows))

  expect_snapshot(cran = TRUE, result)
})
