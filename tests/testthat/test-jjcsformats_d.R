junco_def_d <- c(
  "mean" = "xx.dxxx",
  "mean_sd" = "xx.dx (xx.dxx)",
  "mean_se" = "xx.dx (xx.dxx)",
  "range" = "xx.d - xx.d"
)


test_that("fmt_spec_single_d: fmt_d_def", {
  myfmts <- fmt_spec_single_d(
    d = 1,
    stats_in = c("mean", "mean_se"),
    fmt_d_def = junco_def_d,
    fmt_d_in = NULL
  )

  fmt_details <- get_fmt_details(myfmts, as_tibble = FALSE)
  expect_identical(fmt_details[["mean_se"]][["str"]], "xx.xx (xx.xxx)")

  expect_identical(
    format_value(c(1.2345, 4.5678), format = myfmts[["mean_se"]]),
    "1.23 (4.568)"
  )
})

test_that("fmt_spec_single_d: argument fmt_d_in has higher priority than fmt_d_def", {
  myfmts <- fmt_spec_single_d(
    d = 1,
    stats_in = c("mean", "mean_se"),
    fmt_d_def = junco_def_d,
    fmt_d_in = c("mean_se" = "xx.dx (xx.dxxx)")
  )

  fmt_details <- get_fmt_details(myfmts, as_tibble = FALSE)
  expect_identical(fmt_details[["mean_se"]][["str"]], "xx.xx (xx.xxxx)")

  expect_identical(
    format_value(c(1.2345, 4.5678), format = myfmts[["mean_se"]]),
    "1.23 (4.5678)"
  )
})


test_that("fmt_spec_var_d: apply different d for different variables", {
  var <- c("AGE" = 0, "BMI" = 3, "BMRKR1" = 2)
  var_fmt <- fmt_spec_var_d(var,
    stats_in = NULL,
    fmt_d_def = junco_def_d,
    fmt_d_in = NULL
  )

  fmt_details <- get_fmt_details(var_fmt, recursive = TRUE, as_tibble = FALSE)
  expect_identical(fmt_details[["BMRKR1"]][["mean_se"]][["str"]], "xx.xxx (xx.xxxx)")
  expect_identical(fmt_details[["BMI"]][["mean_se"]][["str"]], "xx.xxxx (xx.xxxxx)")
  expect_identical(fmt_details[["AGE"]][["range"]][["str"]], "xx. - xx.")
})


test_that("apply varying d approach using a column spec on input", {
  df_d <- tribble(
    ~PARAMCD, ~d,
    "DIABP", 2L,
    "PULSE", 3L,
    "RESP", 1L
  )

  yy <- fmt_spec_df_d(df_d,
    d_column = "d",
    fmt_column = "fmt_d",
    stats_in = NULL,
    fmt_d_def = junco_def_d,
    fmt_d_in = NULL
  )


  df <- ex_advs |>
    dplyr::filter(PARAMCD %in% c("DIABP", "PULSE", "RESP")) |>
    dplyr::filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))

  df2 <- df |>
    dplyr::left_join(yy)

  lyt <- basic_table() |>
    split_cols_by("ARMCD") |>
    split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
    split_rows_by("AVISIT", split_fun = drop_split_levels) |>
    analyze(
      vars = "AVAL",
      afun = a_summary,
      extra_args = list(
        .stats = c("n", "mean_se", "range"),
        .formats = "default"
      ),
      formats_var = "fmt_d"
    )

  rslt <- build_table(lyt, df2, alt_counts_df = ex_adsl)

  expect_snapshot(rslt, cran = TRUE)
})


test_that("apply varying d approach using a named var spec", {
  df <- ex_adsl
  df$EOSDY1 <- df$EOSDY / 7

  df <- df |>
    dplyr::relocate(ARMCD, AGE, BMRKR1, EOSDY1, EOSDY)

  # note if a variable is not included the corresponding rslt is blank
  vars_d <- c("AGE" = 0, "BMRKR1" = 2, "BMI" = 3, "EOSDY1" = 4)

  vars_fmt <- fmt_spec_var_d(vars_d,
    stats_in = NULL,
    fmt_d_def = junco_def_d,
    fmt_d_in = NULL
  )

  lyt <- basic_table() |>
    split_cols_by("ARMCD") |>
    analyze(
      vars = c("AGE", "BMRKR1"),
      afun = a_summary,
      extra_args = list(
        .stats = c("n", "mean_se", "range"),
        .formats = "default"
      ),
      format = vars_fmt,
      section_div = " "
    )

  rslt <- build_table(lyt, df)
  expect_snapshot(rslt, cran = TRUE)

  # blank rslt as EOSDY does not have a format spec
  lyt <- basic_table() |>
    split_cols_by("ARMCD") |>
    analyze(
      vars = c("AGE", "BMRKR1", "EOSDY"),
      afun = a_summary,
      extra_args = list(
        .stats = c("n", "mean_se", "range"),
        .formats = "default"
      ),
      format = vars_fmt,
      section_div = " "
    )

  rslt <- build_table(lyt, df)
  expect_snapshot(rslt, cran = TRUE)
})

test_that("investigation of junco_def_d_all", {
  test1_target <- junco_default_formats
  # junco_def_d_all has been constructed with d = 1 as reference
  # compare against junco_default_formats
  test1_d <- fmt_spec_single_d(
    d = 1,
    stats_in = NULL,
    fmt_d_def = junco_def_d_all,
    fmt_d_in = NULL
  )

  check1 <- compare_fmt_specs(test1_d, test1_target)
  expect_identical(check1$diff_stats, "lr_stat_df")

  # junco_def_d_all has been constructed with d = 1 as reference
  # now compare against a version using other d
  # only the formats for non-d-style stats should be different
  test2_d <- fmt_spec_single_d(
    d = 0,
    stats_in = NULL,
    fmt_d_def = junco_def_d_all,
    fmt_d_in = NULL
  )


  d_style <- target_d_style[target_d_style$target_d, ]
  expected_stats <- d_style[["stat"]]
  check2 <- compare_fmt_specs(test2_d, test1_target)
  expect_identical(sort(check2$diff_stats), sort(c(expected_stats, "lr_stat_df")))
})
