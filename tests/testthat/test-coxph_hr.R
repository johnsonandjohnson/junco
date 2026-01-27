library(tern)
library(survival)


test_that("s_coxph_hr works with default arguments and no stratification factors", {
  adtte_f <- tern_ex_adtte |>
    dplyr::filter(PARAMCD == "OS") |>
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f |> dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f |> dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_hr(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = NULL
  )
  res <- expect_silent(result)
  expect_snapshot(res)

  # Try one-sided alternatives.
  result2 <- s_coxph_hr(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = NULL,
    alternative = "less"
  )
  checkmate::assert_true(result2$hr < 1)
  expect_equal(result$pvalue / 2, result2$pvalue)

  result3 <- s_coxph_hr(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strata = NULL,
    alternative = "greater"
  )
  expect_equal(1 - result$pvalue / 2, result3$pvalue)
})

test_that("a_coxph_hr works with custom arguments and stratification factors", {
  adtte_f <- tern_ex_adtte |>
    dplyr::filter(PARAMCD == "OS") |>
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table(round_type = "sas") |>
    split_cols_by(var = "ARMCD") |>
    analyze(
      vars = "AVAL",
      afun = a_coxph_hr,
      na_str = default_na_str(),
      show_labels = "visible",
      var_labels = "Stratified Analysis",
      extra_args = list(
        is_event = "is_event",
        strata = "STRATA1",
        control = control_coxph(
          conf_level = 0.85,
          ties = "breslow",
          pval_method = "wald"
        ),
        ref_path = c("ARMCD", "ARM A"),
        .stats = c("hr_ci_3d", "pvalue")
      )
    ) |>
    build_table(df = adtte_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("a_coxph_hr works with stratification factors for Log-Rank test", {
  adtte_f <- tern_ex_adtte |>
    dplyr::filter(PARAMCD == "OS") |>
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table(round_type = "sas") |>
    split_cols_by(var = "ARMCD") |>
    analyze(
      vars = "AVAL",
      afun = a_coxph_hr,
      na_str = default_na_str(),
      show_labels = "visible",
      var_labels = "Stratified Analysis",
      extra_args = list(
        is_event = "is_event",
        strata = "STRATA1",
        control = control_coxph(
          conf_level = 0.85,
          ties = "breslow",
          pval_method = "log-rank"
        ),
        ref_path = c("ARMCD", "ARM A"),
        .stats = c("hr_ci_3d", "pvalue")
      )
    ) |>
    build_table(df = adtte_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})
