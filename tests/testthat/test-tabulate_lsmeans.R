test_that("s_lsmeans works as expected with MMRM fit when not in reference column", {
  mmrm_results <- fit_mmrm_j(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm::fev_data,
    cor_struct = "unstructured",
    weights_emmeans = "equal",
    averages_emmeans = list(
      "VIS1+2" = c("VIS1", "VIS2")
    )
  )
  df <- broom::tidy(mmrm_results)

  # Two-sided p-value.
  result <- s_lsmeans(df[8, ], FALSE)
  checkmate::expect_list(result, names = "unique")
  checkmate::expect_names(
    names(result),
    identical.to = c(
      "n",
      "adj_mean_se",
      "adj_mean_ci",
      "adj_mean_est_ci",
      "diff_mean_se",
      "diff_mean_ci",
      "diff_mean_est_ci",
      "change",
      "p_value"
    )
  )

  # Two-sided p-value.
  pval <- s_lsmeans(df[8, ], FALSE)$p_value
  expect_snapshot_value(pval, style = "deparse", tolerance = 1e-3)

  # One-sided p-values.
  pval_less <- s_lsmeans(df[8, ], FALSE, alternative = "less")$p_value
  expect_snapshot_value(pval_less, style = "deparse", tolerance = 1e-3)

  pval_greater <- s_lsmeans(df[8, ], FALSE, alternative = "greater")$p_value
  expect_snapshot_value(pval_greater, style = "deparse", tolerance = 1e-3)
})

test_that("summarize_lsmeans can show two- and one-sided p-values correctly", {
  mmrm_results <- fit_mmrm_j(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm::fev_data,
    cor_struct = "unstructured",
    weights_emmeans = "equal",
    averages_emmeans = list(
      "VIS1+2" = c("VIS1", "VIS2")
    )
  )
  df <- broom::tidy(mmrm_results)
  checkmate::expect_subset(
    c("p_value", "p_value_less", "p_value_greater"),
    names(df)
  )

  dat_adsl <- mmrm::fev_data %>%
    select(USUBJID, ARMCD) %>%
    unique()
  start_lyt <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT")

  lyt_two_sided <- start_lyt |>
    analyze(
      "ARMCD",
      afun = a_lsmeans,
      show_labels = "hidden",
      na_str = default_na_str(),
      extra_args = list(
        ref_path = c("ARMCD", mmrm_results$ref_level)
      )
    )
  result_two_sided <- build_table(
    lyt_two_sided,
    df = df,
    alt_counts_df = dat_adsl
  )
  expect_snapshot(result_two_sided)

  lyt_one_sided_less <- start_lyt |>
    analyze(
      "ARMCD",
      afun = a_lsmeans,
      show_labels = "hidden",
      na_str = default_na_str(),
      extra_args = list(
        ref_path = c("ARMCD", mmrm_results$ref_level),
        alternative = "less"
      )
    )
  result_one_sided_less <- build_table(
    lyt_one_sided_less,
    df = df,
    alt_counts_df = dat_adsl
  )
  expect_snapshot(result_one_sided_less)

  lyt_one_sided_greater <- start_lyt |>
    analyze(
      "ARMCD",
      afun = a_lsmeans,
      show_labels = "hidden",
      na_str = default_na_str(),
      extra_args = list(
        ref_path = c("ARMCD", mmrm_results$ref_level),
        alternative = "greater"
      )
    )
  result_one_sided_greater <- build_table(
    lyt_one_sided_greater,
    df = df,
    alt_counts_df = dat_adsl
  )
  expect_snapshot(result_one_sided_greater)
})

test_that("s_lsmeans works as expected with ANCOVA fit when not in reference column", {
  ancova_results <- fit_ancova(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      arm = "ARMCD",
      id = "USUBJID",
      visit = "AVISIT"
    ),
    data = mmrm::fev_data,
    weights_emmeans = "equal"
  )
  df <- broom::tidy(ancova_results)

  # Two-sided p-value.
  result <- s_lsmeans(df[2, ], FALSE)
  checkmate::expect_list(result, names = "unique")
  checkmate::expect_names(
    names(result),
    identical.to = c(
      "n",
      "adj_mean_se",
      "adj_mean_ci",
      "adj_mean_est_ci",
      "diff_mean_se",
      "diff_mean_ci",
      "diff_mean_est_ci",
      "change",
      "p_value"
    )
  )
})
