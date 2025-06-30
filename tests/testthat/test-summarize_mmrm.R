library(tern)
library(broom)

set.seed(123)
longdat <- data.frame(
  ID = rep(DM$ID, 5),
  AVAL = c(
    rep(0, nrow(formatters::DM)),
    rnorm(n = nrow(formatters::DM) * 4)
  ),
  VISIT = factor(rep(paste0("V", 0:4), each = nrow(formatters::DM)))
) |>
  dplyr::inner_join(DM, by = "ID")

testthat::test_that("h_summarize_mmrm works with healthy input", {
  result <- h_summarize_mmrm(
    .var = "AVAL",
    df_parent = longdat,
    variables = list(
      covariates = c("AGE", "SEX"),
      id = "ID",
      arm = "ARM",
      visit = "VISIT"
    ),
    ref_arm_level = "B: Placebo",
    ref_visit_levels = "V0",
    weights_emmeans = "proportional"
  )
  checkmate::expect_data_frame(result)
  expect_snapshot_value(result, style = "deparse", tolerance = 1e-3)
})

test_that("s_summarize_mmrm works with healthy input in non-ref cell", {
  df <- longdat |>
    dplyr::filter(STRATA1 == "A", VISIT == "V1", ARM == "A: Drug X")

  ref_levels <- list(
    VISIT = "V0",
    ARM = "B: Placebo"
  )
  root_df <- longdat
  strata_df <- root_df |> dplyr::filter(STRATA1 == "A")
  visit_df <- strata_df |> dplyr::filter(VISIT == "V1")

  .spl_context <- data.frame(
    split = c("root", "STRATA", "VISIT"),
    value = c("root", "A", "V1"),
    full_parent_df = I(list(root_df, strata_df, visit_df))
  )

  result <- s_summarize_mmrm(
    df = df,
    .var = "AVAL",
    variables = list(
      covariates = c("AGE", "SEX"),
      id = "ID",
      arm = "ARM",
      visit = "VISIT"
    ),
    ref_levels = ref_levels,
    .spl_context = .spl_context,
    cor_struct = "toeplitz",
    weights_emmeans = "proportional"
  )

  checkmate::expect_list(result)
  expect_snapshot_value(result, style = "deparse", tolerance = 1e-3)
})

test_that("s_summarize_mmrm works with healthy input in ref col cell", {
  df <- longdat |>
    dplyr::filter(STRATA1 == "A", VISIT == "V1", ARM == "B: Placebo")

  ref_levels <- list(
    VISIT = "V0",
    ARM = "B: Placebo"
  )
  root_df <- longdat
  strata_df <- root_df |> dplyr::filter(STRATA1 == "A")
  visit_df <- strata_df |> dplyr::filter(VISIT == "V1")

  .spl_context <- data.frame(
    split = c("root", "STRATA", "VISIT"),
    value = c("root", "A", "V1"),
    full_parent_df = I(list(root_df, strata_df, visit_df))
  )

  result <- s_summarize_mmrm(
    df = df,
    .var = "AVAL",
    variables = list(
      covariates = c("AGE", "SEX"),
      id = "ID",
      arm = "ARM",
      visit = "VISIT"
    ),
    ref_levels = ref_levels,
    .spl_context = .spl_context,
    cor_struct = "toeplitz",
    weights_emmeans = "proportional"
  )

  checkmate::expect_list(result)
  expect_snapshot_value(result, style = "serialize", tolerance = 1e-3)
})

test_that("s_summarize_mmrm works with healthy input in ref row cell", {
  df <- longdat |>
    dplyr::filter(STRATA1 == "A", VISIT == "V0", ARM == "C: Combination")

  ref_levels <- list(
    VISIT = "V0",
    ARM = "B: Placebo"
  )
  root_df <- longdat
  strata_df <- root_df |> dplyr::filter(STRATA1 == "A")
  visit_df <- strata_df |> dplyr::filter(VISIT == "V1")

  .spl_context <- data.frame(
    split = c("root", "STRATA", "VISIT"),
    value = c("root", "A", "V1"),
    full_parent_df = I(list(root_df, strata_df, visit_df))
  )

  result <- s_summarize_mmrm(
    df = df,
    .var = "AVAL",
    variables = list(
      covariates = c("AGE", "SEX"),
      id = "ID",
      arm = "ARM",
      visit = "VISIT"
    ),
    ref_levels = ref_levels,
    .spl_context = .spl_context,
    cor_struct = "toeplitz",
    weights_emmeans = "proportional"
  )

  checkmate::expect_list(result)
  expect_snapshot(result)
})

test_that("a_summarize_mmrm works as expected in table layout", {
  lyt <- basic_table() |>
    split_rows_by("VISIT") |>
    split_cols_by("ARM") |>
    analyze(
      vars = "AVAL",
      afun = a_summarize_mmrm,
      na_str = default_na_str(),
      show_labels = "hidden",
      extra_args = list(
        variables = list(
          covariates = c("AGE", "SEX"),
          id = "ID",
          arm = "ARM",
          visit = "VISIT"
        ),
        conf_level = 0.9,
        cor_struct = "toeplitz",
        ref_levels = list(VISIT = "V0", ARM = "B: Placebo"),
        .stats = c("adj_mean_est_ci", "diff_mean_est_ci", "p_value"),
        weights_emmeans = "proportional"
      )
    )
  tbl <- build_table(lyt, longdat)

  res <- expect_silent(tbl)
  expect_snapshot(res)

  # Check that we can prune this correctly.
  res2 <- prune_table(tbl, all_zero)
  expect_snapshot(res2)
})

test_that("a_summarize_mmrm works as expected below row splits", {
  lyt <- basic_table() |>
    split_rows_by("STRATA1", page_by = TRUE) |>
    split_rows_by("VISIT") |>
    split_cols_by("ARM") |>
    analyze(
      vars = "AVAL",
      afun = a_summarize_mmrm,
      na_str = default_na_str(),
      show_labels = "hidden",
      extra_args = list(
        variables = list(
          covariates = c("AGE", "SEX"),
          id = "ID",
          arm = "ARM",
          visit = "VISIT"
        ),
        conf_level = 0.9,
        cor_struct = "toeplitz",
        ref_levels = list(VISIT = "V0", ARM = "B: Placebo"),
        .stats = c("adj_mean_est_ci", "diff_mean_est_ci", "p_value"),
        weights_emmeans = "proportional"
      )
    )
  tbl <- build_table(lyt, longdat)

  res <- expect_silent(tbl)
  expect_snapshot(res)

  # Check that we can prune this correctly.
  res2 <- prune_table(tbl, all_zero)
  expect_snapshot(res2)
})
