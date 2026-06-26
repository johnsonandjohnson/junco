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
  suppressMessages(result <- h_summarize_mmrm(
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
  ))
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

  suppressMessages(result <- s_summarize_mmrm(
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
  ))

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

  suppressMessages(result <- s_summarize_mmrm(
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
  ))

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

  suppressMessages(result <- s_summarize_mmrm(
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
  ))

  checkmate::expect_list(result)
  expect_snapshot(cran = TRUE, result)
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
  suppressMessages(tbl <- build_table(lyt, longdat))

  res <- expect_silent(tbl)
  expect_snapshot(cran = TRUE, res)

  # Check that we can prune this correctly.
  res2 <- prune_table(tbl, all_zero)
  expect_snapshot(cran = TRUE, res2)
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
  suppressMessages(tbl <- build_table(lyt, longdat))

  res <- expect_silent(tbl)
  expect_snapshot(cran = TRUE, res)

  # Check that we can prune this correctly.
  res2 <- prune_table(tbl, all_zero)
  expect_snapshot(cran = TRUE, res2)
})

test_that("a_summarize_mmrm_with_exclude handles excluded and included row split levels", {
  df <- data.frame(
    VISIT = factor(c("V0", "V1")),
    AVAL = c(0, 1)
  )
  excluded_context <- data.frame(
    split = "VISIT",
    value = "V0"
  )
  included_context <- data.frame(
    split = "VISIT",
    value = "V1"
  )

  expect_null(a_summarize_mmrm_with_exclude(
    df = df,
    .var = "AVAL",
    exclude_levels = list(VISIT = "V0"),
    .spl_context = excluded_context
  ))

  mock_a_summarize_mmrm <- function(df,
                                    .var,
                                    .spl_context,
                                    ...,
                                    .stats = NULL,
                                    .formats = NULL,
                                    .labels = NULL,
                                    .indent_mods = NULL) {
    list(
      df = df,
      .var = .var,
      .spl_context = .spl_context,
      .stats = .stats,
      .formats = .formats,
      .labels = .labels,
      .indent_mods = .indent_mods,
      dots = list(...)
    )
  }

  mockery::stub(
    a_summarize_mmrm_with_exclude,
    "a_summarize_mmrm",
    mock_a_summarize_mmrm
  )

  result <- a_summarize_mmrm_with_exclude(
    df = df,
    .var = "AVAL",
    exclude_levels = list(VISIT = "V0"),
    .spl_context = included_context,
    .stats = "adj_mean_est_ci",
    .formats = c(adj_mean_est_ci = "xx.xx"),
    .labels = c(adj_mean_est_ci = "Adjusted mean"),
    .indent_mods = c(adj_mean_est_ci = 1L),
    variables = list(visit = "VISIT")
  )

  expect_identical(result$df, df)
  expect_identical(result$.var, "AVAL")
  expect_identical(result$.spl_context, included_context)
  expect_identical(result$.stats, "adj_mean_est_ci")
  expect_identical(result$.formats, c(adj_mean_est_ci = "xx.xx"))
  expect_identical(result$.labels, c(adj_mean_est_ci = "Adjusted mean"))
  expect_identical(result$.indent_mods, c(adj_mean_est_ci = 1L))
  expect_identical(result$dots$variables, list(visit = "VISIT"))
})
