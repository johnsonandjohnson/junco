test_that("h_ancova works as expected", {
  set.seed(123)
  df_row <- iris |>
    mutate(
      Color = factor(sample(
        c("red", "blue"),
        size = nrow(iris),
        prob = c(0.8, 0.2),
        replace = TRUE
      ))
    )
  df <- df_row |>
    filter(Species == "virginica")
  variables <- list(
    arm = "Species",
    covariates = c("Sepal.Length * Sepal.Width", "Color")
  )
  ref_group <- df_row |>
    filter(Species == "setosa")

  result <- expect_silent(h_ancova(
    .var = "Petal.Length",
    .df_row = df_row,
    variables = variables,
    weights_emmeans = "equal"
  ))
  checkmate::expect_class(result, "emmGrid")
})

test_that("s_ancova_j works as expected", {
  set.seed(123)
  df_row <- iris |>
    mutate(
      Color = factor(sample(
        c("red", "blue"),
        size = nrow(iris),
        prob = c(0.8, 0.2),
        replace = TRUE
      ))
    )
  df <- df_row |>
    filter(Species == "virginica")
  variables <- list(
    arm = "Species",
    covariates = c("Sepal.Length * Sepal.Width", "Color")
  )
  ref_group <- df_row |>
    filter(Species == "setosa")

  result <- s_ancova_j(
    df = df,
    .var = "Petal.Length",
    .df_row = df_row,
    variables = variables,
    .ref_group = ref_group,
    .in_ref_col = FALSE,
    conf_level = 0.95,
    weights_emmeans = "proportional"
  )
  checkmate::expect_list(result)
  checkmate::expect_names(
    names(result),
    identical.to = c(
      "n",
      "lsmean",
      "lsmean_se",
      "lsmean_ci",
      "lsmean_diff",
      "lsmean_diff_ci",
      "lsmean_diffci",
      "pval"
    )
  )

  # We can change the emmeans weights.
  result2 <- s_ancova_j(
    df = df,
    .var = "Petal.Length",
    .df_row = df_row,
    variables = variables,
    .ref_group = ref_group,
    .in_ref_col = FALSE,
    conf_level = 0.95,
    weights_emmeans = "equal"
  )
  expect_false(result$lsmean == result2$lsmean)

  # try with n_obs_trt_lvls < 2
  df_row <- df_row |>
    filter(Color == "red")
  df <- df_row |>
    filter(Species == "virginica")
  variables <- list(
    arm = "Color",
    covariates = c("Sepal.Length * Sepal.Width", "Color")
  )
  ref_group <- df_row |>
    filter(Species == "setosa")

  result <- s_ancova_j(
    df = df,
    .var = "Petal.Length",
    .df_row = df_row,
    variables = variables,
    .ref_group = ref_group,
    .in_ref_col = FALSE,
    conf_level = 0.95,
    weights_emmeans = "proportional"
  )
  checkmate::expect_list(result)
  checkmate::expect_names(
    names(result),
    identical.to = c(
      "n",
      "lsmean",
      "lsmean_se",
      "lsmean_ci",
      "lsmean_diff",
      "lsmean_diff_ci",
      "lsmean_diffci",
      "pval"
    )
  )
})

test_that("s_summarize_ancova works as expected", {
  df <- iris |> filter(Species == "virginica")
  .df_row <- iris
  .var <- "Petal.Length"
  variables <- list(arm = "Species", covariates = "Sepal.Length * Sepal.Width")
  .ref_group <- iris |> filter(Species == "setosa")
  conf_level <- 0.95
  result <- s_summarize_ancova_j(
    df,
    .var = .var,
    .df_row = .df_row,
    variables = variables,
    .ref_group = .ref_group,
    .in_ref_col = FALSE,
    conf_level = conf_level
  )
  expect_snapshot_value(result, style = "deparse", tolerance = 1e-3)
})

test_that("a_summarize_ancova_j  works as expected in table layout", {
  result <- basic_table() %>%
    split_cols_by("Species") %>%
    add_colcounts() %>%
    analyze(
      vars = "Petal.Length",
      afun = a_summarize_ancova_j,
      show_labels = "visible",
      na_str = default_na_str(),
      table_names = "unadj",
      var_labels = "Unadjusted comparison",
      extra_args = list(
        variables = list(arm = "Species", covariates = NULL),
        conf_level = 0.95,
        ref_path = c("Species", "setosa"),
        .stats = c(
          "n",
          "mean_sd",
          "median",
          "range",
          "quantiles",
          "lsmean_diffci",
          "pval"
        )
      )
    ) %>%
    analyze(
      vars = "Petal.Length",
      afun = a_summarize_ancova_j,
      show_labels = "visible",
      na_str = default_na_str(),
      table_names = "adj",
      var_labels = "Adjusted comparison (covariates: Sepal.Length and Sepal.Width)",
      extra_args = list(
        variables = list(
          arm = "Species",
          covariates = c("Sepal.Length", "Sepal.Width")
        ),
        conf_level = 0.95,
        ref_path = c("Species", "setosa"),
        .stats = c(
          "lsmean_diffci",
          "pval"
        )
      )
    ) %>%
    build_table(iris, round_type = "sas")
  expect_snapshot(result)
})
