get_estimates_ttest <- function(res) {
  res_ttest <- list()
  res_ttest[["estimate"]] <- unname(res[["estimate"]][1] - res[["estimate"]][2])
  res_ttest[["conf.int"]] <- res[["conf.int"]]
  res_ttest[["lsmean_diffci"]] <- c(res_ttest[["estimate"]], res_ttest[["conf.int"]])

  res_ttest
}

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
  result <- basic_table(round_type = "sas") |>
    split_cols_by("Species") |>
    add_colcounts() |>
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
    ) |>
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
    ) |>
    build_table(iris)
  expect_snapshot(result)
})

test_that("s_ancova_j works as expected in combined column", {
  df <- iris |> filter(Species %in% c("virginica", "versicolor"))
  .df_row <- iris
  .var <- "Petal.Length"
  variables <- list(arm = "Species", covariates = NULL)
  .ref_group <- iris |> filter(Species == "setosa")
  conf_level <- 0.95
  result <- s_ancova_j(
    df,
    .var = .var,
    .df_row = .df_row,
    variables = variables,
    .ref_group = .ref_group,
    .in_ref_col = FALSE,
    conf_level = conf_level
  )

  mod_ttest <- t.test(
    y = .ref_group[[.var]],
    x = df[[.var]],
    conf.level = conf_level,
    var.equal = TRUE
  )

  res_ttest <- list()
  res_ttest[["estimate"]] <- unname(mod_ttest[["estimate"]][1] - mod_ttest[["estimate"]][2])
  res_ttest[["conf.int"]] <- mod_ttest[["conf.int"]]
  res_ttest[["lsmean_diffci"]] <- c(res_ttest[["estimate"]], res_ttest[["conf.int"]])

  expect_equal(res_ttest[["lsmean_diffci"]], result[["lsmean_diffci"]], ignore_attr = TRUE)
})

test_that("s_ancova_j works as expected in combined column test 2", {
  inputdf <- mtcars
  inputdf[["carb"]] <- factor(inputdf[["carb"]], levels = c("1", "2", "3", "4", "6", "8"))

  df <- inputdf |> filter(carb %in% c("1", "2", "3"))
  .df_row <- inputdf
  .var <- "disp"
  variables <- list(arm = "carb", covariates = NULL)
  .ref_group <- inputdf |> filter(carb == "4")
  conf_level <- 0.95
  result <- s_ancova_j(
    df,
    .var = .var,
    .df_row = .df_row,
    variables = variables,
    .ref_group = .ref_group,
    .in_ref_col = FALSE,
    conf_level = conf_level
  )

  mod_ttest <- t.test(
    y = .ref_group[[.var]],
    x = df[[.var]],
    conf.level = conf_level,
    var.equal = TRUE
  )

  res_ttest <- list()
  res_ttest[["estimate"]] <- unname(mod_ttest[["estimate"]][1] - mod_ttest[["estimate"]][2])
  res_ttest[["conf.int"]] <- mod_ttest[["conf.int"]]
  res_ttest[["lsmean_diffci"]] <- c(res_ttest[["estimate"]], res_ttest[["conf.int"]])

  expect_equal(res_ttest[["lsmean_diffci"]], result[["lsmean_diffci"]], ignore_attr = TRUE)
})

test_that("s_summarize_ancova works as expected in combined column test 3", {
  inputdf <- mtcars
  inputdf[["carb"]] <- factor(
    inputdf[["carb"]],
    levels = c("1", "2", "3", "4", "6", "8"),
    labels = paste0("Carb ", c("1", "2", "3", "4", "6", "8"))
  )

  .var <- "disp"
  inputdf[[.var]][inputdf[["carb"]] == "Carb 1"] <- NA_real_

  .df_row <- inputdf
  df <- inputdf |> filter(carb %in% c("Carb 1", "Carb 2"))
  .ref_group <- inputdf |> filter(carb == "Carb 4")

  variables <- list(arm = "carb", covariates = NULL)

  conf_level <- 0.95

  # current version of s_ancova_j does not identify this case as being in a combined group
  # so model is not the same as 2-sample t-test any more
  # resolved by argument in_combo
  result <- s_ancova_j(
    df,
    .var = .var,
    .df_row = .df_row,
    variables = variables,
    .ref_group = .ref_group,
    .in_ref_col = FALSE,
    conf_level = conf_level,
    in_combo = TRUE
  )

  mod_ttest <- t.test(
    y = .ref_group[[.var]],
    x = df[[.var]],
    conf.level = conf_level,
    var.equal = TRUE
  )

  res_ttest <- get_estimates_ttest(mod_ttest)

  # this is what we expected - update to s_ancova_j
  expect_equal(res_ttest[["lsmean_diffci"]], result[["lsmean_diffci"]], ignore_attr = TRUE)
})

test_that("s_summarize_ancova works as expected in combined column test 4", {
  # construct a combined column that isn't a true combination
  # one of the levels has no actual observed data, here "Carb 1" has no data for "disp"
  inputdf <- mtcars
  inputdf[["carb"]] <- factor(
    inputdf[["carb"]],
    levels = c("1", "2", "3", "4", "6", "8"),
    labels = paste0("Carb ", c("1", "2", "3", "4", "6", "8"))
  )

  .var <- "disp"
  inputdf[[.var]][inputdf[["carb"]] == "Carb 1"] <- NA_real_

  variables <- list(arm = "carb", covariates = NULL)

  conf_level <- 0.9

  # add a combined group in the layout - Carb 1 + 2, no real combo as Carb 1 has no contribution
  reorder_facets <- function(splret,
                             spl,
                             fulldf,
                             myorder = c("Carb 1", "Carb 2", "Carb 1 + 2", "Carb 3", "Carb 4", "Carb 6", "Carb 8"),
                             ...) {
    ord <- match(myorder, names(splret$values))

    make_split_result(
      splret$values[ord],
      splret$datasplit[ord],
      splret$labels[ord]
    )
  }

  mycombo <- make_split_fun(
    post = list(
      add_combo_facet(
        name = "Carb 1 + 2",
        levels = c("Carb 1", "Carb 2"),
        extra = list()
      ),
      reorder_facets
    )
  )

  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("carb", split_fun = mycombo) |>
    analyze(
      vars = "disp",
      afun = a_summarize_ancova_j,
      extra_args = list(
        variables = variables,
        ref_path = c("carb", "Carb 4"),
        conf_level = conf_level,
        .stats = c(
          "n",
          "mean_sd",
          "mean_ci_3d",
          "lsmean_ci",
          "lsmean_diffci"
        ),
        .formats = c(mean_ci_3d = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"))
      )
    )

  tbl <- build_table(lyt, inputdf)
  subtbl <- tbl[c("root", "disp"), c(1:3, 5)]

  # confirm lsmean_ci from combined group are from reduced dataset (combo specifiations)
  cellvals <- cell_values(subtbl)
  cellvals_carb12_comb_diff <- cellvals[["lsmean_diffci"]]$`Carb 1 + 2`
  cellvals_carb12_comb <- cellvals[["lsmean_ci"]]$`Carb 1 + 2`

  # start checking using underlying s-functions
  .ref_group <- inputdf |>
    filter(carb == "Carb 4")
  df <- inputdf |>
    filter(carb %in% c("Carb 1", "Carb 2"))

  # for current model specs, reduced combo spec for difference between ref is same as 2-sample t-test
  mod_ttest <- t.test(
    y = .ref_group[[.var]],
    x = df[[.var]],
    conf.level = 0.9,
    var.equal = TRUE
  )

  res_ttest <- get_estimates_ttest(mod_ttest)

  expect_equal(res_ttest[["lsmean_diffci"]], cellvals_carb12_comb_diff, ignore_attr = TRUE)

  # estimates of mean for combo group: come from model with 2 levels : the ref Carb 4 and the comp Carb 2
  # prep for model inputs according to specs
  .ref_group <- inputdf |>
    filter(carb == "Carb 4") |>
    filter(!is.na(disp))
  df <- inputdf |>
    filter(carb %in% c("Carb 1", "Carb 2")) |>
    filter(!is.na(disp))

  .df_row <- rbind(df, .ref_group)
  .df_row[["carb"]] <- droplevels(.df_row[["carb"]])
  df[["carb"]] <- factor(
    as.character(df[["carb"]]),
    levels = levels(.df_row[["carb"]])
  )
  .ref_group[["carb"]] <- factor(
    as.character(.ref_group[["carb"]]),
    levels = levels(.df_row[["carb"]])
  )

  # confirm that we have 2 levels for carb
  expect_equal(levels(.df_row[["carb"]]), c("Carb 2", "Carb 4"))

  # apply s_ancova_j on these inputs,
  mod_result <- s_ancova_j(
    df = df,
    .var = .var,
    .df_row = .df_row,
    .ref_group = .ref_group,
    .in_ref_col = FALSE,
    variables = variables,
    conf_level = conf_level
  )[["lsmean_ci"]]

  # confirm expected results
  expect_equal(mod_result, cellvals_carb12_comb, ignore_attr = TRUE)
})
