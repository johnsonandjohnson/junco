set.seed(123)
library(dplyr)

iris_plus <- iris |>
  mutate(
    Color = factor(sample(
      c("red", "blue"),
      size = nrow(iris),
      prob = c(0.8, 0.2),
      replace = TRUE
    ))
  )

iris_plus2 <- iris_plus |>
  group_by(Species) |>
  mutate(id = row_number()) |>
  filter(Species == "setosa" & id < 45 |
           Species == "versicolor" & id < 30 |
           Species == "virginica" & id < 50) |>
  ungroup()

get_numbers <- function(tbl, col, rows = c(3)) {
  numbers <- unname(unlist(cell_values(tbl[rows, col])))
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
      "n_fit",
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
      "n_fit",
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
  expect_snapshot(cran = TRUE, result)
})

## tests for ancova in combined columns start here -----

test_that("tern function summarize_ancova cannot deal with a combined column", {
  model_variables <- list(arm = "Species", covariates = c("Color"))
  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    "setosa_virg", "Combined: setosa + virginica", c("setosa", "virginica"), list()
  )

  lyt_1 <- basic_table() |>
    split_cols_by("Species", ref_group = "versicolor",
                  split_fun = add_combo_levels(combodf)) |>
    add_colcounts() |>
    summarize_ancova(
      vars = "Sepal.Length",
      variables = model_variables,
      conf_level = 0.95,
      interaction_item = NULL,
      interaction_y = FALSE,
      weights_emmeans = "proportional",
      var_labels = "Adjusted comparison (covariates Color)",
      table_names = "adjusted"
    )

  expect_error(
    result <- build_table(lyt_1, iris_plus2),
    "Assertion on 'sum_level' failed: Must have length 1."
  )
})

tbl_ancova_j <- function(weights_emmeans = "proportional",
                         weights_combo = "equal",
                         inputdf = iris_plus2,
                         interaction = TRUE,
                         method_combo = "contrasts") {
  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    "setosa_virg", "Combined: setosa + virginica", c("setosa", "virginica"), list()
  )

  if (!interaction) {
    model_variables <- list(arm = "Species", covariates = c("Color"))

    lyt <- basic_table() |>
      split_cols_by("Species", split_fun = add_combo_levels(combodf)) |>
      add_colcounts() |>
      analyze(
        vars = "Sepal.Length",
        afun = a_summarize_ancova_j,
        extra_args = list(
          variables = model_variables,
          conf_level = 0.95,
          interaction_item = NULL,
          interaction_y = FALSE,
          weights_emmeans = weights_emmeans,
          weights_combo = weights_combo,
          method_combo = method_combo,
          ref_path = c("Species", "versicolor"),
          .stats = c("n_fit", "lsmean_ci", "lsmean_diffci")
        ),
        var_labels = "Adjusted comparison (covariates Color)",
        table_names = "adjusted",
        show_labels = "visible"
      )
  } else {
    model_variables <- list(arm = "Species", covariates = c("Color", "Species * Color"))

    lyt <- basic_table() |>
      split_cols_by("Species", split_fun = add_combo_levels(combodf)) |>
      add_colcounts() |>
      analyze(
        vars = "Sepal.Length",
        afun = a_summarize_ancova_j,
        extra_args = list(
          variables = model_variables,
          conf_level = 0.95,
          interaction_item = "Color",
          interaction_y = "red",
          weights_emmeans = weights_emmeans,
          weights_combo = weights_combo,
          method_combo = method_combo,
          ref_path = c("Species", "versicolor"),
          .stats = c("n_fit", "lsmean_ci", "lsmean_diffci")
        ),
        var_labels = "Adjusted comparison (covariates Color - red)",
        table_names = "adjusted"
      ) |>
      analyze(
        vars = "Sepal.Length",
        afun = a_summarize_ancova_j,
        extra_args = list(
          variables = model_variables,
          conf_level = 0.95,
          interaction_item = "Color",
          interaction_y = "blue",
          weights_emmeans = weights_emmeans,
          weights_combo = weights_combo,
          method_combo = method_combo,
          ref_path = c("Species", "versicolor"),
          .stats = c("n_fit", "lsmean_ci", "lsmean_diffci")
        ),
        var_labels = "Adjusted comparison (covariates Color - blue)",
        table_names = "adjusted2"
      )
  }
  result <- build_table(lyt, inputdf)
  result
}

#' actual tests starts here

test_that("a_summarize_ancova_j (s_ancova_j) as expected in combined column for model without interaction", {
  # NOTE : tests in current snippet utilize snapshot approach
  # a non snapshot approach to verify results from combined column can be found in
  # file dev/tests/detailed-summarize_ancova_combined.R
  # testthat::test_file("dev/tests/detailed-summarize_ancova_combined.R") #nolint

  model_variables <- list(arm = "Species", covariates = c("Color"))
  lm_fit <- stats::lm(formula = Sepal.Length ~ Species + Color, data = iris_plus2)

  # table from junco afun a_summarize_ancova_j in layout with combined column
  # - equal weights when averaging over Color covariate
  # - equal weights when deriving estimates for Combined: setosa + virginica column
  result <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = FALSE
  )
  expect_snapshot(cran = TRUE, result)

  # table from junco afun a_summarize_ancova_j in layout with combined column
  # - equal weights when averaging over Color covariate
  # - proportional weights when deriving estimates for Combined: setosa + virginica column
  result_b <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional",
    inputdf = iris_plus2,
    interaction = FALSE
  )
  expect_snapshot(cran = TRUE, result_b)

  # - equal weights when averaging over Color covariate
  # - proportional_marginal weights when deriving estimates for Combined: setosa + virginica column
  # proportional and proportional_marginal is the same when no interaction
  result_c <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional_marginal",
    inputdf = iris_plus2,
    interaction = FALSE
  )
  # no need to snapshot - identical
  expect_equal(result_b, result_c)

  # - proportional weights when averaging over Color covariate
  # - proportional_marginal weights when deriving estimates for Combined: setosa + virginica column
  # proportional and proportional_marginal is the same when no interaction

  result_d <- tbl_ancova_j(
    weights_emmeans = "proportional",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = FALSE
  )
  expect_snapshot(cran = TRUE, result_d)

  # confirmation that proportional and proportional_marginal is the same when no interaction

  expect_any_difference(result, result_b)
  expect_any_difference(result, result_d)
})

test_that("a_summarize_ancova_j (s_ancova_j) with a combined column but no method for weights_combo stops", {
  model_variables <- list(arm = "Species", covariates = c("Color"))

  expect_error(
    result <- tbl_ancova_j(
      weights_emmeans = "equal",
      weights_combo = NULL,
      inputdf = iris_plus2,
      interaction = FALSE
    ),
    "In a combined column specifications for weights_combo must be non-NULL."
  )

  expect_error(
    result <- tbl_ancova_j(
      weights_emmeans = "equal",
      weights_combo = "invalid",
      inputdf = iris_plus2,
      interaction = FALSE
    ),
    "'arg' should be one of \"equal\", \"proportional\", \"proportional_marginal\""
  )
})

test_that("a_summarize_ancova_j (s_ancova_j) with a combined column and method_combo = collapse", {
  model_variables <- list(arm = "Species", covariates = c("Color"))

  result <- tbl_ancova_j(
    weights_emmeans = "equal",
    inputdf = iris_plus2,
    interaction = FALSE,
    method_combo = "collapse"
  )
  resultx <- tbl_ancova_j(
    weights_emmeans = "equal",
    inputdf = iris_plus2,
    interaction = FALSE,
    method_combo = "contrasts",
    weights_combo = "equal"
  )
  # result and resultx are identical excluding combined column
  expect_equal(result[, c(1, 2, 3)], resultx[, c(1, 2, 3)])
  # the combined column we expect differences
  expect_any_difference(result[, c(4)], resultx[, c(4)])

  # use summarize_ancova on data where combined column is level of the input data
  iris_plus2_fix <- iris_plus2
  iris_plus2_fix[["Species"]] <- factor(as.character(iris_plus2_fix[["Species"]]),
                                        levels = c("setosa", "versicolor", "virginica"),
                                        labels = c("Combined: setosa + virginica", "versicolor",
                                                   "Combined: setosa + virginica"))

  weights_emmeans <- "equal"
  result2 <- basic_table() |>
    split_cols_by("Species", ref_group = "versicolor") |>
    add_colcounts() |>
    summarize_ancova(
      vars = "Sepal.Length",
      variables = model_variables,
      conf_level = 0.95,
      interaction_item = NULL,
      interaction_y = FALSE,
      weights_emmeans = weights_emmeans,
      var_labels = "Adjusted comparison (covariates Color)",
      table_names = "adjusted"
    ) |>
    build_table(iris_plus2_fix)

  # combined column estimates from result and result2
  numbers_1 <- get_numbers(result, col = 4, rows = c(3, 4))[c(1, 4, 5, 6)]
  numbers_2 <- get_numbers(result2, col = 1, rows = c(3, 4, 5))

  expect_equal(numbers_1, numbers_2)
})

test_that("a_summarize_ancova_j combined column and interaction, diff versions for weights_combo", {
  # nolint start
  # longer description lintr
  # a_summarize_ancova_j (s_ancova_j) works as expected in combined column for model with interaction 3 sets of weights_combo"
  # nolint end

  # NOTE : tests in current snippet utilize snapshot approach
  # a non snapshot approach to verify results from combined column can be found in
  # file dev/tests/detailed-summarize_ancova_combined.R
  # testthat::test_file("dev/tests/detailed-summarize_ancova_combined.R") #nolint

  model_variables <- list(arm = "Species", covariates = c("Color", "Species * Color"))
  lm_fit <- stats::lm(formula = Sepal.Length ~ Species + Color + Species * Color, data = iris_plus2)

  # results with combo column
  result <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = TRUE
  )
  expect_snapshot(cran = TRUE, result)

  result_b <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional",
    inputdf = iris_plus2,
    interaction = TRUE
  )
  expect_snapshot(cran = TRUE, result_b)

  result_c <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional_marginal",
    inputdf = iris_plus2,
    interaction = TRUE
  )
  expect_snapshot(cran = TRUE, result_c)

  result_d <- tbl_ancova_j(
    weights_emmeans = "proportional",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  ### weights_combo have impact on combined column
  ### weights_emmeans have no impact on combined column (as weights_combo has been decoupled from weights_emmeans)
  # hence no snapshot test is needed for this version
  expect_equal(result, result_d)

  expect_equal(result[, c(1:3)], result_b[, c(1:3)])
  expect_equal(result[, c(1:3)], result_c[, c(1:3)])

  expect_any_difference(result[, c(4)], result_b[, c(4)])
  expect_any_difference(result[, c(4)], result_c[, c(4)])
})

test_that("a_summarize_ancova_j with sparse data", {

  iris_sparse <- iris_plus2 |>
    filter(Species != "versicolor")

  result <- basic_table() |>
    split_cols_by("Species") |>
    add_colcounts() |>
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
          "lsmean_ci",
          "lsmean_diffci",
          "pval"
        )
      )
    ) |>
    build_table(iris_sparse)

  expect_snapshot(cran = TRUE, result)
})

test_that("a_summarize_ancova_j with no data", {

  iris_sparse <- iris_plus2 |>
    filter(Species == "dumb")

  result <- basic_table() |>
    split_cols_by("Species") |>
    add_colcounts() |>
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
          "lsmean_ci",
          "lsmean_diffci",
          "pval"
        )
      )
    ) |>
    build_table(iris_sparse)

  expect_snapshot(cran = TRUE, result)
})

test_that("a_summarize_ancova_j with no data in reference group", {

  iris_sparse <- iris_plus2 |>
    filter(Species != "setosa")

  result <- basic_table() |>
    split_cols_by("Species") |>
    add_colcounts() |>
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
          "lsmean_ci",
          "lsmean_diffci",
          "pval"
        )
      )
    ) |>
    build_table(iris_sparse)

  expect_snapshot(cran = TRUE, result)
})

test_that("a_summarize_ancova_j with multiple combined columns", {
  # NOTE : test in current snippet utilize snapshot approach
  # a non snapshot approach to verify results from combined column can be found in
  # file dev/tests/detailed-summarize_ancova_combined.R
  # testthat::test_file("dev/tests/detailed-summarize_ancova_combined.R") #nolint

  adsl_jnj <- pharmaverseadamjnj::adsl
  advs_jnj <- pharmaverseadamjnj::advs

  fix_usubjid <- function(adsl) {
    rws <- which(adsl$TRT01A == "Xanomeline Medium Dose")

    usubj_char <- as.character(adsl$USUBJID)
    subjid <- as.integer(as.character(adsl$SUBJID))
    subjid[rws] <- subjid[rws] + 1000
    substr(usubj_char, 8, 11) <- as.character(subjid)
    adsl$USUBJID <- factor(usubj_char)
    adsl$SUBJID <- factor(as.character(subjid))
    adsl
  }

  make_fake_adsl <- function(adsl) {
    fakeyfake <- filter(adsl, TRT01A == "Placebo")
    fakeyfake$TRT01A <- "Xanomeline Medium Dose"
    fakeyfake$AGE <- floor(runif(NROW(fakeyfake), 30,  90))
    adsl$TRT01A <- as.character(adsl$TRT01A)
    adsl <- rbind(adsl, fakeyfake)
    adsl$TRT01A <- factor(adsl$TRT01A,
                          levels = c("Placebo",
                                     "Xanomeline Low Dose",
                                     "Xanomeline Medium Dose",
                                     "Xanomeline High Dose"))

    fix_usubjid(adsl)
  }

  borrow_records <- function(df, adsl, mult = 1) { #runif(1, .9, 1.1)) {
    plac_count <- sum(df$TRT01A == "Placebo", na.rm = TRUE)
    new_count <- floor(plac_count * mult)
    soc_usubjids <- as.character(adsl$USUBJID)[!is.na(adsl$TRT01A) & adsl$TRT01A == "Xanomeline Medium Dose"]

    duprows <- sample(seq_len(NROW(df)), new_count, replace = TRUE)

    newrws <- df[duprows, ]
    print(c(new_count, length(duprows), length(unique(duprows)), NROW(newrws)))
    newrws$USUBJID <- sample(soc_usubjids, NROW(newrws), replace = TRUE)
    rbind(df, newrws)
  }

  adsl <- adsl_jnj |>
    make_fake_adsl() |>
    dplyr::select(USUBJID, TRT01A, SEX)

  advs <- advs_jnj |>
    filter(PARAMCD == "DIABP" & AVISIT == "Cycle 02") |>
    borrow_records(adsl) |>
    dplyr::select(USUBJID, PARAMCD, AVISIT, AVAL, CHG, BASE) |>
    inner_join(adsl, by = c("USUBJID"), multiple = "all")

  # nolint start
  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    "low_med", "Combined: Low + Medium", c("Xanomeline Low Dose", "Xanomeline Medium Dose"), list(),
    "med_high", "Combined: Medium + High", c("Xanomeline Medium Dose", "Xanomeline High Dose"), list(),
    "low_med_high", "Combined: Low + Medium + High", c("Xanomeline Low Dose", "Xanomeline Medium Dose", "Xanomeline High Dose"), list()
  )
  # nolint end

  lyt <- basic_table() |>
    split_cols_by("TRT01A", split_fun = add_combo_levels(combodf)) |>
    add_colcounts() |>
    analyze(
      vars = "CHG",
      afun = a_summarize_ancova_j,
      extra_args = list(
        variables = list(arm = "TRT01A", covariates = c("SEX")),
        conf_level = 0.95,
        interaction_item = NULL,
        interaction_y = FALSE,
        weights_emmeans = "proportional",
        weights_combo = "proportional",
        method_combo = "contrasts",
        ref_path = c("TRT01A", "Placebo"),
        .stats = c("n_fit", "lsmean_ci", "lsmean_diffci")
      ),
      var_labels = "Adjusted comparison (covariates SEX)",
      table_names = "adjusted",
      show_labels = "visible"
    )

  result <- build_table(lyt, advs, adsl)
  expect_snapshot(cran = TRUE, result)

})
