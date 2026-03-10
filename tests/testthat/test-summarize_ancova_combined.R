set.seed(123)

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

#' all of the functions included in this test file are very specific to usage with
#' iris_plus2 dataset and investigated models with/without interaction
#' any adjustments to dataset/models will require a thorough re-review of the functions
#' also the layouts in the table (used stats + used reference) are fixed and used throughout this test script

tbl_ancova_tern <- function(weights_emmeans = "proportional",
                            inputdf = iris_plus2,
                            interaction = TRUE) {
  if (!interaction) {
    model_variables <- list(arm = "Species", covariates = c("Color"))

    lyt_1 <- basic_table() %>%
      split_cols_by("Species", ref_group = "versicolor") %>%
      add_colcounts() %>%
      summarize_ancova(
        vars = "Sepal.Length",
        variables = model_variables,
        conf_level = 0.95,
        interaction_item = NULL,
        interaction_y = FALSE,
        weights_emmeans = weights_emmeans,
        var_labels = "Adjusted comparison (covariates Color)",
        table_names = "adjusted"
      )
  } else {
    model_variables <- list(arm = "Species", covariates = c("Color", "Species * Color"))

    lyt_1 <- basic_table() %>%
      split_cols_by("Species", ref_group = "versicolor") %>%
      add_colcounts() %>%
      summarize_ancova(
        vars = "Sepal.Length",
        variables = model_variables,
        conf_level = 0.95,
        interaction_item = "Color",
        interaction_y = "red",
        weights_emmeans = weights_emmeans,
        var_labels = "Adjusted comparison (covariates Color - red)",
        table_names = "adjusted"
      ) |>
      summarize_ancova(
        vars = "Sepal.Length",
        variables = model_variables,
        conf_level = 0.95,
        interaction_item = "Color",
        interaction_y = "blue",
        weights_emmeans = weights_emmeans,
        var_labels = "Adjusted comparison (covariates Color - blue)",
        table_names = "adjusted2"
      )
  }

  result_1 <- build_table(lyt_1, inputdf)
  result_1
}

tbl_ancova_j <- function(weights_emmeans = "proportional",
                         weights_combo = "equal",
                         inputdf = iris_plus2,
                         interaction = TRUE) {
  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    "setosa_virg", "Combined: setosa + virginica", c("setosa", "virginica"), list()
  )

  if (!interaction) {
    model_variables <- list(arm = "Species", covariates = c("Color"))

    lyt <- basic_table() %>%
      split_cols_by("Species", split_fun = add_combo_levels(combodf)) %>%
      add_colcounts() %>%
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
          ref_path = c("Species", "versicolor"),
          .stats = c("n_fit", "lsmean_ci", "lsmean_diffci")
        ),
        var_labels = "Adjusted comparison (covariates Color)",
        table_names = "adjusted",
        show_labels = "visible"
      )
  } else {
    model_variables <- list(arm = "Species", covariates = c("Color", "Species * Color"))

    lyt <- basic_table() %>%
      split_cols_by("Species", split_fun = add_combo_levels(combodf)) %>%
      add_colcounts() %>%
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

derive_from_model <- function(.lm_fit,
                              weights_red = c(virginica = 0.5, setosa = 0.5),
                              weights_blue = c(virginica = 0.5, setosa = 0.5)) {
  est_from_model <- function(.lm_fit, coef_inputs) {
    coef <- .lm_fit$coefficients
    # manual estimation
    mf <- rep(1, length(coef))

    est <- sum(coef[coef_inputs] * mf[coef_inputs])
  }

  print(names(.lm_fit$coefficients))

  if (any(grepl("Species * Color", as.character(.lm_fit$call), fixed = TRUE))) {
    # model with interaction between Species and Color
    # updates to model would require detailed review of coef_inputs used below
    derived_red_virginica <- est_from_model(.lm_fit, c(T, F, T, T, F, T))
    derived_blue_virginica <- est_from_model(.lm_fit, c(T, F, T, F, F, F))

    # versicolor is used as reference
    derived_red_ref <- est_from_model(.lm_fit, c(T, T, F, T, T, F))
    derived_blue_ref <- est_from_model(.lm_fit, c(T, T, F, F, F, F))

    derived_red <- c(derived_red_virginica, derived_red_virginica - derived_red_ref)
    derived_blue <- c(derived_blue_virginica, derived_blue_virginica - derived_blue_ref)

    # derivation for combo column, also get the third species setosa
    derived_red_setosa <- est_from_model(.lm_fit, c(T, F, F, T, F, F))
    derived_blue_setosa <- est_from_model(.lm_fit, c(T, F, F, F, F, F))

    estimates_red <- c(virginica = derived_red_virginica, setosa = derived_red_setosa)
    estimates_blue <- c(virginica = derived_blue_virginica, setosa = derived_blue_setosa)

    # ensure the proper order on all inputs
    estimates_red <- estimates_red[c("virginica", "setosa")]
    estimates_blue <- estimates_blue[c("virginica", "setosa")]
    weights_blue <- weights_blue[c("virginica", "setosa")]
    weights_red <- weights_red[c("virginica", "setosa")]

    # now the weights from virginica and setosa play a role
    derived_red_combined <- sum(weights_red * estimates_red)
    derived_blue_combined <- sum(weights_blue * estimates_blue)

    derived_comb <- list(
      red = c(derived_red_combined, derived_red_combined - derived_red_ref),
      blue = c(derived_blue_combined, derived_blue_combined - derived_blue_ref)
    )
    derived <- list(
      red = derived_red,
      blue = derived_blue,
      combined = derived_comb
    )
  } else {
    stop("model used is not as expected - revise function or update model")
  }

  derived
}

derive_from_sumfit <- function(.lm_fit,
                               weights_contrast = weights_red,
                               weights_emmeans) {
  if (!any(grepl("Species * Color", as.character(.lm_fit$call), fixed = TRUE))) {
    # model without interaction between Species and Color

    emmeans_fit <- emmeans::emmeans(
      .lm_fit,
      specs = "Species",
      data = .lm_fit$model,
      weights_emmeans = weights_emmeans
    )

    sum_fit <- summary(
      emmeans_fit,
      level = 0.95
    )

    emmeans_contrasts <- emmeans::contrast(
      emmeans_fit,
      # Compare all arms versus the control arm.
      method = "trt.vs.ctrl",
      # Take the arm factor from .ref_group as the control arm.
      ref = 2,
      level = 0.95
    )
    sum_contrasts <- summary(
      emmeans_contrasts,
      # Derive confidence intervals, t-tests and p-values.
      infer = TRUE,
      # Do not adjust the p-values for multiplicity.
      adjust = "none"
    )

    derived_virginica <- as.numeric(as.data.frame(sum_fit) |>
      filter(Species == "virginica") |>
      select(c("emmean", "lower.CL", "upper.CL")))

    derived_virginica_against_ref <- as.numeric(as.data.frame(sum_contrasts) |>
      filter(contrast == "virginica - versicolor") |>
      select(c("estimate", "lower.CL", "upper.CL")))
    derived_start <- c(derived_virginica, derived_virginica_against_ref)

    # derivations for combined
    derived_setosa <- as.numeric(as.data.frame(sum_fit) |>
      filter(Species == "setosa") |>
      select(c("emmean")))

    derived_comb <- weights_contrast["virginica"] * derived_virginica[1] +
      weights_contrast["setosa"] * derived_setosa[1]
    names(derived_comb) <- NULL

    derived_ref <- as.numeric(as.data.frame(sum_fit) |>
      filter(Species == "versicolor") |>
      pull(c("emmean")))

    # add estimate for diff
    derived_comb <- c(derived_comb, derived_comb - derived_ref)

    derived <- list(
      est = derived_start,
      combined = derived_comb
    )
  } else {
    stop("model with interaction between Species * Color should use derive_from_model")
  }
}

# get_weights : note that weights vary for red and blue when bycolor = TRUE
# bycolor TRUE is conform with setting weights_combo = proportional
# bycolor FALSE is conform with setting weights_combo = proportional_marginal
get_weights <- function(bycolor = TRUE, color) {
  df_color <- iris_plus2
  if (bycolor) {
    df_color <- df_color |> filter(Color == color)
  }
  w_color <- table(df_color[["Species"]])[c("setosa", "virginica")]
  w_color <- w_color / sum(w_color)
}





#' @param tbl1 tbl generated with tern function summarize_ancova (no combined column)
#' @param tbl2 tbl generated with junco function a_summarize_ancova_j with a combined column
#' @param interaction did model include interaction (Species * Color) or not
#' @param .lm_fit fit from model
#' @param weights_blue weights used for combo derivation for blue section (interaction)
#' @param weights_red weights used for combo derivation for red section (interaction) or no interaction
#' @param weights_emmeans weights_emmeans in a_ancova
#'
#' @return : when no interaction :
#' a list with following elements:
#' tbl1: est - 4 numbers : retrieved from appropriate rows cell_values from tbl1 from virginica column (3)
#' tbl2: est - 4 numbers : retrieved from appropriate rows cell_values from tbl2 from virginica column (3)
#' tbl2_comb: est - 2 numbers : retrieved from appropriate rows cell_values from tbl2 combo column
#'
#' derivations for combo column in this test function
#' derived: est - 6 numbers : adj_mean, lowerCI, upperCI, diff with ref, lowerCI, upperCI for virginica column estimates
#' comb_model: est : 2 numbers: adj_mean, diff with ref for combo column estimates
#'
#'
#' when interaction with Color and Species included:
#' a list with following elements:
#' tbl1: est_red/est_blue - 4 numbers : retrieved from appropriate rows cell_values from tbl1 from virginica column from section red/blue resp (3)
#' tbl2: est_red/est_blue - 4 numbers : retrieved from appropriate rows cell_values from tbl2 from virginica column from section red/blue resp (3)
#' tbl2_comb: est_red/est_blue - 2 numbers : retrieved appropriate rows from cell_values from tbl2 combo column from section red/blue resp (4)
#'
#' derivations for combo column in this test function
#' derived: est_red/est_blue - ONLY 2 numbers : adj_mean, diff with ref for virginica column estimates for red/blue resp
#' comb_model: est : 2 numbers: adj_mean, diff with ref for combo column estimates
#'
#' Note: the appropriate rows depend on what stats we included in the generated tables
#' updates to layouts will impact these rows, so be carefull with adjustments to these

compare_ancova_tbl <- function(tbl1,
                               tbl2,
                               interaction = TRUE,
                               .lm_fit,
                               weights_blue = c(virginica = 0.5, setosa = 0.5),
                               weights_red = c(virginica = 0.5, setosa = 0.5),
                               weights_emmeans) {
  # comparison of column virginica from tbl1 against tbl2
  col <- 3

  get_numbers <- function(tbl, col, rows = c(3)) {
    numbers <- unname(unlist(cell_values(tbl[rows, col])))
  }

  if (!interaction) {
    # tbl without interaction: rows 3/4-5 are of interest
    numbers <- get_numbers(tbl1, col, c(3, 4, 5))
    actual <- get_numbers(tbl2, col, c(3, 4))[c(1, 4, 5, 6)]

    est_comb <- get_numbers(tbl2, 4, c(3, 4))[c(1, 4)]

    sum_1 <- list(est = numbers)
    sum_2 <- list(est = actual)

    sum_2b <- list(est = est_comb)

    # derivations based upon summary fit --- this takes care of the weights for the covariate .lm_fit
    derived <- derive_from_sumfit(
      .lm_fit = .lm_fit,
      weights_contrast = weights_red,
      weights_emmeans = weights_emmeans
    )
    sum_3 <- list(est = derived$est)

    sum_4 <- list(est = derived$combined)
  } else {
    # tbl with interaction: rows 3/4-5 (red) and 9/10-11 (blue) are of interest

    # red
    numbers_red <- get_numbers(tbl1, col, c(3, 4, 5))
    actual_red <- get_numbers(tbl2, col, c(3, 4))[c(1, 4, 5, 6)]

    comb_red <- get_numbers(tbl2, 4, c(3, 4))[c(1, 4)]

    ### blue Color
    numbers_blue <- get_numbers(tbl1, col, c(9, 10, 11))
    actual_blue <- get_numbers(tbl2, col, c(7, 8))[c(1, 4, 5, 6)]

    comb_blue <- get_numbers(tbl2, 4, c(7, 8))[c(1, 4)]

    sum_1 <- list(est_red = numbers_red, est_blue = numbers_blue)
    sum_2 <- list(est_red = actual_red, est_blue = actual_blue)

    sum_2b <- list(est_red = comb_red, est_blue = comb_blue)

    # derivations based upon .lm_fit
    # perform core calculations from model coefficients in lm_fit
    # both for weights and comb
    derived <- derive_from_model(
      .lm_fit = .lm_fit,
      weights_red = weights_red,
      weights_blue = weights_blue
    )

    sum_3 <- list(est_red = derived$red, est_blue = derived$blue)


    sum_4 <- list(est_red = derived$combined$red, est_blue = derived$combined$blue)
  }
  ret <- list(tbl1 = sum_1, tbl2 = sum_2, tbl2_comb = sum_2b, derived = sum_3, comb_model = sum_4)

  ret
}

#' actual tests starts here

test_that("s_ancova_j works as expected in combined column for model with interaction 3 sets of weights_combo", {
  model_variables <- list(arm = "Species", covariates = c("Color", "Species * Color"))
  lm_fit <- stats::lm(formula = Sepal.Length ~ Species + Color + Species * Color, data = iris_plus2)

  result_1 <- tbl_ancova_tern(
    weights_emmeans = "equal",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  result_1b <- tbl_ancova_tern(
    weights_emmeans = "proportional",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  # for this model, weights_emmeans has no effect for non-combo columns, as the model includes interaction between color and species
  expect_equal(result_1, result_1b)
  result_1

  # results with combo column
  result <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  result_b <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  result_c <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional_marginal",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  result_d <- tbl_ancova_j(
    weights_emmeans = "proportional",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = TRUE
  )

  ### weights_combo have impact on combined column
  ### weights_emmeans have no impact on combined column (as weights_combo has been decoupled from weights_emmeans)
  expect_equal(result, result_d)

  # confirm that non-combined columns match with results from tern::summarize_ancova
  # compare_ancova_tbl heavily depends on the used model, updates to model would require detailed review of coefficients
  summary_numbers <- compare_ancova_tbl(result_1,
    result,
    interaction = TRUE,
    .lm_fit = lm_fit,
    # equal weights for combo
    weights_red = c(virginica = 0.5, setosa = 0.5),
    weights_blue = c(virginica = 0.5, setosa = 0.5)
  )


  expect_equal(summary_numbers[["tbl1"]], summary_numbers[["tbl2"]])

  # also match own derivations from model fit
  limited <- list(
    summary_numbers[["tbl1"]][[1]][1:2],
    summary_numbers[["tbl1"]][[2]][1:2]
  )
  names(limited) <- names(summary_numbers[["tbl1"]])
  expect_equal(limited, summary_numbers[["derived"]])

  # confirm that combined column estimates match from model
  expect_equal(summary_numbers[["tbl2_comb"]], summary_numbers[["comb_model"]])

  ### repeat for weights_combo = "proportional" - focus on comb column only

  w_red <- get_weights(bycolor = TRUE, "red")
  w_blue <- get_weights(bycolor = TRUE, "blue")
  print(w_red)
  print(w_blue)

  summary_numbers2 <- compare_ancova_tbl(result_1,
    result_b,
    interaction = TRUE,
    .lm_fit = lm_fit,
    weights_blue = w_blue,
    weights_red = w_red
  )

  expect_equal(summary_numbers2[["tbl2_comb"]], summary_numbers2[["comb_model"]])

  ### repeat for weights_combo = "proportional_marginal" - focus on comb column only
  # bycolor FALSE is setting proportional_marginal, same weights for combo column derivation for both parts of the table

  w_red <- get_weights(bycolor = FALSE, "red")
  w_blue <- get_weights(bycolor = FALSE, "blue")
  print(w_red)
  print(w_blue)

  summary_numbers3 <- compare_ancova_tbl(result_1,
    result_c,
    interaction = TRUE,
    .lm_fit = lm_fit,
    weights_blue = w_blue,
    weights_red = w_red
  )

  expect_equal(summary_numbers3[["tbl2_comb"]], summary_numbers3[["comb_model"]])
})

test_that("s_ancova_j works as expected in combined column for model without interaction", {
  model_variables <- list(arm = "Species", covariates = c("Color"))
  lm_fit <- stats::lm(formula = Sepal.Length ~ Species + Color, data = iris_plus2)

  result_1 <- tbl_ancova_tern(
    weights_emmeans = "equal",
    inputdf = iris_plus2,
    interaction = FALSE
  )

  result_1b <- tbl_ancova_tern(
    weights_emmeans = "proportional",
    inputdf = iris_plus2,
    interaction = FALSE
  )

  # expect_equal(result_1, result_1b) - this is not the case
  # for this model weights_emmeans do play a role for the adjusted mean estimate, not for the difference
  result_1
  result_1b

  result <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = FALSE
  )

  result_b <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional",
    inputdf = iris_plus2,
    interaction = FALSE
  )

  result_c <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional_marginal",
    inputdf = iris_plus2,
    interaction = FALSE
  )

  result_d <- tbl_ancova_j(
    weights_emmeans = "proportional",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = FALSE
  )

  # proportional and proportional_marginal is the same when no interaction
  expect_equal(result_b, result_c)

  ### weights_emmeans have also impact on combined column - for adj mean estimate (not for diff as these cancel out)
  # similar as for the non-combined columns

  # confirm that non-combined columns match with results from tern::summarize_ancova
  # start with: weights_combo = "equal"
  summary_numbers <- compare_ancova_tbl(result_1,
    result,
    interaction = FALSE,
    .lm_fit = lm_fit,
    weights_emmeans = "equal",
    weights_red = c(virginica = 0.5, setosa = 0.5),
    weights_blue = NULL
  )

  expect_equal(summary_numbers[["tbl1"]], summary_numbers[["tbl2"]])

  # also match own derivations from model fit - for derived, get rid of CI for adj mean (element 2 - 3)
  limited <- list(summary_numbers[["tbl2"]][[1]])
  names(limited) <- names(summary_numbers[["tbl2"]])
  limited2 <- list(summary_numbers[["derived"]][["est"]][c(1, 4, 5, 6)])
  names(limited2) <- "est"
  expect_equal(limited, limited2)

  # confirm that combined column estimates match from model
  expect_equal(summary_numbers[["tbl2_comb"]], summary_numbers[["comb_model"]])

  # confirm that non-combined columns match with results from tern::summarize_ancova
  # repeat for: weights_combo = "proportional"
  w_red <- get_weights(bycolor = FALSE, "red")
  w_blue <- get_weights(bycolor = FALSE, "blue")
  print(w_red)
  print(w_blue)

  summary_numbers2 <- compare_ancova_tbl(result_1,
    result_b,
    interaction = FALSE,
    .lm_fit = lm_fit,
    weights_emmeans = "equal",
    weights_blue = NULL,
    weights_red = w_red
  )

  expect_equal(summary_numbers2[["tbl2_comb"]], summary_numbers2[["comb_model"]])
})
