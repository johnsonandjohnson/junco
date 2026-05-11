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

#' NOTE : tests for the combined feature of a_summarize_ancova_j in test script tests/testthat/test-summarize_ancova.R
#' use snapshot approach
#' current file performs tests without snapshot approach
#' instead, model results are re-verified with emmeans based code
#' the same ancova based tables that are included in the snapshot file are considered

#' these are
#' 3 tables for test
#' a_summarize_ancova_j (s_ancova_j) as expected in combined column for model without interaction
#' result, result_b, result_d

#' 3 tables for test
#' a_summarize_ancova_j combined column and interaction, diff versions for weights_combo
#' result, result_b, result_c

#' all of the functions included in this test file are very specific to usage with
#' iris_plus2 dataset and investigated models with/without interaction
#' any adjustments to dataset/models will require a thorough re-review of the functions
#' also the layouts in the table (used stats + used reference) are fixed and used throughout this test script

# note: this same function is in tests/testthat/test-summarize_ancova.R
# function to create results table ancova with combined column on iris_plus2 data
# used to create the different model versions:
# result, result_b, result_d without interaction
# result, result_b, result_c with interaction

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

#' this function is used only in sum_ancova_tbl for testing derivations based upon interaction model
derive_from_model <- function(.lm_fit,
                              weights_red = c(virginica = 0.5, setosa = 0.5),
                              weights_blue = c(virginica = 0.5, setosa = 0.5)) {
  est_from_model <- function(.lm_fit, coef_inputs) {
    coef <- .lm_fit$coefficients
    # manual estimation
    mf <- rep(1, length(coef))

    est <- sum(coef[coef_inputs] * mf[coef_inputs])
  }

  if (any(grepl("Species * Color", as.character(.lm_fit$call), fixed = TRUE))) {
    # model with interaction between Species and Color
    # updates to model would require detailed review of coef_inputs used below
    derived_red_virginica <- est_from_model(.lm_fit, c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
    derived_blue_virginica <- est_from_model(.lm_fit, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))

    # versicolor is used as reference
    derived_red_ref <- est_from_model(.lm_fit, c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
    derived_blue_ref <- est_from_model(.lm_fit, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

    derived_red <- c(derived_red_virginica, derived_red_virginica - derived_red_ref)
    derived_blue <- c(derived_blue_virginica, derived_blue_virginica - derived_blue_ref)

    # derivation for combo column, also get the third species setosa
    derived_red_setosa <- est_from_model(.lm_fit, c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
    derived_blue_setosa <- est_from_model(.lm_fit, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))

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

#' this function is used only in sum_ancova_tbl for testing derivations based upon non interaction model
derive_from_sumfit <- function(.lm_fit,
                               weights_contrast = weights_red,
                               weights_emmeans) {
  if (!any(grepl("Species * Color", as.character(.lm_fit$call), fixed = TRUE))) {
    # model without interaction between Species and Color

    emmeans_fit <- emmeans::emmeans(
      .lm_fit,
      specs = "Species",
      data = .lm_fit$model,
      weights = weights_emmeans
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

    derived_virginica <-
      as.numeric(as.data.frame(sum_fit) |>
                   filter(Species == "virginica") |>
                   select(c("emmean", "lower.CL", "upper.CL")))

    derived_virginica_against_ref <-
      as.numeric(as.data.frame(sum_contrasts) |>
                   filter(contrast == "virginica - versicolor") |>
                   select(c("estimate", "lower.CL", "upper.CL")))
    derived_start <- c(derived_virginica, derived_virginica_against_ref)

    # derivations for combined
    w_start <- as.character(as.data.frame(emmeans_fit)[["Species"]])
    xweights_contrast <- c(weights_contrast, 0)
    names(xweights_contrast) <- c(names(weights_contrast), "versicolor")
    xweights_contrast2 <- c(weights_contrast, -1)
    names(xweights_contrast2) <- c(names(weights_contrast), "versicolor")

    # get the weights in the proper order
    xweights_contrast <- xweights_contrast[w_start]
    xweights_contrast2 <- xweights_contrast2[w_start]

    contr <-
      emmeans::contrast(emmeans_fit,
                        list(setNames(list(xweights_contrast), "Estimate contrast"),
                             setNames(list(xweights_contrast2), "Estimate contrast - ref")),
                        # Derive confidence intervals, t-tests and p-values.
                        infer = TRUE,
                        # Do not adjust the p-values for multiplicity.
                        adjust = "none")

    contr_df <- as.data.frame(contr)

    derived_comb <- contr_df |>
      select(c("estimate", "lower.CL", "upper.CL"))

    derived_comb <- as.numeric(t(derived_comb))

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

get_numbers <- function(tbl, col, rows = c(3)) {
  numbers <- unname(unlist(cell_values(tbl[rows, col])))
}



#' @param tbl tbl generated with junco function a_summarize_ancova_j with a combined column
#' @param interaction did model include interaction (Species * Color) or not
#' @param .lm_fit fit from model
#' @param weights_blue weights used for combo derivation for blue section (interaction)
#' @param weights_red weights used for combo derivation for red section (interaction) or no interaction
#' @param weights_emmeans weights_emmeans in a_ancova
#'
#' @return : when no interaction :
#' a list with following elements:
#' taken from tbl:
#' tbl_virginica: est - 6 numbers: adj_mean, lowerCI, upperCI, diff with ref, lowerCI, upperCI for virginica column
#' tbl_comb: est - 6 numbers: adj_mean, lowerCI, upperCI, diff with ref, lowerCI, upperCI for combined column
#'
#' re-calculated from .lm_fit:
#' derived_virginica: est - 6 numbers: adj_mean, lowerCI, upperCI, diff with ref, lowerCI, upperCI for virginica column
#' derived_comb: est - 6 numbers: adj_mean, lowerCI, upperCI, diff with ref, lowerCI, upperCI for combined column
#'
#'
#' when interaction with Color and Species included: focus on adj_mean and diff with ref, do not derive CI numbers
#' a list with following elements:
#' taken from tbl:
#' tbl_virginica: est_red/est_blue - 2 numbers: adj_mean, diff with ref for virginica column
#' tbl_comb: est_red/est_blue - 2 numbers: adj_mean, diff with ref for combined column
#'
#' re-calculated from .lm_fit:
#' derived_virginica: est_red/est_blue - 2 numbers: adj_mean, diff with ref for virginica column
#' derived_comb: est_red/est_blue - 2 numbers: adj_mean, diff with ref for combined column taken
#'
#'
#' Note: the appropriate rows depend on what stats we included in the generated tables
#' updates to layouts will impact these rows, so be careful with adjustments to these

sum_ancova_tbl <- function(tbl,
                           interaction = TRUE,
                           .lm_fit,
                           weights_blue = c(virginica = 0.5, setosa = 0.5),
                           weights_red = c(virginica = 0.5, setosa = 0.5),
                           weights_emmeans) {
  # comparison of column virginica from tbl
  col <- 3

  if (!interaction) {
    # tbl without interaction: rows 3/4 are of interest
    from_tbl_virg <- get_numbers(tbl, col, c(3, 4))
    from_tbl_comb <- get_numbers(tbl, 4, c(3, 4))
    sum_1 <- list(est = from_tbl_virg)
    sum_2 <- list(est = from_tbl_comb)

    # derivations based upon summary fit --- this takes care of the weights for the covariate .lm_fit
    derived <- derive_from_sumfit(
      .lm_fit = .lm_fit,
      weights_contrast = weights_red,
      weights_emmeans = weights_emmeans
    )
    sum_3 <- list(est = derived$est)
    sum_4 <- list(est = derived$combined)
  } else {
    # tbl with interaction: rows 3/4 (red) and 7/8 (blue) are of interest
    from_tbl_virg_red <- get_numbers(tbl, col, c(3, 4))
    from_tbl_comb_red <- get_numbers(tbl, 4, c(3, 4))
    from_tbl_virg_blue <- get_numbers(tbl, col, c(7, 8))
    from_tbl_comb_blue <- get_numbers(tbl, 4, c(7, 8))

    # for interaction model derivations from derive_from_model
    # does not include CI,
    # so restrict here to estimates only, 1st 4th element as well
    sum_1 <- list(est_red = from_tbl_virg_red[c(1, 4)], est_blue = from_tbl_virg_blue[c(1, 4)])
    sum_2 <- list(est_red = from_tbl_comb_red[c(1, 4)], est_blue = from_tbl_comb_blue[c(1, 4)])

    # derivations based upon .lm_fit
    # perform core calculations from model coefficients in .lm_fit
    # both for weights and comb
    derived <- derive_from_model(
      .lm_fit = .lm_fit,
      weights_red = weights_red,
      weights_blue = weights_blue
    )

    sum_3 <- list(est_red = derived$red, est_blue = derived$blue)
    sum_4 <- list(est_red = derived$combined$red, est_blue = derived$combined$blue)
  }
  ret <- list(tbl_virginica = sum_1, tbl_comb = sum_2, derived_virginica = sum_3, derived_comb = sum_4)

  ret
}


#' actual tests starts here

test_that("a_summarize_ancova_j (s_ancova_j) as expected in combined column for model without interaction", {
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
  # confirm that non-combined columns match with results from tern::summarize_ancova
  # start with: weights_combo = "equal"
  summary_numbers <- sum_ancova_tbl(
    result,
    interaction = FALSE,
    .lm_fit = lm_fit,
    weights_emmeans = "equal",
    weights_red = c(virginica = 0.5, setosa = 0.5),
    weights_blue = NULL
  )
  summary_numbers
  expect_equal(summary_numbers[["derived_virginica"]], summary_numbers[["tbl_virginica"]])
  expect_equal(summary_numbers[["derived_comb"]], summary_numbers[["tbl_comb"]])

  # table from junco afun a_summarize_ancova_j in layout with combined column
  # - equal weights when averaging over Color covariate
  # - proportional weights when deriving estimates for Combined: setosa + virginica column
  result_b <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional",
    inputdf = iris_plus2,
    interaction = FALSE
  )
  summary_numbers_b <- sum_ancova_tbl(
    result_b,
    interaction = FALSE,
    .lm_fit = lm_fit,
    weights_emmeans = "equal",
    weights_blue = NULL,
    weights_red = get_weights(bycolor = FALSE, "red")
  )

  summary_numbers_b
  expect_equal(summary_numbers_b[["derived_virginica"]], summary_numbers_b[["tbl_virginica"]])
  expect_equal(summary_numbers_b[["derived_comb"]], summary_numbers_b[["tbl_comb"]])


  # - proportional weights when averaging over Color covariate
  # - proportional_marginal weights when deriving estimates for Combined: setosa + virginica column
  # proportional and proportional_marginal is the same when no interaction

  result_d <- tbl_ancova_j(
    weights_emmeans = "proportional",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = FALSE
  )
  summary_numbers_d <- sum_ancova_tbl(
    result_d,
    interaction = FALSE,
    .lm_fit = lm_fit,
    weights_emmeans = "proportional",
    weights_blue = NULL,
    weights_red = c(virginica = 0.5, setosa = 0.5)
  )

  summary_numbers_d

  expect_equal(summary_numbers_d[["derived_virginica"]], summary_numbers_d[["tbl_virginica"]])
  expect_equal(summary_numbers_d[["derived_comb"]], summary_numbers_d[["tbl_comb"]])

  ### weights_emmeans have also impact on combined column - for adj mean estimate (not for diff as these cancel out)
  # similar as for the non-combined columns
})

test_that("a_summarize_ancova_j combined column and interaction, diff versions for weights_combo", {
  # nolint start
  # longer description lintr
  # a_summarize_ancova_j (s_ancova_j) works as expected in combined column for model with interaction 3 sets of weights_combo"
  # nolint end
  model_variables <- list(arm = "Species", covariates = c("Color", "Species * Color"))
  lm_fit <- stats::lm(formula = Sepal.Length ~ Species + Color + Species * Color, data = iris_plus2)

  # results with combo column
  result <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "equal",
    inputdf = iris_plus2,
    interaction = TRUE
  )
  summary_numbers <-
    sum_ancova_tbl(result,
                   interaction = TRUE,
                   .lm_fit = lm_fit,
                   # equal weights for combo
                   weights_red = c(virginica = 0.5, setosa = 0.5),
                   weights_blue = c(virginica = 0.5, setosa = 0.5))

  summary_numbers
  expect_equal(summary_numbers[["derived_virginica"]], summary_numbers[["tbl_virginica"]])
  expect_equal(summary_numbers[["derived_comb"]], summary_numbers[["tbl_comb"]])

  result_b <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional",
    inputdf = iris_plus2,
    interaction = TRUE
  )
  summary_numbers_b <-
    sum_ancova_tbl(result_b,
                   interaction = TRUE,
                   .lm_fit = lm_fit,
                   weights_blue = get_weights(bycolor = TRUE, "blue"),
                   weights_red = get_weights(bycolor = TRUE, "red"))

  summary_numbers_b
  expect_equal(summary_numbers_b[["derived_virginica"]], summary_numbers_b[["tbl_virginica"]])
  expect_equal(summary_numbers_b[["derived_comb"]], summary_numbers_b[["tbl_comb"]])

  result_c <- tbl_ancova_j(
    weights_emmeans = "equal",
    weights_combo = "proportional_marginal",
    inputdf = iris_plus2,
    interaction = TRUE
  )
  summary_numbers_c <-
    sum_ancova_tbl(result_c,
                   interaction = TRUE,
                   .lm_fit = lm_fit,
                   weights_blue = get_weights(bycolor = FALSE, "blue"),
                   weights_red = get_weights(bycolor = FALSE, "red"))

  summary_numbers_c
  expect_equal(summary_numbers_c[["derived_virginica"]], summary_numbers_c[["tbl_virginica"]])
  expect_equal(summary_numbers_c[["derived_comb"]], summary_numbers_c[["tbl_comb"]])
})

test_that("a_summarize_ancova_j with multiple combined columns", {
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
    select(USUBJID, TRT01A, SEX)

  advs <- advs_jnj |>
    filter(PARAMCD == "DIABP" & AVISIT == "Cycle 02") |>
    borrow_records(adsl) |>
    select(USUBJID, PARAMCD, AVISIT, AVAL, CHG, BASE) |>
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

  get_weights2 <- function(combo, wm = "equal") {
    df <- advs[!is.na(advs$CHG), ]

    w_start <- table(df[["TRT01A"]])
    w_start_orig <- w_start

    if (wm == "equal") {
      w_start <- rep(1, length(w_start))
    } else {
      w_start <- as.numeric(w_start)
    }
    names(w_start) <- names(w_start_orig)
    w_start[!names(w_start) %in% combo] <- 0

    w_start <- w_start / sum(w_start)
    w_start
  }

  # derivations of model from
  .lm_fit <- stats::lm(formula = CHG ~ TRT01A + SEX, data = advs)
  emmeans_fit <- emmeans::emmeans(
    .lm_fit,
    specs = "TRT01A",
    data = .lm_fit$model,
    weights = "proportional"
  )

  emmeans_contrasts <- emmeans::contrast(
    emmeans_fit,
    # Compare all arms versus the control arm.
    method = "trt.vs.ctrl",
    # Take the arm factor from .ref_group as the control arm.
    ref = "Placebo",
    level = 0.95
  )

  sum_contrasts <- summary(
    emmeans_contrasts,
    # Derive confidence intervals, t-tests and p-values.
    infer = TRUE,
    # Do not adjust the p-values for multiplicity.
    adjust = "none"
  )

  xweights_contrast <- list()
  xweights_contrast2 <- list()
  for (i in seq_len(nrow(combodf))) {
    curcombo <- combodf[i, ][["levelcombo"]][[1]]
    # derivations for combined
    xweights_contrast[[i]] <- get_weights2(combo = curcombo,
                                           wm = "proportional")

    xweights_contrast2[[i]] <- xweights_contrast[[i]]
    xweights_contrast2[[i]]["Placebo"] <- -1

    print(xweights_contrast[[i]])
    print(xweights_contrast2[[i]])
  }
  names(xweights_contrast) <- combodf$label
  names(xweights_contrast2) <- combodf$label

  contr <-
    emmeans::contrast(emmeans_fit,
                      list(setNames(list(xweights_contrast), "Estimate contrast"),
                           setNames(list(xweights_contrast2), "Estimate contrast - ref")),
                      # Derive confidence intervals, t-tests and p-values.
                      infer = TRUE,
                      # Do not adjust the p-values for multiplicity.
                      adjust = "none")

  contr <- as.data.frame(contr)
  contr <- contr[c(1, 4, 2, 5, 3, 6), ]

  combo_numbers_from_tbl <- list()
  combo_numbers_from_model <- list()
  for (i in seq_len(nrow(combodf))) {
    combo_numbers_from_tbl[[i]] <- get_numbers(result, 4 + i, rows = c(3, 4))
    combo_numbers_from_model[[i]] <- contr[c(2 * (i - 1) + 1, 2 * (i - 1) + 2), c("estimate", "lower.CL", "upper.CL")]
    combo_numbers_from_model[[i]] <- c(t(combo_numbers_from_model[[i]]))
  }
  names(combo_numbers_from_tbl) <- combodf$label
  names(combo_numbers_from_model) <- combodf$label

  combo_numbers_from_tbl
  testthat::expect_equal(combo_numbers_from_tbl, combo_numbers_from_model)
})
