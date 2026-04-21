library(mmrm)

# build_formula ----

test_that("build_formula works as expected with subgroup variable", {
  vars <- list(
    response = "AVAL",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT",
    subgroup = "REGION"
  )
  result <- build_formula(vars, "auto-regressive")
  expect_s3_class(result, "formula")
  expected <- AVAL ~ RACE + SEX + ARMCD * AVISIT * REGION + ar1(AVISIT | USUBJID)
  expect_equal(result, expected, ignore_attr = TRUE)
})


# get_mmrm_lsmeans ----

test_that("get_mmrm_lsmeans can calculate the LS mean results", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  conf_level <- 0.95
  weights <- "proportional"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages,
    weights = weights
  ))
  expect_list(result)
  expect_data_frame(result$estimates)
  expect_data_frame(result$contrasts)
  expect_identical(attr(result, "weights"), weights)
  expect_identical(attr(result, "averages"), averages)
})

test_that("get_mmrm_lsmeans preserves combined arm levels.", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  )
  expect_identical(
    levels(mmrm::fev_data$ARMCD),
    levels(result$estimates$ARMCD)
  )
  expect_identical(
    levels(mmrm::fev_data$ARMCD)[-1],
    levels(result$contrasts$ARMCD)
  )
})

test_that("get_mmrm_lsmeans works without arm", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data,
    optimizer = "nlminb"
  )
  conf_level <- 0.95
  weights <- "proportional"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages,
    weights = weights
  ))
  expect_list(result)
  expect_data_frame(result$estimates)
  expect_snapshot_value(result$estimates, tolerance = 1e-3, style = "serialize")
  expect_null(result$contrasts)
})


test_that("get_mmrm_lsmeans can calculate the LS mean results including one- and two-sided p-values", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  conf_level <- 0.95
  weights <- "counterfactual"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages,
    weights = weights
  )

  expect_snapshot(cran = TRUE, result)

  # Additional p-value verification
  contrasts <- result$contrasts
  pvals <- cbind(
    lower = pt(contrasts$t_stat, df = contrasts$df, lower.tail = TRUE),
    upper = pt(contrasts$t_stat, df = contrasts$df, lower.tail = FALSE)
  )
  two_sided_pvals <- 2 * apply(pvals, 1L, min)
  expect_equal(contrasts$p_value, two_sided_pvals)
  expect_equal(contrasts$p_value_less, pvals[, "lower"])
  expect_equal(contrasts$p_value_greater, pvals[, "upper"])
})

test_that("get_mmrm_lsmeans can adjust contrasts for multiplicity within visits", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "RACE",
    visit = "AVISIT"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  conf_level <- 0.95
  weights <- "counterfactual"

  result_dunnett <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights,
    mult_adj = "dunnett"
  )
  expect_snapshot_value(result_dunnett, style = "deparse")

  result_step_down_dunnett <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights,
    mult_adj = "step-down-dunnett"
  )
  expect_snapshot_value(result_step_down_dunnett, style = "deparse")
})

test_that("get_mmrm_lsmeans works as expected with subgroup variable", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT",
    subgroup = "SEX"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT * SEX + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  conf_level <- 0.95
  weights <- "counterfactual"
  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights
  )

  expect_snapshot_value(result, style = "deparse", cran = TRUE)
})

test_that("get_mmrm_lsmeans also works with subgroup and averages of visits", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT",
    subgroup = "SEX"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT * SEX + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  conf_level <- 0.95
  weights <- "counterfactual"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights,
    averages = averages
  )

  expect_snapshot_value(result, style = "deparse", cran = TRUE)
})

test_that("get_mmrm_lsmeans works with subgroup and multiplicity adjustment", {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "RACE",
    visit = "AVISIT",
    subgroup = "SEX"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE * AVISIT * SEX + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  conf_level <- 0.95
  weights <- "counterfactual"
  result_dunnett <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights,
    mult_adj = "dunnett"
  )
  expect_snapshot_value(result_dunnett, style = "deparse", cran = TRUE)
})

# fit_mmrm_j ----

test_that("fit_mmrm_j works as expected", {
  fit <- fit_mmrm_j(
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

  expect_snapshot(cran = TRUE, fit)
})

test_that("fit_mmrm_j works as expected with subgroup variable", {
  fit <- fit_mmrm_j(
    vars = list(
      response = "FEV1",
      covariates = "RACE",
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT",
      subgroup = "SEX"
    ),
    data = mmrm::fev_data,
    cor_struct = "unstructured",
    weights_emmeans = "equal"
  )
  expect_snapshot(cran = TRUE, fit)
})
