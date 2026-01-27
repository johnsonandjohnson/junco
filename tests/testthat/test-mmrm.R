library(mmrm)

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

  expect_snapshot(result)

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

  expect_snapshot(fit)
})
