test_that("h_get_mult_adj_estimates works as expected", {
  vars <- list(
      response = "FEV1",
      id = "USUBJID",
      arm = "RACE",
      visit = "AVISIT"
    )
  conf_level <- 0.95
  weights <- "counterfactual"
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  emmeans_res <- h_get_emmeans_res(fit, vars, weights)
  contrast_specs <- h_single_visit_contrast_specs(emmeans_res, vars)
  contrast_estimates <- h_get_spec_visit_estimates(emmeans_res, contrast_specs, conf_level, tests = TRUE)

  result <- h_get_mult_adj_estimates(
    emmeans_res = emmeans_res,
    vars = vars,
    mult_adj = "dunnett",
    conf_level = conf_level,
    contrast_df = contrast_estimates$df
  )  
  expect_snapshot_value(result, style = "deparse")
})