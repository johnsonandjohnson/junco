library(emmeans)
library(mmrm)

test_that("fit_ancova works as expected", {
  fit <- expect_silent(fit_ancova(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      arm = "ARMCD",
      id = "USUBJID",
      visit = "AVISIT"
    ),
    data = mmrm::fev_data,
    conf_level = 0.9,
    weights_emmeans = "equal"
  ))
  checkmate::expect_class(fit, "tern_model")
  expect_snapshot_value(fit, style = "serialize", tolerance = 1e-3)
})
