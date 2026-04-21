library(mmrm)

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
