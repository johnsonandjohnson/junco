# These helper functions are primarily used in test-lsmeans.R.

get_lsmeans_example <- function() {
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
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  list(
    vars = vars,
    fit = fit,
    emmeans_res = emmeans_res
  )
}

get_lsmeans_example_no_arm <- function() {
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
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  list(
    vars = vars,
    fit = fit,
    emmeans_res = emmeans_res
  )
}

get_lsmeans_example_subgroup <- function() {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT",
    subgroup = "SEX"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ ARMCD * AVISIT * SEX + us(AVISIT | USUBJID),
    data = mmrm::fev_data,
    optimizer = "nlminb"
  )
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  list(
    vars = vars,
    fit = fit,
    emmeans_res = emmeans_res
  )
}

get_lsmeans_example_no_arm_subgroup <- function() {
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT",
    subgroup = "SEX"
  )
  fit <- mmrm::mmrm(
    formula = FEV1 ~ AVISIT * SEX + us(AVISIT | USUBJID),
    data = mmrm::fev_data,
    optimizer = "nlminb"
  )
  emmeans_res <- h_get_emmeans_res(
    fit = fit,
    vars = vars,
    weights = "proportional"
  )
  list(
    vars = vars,
    fit = fit,
    emmeans_res = emmeans_res
  )
}