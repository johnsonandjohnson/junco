library(dplyr)

test_that("lsmeans_wide_cfun works as expected", {
  df <- data.frame(
    TRT01P = factor(
      c("Placebo", "Seltorexant 20 mg"),
      levels = c("Placebo", "Seltorexant 20 mg")
    ),
    AVISIT = factor(
      c("Day 15 (DB)", "Day 15 (DB)"),
      levels = c("Day 15 (DB)", "Day 29 (DB)", "Day 43 (DB)")
    ),
    estimate_est = c(-6.9, -7.2),
    se_est = c(0.6, 0.6),
    df_est = c(387, 387),
    lower_cl_est = c(-8, -8.3),
    upper_cl_est = c(-5.8, -6.2),
    n = c(197, 205),
    estimate_contr = c(NA, -0.4),
    se_contr = c(NA, 0.8),
    df_contr = c(NA, 387),
    lower_cl_contr = c(NA, -1.9),
    upper_cl_contr = c(NA, 1.2),
    t_stat = c(NA, -0.4),
    p_value = c(NA, 0.7),
    p_value_less = c(NA, 0.3),
    p_value_greater = c(NA, 0.7),
    conf_level = c(0.9, 0.9),
    mse = c(NA, 62.1),
    df = c(NA, 387)
  )

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("reference_group", "treatment")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = "Seltorexant 20 mg",
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("comparison", "pval")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = "Seltorexant 20 mg",
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("comparison", "pval")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = "Seltorexant 20 mg",
    pval_sided = "-1",
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("comparison", "pval")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = "Seltorexant 20 mg",
    pval_sided = "1",
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("variance", "mse")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = "Seltorexant 20 mg",
    pval_sided = "1",
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("reference_group", "se")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = "Seltorexant 20 mg",
    formats = list()
  )
  expect_snapshot(result)
})

test_that("lsmeans_wide_cfun works as expected with more than one treatment group", {
  df <- data.frame(
    TRT01P = factor(
      c("Placebo", "Seltorexant 20 mg", "Seltorexant 40 mg"),
      levels = c("Placebo", "Seltorexant 20 mg", "Seltorexant 40 mg")
    ),
    AVISIT = factor(
      rep("Day 15 (DB)", 3),
      levels = c("Day 15 (DB)", "Day 29 (DB)", "Day 43 (DB)")
    ),
    estimate_est = c(-6.9, -7.2, -2.1),
    se_est = c(0.6, 0.6, 0.6),
    df_est = c(387, 387, 387),
    lower_cl_est = c(-8, -8.3, -8.2),
    upper_cl_est = c(-5.8, -6.2, -9.5),
    n = c(197, 205, 201),
    estimate_contr = c(NA, -0.4, -0.6),
    se_contr = c(NA, 0.8, 0.9),
    df_contr = c(NA, 387, 382),
    lower_cl_contr = c(NA, -1.9, -4.6),
    upper_cl_contr = c(NA, 1.2, 4.3),
    t_stat = c(NA, -0.4, -0.8),
    p_value = c(NA, 0.7, 0.6),
    p_value_less = c(NA, 0.3, 0.4),
    p_value_greater = c(NA, 0.7, 0.6),
    conf_level = c(0.9, 0.9, 0.9),
    mse = c(NA, 62.1, 62.1),
    df = c(NA, 387, 387)
  )

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("reference_group", "treatment")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("comparison", "pval")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("variance", "df")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)
})

test_that("lsmeans_wide_cfun works as expected with more than one treatment group for MMRM case", {
  df <- data.frame(
    TRT01P = factor(
      c("Placebo", "Seltorexant 20 mg", "Seltorexant 40 mg"),
      levels = c("Placebo", "Seltorexant 20 mg", "Seltorexant 40 mg")
    ),
    AVISIT = factor(
      rep("Day 15 (DB)", 3),
      levels = c("Day 15 (DB)", "Day 29 (DB)", "Day 43 (DB)")
    ),
    estimate_est = c(-6.9, -7.2, -2.1),
    se_est = c(0.6, 0.6, 0.6),
    df_est = c(387, 387, 387),
    lower_cl_est = c(-8, -8.3, -8.2),
    upper_cl_est = c(-5.8, -6.2, -9.5),
    n = c(197, 205, 201),
    estimate_contr = c(NA, -0.4, -0.6),
    se_contr = c(NA, 0.8, 0.9),
    df_contr = c(NA, 387, 382),
    lower_cl_contr = c(NA, -1.9, -4.6),
    upper_cl_contr = c(NA, 1.2, 4.3),
    t_stat = c(NA, -0.4, -0.8),
    p_value = c(NA, 0.7, 0.6),
    p_value_less = c(NA, 0.3, 0.4),
    p_value_greater = c(NA, 0.7, 0.6),
    conf_level = c(0.9, 0.9, 0.9),
    mse = c(NA, 62.1, 55.1), # Note: Different values per treatment arms here.
    df = c(NA, 387, 343)
  )

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("reference_group", "treatment")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("comparison", "pval")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("variance", "df")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)

  result <- lsmeans_wide_cfun(
    df,
    labelstr = "Day 15 (DB)",
    .spl_context = data.frame(
      cur_col_split_val = I(list(c("variance", "mse")))
    ),
    conf_level = 0.9,
    variables = list(arm = "TRT01P", visit = "AVISIT"),
    ref_level = "Placebo",
    treatment_levels = c("Seltorexant 20 mg", "Seltorexant 40 mg"),
    formats = list()
  )
  expect_snapshot(result)
})

test_that("summarize_lsmeans_wide works as expected", {
  variables <- list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    arm = "ARMCD",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- fit_ancova(
    vars = variables,
    data = mmrm::fev_data,
    conf_level = 0.9,
    weights_emmeans = "equal"
  )

  anl <- tidy.tern_model(fit)

  lyt <- basic_table() |>
    summarize_lsmeans_wide(
      variables = variables,
      ref_level = fit$ref_level,
      treatment_levels = fit$treatment_levels,
      pval_sided = "2",
      conf_level = 0.8
    )
  result <- expect_silent(build_table(lyt, df = anl))
  expect_snapshot(result)
})

test_that("summarize_lsmeans_wide works as expected with more than 1 treatment group", {
  set.seed(123)
  fev_data2_add <- mmrm::fev_data |>
    filter(ARMCD == "TRT") |>
    mutate(FEV1 = FEV1 + rnorm(length(FEV1), 1, 10)) |>
    mutate(ARMCD = "TRT2") |>
    mutate(
      USUBJID = gsub(
        pattern = "PT",
        replacement = "XT",
        x = USUBJID,
        fixed = TRUE
      )
    )

  fev_data2 <- rbind(mmrm::fev_data, fev_data2_add, make.row.names = FALSE) |>
    mutate(ARMCD = factor(ARMCD, levels = c("PBO", "TRT", "TRT2")))
  variables <- list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    arm = "ARMCD",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- expect_silent(fit_ancova(
    vars = variables,
    data = fev_data2,
    conf_level = 0.7,
    weights_emmeans = "proportional"
    # TODO: in future release, change to: weights_emmeans = "counterfactual"
  ))

  anl <- tidy.tern_model(fit)
  lyt <- basic_table() |>
    summarize_lsmeans_wide(
      variables = variables,
      ref_level = fit$ref_level,
      treatment_levels = fit$treatment_levels,
      pval_sided = "2",
      conf_level = 0.8
    )
  result <- expect_silent(build_table(lyt, df = anl))
  expect_snapshot(result)
})

test_that("summarize_lsmeans_wide can omit variance and p-value columns", {
  variables <- list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    arm = "ARMCD",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- fit_ancova(
    vars = variables,
    data = mmrm::fev_data,
    conf_level = 0.9,
    weights_emmeans = "equal"
  )

  anl <- tidy.tern_model(fit)

  lyt <- basic_table() |>
    summarize_lsmeans_wide(
      variables = variables,
      ref_level = fit$ref_level,
      treatment_levels = fit$treatment_levels,
      include_variance = FALSE,
      include_pval = FALSE,
      conf_level = 0.8
    )
  result <- expect_silent(build_table(lyt, df = anl))
  expect_snapshot(result)
})
