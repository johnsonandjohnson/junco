test_that("h_extract_coxreg_multivar works as expected", {
  anl <- tern::tern_ex_adtte |>
    dplyr::mutate(EVENT = 1 - CNSR)
  checkmate::assert_true(is.character(anl$SITEID))
  variables <- list(
    time = "AVAL",
    event = "EVENT",
    arm = "ARM",
    # Note: The column SITEID is character, which also should work.
    covariates = c("SEX", "AGE", "SITEID")
  )
  control <- tern::control_coxreg(
    conf_level = 0.9,
    ties = "efron"
  )
  fit <- tern::fit_coxreg_multivar(
    data = anl,
    variables = variables,
    control = control
  )
  result <- expect_silent(h_extract_coxreg_multivar(fit))
  checkmate::assert_tibble(result, nrows = 69, ncols = 6)
  checkmate::assert_names(
    names(result),
    identical.to = c("term", "coef_se", "p.value", "hr", "hr_ci", "labels")
  )
})

test_that("tefos03_first_split_fun works as expected", {
  lyt <- basic_table() |>
    split_cols_by("ID", split_fun = tefos03_first_split_fun)
  result <- expect_silent(build_table(lyt, DM))
  expect_snapshot(col_info(result))
  expect_snapshot(result)
})

test_that("tefos03_second_split_fun_fct works as expected", {
  split_fun <- tefos03_second_split_fun_fct(conf_level = 0.92)
  lyt <- basic_table() |>
    split_cols_by("ID", split_fun = tefos03_first_split_fun) |>
    split_cols_by("ID", split_fun = split_fun)
  result <- expect_silent(build_table(lyt, DM))
  expect_snapshot(col_info(result))
  expect_snapshot(result)
})

test_that("tefos03_afun works as expected", {
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c("model_fit", "pval")))
  )
  anl <- tern::tern_ex_adtte |>
    dplyr::mutate(EVENT = 1 - CNSR)
  variables <- list(
    time = "AVAL",
    event = "EVENT",
    arm = "ARM",
    covariates = c("SEX", "AGE")
  )
  control <- tern::control_coxreg(
    conf_level = 0.9,
    ties = "efron"
  )
  result <- expect_silent(tefos03_afun(
    df = anl,
    .var = "STUDYID",
    .spl_context = fake_spl_context,
    variables = variables,
    control = control,
    formats = list(
      pval = jjcsformat_pval_fct(0.1)
    )
  ))
  expect_snapshot(result)
})

test_that("summarize_coxreg_multivar works as expected with defaults", {
  anl <- tern::tern_ex_adtte |>
    dplyr::mutate(EVENT = 1 - CNSR)
  variables <- list(
    time = "AVAL",
    event = "EVENT",
    arm = "ARM",
    covariates = c("SEX", "AGE")
  )
  lyt <- basic_table() |>
    summarize_coxreg_multivar(
      var = "STUDYID",
      variables = variables
    )
  result <- expect_silent(build_table(lyt, anl))
  expect_snapshot(result)
})

test_that("summarize_coxreg_multivar works as expected with custom options", {
  anl <- tern::tern_ex_adtte |>
    dplyr::mutate(EVENT = 1 - CNSR)
  variables <- list(
    time = "AVAL",
    event = "EVENT",
    arm = "ARM",
    covariates = c("SEX", "AGE")
  )
  lyt <- basic_table(round_type = "sas") |>
    summarize_coxreg_multivar(
      var = "STUDYID",
      variables = variables,
      control = tern::control_coxreg(
        pval_method = "likelihood",
        conf_level = 0.5,
        ties = "breslow"
      ),
      formats = list(
        coef_se = jjcsformat_xx("xx.x (xx.x)"),
        hr_est = jjcsformat_xx("xx.xxxx"),
        hr_ci = jjcsformat_xx("(xx.x, xx.x)"),
        pval = jjcsformat_pval_fct(0.1)
      )
    )
  result <- expect_silent(build_table(lyt, anl))
  expect_snapshot(result)
})

test_that("summarize_coxreg_multivar works with row splits", {
  anl <- tern::tern_ex_adtte |>
    dplyr::mutate(EVENT = 1 - CNSR)
  variables <- list(
    time = "AVAL",
    event = "EVENT",
    arm = "ARM",
    covariates = c("SEX", "AGE")
  )
  lyt <- basic_table() |>
    split_rows_by("BMRKR2") |>
    summarize_coxreg_multivar(
      var = "STUDYID",
      variables = variables
    )
  result <- expect_silent(build_table(lyt, anl))
  expect_snapshot(result)
})
