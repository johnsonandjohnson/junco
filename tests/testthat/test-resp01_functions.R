library(rtables)

test_that("resp01_split_fun_fct works as expected", {
  split_fun <- resp01_split_fun_fct(
    method = "or_cmh",
    conf_level = 0.95
  )
  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("Overall")) %>%
    split_cols_by("ID", split_fun = split_fun) |>
    build_table(formatters::DM)
  expect_snapshot(result)

  split_fun <- resp01_split_fun_fct(
    method = "rr",
    conf_level = 0.92
  )
  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("Overall")) %>%
    split_cols_by("ID", split_fun = split_fun) |>
    build_table(formatters::DM)
  expect_snapshot(col_info(result))
})

test_that("resp01_counts_cfun works as expected", {
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
  )
  result <- resp01_counts_cfun(
    df = data.frame(a = 1),
    labelstr = "Blue",
    .spl_context = fake_spl_context,
    .alt_df = data.frame(a = c(1, 2)),
    label_fstr = "Color: %s"
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "Overall", count_prop = "count_prop")))
  )
  result <- resp01_counts_cfun(
    df = data.frame(a = 1),
    labelstr = "Blue",
    .spl_context = fake_spl_context,
    .alt_df = data.frame(a = c(1, 2)),
    label_fstr = "Color: %s"
  )
  expect_null(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "bla")))
  )
  result <- resp01_counts_cfun(
    df = data.frame(a = 1),
    labelstr = "Blue",
    .spl_context = fake_spl_context,
    .alt_df = data.frame(a = c(1, 2)),
    label_fstr = "Color: %s"
  )
  expect_null(result)
})

test_that("resp01_a_comp_stat_logical works as expected", {
  dm <- droplevels(subset(DM, SEX %in% c("F", "M")))
  set.seed(123)
  dm$RESP <- as.logical(sample(
    c(TRUE, FALSE),
    size = nrow(formatters::DM),
    replace = TRUE
  ))
  result <- resp01_a_comp_stat_logical(
    dm,
    .var = "RESP",
    conf_level = 0.9,
    include = TRUE,
    arm = "SEX",
    strata = "RACE",
    stat = "comp_stat_ci",
    methods = list(
      comp_stat_ci = "or_cmh",
      pval = ""
    ),
    formats = list(
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)

  result <- resp01_a_comp_stat_logical(
    dm,
    .var = "RESP",
    conf_level = 0.9,
    include = TRUE,
    arm = "SEX",
    strata = NULL,
    stat = "pval",
    methods = list(
      comp_stat_ci = "or_logistic",
      pval = "fisher"
    ),
    formats = list(
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)
})

test_that("resp01_a_comp_stat_factor works as expected", {
  dm <- droplevels(subset(DM, SEX %in% c("F", "M")))
  result <- resp01_a_comp_stat_factor(
    dm,
    .var = "COUNTRY",
    conf_level = 0.9,
    include = c("USA", "CHN"),
    arm = "SEX",
    strata = "RACE",
    stat = "comp_stat_ci",
    methods = list(
      comp_stat_ci = "or_cmh",
      pval = ""
    ),
    formats = list(
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)

  result <- resp01_a_comp_stat_factor(
    dm,
    .var = "COUNTRY",
    conf_level = 0.9,
    include = c("USA", "CHN"),
    arm = "SEX",
    strata = NULL,
    stat = "pval",
    methods = list(
      comp_stat_ci = "or_logistic",
      pval = "fisher"
    ),
    formats = list(
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)
})

test_that("resp01_acfun works as expected", {
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
  )
  dm <- droplevels(subset(DM, SEX %in% c("F", "M")))
  result <- resp01_acfun(
    dm,
    .alt_df = dm,
    .var = "COUNTRY",
    .spl_context = fake_spl_context,
    conf_level = 0.9,
    include_comp = c("USA", "CHN"),
    arm = "SEX",
    strata = "RACE",
    methods = list(),
    formats = list(
      prop_ci = jjcsformat_xx("xx.% - xx.%"),
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(
      ARM = "Overall",
      comp_stat_ci = "comp_stat_ci"
    )))
  )
  result <- resp01_acfun(
    dm,
    .alt_df = dm,
    .var = "COUNTRY",
    .spl_context = fake_spl_context,
    conf_level = 0.9,
    include_comp = c("USA", "CHN"),
    arm = "SEX",
    strata = "RACE",
    methods = list(
      comp_stat_ci = "or_cmh",
      pval = "",
      prop_ci = "wald"
    ),
    formats = list(
      prop_ci = jjcsformat_xx("xx.% - xx.%"),
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)

  dm$AGEOVER50 <- dm$AGE > 50
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(
      ARM = "Overall",
      comp_stat_ci = "comp_stat_ci"
    )))
  )
  result <- resp01_acfun(
    dm,
    labelstr = "bla",
    .alt_df = dm,
    .var = "AGEOVER50",
    .spl_context = fake_spl_context,
    conf_level = 0.9,
    include_comp = TRUE,
    arm = "SEX",
    strata = "RACE",
    methods = list(
      comp_stat_ci = "or_cmh",
      pval = "",
      prop_ci = "wald"
    ),
    formats = list(
      prop_ci = jjcsformat_xx("xx.% - xx.%"),
      comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0.05)
    )
  )
  expect_snapshot(result)
})
