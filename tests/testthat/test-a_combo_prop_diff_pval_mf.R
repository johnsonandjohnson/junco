test_that("a_combo_prop_diff_pval_mf works as expected", {
  adrs_f <- formatters::ex_adrs
  adrs_f <- subset(
    adrs_f,
    ARM %in% c("A: Drug X", "B: Placebo") & PARAMCD == "BESRSPI"
  )
  adrs_f$ARM <- droplevels(adrs_f$ARM)
  adrs_f$PARAMCD <- droplevels(adrs_f$PARAMCD)
  adrs_f$rsp <- factor(ifelse(adrs_f$AVALC == "CR", "Y", "N"))
  # Required for Risk Difference and p-value columns
  adrs_f$ARM_FOR_RISKCOLS <- adrs_f$ARM

  riskcols_combodf <- data.frame(
    valname = c("diff", "pval"),
    label = c("Risk Difference", "p-value"),
    levelcombo = c("A: Drug X", "B: Placebo"),
    exargs = I(list(list(), list()))
  )

  col_split_fun <- add_combo_levels(
    riskcols_combodf,
    keep_levels = riskcols_combodf$valname
  )

  # Additional arguments for a_combo_prop_diff_pval_mf().
  extra_args <- list(
    colpaths = list(
      prop = c("ARM", "*"),
      diff = c("ARM_FOR_RISKCOLS", "diff"),
      pval = c("ARM_FOR_RISKCOLS", "pval")
    ),
    id = "SUBJID",
    trt_var = "ARM",
    ctrl_group = "B: Placebo",
    val = "Y",
    variables = list(strata = "BMRKR2"),
    .stats = list(diff = "diff")
  )

  lyt <- basic_table(top_level_section_div = " ") |>
    split_cols_by("ARM") |>
    split_cols_by(
      "ARM_FOR_RISKCOLS",
      split_fun = col_split_fun,
      nested = FALSE
    ) |>
    analyze(
      "rsp",
      afun = a_combo_prop_diff_pval_mf,
      extra_args = extra_args
    ) |>
    split_rows_by(
      var = "SEX",
      split_label = "Gender",
      child_labels = "hidden",
      label_pos = "visible"
    ) |>
    analyze(
      vars = "rsp",
      afun = a_combo_prop_diff_pval_mf,
      extra_args = extra_args,
      show_labels = "hidden"
    )

  expect_silent(
    tbl <- build_table(lyt, adrs_f)
  )

  expect_snapshot(tbl, cran = TRUE)
})

test_that("a_combo_prop_diff_pval_mf works with custom mf_threshold", {
  data <- DM
  data <- subset(data, ARM %in% c("A: Drug X", "B: Placebo"))
  data$ARM <- droplevels(data$ARM)
  data$rsp <- factor(ifelse(data$BMRKR1 >= mean(data$BMRKR1), "Y", "N"))
  # Required for Risk Difference and p-value columns
  data$ARM_FOR_RISKCOLS <- data$ARM

  riskcols_combodf <- data.frame(
    valname = c("diff", "pval"),
    label = c("Risk Difference", "p-value"),
    levelcombo = c("A: Drug X", "B: Placebo"),
    exargs = I(list(list(), list()))
  )

  col_split_fun <- add_combo_levels(
    riskcols_combodf,
    keep_levels = riskcols_combodf$valname
  )

  # Additional arguments for a_combo_prop_diff_pval_mf().
  extra_args <- list(
    colpaths = list(
      prop = c("ARM", "*"),
      diff = c("ARM_FOR_RISKCOLS", "diff"),
      pval = c("ARM_FOR_RISKCOLS", "pval")
    ),
    id = "ID",
    trt_var = "ARM",
    ctrl_group = "B: Placebo",
    val = "Y",
    variables = list(strata = "STRATA1"),
    mf_threshold = 47,
    .stats = list(diff = "diff")
  )

  lyt <- basic_table(top_level_section_div = " ") |>
    split_cols_by("ARM") |>
    split_cols_by(
      "ARM_FOR_RISKCOLS",
      split_fun = col_split_fun,
      nested = FALSE
    ) |>
    analyze(
      "rsp",
      afun = a_combo_prop_diff_pval_mf,
      extra_args = extra_args
    )

  expect_silent(
    tbl <- build_table(lyt, data)
  )

  expect_snapshot(tbl, cran = TRUE)
})
