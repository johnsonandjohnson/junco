library(testthat)
library(rtables)
library(dplyr)
library(tern)

test_that("a_summarize_aval_chg_diff_j comp_btw_group = FALSE works as expected", {
  adsl <- ex_adsl
  advs <- ex_advs |>
    filter(PARAMCD %in% c("DIABP", "PULSE")) |>
    filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) |>
    mutate(
      PARAMCD = droplevels(PARAMCD),
      AVISIT = droplevels(AVISIT)
    )

  multivars <- c("AVAL", "AVAL", "CHG")
  extra_args_3col <- list(
    d = 1,
    ancova = FALSE,
    comp_btw_group = TRUE,
    ref_path = c("ARM", "B: Placebo"),
    multivars = multivars
  )


  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    split_rows_by("PARAMCD") |>
    split_rows_by("AVISIT", child_labels = "hidden") |>
    split_cols_by_multivar(multivars,
      varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
    ) |>
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)

  tbl <- expect_silent(build_table(lyt, advs, alt_counts_df = adsl))
  expect_s4_class(tbl, "TableTree")
  expect_snapshot(cran = TRUE, tbl)

  # this one also ran fine prior hotfix78
  extra_args_3col2 <- list(
    d = 1,
    ancova = FALSE,
    comp_btw_group = FALSE,
    multivars = multivars
  )


  lyt2 <- basic_table() |>
    split_cols_by("ARM") |>
    split_rows_by("PARAMCD") |>
    split_rows_by("AVISIT", child_labels = "hidden") |>
    split_cols_by_multivar(multivars,
      varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
    ) |>
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col2)

  tbl2 <- build_table(lyt2, advs, alt_counts_df = adsl)
  expect_s4_class(tbl2, "TableTree")
  expect_snapshot(cran = TRUE, tbl2)
})

test_that("a_summarize_aval_chg_diff_j t-test sparse data works as expected", {
  ctrl_grp <- "B: Placebo"
  trtvar <- "ARM"

  adsl <- ex_adsl |>
    select(STUDYID, USUBJID, ARM) |>
    mutate(
      colspan_trt = factor(ifelse(ARM == ctrl_grp, " ", "Active treatment"),
        levels = c(" ", "Active treatment")
      ),
      rrisk_header = "Difference in Mean Change (95% CI)",
      rrisk_label = paste0(ARM, " vs ", ctrl_grp)
    )

  advs <- ex_advs |>
    filter(PARAMCD %in% c("DIABP", "PULSE")) |>
    filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) |>
    mutate(
      PARAMCD = droplevels(PARAMCD),
      AVISIT = droplevels(AVISIT)
    )

  advs <- advs |>
    inner_join(adsl, by = join_by(STUDYID, USUBJID, ARM))

  # introduce sparse data for DIABP at WEEK 1 DAY 8
  # keep 2 records in A: Drug X, and 2 records in B: Placebo with values c(50, 50) and c(45, 45)
  select_sub <- adsl |>
    group_by(ARM) |>
    slice_head(n = 2) |>
    ungroup() |>
    pull(USUBJID)

  advs_2 <- advs |>
    mutate(AVAL = case_when(
      PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & !(USUBJID %in% select_sub) ~ NA_real_,
      PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "A: Drug X" ~ 50,
      PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "B: Placebo" ~ 45,
      TRUE ~ AVAL
    )) |>
    mutate(CHG = case_when(
      PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & !(USUBJID %in% select_sub) ~ NA_real_,
      PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "A: Drug X" ~ -5,
      PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "B: Placebo" ~ -7,
      TRUE ~ CHG
    ))

  colspan_trt_map <- create_colspan_map(adsl,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active treatment",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )


  multivars <- c("AVAL", "AVAL", "CHG")
  extra_args_3col <- list(
    d = 1,
    ancova = FALSE,
    comp_btw_group = TRUE,
    ref_path = c("colspan_trt", " ", "ARM", "B: Placebo"),
    multivars = multivars
  )


  lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by("ARM", show_colcounts = TRUE) |>
    split_rows_by("PARAMCD") |>
    split_rows_by("AVISIT", child_labels = "hidden") |>
    split_cols_by_multivar(multivars,
      varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
    ) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by("ARM",
      split_fun = remove_split_levels(ctrl_grp), labels_var = "rrisk_label",
      show_colcounts = FALSE
    ) |>
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(multivars[3], varlabels = c(" ")) |>
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)

  tbl <- build_table(lyt, advs, alt_counts_df = adsl)
  expect_s4_class(tbl, "TableTree")
  expect_snapshot(cran = TRUE, tbl)

  tbl2 <- build_table(lyt, advs_2, alt_counts_df = adsl)
  expect_s4_class(tbl2, "TableTree")
  expect_snapshot(cran = TRUE, tbl2)
})



test_that("a_summarize_aval_chg_diff_j works as expected", {
  # Create test data as shown in the example
  ADEG <- data.frame(
    STUDYID = c(
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY"
    ),
    USUBJID = c(
      "XXXXX01",
      "XXXXX02",
      "XXXXX03",
      "XXXXX04",
      "XXXXX05",
      "XXXXX06",
      "XXXXX07",
      "XXXXX08",
      "XXXXX09",
      "XXXXX10"
    ),
    TRT01A = c(
      "ARMA",
      "ARMA",
      "ARMA",
      "ARMA",
      "ARMA",
      "Placebo",
      "Placebo",
      "Placebo",
      "ARMA",
      "ARMA"
    ),
    PARAM = c("BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP"),
    AVISIT = c(
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1"
    ),
    AVAL = c(56, 78, 67, 87, 88, 93, 39, 87, 65, 55),
    CHG = c(2, 3, -1, 9, -2, 0, 6, -2, 5, 2)
  )

  ADEG <- ADEG |>
    mutate(
      TRT01A = as.factor(TRT01A),
      STUDYID = as.factor(STUDYID)
    )

  ADEG$colspan_trt <- factor(
    ifelse(ADEG$TRT01A == "Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  ADEG$rrisk_header <- "Risk Difference (%) (95% CI)"
  ADEG$rrisk_label <- paste(ADEG$TRT01A, paste("vs", "Placebo"))

  colspan_trt_map <- create_colspan_map(
    ADEG,
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "TRT01A"
  )

  ## for coverage
  colspan_trt_maprev <- create_colspan_map(
    ADEG,
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "TRT01A",
    active_first = FALSE
  )

  expect_equal(colspan_trt_map, colspan_trt_maprev[2:1, ], ignore_attr = TRUE)

  ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")

  lyt <- basic_table(round_type = "sas") |>
    ### first columns
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) |>
    split_cols_by("TRT01A") |>
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Blood Pressure",
      section_div = " ",
      split_fun = drop_split_levels
    ) |>
    split_rows_by(
      "AVISIT",
      label_pos = "topleft",
      split_label = "Study Visit",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) |>
    ## set up a 3 column split
    split_cols_by_multivar(
      c("AVAL", "AVAL", "CHG"),
      varlabels = c("n/N (%)", "Mean (CI)", "CFB (CI)")
    ) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      "TRT01A",
      split_fun = remove_split_levels("Placebo"),
      labels_var = "rrisk_label"
    ) |>
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(c("CHG"), varlabels = c(" ")) |>
    ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
    ### the function a_summarize_aval_chg_diff_j grabs the required vars from cols_by_multivar calls
    analyze(
      "STUDYID",
      afun = a_summarize_aval_chg_diff_j,
      extra_args = list(
        format_na_str = "-",
        d = 0,
        ref_path = ref_path,
        variables = list(arm = "TRT01A", covariates = NULL)
      )
    )

  # Test that the table builds without errors
  result <- expect_no_error(build_table(lyt, ADEG))

  # Check that the result is a valid rtable
  expect_s4_class(result, "TableTree")

  # Check that the table has the expected structure
  expect_equal(ncol(result), 7) # 3 columns for ARMA, 3 for Placebo, 1 for Blood Pressure
})

test_that("a_summarize_aval_chg_diff_j works with ancova = TRUE", {
  # Create test data as shown in the example
  ADEG <- data.frame(
    STUDYID = c(
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY",
      "DUMMY"
    ),
    USUBJID = c(
      "XXXXX01",
      "XXXXX02",
      "XXXXX03",
      "XXXXX04",
      "XXXXX05",
      "XXXXX06",
      "XXXXX07",
      "XXXXX08",
      "XXXXX09",
      "XXXXX10"
    ),
    TRT01A = c(
      "ARMA",
      "ARMA",
      "ARMA",
      "ARMA",
      "ARMA",
      "Placebo",
      "Placebo",
      "Placebo",
      "ARMA",
      "ARMA"
    ),
    PARAM = c("BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP"),
    AVISIT = c(
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1",
      "Visit 1"
    ),
    AVAL = c(56, 78, 67, 87, 88, 93, 39, 87, 65, 55),
    CHG = c(2, 3, -1, 9, -2, 0, 6, -2, 5, 2)
  )

  ADEG <- ADEG |>
    mutate(
      TRT01A = as.factor(TRT01A),
      STUDYID = as.factor(STUDYID)
    )

  ADEG$colspan_trt <- factor(
    ifelse(ADEG$TRT01A == "Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  ADEG$rrisk_header <- "Risk Difference (%) (95% CI)"
  ADEG$rrisk_label <- paste(ADEG$TRT01A, paste("vs", "Placebo"))

  colspan_trt_map <- create_colspan_map(
    ADEG,
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "TRT01A"
  )
  ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")

  lyt <- basic_table(round_type = "sas") |>
    ### first columns
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) |>
    split_cols_by("TRT01A") |>
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Blood Pressure",
      section_div = " ",
      split_fun = drop_split_levels
    ) |>
    split_rows_by(
      "AVISIT",
      label_pos = "topleft",
      split_label = "Study Visit",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) |>
    ## set up a 3 column split
    split_cols_by_multivar(
      c("AVAL", "AVAL", "CHG"),
      varlabels = c("n/N (%)", "Mean (CI)", "CFB (CI)")
    ) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      "TRT01A",
      split_fun = remove_split_levels("Placebo"),
      labels_var = "rrisk_label"
    ) |>
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(c("CHG"), varlabels = c(" ")) |>
    ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
    ### the function a_summarize_aval_chg_diff_j grabs the required vars from cols_by_multivar calls
    analyze(
      "STUDYID",
      afun = a_summarize_aval_chg_diff_j,
      extra_args = list(
        format_na_str = "-",
        d = 0,
        ref_path = ref_path,
        variables = list(arm = "TRT01A", covariates = NULL),
        ancova = TRUE # Set ancova = TRUE to test the else branch
      )
    )

  # Test that the table builds without errors
  result <- expect_no_error(build_table(lyt, ADEG))

  # Check that the result is a valid rtable
  expect_s4_class(result, "TableTree")

  # Check that the table has the expected structure
  expect_equal(ncol(result), 7) # 3 columns for ARMA, 3 for Placebo, 1 for Blood Pressure
})

test_that("s_summarize_desc_j does not fail for almost constant data", {
  df <- data.frame(
    "AVAL" = c(
      1.709999999999999964473,
      1.710000000000000186517,
      1.710000000000000186517,
      1.710000000000000186517,
      1.710000000000000186517
    )
  )

  stats <- s_summarize_desc_j(df, "AVAL", .ref_group = df, .in_ref_col = FALSE)
  expect_no_error(stats)
})

test_that("s_summarize_desc_j with empty vectors", {
  df <- data.frame(
    "AVAL" = rep(NA_real_, 3)
  )
  expect_error(t.test(df[["AVAL"]], df[["AVAL"]]))

  stats <- s_summarize_desc_j(df, "AVAL", .ref_group = df, .in_ref_col = FALSE)
  expect_equal(stats[["mean_diffci"]], rep(NA_real_, 3), ignore_attr = TRUE)
})

test_that("a_summarize_aval_chg_diff_j ancova in a combined column work as expected", {
  ctrl_grp <- "B: Placebo"
  adsl <- ex_adsl
  adsl$rrisk_header <- "Difference in Mean Change (95% CI)"
  adsl$rrisk_label <- paste(adsl[["ARM"]], paste("vs", ctrl_grp))
  adsl$colspan_trt_dummy <- " "
  adsl <- adsl |> select(USUBJID, ARM, rrisk_header, rrisk_label, colspan_trt_dummy)

  advs <- ex_advs |>
    filter(PARAMCD %in% c("DIABP", "PULSE")) |>
    filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) |>
    mutate(
      PARAMCD = droplevels(PARAMCD),
      AVISIT = droplevels(AVISIT)
    ) |>
    left_join(adsl, by = c("USUBJID", "ARM"))

  multivars <- c("AVAL", "AVAL", "CHG")
  extra_args_3col <- list(
    d = 1,
    ancova = TRUE,
    comp_btw_group = TRUE,
    ref_path = c("ARM", "B: Placebo"),
    multivars = multivars,
    variables = list(arm = "ARM", covariates = NULL),
    method_combo = "contrasts",
    weights_combo = "equal"
  )

  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined A + C",
    levels = c("A: Drug X", "C: Combination")
  )

  split_fun1 <- make_split_fun(post = list(add_combo))

  add_combo2 <- add_combo_facet(
    "Combined",
    label = "Combined A + C vs B: Placebo",
    levels = c("A: Drug X", "C: Combination")
  )
  rm_placebo_from_rdiff <- cond_rm_facets(
    facets = "B: Placebo",
    ancestor_pos = -1,
    value = "Difference in Mean Change (95% CI)",
    split = "rrisk_header",
    keep_matches = FALSE
  )

  split_fun2 <- make_split_fun(post = list(add_combo2, rm_placebo_from_rdiff))

  lyt <- basic_table() |>
    split_cols_by("colspan_trt_dummy") |>
    split_cols_by("ARM", split_fun = split_fun1) |>
    split_cols_by_multivar(
      multivars,
      varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
    ) |>
    ### restart for the rrisk_header columns - note the nested = FALSE option
    ### also note the child_labels = "hidden" in both PARAM and AVISIT
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      "ARM",
      split_fun = split_fun2,
      labels_var = "rrisk_label",
      colcount_format = "N=xx"
    ) |>
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(multivars[3], varlabels = c(" "))

  lyt <- lyt |>
    split_rows_by("PARAMCD") |>
    split_rows_by("AVISIT", child_labels = "hidden") |>
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)

  tbl <- expect_silent(build_table(lyt, advs, alt_counts_df = adsl))
  expect_s4_class(tbl, "TableTree")
  expect_snapshot(tbl)
})
