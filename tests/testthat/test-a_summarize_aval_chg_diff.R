library(testthat)
library(dplyr)

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

  ADEG <- ADEG %>%
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

  lyt <- basic_table() %>%
    ### first columns
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) %>%
    split_cols_by("TRT01A") %>%
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Blood Pressure",
      section_div = " ",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      "AVISIT",
      label_pos = "topleft",
      split_label = "Study Visit",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) %>%
    ## set up a 3 column split
    split_cols_by_multivar(
      c("AVAL", "AVAL", "CHG"),
      varlabels = c("n/N (%)", "Mean (CI)", "CFB (CI)")
    ) %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      "TRT01A",
      split_fun = remove_split_levels("Placebo"),
      labels_var = "rrisk_label"
    ) %>%
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(c("CHG"), varlabels = c(" ")) %>%
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

  ADEG <- ADEG %>%
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

  lyt <- basic_table() %>%
    ### first columns
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) %>%
    split_cols_by("TRT01A") %>%
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Blood Pressure",
      section_div = " ",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      "AVISIT",
      label_pos = "topleft",
      split_label = "Study Visit",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) %>%
    ## set up a 3 column split
    split_cols_by_multivar(
      c("AVAL", "AVAL", "CHG"),
      varlabels = c("n/N (%)", "Mean (CI)", "CFB (CI)")
    ) %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      "TRT01A",
      split_fun = remove_split_levels("Placebo"),
      labels_var = "rrisk_label"
    ) %>%
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(c("CHG"), varlabels = c(" ")) %>%
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
