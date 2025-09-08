library(testthat)
library(rtables)
library(dplyr)

adsl <- ex_adsl
adae <- ex_adae
adae$TRTEMFL <- "Y"


test_that("a_freq_j with val = NA and denom option", {
  # scenario 1 : denom = N, all values
  extra_args_1 <- list(
    .stats = c("count_unique_denom_fraction")
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # apply to adae
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1b)

  # scenario 1c : denom = .N_col, all values
  extra_args_2 <- list(
    denom = "N_col",
    .stats = c("count_unique_denom_fraction")
  )
  lyt1c <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_2
    )

  tbl1c <- build_table(lyt1c, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1c)
})

test_that("a_freq_j with specific val (CHN) and denom option", {
  # scenario 1 : denom = N, all values
  extra_args_1 <- list(
    .stats = c("count_unique_denom_fraction"),
    val = "CHN"
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # apply to adae
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1b)

  # scenario 1c : denom = .N_col, all values
  extra_args_2 <- list(
    denom = "N_col",
    .stats = c("count_unique_denom_fraction"),
    val = "CHN"
  )
  lyt1c <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_2
    )

  tbl1c <- build_table(lyt1c, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1c)
})


test_that("a_freq_j with N_only", {
  # scenario 1 : denom = N, all values
  extra_args_1 <- list(
    .stats = c("count_unique")
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # apply to adae
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1b)
})


test_that("a_freq_j with TotCol_only", {
  # scenario 1 : denom = N, all values
  extra_args_1 <- list(
    .stats = c("count_unique"),
    restr_columns = "Total"
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    add_overall_col("Total") %>%
    analyze(vars = "COUNTRY", afun = a_freq_j, extra_args = extra_args_1)

  # apply to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # apply to adae
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1b)
})


test_that("a_freq_j as cfun", {
  adsl_col <- adsl[adsl$ARM == "A: Drug X", ]
  Ncol <- length(unique(adsl_col[["USUBJID"]]))

  adae_col <- adae[adae$ARM == "A: Drug X", ]
  adae_col_bs <- unique(adae_col %>% select(USUBJID, AEBODSYS))

  # scenario 1 :
  extra_args_1 <- list(
    denom = "N_col",
    .stats = c("count_unique_denom_fraction")
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by("AEBODSYS") %>%
    summarize_row_groups(
      "AEBODSYS",
      cfun = a_freq_j,
      extra_args = extra_args_1
    ) %>%
    analyze(
      vars = "AEDECOD",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adae
  tbl1 <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # scenario 2 : label using label_fstr method works
  extra_args_2 <- append(extra_args_1, list(label_fstr = "Bodysystem %s"))

  lyt2 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by("AEBODSYS") %>%
    summarize_row_groups(
      "AEBODSYS",
      cfun = a_freq_j,
      extra_args = extra_args_2
    ) %>%
    analyze(
      vars = "AEDECOD",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adae
  tbl2 <- build_table(lyt2, adae, adsl, round_type = "sas")
  expect_snapshot(tbl2)
})

test_that("a_freq_j with label map", {
  adsl_col <- adsl[adsl$ARM == "A: Drug X", ]
  Ncol <- length(unique(adsl_col[["USUBJID"]]))

  adae_col <- adae[adae$ARM == "A: Drug X", ]
  adae_col_sub <- unique(adae_col %>% select(USUBJID))

  Subjs_with_AEs <- tibble::tribble(
    ~value,
    ~label,
    "Y",
    "Subjects with >= 1 AE"
  )

  # scenario 1 : with label_map
  extra_args_1 <- list(label_map = Subjs_with_AEs)
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "TRTEMFL",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adae
  tbl1 <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # scenario 2 : set row label using label parameter
  extra_args_2 <- list(label = "Subjects with >= 1 AE")
  lyt2 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "TRTEMFL",
      afun = a_freq_j,
      extra_args = extra_args_2
    )

  tbl2 <- build_table(lyt2, adae, adsl, round_type = "sas")
  expect_snapshot(tbl2)
})


test_that("a_freq_j (old count_pats case)", {
  # scenario 1 : denom = N, all values
  extra_args_1 <- list(
    .stats = c("count_unique_denom_fraction")
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # apply to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # apply to adae
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1b)

  # scenario 1c : denom = .N_col, all values
  extra_args_2 <- list(
    denom = "N_col",
    .stats = c("count_unique_denom_fraction")
  )
  lyt1c <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_2
    )

  tbl1c <- build_table(lyt1c, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1c)
})


test_that("a_freq_j with N_subgroup as denom", {
  # scenario 1: denom = N_subgroup, all values
  extra_args_1 <- list(denom = "n_df")

  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  # applied to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # applied to adae: when denomdf is not specified in the layout,
  # it will take Nsubgroup from df, not from alt_counts_df
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1b)

  # applied to adae: with denomdf specified in layout
  # it will take Nsubgroup from this denomdf dataset
  extra_args_2 <- list(
    denom = "n_altdf",
    denom_by = c("SEX"),
    .stats = c("count_unique_denom_fraction")
  )

  lyt1c <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_2
    )

  tbl1c <- build_table(lyt1c, adae, adsl, round_type = "sas")
  expect_snapshot(tbl1c)
})


test_that("a_freq_j with N_trt as denom - special situation", {
  "Only in special layout"

  #### columns : ARM / Severity (including a total column)
  #### derivation of max severity by body system and by pt

  ### spanning header for severity

  adsl_ <- ex_adsl %>% select(USUBJID, ARM)
  adae_ <- ex_adae %>% select(USUBJID, ARM, AEBODSYS, AEDECOD, AESEV)
  adae_$TRTEMFL <- "Y"

  trtvar <- "ARM"

  ## Total column on adsl
  adsl_ <- adsl_ %>%
    mutate(ASEV = factor("Total", levels = c("Total", levels(adae$AESEV))))
  adsl_$spanheader <- factor(
    ifelse(adsl_$ASEV == "Total", " ", "Severity"),
    levels = c(" ", "Severity")
  )

  ## ae : max severity per subject per SOC
  adaetot <- adae_ %>%
    mutate(
      AESEV = "Total",
      AEBODSYSx = AEBODSYS
    ) %>%
    arrange(USUBJID, AEBODSYS, AEDECOD) %>%
    group_by(USUBJID, AEBODSYS, AEDECOD) %>%
    slice(1) %>%
    ungroup()

  # Take maximum severity - per PT
  adaemaxpt <- adae_ %>%
    filter(toupper(AESEV) %in% toupper(c("Mild", "Moderate", "Severe"))) %>%
    mutate(
      AESEVN = case_when(
        toupper(AESEV) == "MILD" ~ 3,
        toupper(AESEV) == "MODERATE" ~ 2,
        toupper(AESEV) == "SEVERE" ~ 1
      )
    ) %>%
    arrange(USUBJID, AEBODSYS, AEDECOD, AESEVN) %>%
    group_by(USUBJID, AEBODSYS, AEDECOD) %>%
    slice(1) %>%
    ungroup()

  # Take maximum severity - per SOC
  adaemaxsoc <- adae_ %>%
    filter(toupper(AESEV) %in% toupper(c("Mild", "Moderate", "Severe"))) %>%
    mutate(
      AESEVN = case_when(
        toupper(AESEV) == "MILD" ~ 3,
        toupper(AESEV) == "MODERATE" ~ 2,
        toupper(AESEV) == "SEVERE" ~ 1
      ),
      AEBODSYSx = AEBODSYS
    ) %>%
    arrange(USUBJID, AEBODSYS, AESEVN) %>%
    group_by(USUBJID, AEBODSYS) %>%
    slice(1) %>%
    ungroup() %>%
    select(USUBJID, AEBODSYS, AESEV, AEBODSYSx)

  # Merge back in an create a new SOC variable that is only populated
  # for max severity SOC rows
  adaemax <- left_join(
    adaemaxpt,
    adaemaxsoc,
    by = c("USUBJID", "AEBODSYS", "AESEV")
  )

  # Add total
  adaetot <- adae_ %>%
    mutate(
      AESEV = "Total",
      AEBODSYSx = AEBODSYS
    ) %>%
    arrange(USUBJID, AEBODSYS, AEDECOD) %>%
    group_by(USUBJID, AEBODSYS, AEDECOD) %>%
    slice(1) %>%
    ungroup()

  # Set data together
  adaeall <- bind_rows(adaemax, adaetot) %>%
    mutate(
      ASEV = factor(
        as.character(AESEV),
        levels = c("Total", levels(ex_adae$AESEV))
      )
    ) %>%
    select(USUBJID, TRTEMFL, ASEV, AEBODSYS, AEBODSYSx, AEDECOD)

  adaeall <- adaeall %>%
    inner_join(., adsl_ %>% select(USUBJID, ARM), by = c("USUBJID"))

  adaeall$spanheader <- factor(
    ifelse(adaeall$ASEV == "Total", " ", "Severity"),
    levels = c(" ", "Severity")
  )

  ### layout using the denom = N_trt option
  ### note that in this case also the argument trtvar should be set
  extra_args_1 <- list(
    denom = "N_colgroup",
    colgroup = "ARM",
    riskdiff = FALSE
  )
  lyt <- basic_table() %>%
    split_cols_by(trtvar, show_colcounts = TRUE) %>%
    split_cols_by("spanheader", split_fun = trim_levels_in_group("ASEV")) %>%
    split_cols_by("ASEV", show_colcounts = TRUE) %>%
    split_rows_by(
      "AEBODSYS",
      split_label = "System Organ Class",
      split_fun = trim_levels_in_group("AEDECOD"),
      label_pos = "topleft",
      section_div = c(" ")
    ) %>%
    summarize_row_groups(
      "AEBODSYSx",
      cfun = a_freq_j,
      extra_args = extra_args_1
    )

  ## main focus of this test is on the denominator
  tbl <- build_table(lyt, adaeall, alt_counts_df = adsl_, round_type = "sas")
  expect_snapshot(tbl)

  ## additionally check if the denominator values are as expected
  sub_tbl <- tbl[, "A: Drug X"]
  res1 <- cell_values(sub_tbl["cl A.", seq_len(ncol(sub_tbl))])
  denoms <- unname(unlist(lapply(res1, function(x) x[[2]])))
  Ncol <- length(unique(adsl[adsl$ARM == "A: Drug X", ][["USUBJID"]]))
  denoms_expected <- rep(Ncol, ncol(sub_tbl))

  # equal but not identical - switch to equal for this one
  expect_equal(denoms, denoms_expected)
})


test_that("a_freq_j with keep_levels (CHN, NGA) ", {
  # scenario 1 : denom = N, all values
  extra_args_1 <- list(
    denom = "N_col",
    val = c("CHN", "NGA")
  )
  lyt1 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze(vars = "COUNTRY", afun = a_freq_j, extra_args = extra_args_1)

  # apply to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  expect_snapshot(tbl1)

  # Also keep the original test to verify the specific row names
  result <- row.names(tbl1)
  expected <- c("CHN", "NGA")
  expect_identical(result, expected)
})
