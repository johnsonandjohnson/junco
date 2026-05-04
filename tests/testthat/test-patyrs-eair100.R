library(testthat)
library(rtables)
library(dplyr)

ref_path <- c("ARM", "B: Placebo")

adsl <- ex_adsl |>
  mutate(TRTDURY = substring(USUBJID, nchar(USUBJID) - 3 + 1), "-", "") |>
  mutate(TRTDURY = sub("-", "", TRTDURY)) |>
  mutate(TRTDURY = sub("d", "", TRTDURY)) |>
  mutate(TRTDURY = as.numeric(TRTDURY)) |>
  mutate(TRTDURY2 = TRTDURY + 25) |>
  select(USUBJID, ARM, COUNTRY, STRATA1, TRTDURY, TRTDURY2, SEX)

adae <- ex_adae |>
  select(USUBJID, AEDECOD, AEBODSYS, ASTDY)

adae$TRTEMFL <- "Y"

# set up occurrence flag for first occurrence of event
adaefirst <- adae |>
  arrange(USUBJID, AEBODSYS, AEDECOD, ASTDY) |>
  group_by(USUBJID, AEBODSYS, AEDECOD) |>
  slice(1) |>
  ungroup() |>
  mutate(AOCCPFL = "Y") |>
  select(USUBJID, AEBODSYS, AEDECOD, ASTDY, AOCCPFL)

adae <- left_join(
  adae,
  adaefirst,
  by = c("USUBJID", "AEBODSYS", "AEDECOD", "ASTDY")
)

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"

adsl$colspan_trt <- factor(
  ifelse(adsl[["ARM"]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


adsl$rrisk_header <- "Risk Difference (95% CI)"
adsl$rrisk_label <- paste(adsl[["ARM"]], "vs Placebo")

adae <- left_join(adsl, adae, by = "USUBJID") |>
  mutate(ASTDY2 = ASTDY + 10)

core_lyt <- basic_table(show_colcounts = FALSE, round_type = "sas") |>
  split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) |>
  split_cols_by("ARM") |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(
    "ARM",
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  )


#### Actual start of tests

test_that("Check patient years numbers are giving expected result", {
  extra_args <- list(
    label = c("Subject years\u1D43")
  )

  lyt1 <- core_lyt |>
    analyze(
      "TRTDURY",
      nested = FALSE,
      afun = a_patyrs_j,
      extra_args = extra_args
    )
  tbl1 <- build_table(lyt1, adae, adsl)

  res1 <- cell_values(tbl1[c("TRTDURY", "patyrs"), "A: Drug X"])
  result <- as.numeric(unlist(unname(res1))[[1]])

  adae_sub <- adae |>
    filter(ARM == "A: Drug X") |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup()

  expected <- sum(adae_sub$TRTDURY)

  expect_identical(
    result,
    expected
  )
})

test_that("Check aeir100 numbers are giving expected result", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      nested = FALSE,
      inclNAs = TRUE,
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path
      )
    )
  tbl1 <- build_table(lyt1, adae, adsl)

  res1 <- cell_values(tbl1["dcd A.1.1.1.1", "A: Drug X"])
  result <- as.numeric(unlist(unname(res1))[[1]])

  adae_onecode <- adae |>
    filter(AEDECOD == "dcd A.1.1.1.1" & !is.na(AOCCPFL)) |>
    select(USUBJID, AEDECOD, AEBODSYS, ASTDY, TRTEMFL, AOCCPFL)

  adae_sub <- left_join(adsl, adae_onecode, by = "USUBJID")

  adae_sub <- adae_sub |>
    filter(ARM == "A: Drug X") |>
    arrange(USUBJID, AEDECOD, ASTDY) |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup() |>
    mutate(EXP_TIME = if_else(!is.na(ASTDY), (ASTDY / 365.25), TRTDURY))

  total_exp_years <- sum(adae_sub$EXP_TIME)

  adae_sub2 <- adae |>
    filter(
      AEDECOD == "dcd A.1.1.1.1" & ARM == "A: Drug X" & !is.na(AOCCPFL)
    ) |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup()

  number_with_event <- nrow(adae_sub2)

  expected <- (100 * number_with_event) / total_exp_years

  expect_equal(
    result,
    expected
  )
})

test_that("Check aeir100 numbers are giving expected result when fup_var argument is changed", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      nested = FALSE,
      inclNAs = TRUE,
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY2",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path
      )
    )
  tbl1 <- build_table(lyt1, adae, adsl)

  res1 <- cell_values(tbl1["dcd A.1.1.1.1", "A: Drug X"])
  result <- as.numeric(unlist(unname(res1))[[1]])

  adae_onecode <- adae |>
    filter(AEDECOD == "dcd A.1.1.1.1" & !is.na(AOCCPFL)) |>
    select(USUBJID, AEDECOD, AEBODSYS, ASTDY, TRTEMFL, AOCCPFL)

  adae_sub <- left_join(adsl, adae_onecode, by = "USUBJID")

  adae_sub <- adae_sub |>
    filter(ARM == "A: Drug X") |>
    arrange(USUBJID, AEDECOD, ASTDY) |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup() |>
    mutate(EXP_TIME = if_else(!is.na(ASTDY), (ASTDY / 365.25), TRTDURY2))

  total_exp_years <- sum(adae_sub$EXP_TIME)

  adae_sub2 <- adae |>
    filter(
      AEDECOD == "dcd A.1.1.1.1" & ARM == "A: Drug X" & !is.na(AOCCPFL)
    ) |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup()

  number_with_event <- nrow(adae_sub2)

  expected <- (100 * number_with_event) / total_exp_years

  expect_equal(
    result,
    expected
  )
})

test_that("Check aeir100 numbers are giving expected result when occ_dy argument is changed", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      nested = FALSE,
      inclNAs = TRUE,
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY2",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY2",
        ref_path = ref_path
      )
    )
  tbl1 <- build_table(lyt1, adae, adsl)

  res1 <- cell_values(tbl1["dcd A.1.1.1.1", "A: Drug X"])
  result <- as.numeric(unlist(unname(res1))[[1]])

  adae_onecode <- adae |>
    filter(AEDECOD == "dcd A.1.1.1.1" & !is.na(AOCCPFL)) |>
    select(USUBJID, AEDECOD, AEBODSYS, ASTDY2, TRTEMFL, AOCCPFL)

  adae_sub <- left_join(adsl, adae_onecode, by = "USUBJID")

  adae_sub <- adae_sub |>
    filter(ARM == "A: Drug X") |>
    arrange(USUBJID, AEDECOD, ASTDY2) |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup() |>
    mutate(EXP_TIME = if_else(!is.na(ASTDY2), (ASTDY2 / 365.25), TRTDURY2))

  total_exp_years <- sum(adae_sub$EXP_TIME)

  adae_sub2 <- adae |>
    filter(
      AEDECOD == "dcd A.1.1.1.1" & ARM == "A: Drug X" & !is.na(AOCCPFL)
    ) |>
    group_by(USUBJID) |>
    slice(1) |>
    ungroup()

  number_with_event <- nrow(adae_sub2)

  expected <- (100 * number_with_event) / total_exp_years

  expect_equal(
    result,
    expected
  )
})

test_that("Check aeir100 numbers are giving expected result relative risk in combined facet is available", {

  colspan_trt_map <- create_colspan_map(
    adsl,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )



  # Set up levels and label for the required combined columns
  comb_group <- setdiff(unique(adsl[[trtvar]]), ctrl_grp)
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = comb_group
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit_comb <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo2 <- cond_rm_facets(
    facets = ctrl_grp,
    ancestor_pos = NA,
    value = "Risk Difference (95% CI)",
    split = "rrisk_header"
  )
  add_combo2 <- add_combo_facet(
    "Combined",
    label = paste0("Combined vs ", ctrl_grp),
    levels = comb_group
  )

  mysplit_comb2 <- make_split_fun(post = list(add_combo2, rm_combo_from_placebo2))
  ################################################################################
  # Define layout and build table:
  ################################################################################

  lyt <- basic_table(
    show_colcounts = TRUE,
    colcount_format = "N=xx",
    top_level_section_div = " "
  ) |>
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) |>
    split_cols_by(trtvar, split_fun = mysplit_comb) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = mysplit_comb2
    ) |>
    analyze(
      "TRTDURY",
      nested = FALSE,
      show_labels = "hidden",
      afun = a_patyrs_j,
      extra_args = list(.labels = c(patyrs = "Subject years\u1D43"))
    ) |>
    analyze(
      vars = "AEDECOD",
      nested = FALSE,
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path,
        drop_levels = TRUE
      )
    ) |>
    append_topleft("Preferred Term, EAIR Per 100 SY")


  result <- build_table(lyt, adae, alt_counts_df = adsl)
  result

  ################################################################################
  # actual comparison testthat code
  ################################################################################
  # get numbers from single sel_AEDECOD, like dcd A.1.1.1.1, selected treatment group
  eair_numbers <- function(adae, adsl, sel_AEDECOD, comb_group) {
    adae_onecode <- adae |>
      filter(AEDECOD == sel_AEDECOD & !is.na(AOCCPFL)) |>
      select(USUBJID, AEDECOD, AOCCPFL, ASTDY)

    adae_sub <- left_join(adsl, adae_onecode, by = "USUBJID")

    adae_sub <- adae_sub |>
      filter(!!rlang::sym(trtvar) %in% comb_group) |>
      arrange(USUBJID, AEDECOD, ASTDY) |>
      group_by(USUBJID) |>
      slice(1) |>
      ungroup() |>
      mutate(EXP_TIME = if_else(!is.na(ASTDY), (ASTDY / 365.25), TRTDURY))

    total_exp_years <- sum(adae_sub$EXP_TIME)

    adae_sub2 <- adae |>
      filter(
        AEDECOD == sel_AEDECOD & !!rlang::sym(trtvar) %in% comb_group & !is.na(AOCCPFL)
      ) |>
      group_by(USUBJID) |>
      slice(1) |>
      ungroup()

    number_with_event <- nrow(adae_sub2)

    expected <- (100 * number_with_event) / total_exp_years

    list(
      count = number_with_event,
      texpy = total_exp_years,
      eair = (100 * number_with_event) / total_exp_years
    )
  }

  # dcd A.1.1.1.1 is the second row in the result object
  actual_row <- result[2, ]
  actual_row

  eair_numbers_comb <- eair_numbers(adae, adsl, "dcd A.1.1.1.1", comb_group)
  eair_numbers_ctrl <- eair_numbers(adae, adsl, "dcd A.1.1.1.1", ctrl_grp)
  rdiff_eair <- function(eair_numbers_comb,
                         eair_numbers_ctrl, conf_level = 0.95) {
    rdiff <- eair_numbers_comb[["eair"]] - eair_numbers_ctrl[["eair"]]
    sd <- sqrt(
      eair_numbers_comb[["count"]] / eair_numbers_comb[["texpy"]]^2 +
        eair_numbers_ctrl[["count"]] / eair_numbers_ctrl[["texpy"]]^2
    ) * 100

    coeff <- stats::qnorm((1 + conf_level) / 2)
    lcl <- rdiff - (coeff * sd)
    ucl <- rdiff + (coeff * sd)

    eair_diff <- c(rdiff, lcl, ucl)
  }
  rdiff <- rdiff_eair(eair_numbers_comb, eair_numbers_ctrl)

  # actual comparisons
  # eair for combined
  expect_equal(
    cell_values(actual_row[, 3])[[1]],
    eair_numbers_comb[["eair"]],
    ignore_attr = TRUE
  )

  # eair for ctrl
  expect_equal(
    cell_values(actual_row[, 4])[[1]],
    eair_numbers_ctrl[["eair"]],
    ignore_attr = TRUE
  )

  # diff in eair for combined vs ctrl
  expect_equal(
    cell_values(actual_row[, 7])[[1]],
    rdiff,
    ignore_attr = TRUE
  )
})
