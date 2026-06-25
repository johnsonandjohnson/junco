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


rdiff_eair <- function(
  eair_numbers_comb,
  eair_numbers_ctrl, conf_level = 0.95
) {
  rdiff <- eair_numbers_comb[["eair"]] - eair_numbers_ctrl[["eair"]]
  se <- sqrt(
    eair_numbers_comb[["count"]] / eair_numbers_comb[["texpy"]]^2 +
      eair_numbers_ctrl[["count"]] / eair_numbers_ctrl[["texpy"]]^2
  ) * 100

  coeff <- stats::qnorm((1 + conf_level) / 2)
  lcl <- rdiff - (coeff * se)
  ucl <- rdiff + (coeff * se)

  eair_diff <- c(rdiff, lcl, ucl)
}

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

test_that("Check a_eair100_j numbers are giving expected result", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
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

test_that("Check a_eair100_j numbers are giving expected result when fup_var argument is changed", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
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

test_that("Check a_eair100_j numbers are giving expected result when occ_dy argument is changed", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
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

test_that("Check a_eair100_j numbers are giving expected result relative risk in combined facet is available", {
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
      show_labels = "hidden",
      afun = a_patyrs_j,
      extra_args = list(.labels = c(patyrs = "Subject years\u1D43"))
    ) |>
    analyze(
      vars = "AEDECOD",
      afun = a_eair100_j,
      nested = FALSE,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path,
        drop_levels = TRUE,
        row_labels_adj = TRUE
      )
    ) |>
    append_topleft("Preferred Term, EAIR Per 100 SY")


  result <- build_table(lyt, adae, alt_counts_df = adsl)
  result

  ################################################################################
  # actual comparison testthat code
  ################################################################################

  # dcd A.1.1.1.1 is the second row in the result object
  actual_row <- result[2, ]
  actual_row

  eair_numbers_comb <- eair_numbers(adae, adsl, "dcd A.1.1.1.1", comb_group)
  eair_numbers_ctrl <- eair_numbers(adae, adsl, "dcd A.1.1.1.1", ctrl_grp)


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

test_that("Check a_eair100_j function stops with incorrect input", {
  extra_record <- adae[which(adae[["AEDECOD"]] == "dcd A.1.1.1.1")[1], ]
  extra_record$ASTDY <- extra_record$ASTDY + 1
  adae_incorrect <- rbind(
    adae,
    extra_record
  )
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path
      )
    )

  expect_error(
    tbl1 <- build_table(lyt1, adae_incorrect, adsl),
    "Input dataset must uniquely identify one record per subject/.var/occ_var."
  )
})

test_that("Check a_eair100_j function does not allow occ_var = NULL", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = NULL,
        occ_dy = "ASTDY",
        ref_path = ref_path
      )
    )
  expect_no_error(tbl1 <- build_table(lyt1, adae, adsl))
})

test_that("Check a_eair100_j function request alt_counts_df", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path
      )
    )
  expect_error(
    tbl1 <- build_table(lyt1, adae),
    ".alt_df_full cannot be NULL. Specify `alt_counts_df`"
  )
})

test_that("Check a_eair100_j function does perform variable existence check", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "fake",
        occ_dy = "ASTDY",
        ref_path = ref_path
      )
    )
  expect_error(tbl1 <- build_table(lyt1, adae, adsl),
    "Assertion on 'colnames(df)' failed",
    fixed = TRUE
  )
})

test_that("Check a_eair100_j with occ_var NULL and count_multiple_events", {
  lyt1 <- core_lyt |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = NULL,
        occ_dy = NULL,
        ref_path = ref_path,
        .stats = c("person_years", "n_eair"),
        .formats = c("n_eair" = jjcsformat_xx("xx (xx.xxx)")),
        .labels = c("n_eair" = "n (eair per 100 SY)"),
        count_multiple_events = FALSE
      )
    )

  lyt2 <- core_lyt |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = NULL,
        occ_dy = NULL,
        ref_path = ref_path,
        .stats = c("person_years", "n_eair"),
        .formats = c("n_eair" = jjcsformat_xx("xx (xx.xxx)")),
        .labels = c("n_eair" = "n (eair per 100 SY)"),
        count_multiple_events = TRUE
      )
    )

  adae_sel <- adae |>
    filter(AEDECOD %in% c("dcd A.1.1.1.1", "dcd A.1.1.1.2")) |>
    mutate(AEDECOD = factor(as.character(AEDECOD)))

  tbl1 <- build_table(lyt1, adae_sel, adsl)
  tbl2 <- build_table(lyt2, adae_sel, adsl)

  expect_snapshot(cran = TRUE, tbl1)
  expect_snapshot(cran = TRUE, tbl2)

  # manual confirmation of some numbers
  expect_equal(
    cell_values(tbl1["dcd A.1.1.1.1.person_years"]),
    cell_values(tbl2["dcd A.1.1.1.1.person_years"])
  )

  cps <- col_paths(tbl1)
  cols <- which(grepl("Drug X", cps) & grepl("colspan_trt", cps))

  c1_py <- cell_values(tbl1["dcd A.1.1.1.1.person_years", "A: Drug X"])[[1]]
  man_py <- sum(adsl |> filter(.data[[trtvar]] == "A: Drug X") |> pull(TRTDURY))
  expect_equal(c1_py, man_py, ignore_attr = TRUE)


  c1_n_eair <- cell_values(tbl1["dcd A.1.1.1.1.n_eair", cols])[[1]]
  c2_n_eair <- cell_values(tbl2["dcd A.1.1.1.1.n_eair", cols])[[1]]

  # n part of events:
  # tbl1 : total number of subjects with aedecod
  # tbl2 : total number of events with aedecod

  man_n_event <- length(
    unique(
      adae_sel |>
        filter(.data[[trtvar]] == "A: Drug X") |>
        filter(AEDECOD == "dcd A.1.1.1.1") |>
        pull(USUBJID)
    )
  )

  man2_n_event <- length(
    adae_sel |>
      filter(.data[[trtvar]] == "A: Drug X") |>
      filter(AEDECOD == "dcd A.1.1.1.1") |>
      pull(USUBJID)
  )

  expect_equal(c1_n_eair[1], man_n_event, ignore_attr = TRUE)
  expect_equal(c2_n_eair[1], man2_n_event, ignore_attr = TRUE)

  # check a single value of eair in column
  expect_equal(c1_n_eair[2], 100 * c1_n_eair[1] / c1_py, ignore_attr = TRUE)
  expect_equal(c2_n_eair[2], 100 * c2_n_eair[1] / c1_py, ignore_attr = TRUE)

  # check a single value of eair difference in column
  # pbo : 3rd column
  pbo_vals <- cell_values(tbl1["dcd A.1.1.1.1", 3])
  pbo_py <- pbo_vals[[1]][[1]]
  pbo_n <- pbo_vals[[2]][[1]][1]

  x_vals <- cell_values(tbl1["dcd A.1.1.1.1", 1])
  x_py <- x_vals[[1]][[1]]
  x_n <- x_vals[[2]][[1]][1]

  target <- cell_values(tbl1["dcd A.1.1.1.1.n_eair", 4])[[1]]
  diff <- h_s_eair_diff(
    x1 = x_n,
    n1 = x_py,
    x2 = pbo_n,
    n2 = pbo_py,
    conf_type = "wald",
    conf_level = 0.95
  )

  expect_equal(target, as.numeric(100 * diff), ignore_attr = TRUE)
})

test_that("dynamic labels for a_eair100_j", {
  adaex <- adae |>
    filter(AEDECOD %in% c("dcd A.1.1.1.1", "dcd A.1.1.1.2", "dcd B.1.1.1.1")) |>
    mutate(AEDECOD = droplevels(AEDECOD))

  # no user defined labels ----
  # person_years comes from junco_default_labels
  # "n_eair" "eair" comes from labels from x_stats statistics
  lyt1 <- core_lyt |>
    split_rows_by("AEDECOD", parent_name = "AEDECOD") |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path,
        .stats = c("person_years", "n_eair", "eair"),
        num_p_year = 1000
      )
    )
  tbl1 <- build_table(lyt1, adaex, adsl)
  tbl1

  expect_snapshot(cran = TRUE, tbl1)

  # user defined label ----
  # person_years comes from user defined input
  # "n_eair" "eair" comes from labels from x_stats statistics
  lyt2 <- core_lyt |>
    split_rows_by("AEDECOD", parent_name = "AEDECOD") |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path,
        .stats = c("person_years", "n_eair", "eair"),
        num_p_year = 100,
        .labels = c("person_years" = "Subject years of exposure")
      )
    )
  tbl2 <- build_table(lyt2, adaex, adsl)
  tbl2

  expect_snapshot(cran = TRUE, tbl2)

  # user defined label ----
  # person_years and n_eair comes from user defined input
  # "eair" comes from labels from x_stats statistics
  lyt3 <- core_lyt |>
    split_rows_by("AEDECOD", parent_name = "AEDECOD") |>
    analyze(
      "AEDECOD",
      afun = a_eair100_j,
      extra_args = list(
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        ref_path = ref_path,
        .stats = c("person_years", "n_eair", "eair"),
        num_p_year = 100,
        .labels = c(
          "person_years" = "Subject years of exposure",
          "n_eair" = "Number of subjects with event (incidence rate (per 100 person years))"
        )
      )
    )
  tbl3 <- build_table(lyt3, adaex, adsl)
  tbl3

  expect_snapshot(cran = TRUE, tbl3)
})
