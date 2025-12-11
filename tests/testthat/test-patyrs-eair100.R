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


adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
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
