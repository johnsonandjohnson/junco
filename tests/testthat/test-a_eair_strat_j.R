library(testthat)
library(rtables)
library(dplyr)

# ---- shared test data -------------------------------------------------------

set.seed(42)

adsl_strat <- ex_adsl |>
  mutate(
    TRTDURY = as.numeric(sub("d", "", sub("-", "", substring(USUBJID, nchar(USUBJID) - 2)))),
    STUDY = rep(c("Study1", "Study2", "Study3"), length.out = n())
  ) |>
  select(USUBJID, ARM, STUDY, TRTDURY)

adae_strat <- ex_adae |>
  select(USUBJID, AEDECOD, ASTDY) |>
  mutate(AOCCPFL = "Y") |>
  # keep first occurrence per subject/term
  arrange(USUBJID, AEDECOD, ASTDY) |>
  group_by(USUBJID, AEDECOD) |>
  slice(1) |>
  ungroup() |>
  filter(AEDECOD %in% c("dcd A.1.1.1.1", "dcd A.1.1.1.2", "dcd B.1.1.1.1", "dcd B.2.1.2.1")) |>
  mutate(AEDECOD = droplevels(AEDECOD))


aefup_strat <- left_join(adsl_strat, adae_strat, by = "USUBJID")

ref_path_strat <- c("ARM", "B: Placebo")

base_lyt <- function(extra = list()) {
  basic_table(show_colcounts = FALSE) |>
    split_cols_by("ARM") |>
    split_rows_by("AEDECOD") |>
    analyze(
      "AEDECOD",
      afun = a_eair_strat_j,
      extra_args = c(
        list(
          strata = "STUDY",
          fup_var = "TRTDURY",
          occ_var = "AOCCPFL",
          occ_dy = "ASTDY",
          .stats = c(
            "n_event_total", "person_years_total", "eair_strat", "eair_strat_ci",
            "eair_strat_diff_est_ci"
          ),
          ref_path = ref_path_strat,
          riskdiff = TRUE,
          riskdiff_setup = "vertical"
        ),
        extra
      )
    )
}

# ---- manual reference helpers -----------------------------------------------

# Compute stratified EAIR by hand for one arm / one AEDECOD
manual_strat_eair <- function(adsl, aefup, sel_aedecod, sel_arm,
                              num_p_year = 100) {
  if (!is.null(sel_aedecod)) {
    num <- aefup |>
      filter(AEDECOD == sel_aedecod, !is.na(AOCCPFL)) |>
      arrange(USUBJID, ASTDY) |>
      group_by(USUBJID) |>
      slice(1) |>
      ungroup()

    # modified follow-up: use ASTDY/365.25 for subjects with event, TRTDURY otherwise
    denom <- adsl |>
      left_join(num |>
                  select(USUBJID, ASTDY), by = "USUBJID") |>
      mutate(mod_fup = if_else(!is.na(ASTDY), ASTDY / 365.25, TRTDURY))
  } else {
    num <- aefup
    denom <- adsl |>
      mutate(mod_fup = TRTDURY)
  }

  # weights: total PY per study across ALL arms
  w_by_study <- denom |>
    group_by(STUDY) |>
    summarise(w = sum(mod_fup), .groups = "drop")

  studies <- w_by_study$STUDY
  W <- sum(w_by_study$w)

  arm_df <- num |> filter(ARM == sel_arm)

  per_study <- lapply(studies, function(s) {
    denom_s <- denom |> filter(STUDY == s, ARM == sel_arm)
    num_s <- num |> filter(STUDY == s, ARM == sel_arm)
    list(
      n  = nrow(num_s),
      py = sum(denom_s$mod_fup)
    )
  })

  n_vec <- sapply(per_study, `[[`, "n")
  py_vec <- sapply(per_study, `[[`, "py")
  w_vec <- w_by_study$w

  r_vec <- num_p_year * (n_vec) / py_vec
  eair <- sum(w_vec * r_vec) / W
  eair
}

# ---- tests ------------------------------------------------------------------

test_that("a_eair_strat_j: basic stratified EAIR matches manual calculation", {
  tbl <- build_table(base_lyt(), aefup_strat, alt_counts_df = adsl_strat)

  sel_term <- "dcd A.1.1.1.1"
  result <- as.numeric(
    cell_values(tbl[paste0(sel_term, ".eair_strat"), "A: Drug X"])[[1]]
  )

  expected <- manual_strat_eair(adsl_strat, aefup_strat, sel_term, "A: Drug X")

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("a_eair_strat_j: rate difference column is produced with riskdiff = TRUE", {
  lyt <- basic_table(show_colcounts = FALSE) |>
    split_cols_by("ARM") |>
    analyze(
      "AEDECOD",
      afun = a_eair_strat_j,
      extra_args = list(
        strata = "STUDY",
        fup_var = "TRTDURY",
        occ_var = "AOCCPFL",
        occ_dy = "ASTDY",
        riskdiff = TRUE,
        ref_path = ref_path_strat,
        .stats = c("eair_strat", "eair_strat_diff_est_ci"),
        riskdiff_setup = "vertical"
      )
    )

  expect_no_error(tbl <- build_table(lyt, aefup_strat, alt_counts_df = adsl_strat))

  # diff stat should be non-NULL for non-reference arm
  diff_val <- cell_values(tbl[paste0("dcd A.1.1.1.1", ".eair_strat_diff_est_ci"), "A: Drug X"])[[1]]
  expect_length(diff_val, 3) # estimate, lcl, ucl
})

test_that("a_eair_strat_j: errors when alt_counts_df is not supplied", {
  expect_error(
    build_table(base_lyt(), aefup_strat),
    "alt_counts_df"
  )
})

test_that("a_eair_strat_j: errors when riskdiff = TRUE but ref_path is NULL", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "AEDECOD",
      afun = a_eair_strat_j,
      extra_args = list(
        strata   = "STUDY",
        fup_var  = "TRTDURY",
        occ_var  = "AOCCPFL",
        occ_dy   = "ASTDY",
        riskdiff = TRUE,
        ref_path = NULL
      )
    )

  expect_error(
    build_table(lyt, aefup_strat, alt_counts_df = adsl_strat),
    "ref_path cannot be NULL"
  )
})

test_that("a_eair_strat_j: errors when strata column is absent from alt_counts_df", {
  adsl_no_study <- select(adsl_strat, -STUDY)

  expect_error(
    build_table(base_lyt(), aefup_strat, alt_counts_df = adsl_no_study),
    "Assertion"
  )
})

test_that("a_eair_strat_j: snapshot of full table output", {
  tbl <- build_table(base_lyt(), aefup_strat, alt_counts_df = adsl_strat)
  expect_snapshot(cran = TRUE, tbl)
})

# ---- Crowe, B., Chuang-Stein, C., Lettis, S., & Brueckner, A. (2016) Table 2 reference test ------------------------
#
# Table 2: Observed Incidence Rates for a Particular Adverse Event in the
# New Treatment and Placebo Group in 3 Trials.
#
#   Study                        Trt n/PY (IR)    Pbo n/PY (IR)   Total PY
#   Phase 2 study                30/150  (20.0)   10/50   (20.0)    200
#   Phase 3 study               133/700  (19.0)   67/350  (19.1)   1050
#   Phase 3 study (refractory)  200/500  (40.0)  200/500  (40.0)   1000
#
# Study size-adjusted EAIR (per 100 PY):
#   Treatment: (200/2250*30/150 + 1050/2250*133/700 + 1000/2250*200/500)*100 = 28.4 # nolint
#   Placebo  : (200/2250*10/50  + 1050/2250*67/350  + 1000/2250*200/500)*100 = 28.5 # nolint
#
#' Crowe, B., Chuang-Stein, C., Lettis, S., & Brueckner, A. (2016).
#' Reporting adverse drug reactions in product labels.
#' Therapeutic Innovation & Regulatory Science, 50(4), 455-463.
#' doi: 10.1177/2168479016628574
#'
#'
test_that("a_eair_strat_j: matches Scosyrev & Pethe (2022) Table 2 study size-adjusted EAIR", {
  # Reconstruct individual-level data reproducing Table 2 counts and person-years.
  # occ_var = NULL so TRTDURY is used directly as follow-up (already in years).
  # n_total subjects per arm/study each contribute total_py / n_total years,
  # so sum(TRTDURY) == total PY for that arm/study cell.
  make_arm_data <- function(study, arm, n_event, total_py, n_total) {
    data.frame(
      USUBJID = paste(study, arm, seq_len(n_total), sep = "_"),
      ARM = arm,
      STUDY = study,
      TRTDURY = total_py / n_total,
      AOCCPFL = c(rep("Y", n_event), rep(NA_character_, n_total - n_event)),
      stringsAsFactors = FALSE
    )
  }

  # Phase 2: Trt 30 events / 150 PY, Pbo 10 events / 50 PY
  ph2_trt <- make_arm_data("Phase2", "New Treatment", 30, 150, 150)
  ph2_pbo <- make_arm_data("Phase2", "Placebo", 10, 50, 50)
  # Phase 3: Trt 133 events / 700 PY, Pbo 67 events / 350 PY
  ph3_trt <- make_arm_data("Phase3", "New Treatment", 133, 700, 700)
  ph3_pbo <- make_arm_data("Phase3", "Placebo", 67, 350, 350)
  # Phase 3 refractory: Trt 200 events / 500 PY, Pbo 200 events / 500 PY
  ph3r_trt <- make_arm_data("Phase3R", "New Treatment", 200, 500, 500)
  ph3r_pbo <- make_arm_data("Phase3R", "Placebo", 200, 500, 500)

  adsl_paper <- rbind(ph2_trt, ph2_pbo, ph3_trt, ph3_pbo, ph3r_trt, ph3r_pbo)
  adsl_paper$ARM <- factor(adsl_paper$ARM, levels = c("New Treatment", "Placebo"))

  # AE dataset: one record per subject that had an event
  adae_paper <- adsl_paper[!is.na(adsl_paper$AOCCPFL), c("USUBJID", "STUDY", "ARM")]
  adae_paper$AEDECOD <- "AE"

  lyt_paper <- basic_table(show_colcounts = FALSE) |>
    split_cols_by("ARM") |>
    analyze(
      "AEDECOD",
      afun = a_eair_strat_j,
      extra_args = list(
        strata     = "STUDY",
        fup_var    = "TRTDURY",
        occ_var    = NULL,
        occ_dy     = NULL,
        .stats     = "eair_strat",
        num_p_year = 100
      )
    )

  tbl_paper <- build_table(
    lyt_paper,
    adae_paper,
    alt_counts_df = adsl_paper[, c("USUBJID", "ARM", "STUDY", "TRTDURY")]
  )

  eair_trt <- as.numeric(cell_values(tbl_paper["AE.eair_strat", "New Treatment"])[[1]])
  eair_pbo <- as.numeric(cell_values(tbl_paper["AE.eair_strat", "Placebo"])[[1]])

  # Paper reports 28.4 for Treatment and 28.5 for Placebo (rounded to 1 dp)
  expect_equal(round(eair_trt, 1), 28.4)
  expect_equal(round(eair_pbo, 1), 28.5)
})

# ---- Scosyrev & Pethe (2022) Table 3 reference test ------------------------
#
# Table 3: Real data example — point and interval estimates of the rate difference.
# Single trial (one stratum), so stratified EAIR == unstratified EAIR.
# Numbers from OccR - Asthma
#
#   Arm         N    Time (mo)   Asthma events
#   Treatment  396    1189            25
#   Control    399    1186            80
#
# Rates per 100 patient-months (IR = first episode per patient only):
#   Asthma  Trt: 25/1189*100 = 2.102...  -> reported 2.10
#   Asthma  Ctl: 80/1186*100 = 6.745...  -> reported 6.74
#
# The Wald CI produced by a_eair_strat_j is reported in the Poisson column
# the point estimate (rate difference) and the Wald CI formula
#
# Reference: Scosyrev E & Pethe A (2022). Pharmaceutical Statistics, 21:103-121.
#            doi: 10.1002/pst.2155 # nolint

test_that("a_eair_strat_j: matches Scosyrev & Pethe (2022) Table 3 OccR rate difference (Asthma)", {
  # One stratum (single trial). Each subject gets equal follow-up so that
  # sum(TRTDURY) == reported total person-months for that arm.
  make_arm_t3 <- function(arm, n_total, total_pm, n_event, aedecod) {
    data.frame(
      USUBJID = paste(arm, seq_len(n_total), sep = "_"),
      ARM = arm,
      STUDY = "Trial1",
      TRTDURY = total_pm / n_total, # person-months per subject
      AOCCPFL = c(rep("Y", n_event), rep(NA_character_, n_total - n_event)),
      AEDECOD = aedecod,
      stringsAsFactors = FALSE
    )
  }

  # IR Asthma: Trt 21 events / 1189 pm, Ctl 61 events / 1186 pm
  trt_asthma <- make_arm_t3("Treatment", 396, 1189, 25, "Asthma")
  ctl_asthma <- make_arm_t3("Control", 399, 1186, 80, "Asthma")

  adsl_t3 <- rbind(
    trt_asthma[, c("USUBJID", "ARM", "STUDY", "TRTDURY")],
    ctl_asthma[, c("USUBJID", "ARM", "STUDY", "TRTDURY")]
  )
  adsl_t3$ARM <- factor(adsl_t3$ARM, levels = c("Treatment", "Control"))

  aefup_t3 <- rbind(trt_asthma, ctl_asthma)
  aefup_t3$ARM <- factor(aefup_t3$ARM, levels = c("Treatment", "Control"))
  aefup_t3$AEDECOD <- factor(aefup_t3$AEDECOD)
  # keep only subjects with an event in the AE dataset
  aefup_t3 <- aefup_t3[!is.na(aefup_t3$AOCCPFL), ]

  lyt_t3 <- basic_table(show_colcounts = FALSE) |>
    split_cols_by("ARM") |>
    analyze(
      "AEDECOD",
      afun = a_eair_strat_j,
      extra_args = list(
        strata = "STUDY",
        fup_var = "TRTDURY",
        occ_var = NULL,
        occ_dy = NULL,
        riskdiff = TRUE,
        ref_path = c("ARM", "Control"),
        .stats = c("person_years_total", "n_eair_strat", "eair_strat_diff_est_ci"),
        .formats = c("n_eair_strat" = jjcsformat_xx("xx (xx.xx)")),
        num_p_year = 100,
        riskdiff_setup = "vertical"
      )
    )

  tbl_t3 <- build_table(lyt_t3, aefup_t3, alt_counts_df = adsl_t3)
  tbl_t3

  eair_trt <- as.numeric(cell_values(tbl_t3["Asthma.n_eair_strat", "Treatment"])[[1]][2])
  eair_ctl <- as.numeric(cell_values(tbl_t3["Asthma.n_eair_strat", "Control"])[[1]][2])
  diff_val <- cell_values(tbl_t3["Asthma.eair_strat_diff_est_ci", "Treatment"])[[1]]

  # Rates per 100 pm: Trt = 21/1189*100, Ctl = 61/1186*100
  expect_equal(eair_trt, 25 / 1189 * 100, tolerance = 1e-6)
  expect_equal(eair_ctl, 80 / 1186 * 100, tolerance = 1e-6)

  # Point estimate of rate difference (Trt - Ctl)
  expected_diff <- 25 / 1189 * 100 - 80 / 1186 * 100
  expect_equal(diff_val[["estimate"]], expected_diff, tolerance = 1e-6)

  # Paper reports rate difference = -4.64 (rounded to 2 dp) and Poisson CI -6.33, -2.95
  expect_equal(round(diff_val[["estimate"]], 2), -4.64)
  expect_equal(round(diff_val[["lcl"]], 2), -6.33, tolerance = 1e-1)
  expect_equal(round(diff_val[["ucl"]], 2), -2.95, tolerance = 1e-1)
})
