library(rtables)
library(dplyr)
suppressPackageStartupMessages(library(tern))

adsl <- ex_adsl %>% select(USUBJID, ARM, COUNTRY, STRATA1, SEX)
adae <- ex_adae %>% select(USUBJID, AEDECOD, AEBODSYS, ARM)
adae$TRTEMFL <- "Y"

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
ref_path <- c("colspan_trt", " ", trtvar, "B: Placebo")

adsl$colspan_trt <- factor(
  ifelse(adsl[["ARM"]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[["ARM"]], "vs Placebo")

adae <- left_join(adae, adsl, by = join_by(USUBJID, ARM))

core_lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) %>%
  split_cols_by("ARM") %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    "ARM",
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  )


wald_diff <- function(inputs) {
  alpha <- 0.05

  n1 <- inputs[1]
  N1 <- inputs[2]
  n2 <- inputs[3]
  N2 <- inputs[4]

  p1_hat <- n1 / N1
  p2_hat <- n2 / N2

  # group 2 is the reference
  est <- p1_hat - p2_hat

  vd <- p1_hat * (1 - p1_hat) / N1 + p2_hat * (1 - p2_hat) / N2
  kappa <- stats::qnorm(1 - alpha / 2)
  term2 <- kappa * sqrt(vd)

  ci_lwr <- max(-1, est - term2)
  ci_upr <- min(1, est + term2)

  return(100 * c(est, ci_lwr, ci_upr))
}

count_unique_subjects <- function(
  df,
  id = "USUBJID",
  sub_set = NULL,
  var = NULL
) {
  if (!is.null(sub_set)) {
    df <- subset(df, sub_set)
  }

  if (!is.null(var)) {
    df <- unique(df[, c(var, id)])
  }

  n <- length(unique(df[[id]]))
  return(n)
}

#### Actual start of tests

test_that("a_freq_j with val = NA and denom option", {
  adsl_col <- adsl[adsl$ARM == "A: Drug X", ]
  adae_col <- adae[adae$ARM == "A: Drug X", ]

  adsl_colPBO <- adsl[adsl$ARM == ctrl_grp, ]
  adae_colPBO <- adae[adae$ARM == ctrl_grp, ]

  DrugX_column_val <- "Active Study Agent.A: Drug X"
  DrugX_column_rr <- "Risk Difference (%) (95% CI).A: Drug X"
  PBO_column_val <- "Active Study Agent.B: Placebo"

  # scenario 1 : denom = N
  extra_args <- list(
    denom = "n_df",
    ctrl_grp = ctrl_grp,
    ref_path = ref_path,
    .alt_df_full = adsl,
    method = "wald"
  )

  lyt1 <- core_lyt %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args
    )

  # apply to adsl - here it is not yet critical to set parameter denom
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  res1 <- cell_values(tbl1["CHN", "A: Drug X"])
  res1_val <- unlist(unname(res1[[DrugX_column_val]]))
  res1_rr <- res1[[DrugX_column_rr]] %>% as.numeric()

  Ncol <- count_unique_subjects(adsl_col)
  N <- count_unique_subjects(adsl_col)

  freq <- table(adsl_col[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  Ncol_PBO <- count_unique_subjects(adsl_colPBO)
  freq_val_PBO <- table(adsl_colPBO[["COUNTRY"]])[["CHN"]]

  expected1 <- c(freq_val, N, freq_val / N)

  inputs <- c(
    freq_val,
    Ncol,
    freq_val_PBO,
    Ncol_PBO
  )

  expected1_rr <- wald_diff(inputs)

  expect_identical(
    res1_val,
    expected1
  )

  expect_identical(
    res1_rr,
    expected1_rr
  )

  # Same layout but now applied to ADAE --- has less subjects
  # denom = N has impact
  extra_args <- list(
    denom = "n_df",
    ctrl_grp = ctrl_grp,
    ref_path = ref_path,
    method = "wald"
  )

  lyt1b <- core_lyt %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args
    )
  tbl1b <- build_table(lyt1b, adae, adsl, round_type = "sas")
  res1b <- cell_values(tbl1b["CHN", "A: Drug X"])
  res1b_val <- unlist(unname(res1b[[DrugX_column_val]]))
  res1b_rr <- res1b[[DrugX_column_rr]] %>% as.numeric()

  ### comparison of main columns (similar to tests in test-count_pct.R )
  Ncol <- count_unique_subjects(adsl_col)
  N <- count_unique_subjects(adae_col, var = "COUNTRY")

  adae_col_x <- unique(adae_col[, c("COUNTRY", "USUBJID")])
  freq <- table(adae_col_x[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  expected1b <- c(freq_val, N, freq_val / N)

  expect_identical(
    res1b_val,
    expected1b
  )

  ### comparison of relative risk columns - NEW part of testing
  N_PBO <- count_unique_subjects(adae_colPBO, var = "COUNTRY")
  adae_colPBO_x <- unique(adae_colPBO[, c("COUNTRY", "USUBJID")])
  freq_val_PBO <- table(adae_colPBO_x[["COUNTRY"]])[["CHN"]]

  inputs <- c(freq_val, N, freq_val_PBO, N_PBO)
  expected1b_rr <- wald_diff(inputs)

  expect_identical(
    res1b_rr,
    expected1b_rr
  )

  # Same layout but now applied to ADAE but with denom = .N_col
  # !!!! here it is critical to set denomdf = adsl,
  # in the layout definition, otherwise incorrect results
  extra_args <- list(
    denom = "N_col",
    ctrl_grp = ctrl_grp,
    ref_path = ref_path,
    method = "wald"
  )

  lyt1c <- core_lyt %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args
    )
  tbl1c <- build_table(lyt1c, adae, adsl, round_type = "sas")

  res1c <- cell_values(tbl1c["CHN", "A: Drug X"])
  res1c_val <- unlist(unname(res1c[[DrugX_column_val]]))
  res1c_rr <- res1c[[DrugX_column_rr]] %>% as.numeric()

  ### comparison of main columns (similar to tests in test-count_pct.R )
  Ncol <- count_unique_subjects(adsl_col)

  adae_col_x <- unique(adae_col[, c("COUNTRY", "USUBJID")])
  freq <- table(adae_col_x[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  expected1c <- c(freq_val, Ncol, freq_val / Ncol)

  expect_identical(
    res1c_val,
    expected1c
  )

  ### comparison of relative risk columns - NEW part of testing
  Ncol_PBO <- count_unique_subjects(adsl_colPBO)
  adae_colPBO_x <- unique(adae_colPBO[, c("COUNTRY", "USUBJID")])
  freq_val_PBO <- table(adae_colPBO_x[["COUNTRY"]])[["CHN"]]

  inputs <- c(freq_val, Ncol, freq_val_PBO, Ncol_PBO)
  expected1c_rr <- wald_diff(inputs)

  expect_identical(
    res1c_rr,
    expected1c_rr
  )
})


test_that("a_freq_j with risk difference method cmh", {
  adsl_col <- adsl[adsl$ARM == "A: Drug X", ]
  adae_col <- adae[adae$ARM == "A: Drug X", ]

  adsl_colPBO <- adsl[adsl$ARM == ctrl_grp, ]
  adae_colPBO <- adae[adae$ARM == ctrl_grp, ]

  DrugX_column_val <- "Active Study Agent.A: Drug X"
  DrugX_column_rr <- "Risk Difference (%) (95% CI).A: Drug X"
  PBO_column_val <- "Active Study Agent.B: Placebo"

  # Same layout but now applied to ADAE but with denom = .N_col
  # demonstration what happens when denomdf = adsl is not submitted
  # despite the denom = .N_col, and the main columns are using .N_col
  # the diff column would utilize N instead
  extra_args <- list(
    denom = "N_col",
    denomdf = adsl,
    ctrl_grp = ctrl_grp,
    method = "cmh",
    ref_path = ref_path,
    variables = list(strata = "STRATA1")
  )
  lyt1d <- core_lyt %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args
    )

  tbl1d <- build_table(lyt1d, adae, adsl, round_type = "sas")

  res1d <- cell_values(tbl1d["CHN", "A: Drug X"])
  res1d_val <- unlist(unname(res1d[[DrugX_column_val]]))
  res1d_rr <- res1d[[DrugX_column_rr]] %>% as.numeric()

  ### comparison of main columns (similar to tests in test-count_pct.R )
  Ncol <- count_unique_subjects(adsl_col)

  adae_col_x <- unique(adae_col[, c("COUNTRY", "USUBJID")])
  N <- count_unique_subjects(adae_col_x)
  freq <- table(adae_col_x[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  expected1d <- c(freq_val, Ncol, freq_val / Ncol)

  expect_identical(
    res1d_val,
    expected1d
  )

  ### comparison of relative risk columns - NEW part of testing
  Ncol_PBO <- count_unique_subjects(adsl_colPBO)
  adae_colPBO_x <- unique(adae_colPBO[, c("COUNTRY", "USUBJID")])
  freq_val_PBO <- table(adae_colPBO_x[["COUNTRY"]])[["CHN"]]

  N_PBO <- count_unique_subjects(adae_colPBO_x)

  ### construct input vectors to utilize tern::prop_diff_cmh
  adae_col_chn <- adae %>%
    filter(ARM == "A: Drug X" & COUNTRY == "CHN")
  adae_colPBO_chn <- adae %>%
    filter(ARM == ctrl_grp & COUNTRY == "CHN")

  subj_col <- adsl_col[["USUBJID"]]
  subj_ae <- unique(adae_col_chn[["USUBJID"]])

  rsp_col <- rep(FALSE, length.out = length(subj_col))
  rsp_col[subj_col %in% subj_ae] <- TRUE
  grp_col <- rep("A: Drug X", length.out = length(subj_col))
  strata_col <- adsl_col[["STRATA1"]]

  subj_ctrl <- adsl_colPBO[["USUBJID"]]
  subj_ae_ctrl <- unique(adae_colPBO_chn[["USUBJID"]])

  rsp_ctrl <- rep(FALSE, length.out = length(subj_ctrl))
  rsp_ctrl[subj_ctrl %in% subj_ae_ctrl] <- TRUE
  grp_ctrl <- rep(ctrl_grp, length.out = length(subj_ctrl))
  strata_ctrl <- adsl_colPBO[["STRATA1"]]

  # for prop_diff_cmh diff is second group - first group
  # therefor, set the levels to grp in this order : ref group first

  rsp <- c(rsp_col, rsp_ctrl)
  grp <- c(grp_col, grp_ctrl)
  grp <- factor(grp, levels = c(ctrl_grp, "A: Drug X"))
  strata <- c(strata_col, strata_ctrl)

  rr_stats <- tern::prop_diff_cmh(rsp, grp, strata)

  expected1d_rr <- 100 * c(rr_stats[["diff"]], rr_stats[["diff_ci"]])

  expect_identical(
    res1d_rr,
    expected1d_rr
  )
})

test_that("a_freq_j with N_subgroup as denom", {
  adsl_col <- adsl[adsl$ARM == "A: Drug X", ]
  adae_col <- adae[adae$ARM == "A: Drug X", ]

  adsl_colPBO <- adsl[adsl$ARM == ctrl_grp, ]
  adae_colPBO <- adae[adae$ARM == ctrl_grp, ]

  adsl_col_subgroup <- adsl[adsl$ARM == "A: Drug X" & adsl$SEX == "F", ]
  adae_col_subgroup <- adae[adae$ARM == "A: Drug X" & adae$SEX == "F", ]

  adsl_colPBO_subgroup <- adsl[adsl$ARM == ctrl_grp & adsl$SEX == "F", ]
  adae_colPBO_subgroup <- adae[adae$ARM == ctrl_grp & adae$SEX == "F", ]

  DrugX_column_val <- "Active Study Agent.A: Drug X"
  DrugX_column_rr <- "Risk Difference (%) (95% CI).A: Drug X"
  PBO_column_val <- "Active Study Agent.B: Placebo"

  # scenario 1 : denom = N_subgroup, all values
  extra_args <- list(
    # the following 2 lines make the denominators relative to each Sex group
    denom = "n_df",
    denom_by = c("SEX"),
    ctrl_grp = ctrl_grp,
    ref_path = ref_path
  )
  lyt1 <- core_lyt %>%
    split_rows_by("SEX") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args
    )

  # applied to adsl
  tbl1 <- build_table(lyt1, adsl, round_type = "sas")
  tbl1x <- tbl1[
    c("SEX", "F", "COUNTRY", "count_unique_denom_fraction.CHN"),
    seq_len(ncol(tbl1))
  ]

  res1 <- cell_values(
    tbl1[
      c("SEX", "F", "COUNTRY", "count_unique_denom_fraction.CHN"),
      "A: Drug X"
    ]
  )
  res1_val <- unlist(unname(res1[[DrugX_column_val]]))
  res1_rr <- res1[[DrugX_column_rr]] %>% as.numeric()

  Ncol <- length(unique(adsl_col[["USUBJID"]]))
  Nsubgroup <- length(unique(adsl_col_subgroup[["USUBJID"]]))

  freq <- table(adsl_col_subgroup[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  Ncol_PBO <- length(unique(adsl_colPBO[["USUBJID"]]))
  Nsubgroup_PBO <- length(unique(adsl_colPBO_subgroup[["USUBJID"]]))
  freq_val_PBO <- table(adsl_colPBO_subgroup[["COUNTRY"]])[["CHN"]]

  expected1 <- c(freq_val, Nsubgroup, freq_val / Nsubgroup)

  inputs <- c(
    freq_val,
    Nsubgroup,
    freq_val_PBO,
    Nsubgroup_PBO
  )

  expected1_rr <- wald_diff(inputs)

  expect_identical(
    res1_val,
    expected1
  )

  expect_identical(
    res1_rr,
    expected1_rr
  )

  # applied to adae: HERE, when denomdf is not specified in the layout,
  # it will take Nsubgroup from df, not from alt_counts_df
  tbl1b <- build_table(lyt1, adae, adsl, round_type = "sas")
  tbl1bx <- tbl1b[
    c("SEX", "F", "COUNTRY", "count_unique_denom_fraction.CHN"),
    seq_len(ncol(tbl1b))
  ]

  res1b <- cell_values(
    tbl1b[
      c("SEX", "F", "COUNTRY", "count_unique_denom_fraction.CHN"),
      "A: Drug X"
    ]
  )
  res1b_val <- unlist(unname(res1b[[DrugX_column_val]]))
  res1b_rr <- res1b[[DrugX_column_rr]] %>% as.numeric()

  Ncol <- length(unique(adsl_col[["USUBJID"]]))
  Nsubgroup <- length(unique(adsl_col_subgroup[["USUBJID"]]))
  Nsubgroup_domain <- length(unique(adae_col_subgroup[["USUBJID"]]))

  adae_col_x <- unique(adae_col_subgroup[, c("COUNTRY", "USUBJID")])
  freq <- table(adae_col_x[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  Ncol_PBO <- length(unique(adsl_colPBO[["USUBJID"]]))
  Nsubgroup_PBO <- length(unique(adsl_colPBO_subgroup[["USUBJID"]]))
  Nsubgroup_PBO_domain <- length(unique(adae_colPBO_subgroup[["USUBJID"]]))

  adae_colPBO_x <- unique(adae_colPBO_subgroup[, c("COUNTRY", "USUBJID")])
  freq_val_PBO <- table(adae_colPBO_x[["COUNTRY"]])[["CHN"]]

  expected1b <- c(freq_val, Nsubgroup_domain, freq_val / Nsubgroup_domain)

  inputs <- c(
    freq_val,
    Nsubgroup_domain,
    freq_val_PBO,
    Nsubgroup_PBO_domain
  )

  expected1b_rr <- wald_diff(inputs)

  expect_identical(
    res1b_val,
    expected1b
  )

  expect_identical(
    res1b_rr,
    expected1b_rr
  )

  # applied to adae: HERE, denomdf specified in layout
  # it will take Nsubgroup from this denomdf dataset
  extra_args_2 <- list(
    # the following 2 lines make the denominators relative to each Sex group
    denom = "n_altdf",
    denom_by = c("SEX"),
    ctrl_grp = ctrl_grp,
    ref_path = ref_path
  )

  lyt1c <- core_lyt %>%
    split_rows_by("SEX") %>%
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_2
    )

  tbl1c <- build_table(lyt1c, adae, adsl, round_type = "sas")
  tbl1cx <- tbl1c[
    c("SEX", "F", "COUNTRY", "count_unique_denom_fraction.CHN"),
    seq_len(ncol(tbl1c))
  ]

  res1c <- cell_values(
    tbl1c[
      c("SEX", "F", "COUNTRY", "count_unique_denom_fraction.CHN"),
      "A: Drug X"
    ]
  )
  res1c_val <- unlist(unname(res1c[[DrugX_column_val]]))
  res1c_rr <- res1c[[DrugX_column_rr]] %>% as.numeric()

  Ncol <- length(unique(adsl_col[["USUBJID"]]))
  Nsubgroup <- length(unique(adsl_col_subgroup[["USUBJID"]]))
  Nsubgroup_domain <- length(unique(adae_col_subgroup[["USUBJID"]]))

  adae_col_x <- unique(adae_col_subgroup[, c("COUNTRY", "USUBJID")])
  freq <- table(adae_col_x[["COUNTRY"]])
  freq_val <- freq[["CHN"]]

  Ncol_PBO <- length(unique(adsl_colPBO[["USUBJID"]]))
  Nsubgroup_PBO <- length(unique(adsl_colPBO_subgroup[["USUBJID"]]))
  Nsubgroup_PBO_domain <- length(unique(adae_colPBO_subgroup[["USUBJID"]]))

  adae_colPBO_x <- unique(adae_colPBO_subgroup[, c("COUNTRY", "USUBJID")])
  freq_val_PBO <- table(adae_colPBO_x[["COUNTRY"]])[["CHN"]]

  expected1c <- c(freq_val, Nsubgroup, freq_val / Nsubgroup)

  inputs <- c(
    freq_val,
    Nsubgroup,
    freq_val_PBO,
    Nsubgroup_PBO
  )

  expected1c_rr <- wald_diff(inputs)

  expect_identical(
    res1c_val,
    expected1c
  )

  expect_identical(
    res1c_rr,
    expected1c_rr
  )
})
