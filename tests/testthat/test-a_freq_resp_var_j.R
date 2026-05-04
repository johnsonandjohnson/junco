library(testthat)
library(rtables)
suppressMessages(library(dplyr))

# Create test datasets based on patterns in other test files
test_that("a_freq_resp_var_j works as expected with basic usage", {
  # Create simple test data
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:20], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 20),
    ARM = rep(adsl$ARM[1:20], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:20], each = 2),
    SEX = rep(adsl$SEX[1:20], each = 2),
    RSP = sample(c("Y", "N"), size = 40, replace = TRUE, prob = c(0.3, 0.7))
  )

  # Create the layout based on patterns seen in test-varia.R
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j,
      extra_args = list(
        resp_var = "RSP",
        riskdiff = FALSE # Explicitly set to FALSE to avoid treatment reference issues
      )
    )

  # Build the table
  tbl <- build_table(lyt, adrs)
  expect_true(!is.null(tbl))

  # Extract and check one cell for basic validation
  cell <- cell_values(tbl[c("SEX", "F"), 1])[[1]]
  vals <- unname(unlist(cell))
  expect_length(vals, 3)
  expect_equal(vals[3], vals[1] / vals[2])
})

test_that("a_freq_resp_var_j works with factor responses", {
  # Create simple test data with factor response
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:20], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 20),
    ARM = rep(adsl$ARM[1:20], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:20], each = 2),
    SEX = rep(adsl$SEX[1:20], each = 2)
  )
  adrs$RSP <- factor(
    sample(c("Y", "N"), size = 40, replace = TRUE, prob = c(0.3, 0.7)),
    levels = c("Y", "N")
  )

  # Create the layout
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j,
      extra_args = list(
        resp_var = "RSP",
        riskdiff = FALSE # Explicitly set to FALSE to avoid treatment reference issues
      )
    )

  # Should not throw an error
  expect_no_error(build_table(lyt, adrs))
})

test_that("a_freq_resp_var_j handles missing values correctly", {
  # Create test data with missing values
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:20], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 20),
    ARM = rep(adsl$ARM[1:20], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:20], each = 2),
    SEX = rep(adsl$SEX[1:20], each = 2),
    RSP = sample(c("Y", "N"), size = 40, replace = TRUE, prob = c(0.3, 0.7))
  )

  # Introduce missing values
  adrs$RSP[1:5] <- NA
  adrs$SEX[6:10] <- NA

  # Create the layout
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j,
      extra_args = list(
        resp_var = "RSP",
        riskdiff = FALSE # Explicitly set to FALSE to avoid treatment reference issues
      )
    )

  # Should not throw an error
  expect_no_error(suppressWarnings(build_table(lyt, adrs)))
})

test_that("a_freq_resp_var_j errors on invalid responses", {
  # Create test data with invalid responses
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:10], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 10),
    ARM = rep(adsl$ARM[1:10], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:10], each = 2),
    SEX = rep(adsl$SEX[1:10], each = 2),
    RSP = sample(c("Y", "N"), size = 20, replace = TRUE)
  )

  # Add invalid response value
  adrs$RSP[1:3] <- "MAYBE"

  # Create the layout
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j,
      extra_args = list(
        resp_var = "RSP",
        riskdiff = FALSE # Explicitly set to FALSE to avoid treatment reference issues
      )
    )

  # Should throw an error about invalid response values
  expect_error(
    build_table(lyt, adrs),
    "resp_var must contain only Y/N values"
  )
})

test_that("a_freq_resp_var_j errors when resp_var is null", {
  # Create test data
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:10], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 10),
    ARM = rep(adsl$ARM[1:10], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:10], each = 2),
    SEX = rep(adsl$SEX[1:10], each = 2),
    RSP = sample(c("Y", "N"), size = 20, replace = TRUE)
  )

  # Create layout with missing resp_var
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j
    )

  # Should throw an error about missing resp_var
  expect_error(
    build_table(lyt, adrs),
    "resp_var cannot be NULL."
  )
})

test_that("a_freq_resp_var_j works with drop_levels parameter", {
  # Create test data with factors
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:20], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 20),
    ARM = rep(adsl$ARM[1:20], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:20], each = 2),
    SEX = factor(
      rep(adsl$SEX[1:20], each = 2),
      levels = c("F", "M", "U", "OTHER")
    ),
    RSP = sample(c("Y", "N"), size = 40, replace = TRUE, prob = c(0.3, 0.7))
  )

  # Create layout with drop_levels = TRUE
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j,
      extra_args = list(
        resp_var = "RSP",
        drop_levels = TRUE,
        riskdiff = FALSE # Explicitly set to FALSE to avoid treatment reference issues
      )
    )

  # Should not throw an error
  expect_no_error(build_table(lyt, adrs))
})

test_that("a_freq_resp_var_j works with riskdiff parameter", {
  # Create test data with factors
  adsl <- ex_adsl
  adrs <- data.frame(
    USUBJID = rep(adsl$USUBJID[1:20], each = 2),
    AVISIT = rep(c("WEEK 1", "WEEK 2"), times = 20),
    ARM = rep(adsl$ARM[1:20], each = 2),
    COUNTRY = rep(adsl$COUNTRY[1:20], each = 2),
    SEX = factor(
      rep(adsl$SEX[1:20], each = 2),
      levels = c("F", "M", "U", "OTHER")
    ),
    RSP = sample(c("Y", "N"), size = 40, replace = TRUE, prob = c(0.3, 0.7))
  )

  # Create layout with drop_levels = TRUE
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      "SEX",
      afun = a_freq_resp_var_j,
      extra_args = list(
        resp_var = "RSP",
        drop_levels = TRUE,
        ref_path = c("ARM", "B: Placebo")
      )
    )

  # Should not throw an error
  expect_no_error(build_table(lyt, adrs))
})

test_that("a_freq_resp_var_j in layout with relative risk column for combined facet", {
  library(dplyr)
  trtvar <- "ARM"
  ctrl_grp <- "B: Placebo"

  adsl <- ex_adsl |> select(c("USUBJID", "SEX", all_of(trtvar)))
  adsl$colspan_trt <- factor(
    ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
  had_ae <- ex_adae |>
    mutate(TRTEMFL = "Y") |>
    select(USUBJID, TRTEMFL) |>
    distinct(USUBJID, .keep_all = TRUE)

  adsl <- adsl |>
    left_join(had_ae) |>
    mutate(TRTEMFL = ifelse(is.na(TRTEMFL), "N", "Y"))

  colspan_trt_map <- create_colspan_map(
    df = adsl,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

  a_freq_j_args <- list(
    resp_var = "TRTEMFL", drop_levels = TRUE,
    riskdiff = TRUE,
    ref_path = c("colspan_trt", " ", trtvar, ctrl_grp),
    method = "wald"
  )

  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = setdiff(unique(adsl[[trtvar]]), ctrl_grp)
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
    value = "Risk Difference (%) (95% CI)",
    split = "rrisk_header"
  )
  add_combo2 <- add_combo_facet(
    "Combined",
    label = paste0("Combined vs ", ctrl_grp),
    levels = setdiff(unique(adsl[[trtvar]]), ctrl_grp)
  )

  mysplit_comb2 <- make_split_fun(post = list(add_combo2, rm_combo_from_placebo2))
  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by(trtvar, split_fun = mysplit_comb) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = mysplit_comb2) |>
    analyze(
      "SEX",
      show_labels = "visible",
      afun = a_freq_resp_var_j,
      extra_args = a_freq_j_args
    )
  result <- build_table(lyt, adsl, alt_counts_df = adsl)

  # confirm column is not NE (NE, NE) and numbers are correct
  actual_row <- result[2, ]
  actual <- cell_values(result[2, 7])

  combcols <- 1:2
  ctrlcol <- 4

  countscomb <- sum(sapply(cell_values(actual_row[1, combcols]), function(v) v[["count_unique"]]), na.rm = TRUE)
  countsctrl <- cell_values(actual_row[1, ctrlcol])[[1]][["count_unique"]]

  denomcomb <- sum(sapply(cell_values(actual_row[1, combcols]), function(v) v[["d"]]), na.rm = TRUE)
  denomctrl <- cell_values(actual_row[1, ctrlcol])[[1]][["d"]]

  # default method for relative risk in a_freq_j is wald stat which is based upon tern function
  expected <- tern::stat_propdiff_ci(
    x = list(countscomb),
    y = list(countsctrl),
    N_x = denomcomb,
    N_y = denomctrl
  )

  testthat::expect_equal(actual, expected, ignore_attr = TRUE)
})
