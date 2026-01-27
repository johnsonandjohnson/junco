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
