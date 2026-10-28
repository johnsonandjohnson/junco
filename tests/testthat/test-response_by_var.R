library(testthat)
library(rtables)
library(dplyr)

adsl <- ex_adsl
adae <- ex_adae
adae$TRTEMFL <- "Y"

had_ae <- adae |>
  filter(TRTEMFL == "Y") |>
  select(USUBJID, TRTEMFL) |>
  distinct(USUBJID, .keep_all = TRUE)

adsl <- adsl |>
  left_join(had_ae, by = "USUBJID") |>
  mutate(TRTEMFL = ifelse(is.na(TRTEMFL), "N", "Y"))

test_that("response_by_var various scenarios", {
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") |>
    split_cols_by("ARM") |>
    analyze(
      vars = "SEX",
      var_labels = "Sex, n/Ns (%)",
      show_labels = "visible",
      afun = response_by_var,
      extra_args = list(resp_var = "TRTEMFL"),
      nested = FALSE
    )

  ## Scenario 1: TRTEMFL has no missing values and values Y/N
  tbl <- build_table(lyt, adsl)
  res1 <- cell_values(tbl[c("SEX", "F"), "A: Drug X"])
  res1 <- unlist(unname(res1))

  resp <- table(adsl[, c("ARM", "SEX", "TRTEMFL")])
  resp_val <- resp["A: Drug X", "F", "Y"]
  denom_val <- sum(resp["A: Drug X", "F", ])

  expected1 <- c(resp_val, denom_val, resp_val / denom_val)
  expect_identical(res1, expected1)

  ## Snapshot for the full table
  expect_snapshot(cran = TRUE, tbl)

  ## Scenario 2: TRTEMFL has missing values and Y only, but is factor with just this level
  ## NAs in resp_var are excluded from denominator
  adsl2 <- adsl
  adsl2$TRTEMFL <- ifelse(adsl2$TRTEMFL == "Y", "Y", NA)
  adsl2$TRTEMFL <- factor(adsl2$TRTEMFL, levels = "Y")

  tbl2 <- build_table(lyt, adsl2)

  res2 <- cell_values(tbl2[c("SEX", "F"), "A: Drug X"])
  res2 <- unlist(unname(res2))

  resp2 <- table(adsl2[!is.na(adsl2$TRTEMFL), c("ARM", "SEX", "TRTEMFL")])
  resp_val2 <- resp2["A: Drug X", "F", "Y"]
  denom_val2 <- sum(resp2["A: Drug X", "F", ])

  expected2 <- c(resp_val2, denom_val2, resp_val2 / denom_val2)
  expect_identical(res2, expected2)

  expect_snapshot(cran = TRUE, tbl2)

  ## Scenario 3: TRTEMFL has missing values and Y only, and analysis variable has missing values
  ## NAs in both .var and resp_var excluded from denominator
  adsl3 <- adsl |> select(USUBJID, ARM, SEX, TRTEMFL)
  adsl3$TRTEMFL <- ifelse(adsl3$TRTEMFL == "Y", "Y", NA)
  adsl3$TRTEMFL <- factor(adsl3$TRTEMFL, levels = "Y")
  adsl3$SEX[1:10] <- NA_character_

  tbl3 <- build_table(lyt, adsl3)
  res3 <- cell_values(tbl3[c("SEX", "F"), "A: Drug X"])
  res3 <- unlist(unname(res3))

  ## Only rows where both SEX and TRTEMFL are non-NA count
  adsl3_clean <- adsl3[!is.na(adsl3$SEX) & !is.na(adsl3$TRTEMFL), ]
  resp3 <- table(adsl3_clean[, c("ARM", "SEX", "TRTEMFL")])
  resp_val3 <- resp3["A: Drug X", "F", "Y"]
  denom_val3 <- sum(resp3["A: Drug X", "F", ])

  expected3 <- c(resp_val3, denom_val3, resp_val3 / denom_val3)
  expect_identical(res3, expected3)

  expected3 <- c(resp_val3, denom_val3, resp_val3 / denom_val3)
  expect_identical(res3, expected3)

  ## Snapshot for the full table
  expect_snapshot(cran = TRUE, tbl3)

  ## Scenario 4: TRTEMFL has missing values and Y/N, and analysis variable has missing values
  adsl4 <- adsl |> select(USUBJID, ARM, SEX, TRTEMFL)
  adsl4$SEX[1:10] <- NA_character_
  adsl4$TRTEMFL[8:15] <- NA_character_

  tbl4 <- build_table(lyt, adsl4)
  res4 <- cell_values(tbl4[c("SEX", "F"), "A: Drug X"])
  res4 <- unlist(unname(res4))

  resp4 <- table(adsl4[, c("ARM", "SEX", "TRTEMFL")], useNA = "ifany")
  resp_val4 <- resp4["A: Drug X", "F", "Y"]
  denom_val4 <- resp4["A: Drug X", "F", "N"] + resp4["A: Drug X", "F", "Y"]

  expected4 <- c(resp_val4, denom_val4, resp_val4 / denom_val4)
  expect_identical(res4, expected4)

  ## Snapshot for the full table
  expect_snapshot(cran = TRUE, tbl4)

  ## Scenario 5: Analysis variable has a level not observed in data
  adsl5 <- adsl |> select(USUBJID, ARM, SEX, TRTEMFL)
  adsl5$SEX <- factor(
    as.character(adsl5$SEX),
    levels = c(levels(adsl5$SEX), "extra level")
  )

  tbl5 <- build_table(lyt, adsl5)
  res5 <- cell_values(tbl5[c("SEX", "extra level"), "A: Drug X"])
  res5 <- unlist(unname(res5))

  resp5 <- table(adsl5[, c("ARM", "SEX", "TRTEMFL")], useNA = "ifany")
  resp_val5 <- resp5["A: Drug X", "extra level", "Y"]
  denom_val5 <- resp5["A: Drug X", "extra level", "N"] +
    resp5["A: Drug X", "extra level", "Y"]

  expected5 <- c(resp_val5, denom_val5, resp_val5 / denom_val5)
  expect_identical(res5, expected5)

  ## Snapshot for the full table
  expect_snapshot(cran = TRUE, tbl5)
})

# --- character resp_var with Y + NA in subgroup after split -------------------
test_that("character resp_var with Y + NA in subgroup works after split", {
  adae_bug <- data.frame(
    USUBJID = c("XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04"),
    SEX = factor(c("Female", "Female", "Male", "Male")),
    TRT01A = c("ARMA", "ARMB", "ARMA", "ARMB"),
    TRTEMFL = c("Y", NA, "N", "Y")
  )

  lyt <- basic_table() |>
    split_cols_by("TRT01A") |>
    analyze(
      vars = "SEX",
      var_labels = "Sex, n/Ns (%)",
      show_labels = "visible",
      afun = response_by_var,
      extra_args = list(resp_var = "TRTEMFL"),
      nested = FALSE
    )

  expect_no_error(tbl <- build_table(lyt, adae_bug))
  expect_snapshot(cran = TRUE, tbl)
})

# --- h_check_char_resp_var -----------------------------------------------------
test_that("h_check_char_resp_var rejects invalid inputs", {
  df <- data.frame(
    USUBJID = c("A", "B"),
    SEX = factor(c("F", "M")),
    TRTEMFL = c("Y", "N"),
    NUMVAR = c(1, 2)
  )

  # valid input passes silently
  expect_invisible(h_check_char_resp_var(df, "TRTEMFL", "SEX", "USUBJID"))

  # resp_var must be character/factor, not numeric
  expect_error(h_check_char_resp_var(df, "NUMVAR", "SEX", "USUBJID"))

  # resp_var values must be Y/N only
  df$BAD <- c("Y", "X")
  expect_error(h_check_char_resp_var(df, "BAD", "SEX", "USUBJID"))
})
test_that("response_by_var rejects invalid resp_var", {
  df <- data.frame(
    USUBJID = c("A", "B"),
    SEX = factor(c("F", "M")),
    TRTEMFL = c("Y", "N")
  )

  lyt_factory <- function(resp) {
    basic_table() |>
      analyze(
        vars = "SEX",
        afun = response_by_var,
        extra_args = list(resp_var = resp),
        nested = FALSE
      )
  }

  expect_error(build_table(lyt_factory(NULL), df))
  expect_error(build_table(lyt_factory(NA_character_), df))
  expect_error(build_table(lyt_factory(""), df))
  expect_error(build_table(lyt_factory(TRUE), df))
  expect_error(build_table(lyt_factory("NONEXISTENT"), df))
})
