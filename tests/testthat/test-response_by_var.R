library(testthat)
library(rtables)
library(dplyr)

adsl <- ex_adsl
adae <- ex_adae
adae$TRTEMFL <- "Y"

had_ae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  select(USUBJID, TRTEMFL) %>%
  distinct(USUBJID, .keep_all = TRUE)

adsl <- adsl %>%
  left_join(had_ae, by = "USUBJID") %>%
  mutate(TRTEMFL = ifelse(is.na(TRTEMFL), "N", "Y"))

test_that("response_by_var various scenarios", {
  lyt <- basic_table(show_colcounts = TRUE, round_type = "sas") %>%
    split_cols_by("ARM") %>%
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
  expect_snapshot(tbl)

  ## Scenario 2: TRTEMFL has missing values and Y only, but is factor with just this level
  adsl2 <- adsl
  adsl2$TRTEMFL <- ifelse(adsl2$TRTEMFL == "Y", "Y", NA)
  adsl2$TRTEMFL <- factor(adsl2$TRTEMFL, levels = "Y")

  tbl2 <- build_table(lyt, adsl2)
  expect_snapshot(tbl2)

  ## Scenario 3: TRTEMFL has missing values and Y only, and analysis variable has missing values
  adsl3 <- adsl %>% select(USUBJID, ARM, SEX, TRTEMFL)
  adsl3$TRTEMFL <- ifelse(adsl3$TRTEMFL == "Y", "Y", NA)
  adsl3$TRTEMFL <- factor(adsl3$TRTEMFL, levels = "Y")
  adsl3$SEX[1:10] <- NA_character_

  tbl3 <- build_table(lyt, adsl3)
  res3 <- cell_values(tbl3[c("SEX", "F"), "A: Drug X"])
  res3 <- unlist(unname(res3))

  resp3 <- table(adsl3[, c("ARM", "SEX", "TRTEMFL")], useNA = "ifany")
  resp_val3 <- resp3["A: Drug X", "F", "Y"]
  denom_val3 <- sum(resp3["A: Drug X", "F", ])

  expected3 <- c(resp_val3, denom_val3, resp_val3 / denom_val3)
  expect_identical(res3, expected3)

  ## Snapshot for the full table
  expect_snapshot(tbl3)

  ## Scenario 4: TRTEMFL has missing values and Y/N, and analysis variable has missing values
  adsl4 <- adsl %>% select(USUBJID, ARM, SEX, TRTEMFL)
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
  expect_snapshot(tbl4)

  ## Scenario 5: Analysis variable has a level not observed in data
  adsl5 <- adsl %>% select(USUBJID, ARM, SEX, TRTEMFL)
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
  expect_snapshot(tbl5)
})
