suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(rbmi)
})

test_that("find_missing_chg_after_avisit works as expected", {
  df <- data.frame(
    AVISIT = factor(c(1, 2, 3, 4, 5)),
    CHG = c(5, NA, NA, NA, 3)
  )
  result <- find_missing_chg_after_avisit(df)
  expect_identical(result, NA_character_)

  df2 <- data.frame(
    AVISIT = factor(c(1, 2, 3, 4, 5)),
    CHG = c(5, NA, 3, NA, NA)
  )
  result2 <- find_missing_chg_after_avisit(df2)
  expect_identical(result2, "4")

  df3 <- data.frame(
    AVISIT = factor(c(1, 2, 3, 4, 5)),
    CHG = c(NA, NA, NA, NA, NA)
  )
  result3 <- find_missing_chg_after_avisit(df3)
  expect_identical(result3, "1")
})

test_that("find_missing_chg_after_avisit handles unsorted input correctly", {
  df_unsorted <- data.frame(
    AVISIT = factor(c(5, 4, 3, 2, 1)),
    CHG = c(NA, NA, 3, NA, 5)
  )
  result <- find_missing_chg_after_avisit(df_unsorted)
  expect_identical(result, "4")

  df_nonseq <- data.frame(
    AVISIT = factor(c("Visit 10", "Visit 20", "Visit 30")),
    CHG = c(5, NA, NA)
  )
  result2 <- find_missing_chg_after_avisit(df_nonseq)
  expect_identical(result2, "Visit 20")
})

test_that("find_missing_chg_after_avisit validates input correctly", {
  expect_error(
    find_missing_chg_after_avisit(list(AVISIT = factor(1:3), CHG = c(1, 2, 3))),
    "Assertion on 'df' failed"
  )
  df_char <- data.frame(AVISIT = c("1", "2", "3"), CHG = c(1, 2, 3))
  expect_error(
    find_missing_chg_after_avisit(df_char),
    "Assertion on 'df\\$AVISIT' failed"
  )
  df_char_chg <- data.frame(AVISIT = factor(1:3), CHG = c("1", "2", "3"))
  expect_error(
    find_missing_chg_after_avisit(df_char_chg),
    "Assertion on 'df\\$CHG' failed"
  )
})
