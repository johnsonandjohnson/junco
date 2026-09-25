suppressPackageStartupMessages({
  library(testthat)
  library(rtables)
})

# Helper to construct a simple .spl_context data.frame
mk_context <- function(col_vals) {
  # .spl_context is a data.frame with col cur_col_split_val containing lists
  ctx <- data.frame(
    cur_col_split_val = I(list(col_vals)),
    stringsAsFactors = FALSE
  )
  return(ctx)
}

test_that("column_stats returns correct row values for AVAL mean", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10, 20, 30, 40, 50)
  )
  # Create context for AVAL Mean
  ctx <- mk_context(c("AVAL", "mean"))
  rows <- column_stats(df, "AVISIT", ctx)
  expect_s3_class(rows, "RowsVerticalSection")
  expect_equal(length(rows), 3)
  expect_equal(names(rows), paste0(c("Baseline (DB)", "Week 1", "Week 2"), ".mean"))
  # Check that each entry is an rcell of length 1 (character or numeric)
  for (val in rows) {
    expect_s3_class(val, "CellValue")
  }
})

test_that("column_stats excludes Baseline for CHG stat N", {
  # Sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    CHG = c(1, 2, 3, 4, 5)
  )
  # Context for CHG and N
  ctx <- mk_context(c("CHG", "n"))
  rows <- column_stats(df, "AVISIT", ctx)
  # RowsVerticalSection of length 3
  expect_s3_class(rows, "RowsVerticalSection")
  expect_equal(names(rows), paste0(c("Baseline (DB)", "Week 1", "Week 2"), ".n"))
  # Baseline is excluded: should produce an empty cell
  expect_equal(as.character(rows[["Baseline (DB).n"]]), "NULL")
  # Others should be CellValue with counts
  expect_s3_class(rows[["Week 1.n"]], "CellValue")
  expect_equal(as.numeric(rows[["Week 1.n"]]), 2)
})

test_that("column_stats calculates SD statistic correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10, 20, 30, 40, 50)
  )
  # Create context for AVAL SD
  ctx <- mk_context(c("AVAL", "sd"))
  rows <- column_stats(df, "AVISIT", ctx)

  week1_sd <- format_rcell(rows[["Week 1.sd"]])
  expect_equal(week1_sd, "7.071")
})

test_that("column_stats calculates mean_sd statistic correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10, 20, 30, 40, 50)
  )
  # Create context for AVAL mean_sd
  ctx <- mk_context(c("AVAL", "mean_sd"))
  rows <- column_stats(df, "AVISIT", ctx)

  week1_mean_sd <- format_rcell(rows[["Week 1.mean_sd"]])
  expect_equal(week1_mean_sd, "25.00 (7.071)")
})

test_that("column_stats calculates SE statistic correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c(
      "Baseline (DB)",
      "Week 1",
      "Week 1",
      "Week 1",
      "Week 2",
      "Week 2"
    ),
    AVAL = c(10, 20, 30, 40, 50, 60)
  )
  # Create context for AVAL SE
  ctx <- mk_context(c("AVAL", "se"))
  rows <- column_stats(df, "AVISIT", ctx)

  week1_se <- format_rcell(rows[["Week 1.se"]])
  expect_equal(week1_se, "5.774")
})

test_that("column_stats calculates Med, Min, Max correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c(
      "Baseline (DB)",
      "Week 1",
      "Week 1",
      "Week 1",
      "Week 2",
      "Week 2"
    ),
    AVAL = c(10, 20, 30, 40, 50, 60)
  )

  # Test Med
  ctx_med <- mk_context(c("AVAL", "median"))
  rows_med <- column_stats(df, "AVISIT", ctx_med, .formats = "xx.x")
  expect_equal(format_rcell(rows_med[["Week 1.median"]]), "30.0")

  # Test Min
  ctx_min <- mk_context(c("AVAL", "min"))
  rows_min <- column_stats(df, "AVISIT", ctx_min, .formats = "xx.")
  expect_equal(format_rcell(rows_min[["Week 1.min"]]), "20")

  # Test Max
  ctx_max <- mk_context(c("AVAL", "max"))
  rows_max <- column_stats(df, "AVISIT", ctx_max, .formats = "xx.")
  expect_equal(format_rcell(rows_max[["Week 1.max"]]), "40")
})

test_that("column_stats handles BASE variable correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    BASE = c(10, 15, 25, 20, 30)
  )

  # Create context for BASE Mean
  ctx <- mk_context(c("BASE", "mean"))
  rows <- column_stats(df, "AVISIT", ctx)

  # Test that BASE is excluded for Baseline
  expect_equal(format_rcell((rows[["Baseline (DB).mean"]])), "")

  # Test that BASE for Week 1 is calculated correctly
  week1_base_mean <- format_rcell(rows[["Week 1.mean"]])
  expect_equal(week1_base_mean, "20.00")
})

test_that("column_stats handles round_type correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10.345, 20.345, 30.345, 40.345, 50.345)
  )

  ctx <- mk_context(c("AVAL", "mean"))
  rows <- column_stats(df, "AVISIT", ctx)

  expect_equal(format_rcell(rows[["Week 1.mean"]], round_type = "iec"), "25.34")
  expect_equal(format_rcell(rows[["Week 1.mean"]], round_type = "sas"), "25.35")
  expect_equal(format_rcell(rows[["Week 1.mean"]], round_type = "iec_mod"), "25.34")
})

test_that("column_stats requires statnm from s_summary", {
  # Sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    CHG = c(1, 2, 3, 4, 5)
  )
  # Context for CHG and N
  ctx <- mk_context(c("CHG", "N"))
  expect_error(
    rows <- column_stats(df, "AVISIT", ctx),
    "Assertion on 'statnm' failed: Must be element of set"
  )
})

test_that("calc_N returns NULL for non-AVAL variables", {
  result <- calc_N(datvec = c(1, 2, 3), statnm = "N", varnm = "CHG")
  expect_null(result)
})
