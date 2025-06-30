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
    AVAL = c(10, 20, 30, 40, 50),
    dp = c(1, 1, 1, 1, 1)
  )
  # Create context for AVAL Mean
  ctx <- mk_context(c("AVAL", "Mean"))
  fun <- column_stats()
  rows <- fun(df, "AVISIT", ctx)
  expect_s3_class(rows, "RowsVerticalSection")
  expect_equal(length(rows), 3)
  expect_equal(names(rows), c("Baseline (DB)", "Week 1", "Week 2"))
  # Check that each entry is an rcell of length 1 (character or numeric)
  for (val in rows) {
    expect_s3_class(val, "CellValue")
  }
})

test_that("column_stats excludes Baseline for CHG stat N", {
  # Sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    CHG = c(1, 2, 3, 4, 5),
    dp = c(1, 1, 1, 1, 1)
  )
  # Context for CHG and N
  ctx <- mk_context(c("CHG", "N"))
  fun <- column_stats()
  rows <- fun(df, "AVISIT", ctx)
  # RowsVerticalSection of length 3
  expect_s3_class(rows, "RowsVerticalSection")
  expect_equal(names(rows), c("Baseline (DB)", "Week 1", "Week 2"))
  # Baseline is excluded: should produce an empty cell
  expect_equal(as.character(rows[[1]]), "NULL")
  # Others should be CellValue with counts
  expect_s3_class(rows[[2]], "CellValue")
  expect_equal(as.numeric(rows[[2]]), 2)
})

test_that("column_stats calculates SD statistic correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10, 20, 30, 40, 50),
    dp = c(1, 1, 1, 1, 1)
  )
  # Create context for AVAL SD
  ctx <- mk_context(c("AVAL", "SD"))
  fun <- column_stats()
  rows <- fun(df, "AVISIT", ctx)

  week1_sd <- as.character(rows[["Week 1"]])
  expect_equal(week1_sd, "7.071")
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
    AVAL = c(10, 20, 30, 40, 50, 60),
    dp = c(1, 1, 1, 1, 1, 1)
  )
  # Create context for AVAL SE
  ctx <- mk_context(c("AVAL", "SE"))
  fun <- column_stats()
  rows <- fun(df, "AVISIT", ctx)

  week1_se <- as.character(rows[["Week 1"]])
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
    AVAL = c(10, 20, 30, 40, 50, 60),
    dp = c(0, 0, 0, 0, 0, 0)
  )

  # Test Med
  ctx_med <- mk_context(c("AVAL", "Med"))
  fun_med <- column_stats()
  rows_med <- fun_med(df, "AVISIT", ctx_med)
  expect_equal(as.character(rows_med[["Week 1"]]), "30.0")

  # Test Min
  ctx_min <- mk_context(c("AVAL", "Min"))
  fun_min <- column_stats()
  rows_min <- fun_min(df, "AVISIT", ctx_min)
  expect_equal(as.character(rows_min[["Week 1"]]), "20")

  # Test Max
  ctx_max <- mk_context(c("AVAL", "Max"))
  fun_max <- column_stats()
  rows_max <- fun_max(df, "AVISIT", ctx_max)
  expect_equal(as.character(rows_max[["Week 1"]]), "40")
})

test_that("column_stats handles BASE variable correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    BASE = c(10, 15, 25, 20, 30),
    dp = c(1, 1, 1, 1, 1)
  )

  # Create context for BASE Mean
  ctx <- mk_context(c("BASE", "Mean"))
  fun <- column_stats()
  rows <- fun(df, "AVISIT", ctx)

  # Test that BASE is excluded for Baseline
  expect_equal(as.character(rows[["Baseline (DB)"]]), "NULL")

  # Test that BASE for Week 1 is calculated correctly
  week1_base_mean <- as.character(rows[["Week 1"]])
  expect_equal(week1_base_mean, "20.00")
})

test_that("column_stats handles iec roundmethod correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10.345, 20.345, 30.345, 40.345, 50.345),
    dp = c(1, 1, 1, 1, 1)
  )

  # Function to test with iec roundmethod
  calc_one_visit_R <- function(datvec, decimal, statnm, visit, varnm) {
    calc_one_visit(
      datvec,
      decimal,
      statnm,
      visit,
      varnm,
      roundmethod = "iec",
      exclude_visits = "Baseline (DB)"
    )
  }

  # Apply function to test data
  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Mean",
    "Week 1",
    "AVAL"
  )

  expect_equal(result_R, "25.34")

  # Compare to SAS rounding
  result_SAS <- calc_one_visit(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Mean",
    "Week 1",
    "AVAL",
    roundmethod = "sas",
    exclude_visits = "Baseline (DB)"
  )
  expect_equal(result_SAS, "25.35")
})

test_that("column_N function works correctly", {
  # Create sample data
  df <- data.frame(
    TRT = c("Drug A", "Drug A", "Drug B", "Drug B", "Drug B"),
    USUBJID = c("001", "002", "003", "004", "004"), # Note 004 is duplicated
    AVAL = c(10, 20, 30, 40, 50)
  )

  # Create context for AVAL N
  ctx <- mk_context(c("AVAL", "N"))

  # Call column_N
  rows <- suppressWarnings(column_N(df, "TRT", ctx))

  # Should have counts of subjects by TRT group
  expect_equal(as.numeric(rows[["Drug A"]]), 2) # 2 unique subjects
  expect_equal(as.numeric(rows[["Drug B"]]), 2) # 2 unique subjects (one duplicate)
})

test_that("postfun_cog works correctly", {
  # Create a basic return value and context
  ret <- list()
  fulldf <- data.frame(AVAL = c(10, 20, 30))

  # Create spl_context for AVAL
  spl_context <- data.frame(
    value = I(list("AVAL")),
    stringsAsFactors = FALSE
  )

  # Call postfun_cog
  result <- postfun_cog(ret, NULL, fulldf, spl_context)

  # Check structure of result
  expect_type(result, "list")
  expect_equal(
    names(result$values),
    c("N", "Mean", "SD", "SE", "Med", "Min", "Max")
  )
  expect_equal(length(result$datasplit), 7)
})

test_that("postfun_eq5d works correctly", {
  # Create a basic return value and context
  ret <- list()
  fulldf <- data.frame(AVAL = c(10, 20, 30))

  # Create spl_context for AVAL
  spl_context <- data.frame(
    value = I(list("AVAL")),
    stringsAsFactors = FALSE
  )

  # Call postfun_eq5d
  result <- postfun_eq5d(ret, NULL, fulldf, spl_context)

  # Check structure of result
  expect_type(result, "list")
  expect_equal(names(result$values), c("N", "Mean", "SD", "Med", "Min", "Max"))
  expect_equal(length(result$datasplit), 6)

  # Test for CHG
  spl_context$value <- list("CHG")
  result_chg <- postfun_eq5d(ret, NULL, fulldf, spl_context)
  expect_equal(
    names(result_chg$values),
    c("N", "Mean", "SE", "SD", "Med", "Min", "Max")
  )
  expect_equal(length(result_chg$datasplit), 7)

  # Test for BASE
  spl_context$value <- list("BASE")
  result_base <- postfun_eq5d(ret, NULL, fulldf, spl_context)
  expect_equal(names(result_base$values), c("mean_sd"))
  expect_equal(length(result_base$datasplit), 1)
})

test_that("postfun_cog and postfun_eq5d handle error cases", {
  # Create a basic return value and context
  ret <- list()
  fulldf <- data.frame(AVAL = c(10, 20, 30))

  # Create spl_context with invalid value
  spl_context <- data.frame(
    value = I(list("INVALID")),
    stringsAsFactors = FALSE
  )

  # Expect error for postfun_cog
  expect_error(
    postfun_cog(ret, NULL, fulldf, spl_context),
    "something bad happened"
  )

  # Expect error for postfun_eq5d
  expect_error(
    postfun_eq5d(ret, NULL, fulldf, spl_context),
    "something bad happened"
  )
})

test_that("calc_N returns NULL for non-AVAL variables", {
  result <- calc_N(datvec = c(1, 2, 3), statnm = "N", varnm = "CHG")
  expect_null(result)
})
