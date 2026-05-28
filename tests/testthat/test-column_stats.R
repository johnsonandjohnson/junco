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

test_that("column_stats calculates mean_sd statistic correctly", {
  # Create sample data
  df <- data.frame(
    AVISIT = c("Baseline (DB)", "Week 1", "Week 1", "Week 2", "Week 2"),
    AVAL = c(10, 20, 30, 40, 50),
    dp = c(1, 1, 1, 1, 1)
  )
  # Create context for AVAL mean_sd
  ctx <- mk_context(c("AVAL", "mean_sd"))
  fun <- column_stats()
  rows <- fun(df, "AVISIT", ctx)

  week1_mean_sd <- as.character(rows[["Week 1"]])
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

test_that("column_stats handles iec round_type correctly", {
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
      round_type = "iec",
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

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "N",
    "Week 1",
    "AVAL"
  )

  expect_equal(result_R, 2)

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "SE",
    "Week 1",
    "AVAL"
  )

  expect_equal(result_R, "5.000")

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "SD",
    "Week 1",
    "AVAL"
  )

  expect_equal(result_R, "7.071")

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "mean_sd",
    "Week 1",
    "AVAL"
  )

  expect_equal(result_R, "25.34 (7.071)")

  # Compare to SAS rounding
  result_SAS <- calc_one_visit(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Mean",
    "Week 1",
    "AVAL",
    round_type = "sas",
    exclude_visits = "Baseline (DB)"
  )
  expect_equal(result_SAS, "25.35")

  # Compare to SAS rounding
  result_R_mod <- calc_one_visit(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Mean",
    "Week 1",
    "AVAL",
    round_type = "iec_mod",
    exclude_visits = "Baseline (DB)"
  )
  expect_equal(result_R_mod, "25.34")


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

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Med",
    "Week 1",
    "AVAL"
  )
  expect_equal(result_R, "30.00")

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Min",
    "Week 1",
    "AVAL"
  )
  expect_equal(result_R, "20.0")

  result_R <- calc_one_visit_R(
    df$AVAL[df$AVISIT == "Week 1"],
    1,
    "Max",
    "Week 1",
    "AVAL"
  )
  expect_equal(result_R, "40.0")
})

test_that("calc_N returns NULL for non-AVAL variables", {
  result <- calc_N(datvec = c(1, 2, 3), statnm = "N", varnm = "CHG")
  expect_null(result)
})

# Helper to build a .spl_context with a "value" column (used by postfun_eq5d)
mk_val_context <- function(val) {
  data.frame(value = I(list(val)), stringsAsFactors = FALSE)
}

test_that("postfun_eq5d returns correct split result for AVAL", {
  fulldf <- data.frame(x = 1:3)
  ctx <- mk_val_context("AVAL")
  res <- postfun_eq5d(NULL, NULL, fulldf, ctx)
  expect_equal(vapply(res$values, methods::slot, character(1), "value"),
               c(N = "N", Mean = "Mean", SD = "SD", Med = "Med", Min = "Min", Max = "Max"))
  expect_equal(res$labels, c(N = "N", mean = "Mean", SD = "SD", Med = "Med", Min = "Min", Max = "Max"))
  expect_length(res$datasplit, 6)
  expect_true(all(vapply(res$datasplit, identical, logical(1), fulldf)))
})

test_that("postfun_eq5d returns correct split result for BASE", {
  fulldf <- data.frame(x = 1:3)
  ctx <- mk_val_context("BASE")
  res <- postfun_eq5d(NULL, NULL, fulldf, ctx)
  expect_equal(vapply(res$values, methods::slot, character(1), "value"), c(mean_sd = "mean_sd"))
  expect_equal(res$labels, c(mean_sd = "Base Mean (SD)"))
  expect_length(res$datasplit, 1)
  expect_identical(res$datasplit$mean_sd, fulldf)
})

test_that("postfun_eq5d returns correct split result for CHG", {
  fulldf <- data.frame(x = 1:3)
  ctx <- mk_val_context("CHG")
  res <- postfun_eq5d(NULL, NULL, fulldf, ctx)
  expect_equal(
    vapply(res$values, methods::slot, character(1), "value"),
    c(N = "N", Mean = "Mean", SE = "SE", SD = "SD", Med = "Med", Min = "Min", Max = "Max")
  )
  expect_length(res$datasplit, 7)
  expect_true(all(vapply(res$datasplit, identical, logical(1), fulldf)))
})

test_that("postfun_eq5d errors on unknown colset", {
  fulldf <- data.frame(x = 1:3)
  ctx <- mk_val_context("UNKNOWN")
  expect_error(postfun_eq5d(NULL, NULL, fulldf, ctx), "something bad happened")
})
