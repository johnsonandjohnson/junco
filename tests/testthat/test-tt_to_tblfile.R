library(rtables)
library(rlistings)

options(tidytlg.add_datetime = FALSE)

mk_part_names <- function(nfiles, fname) {
  if (nfiles > 1) {
    vapply(seq_len(nfiles),
      FUN.VALUE = "",
      function(i) {
        fmti <- paste0("%0", ceiling(log(nfiles, base = 10)), "d")
        paste0(fname, "part", sprintf(fmti, i), "of", nfiles)
      }
    )
  } else {
    fname
  }
}

rtf_out_wrapper <- function(tt, filnm, ..., part = 1, combined = FALSE) {
  fullfl <- file.path(tempdir(), filnm)
  res <- tt_to_tlgrtf(tt, file = fullfl, ..., combined_rtf = combined)
  nf <- length(res)
  if (combined) {
    paste0(fullfl, "allparts.rtf")
  } else {
    fpaths <- mk_part_names(nf, fullfl)
    res <- paste0(fpaths, ".rtf")
    if (!is.na(part)) {
      res <- res[part]
    }
    res
  }
}

test_that("tt_to_tlgrtf works with input Table and fontspec size 8", {
  lyt_wide <- basic_table() |>
    split_cols_by("ARM") |>
    split_cols_by("STRATA1") |>
    split_cols_by("SEX") |>
    split_rows_by("RACE") |>
    summarize_row_groups() |>
    analyze("AGE")

  tbl_wide <- build_table(lyt_wide, ex_adsl)
  fontspec <- font_spec("Times", 8L, 1.2)
  expect_no_error(rtf_out_wrapper(tbl_wide, "testfontsize8", fontspec = fontspec))
})

test_that("tt_to_tlgrtf works with an empty listing", {
  empty_lsting <- as_listing(ex_adsl[numeric(), 1:10])
  expect_snapshot_file(rtf_out_wrapper(empty_lsting, "testemptylisting"), cran = TRUE)
  expect_error(
    tt_to_tlgrtf("hi"),
    "unable to determine tlg type"
  )
})

test_that("get_ncol works as expected", {
  tt <- as_listing(ex_adsl[, 1:10])
  expect_equal(get_ncol(tt), ncol(tt))

  lyt_wide <- basic_table() |>
    split_cols_by("ARM") |>
    split_cols_by("STRATA1") |>
    split_cols_by("SEX") |>
    split_rows_by("RACE") |>
    summarize_row_groups() |>
    analyze("AGE")

  tbl_wide <- build_table(lyt_wide, ex_adsl)
  expect_equal(get_ncol(tbl_wide), ncol(tbl_wide))

  mpf <- rlistings::matrix_form(tbl_wide)
  expect_equal(get_ncol(mpf), ncol(tbl_wide))

  l <- list(tbl_wide)
  expect_equal(get_ncol(l), ncol(tbl_wide))

  l <- list(mpf)
  expect_equal(get_ncol(l), ncol(tbl_wide))
})

test_that("tt_to_tlgrtf works with wide table", {
  ## wide enough for pagination:

  lyt_wide <- basic_table() |>
    split_cols_by("ARM") |>
    split_cols_by("STRATA1") |>
    split_cols_by("SEX") |>
    split_rows_by("RACE") |>
    summarize_row_groups() |>
    analyze("AGE")

  tbl_wide <- build_table(lyt_wide, ex_adsl)
  expect_silent(suppressMessages(res_wide <- rtf_out_wrapper(tbl_wide, "test2", part = NA)))
  for (fl in res_wide) {
    expect_snapshot_file(fl, cran = TRUE)
    expect_snapshot_file(gsub("rtf$", "csv", fl))
  }
})

test_that("tt_to_tlgrtf works with argument combined_rtf = TRUE", {
  ## wide enough for pagination:

  lyt_wide <- basic_table() |>
    split_cols_by("ARM") |>
    split_cols_by("STRATA1") |>
    split_cols_by("SEX") |>
    split_rows_by("RACE") |>
    summarize_row_groups() |>
    analyze("AGE")

  tbl_wide <- build_table(lyt_wide, ex_adsl)
  expect_silent(suppressMessages(cmb_fl <- rtf_out_wrapper(tbl_wide, "test3", combined = TRUE)))
  expect_snapshot_file(cmb_fl, cran = TRUE)
  res_nullfl <- expect_silent(tt_to_tlgrtf(tbl_wide, file = NULL))
  expect_equal(length(res_nullfl), 7)
  expect_equal(sapply(res_nullfl, nrow), rep(nrow(tbl_wide) + nlines(col_info(tbl_wide)), 7))
})

test_that("tt_to_tlgrtf converts table tree to tlg without error", {
  # Create a simple table for testing
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE")

  tbl <- build_table(lyt, ex_adsl)

  # test that it runs without error
  expect_snapshot_file(rtf_out_wrapper(tbl, "test1"), cran = TRUE)
  expect_snapshot_file(rtf_out_wrapper(tbl, "test1b", colwidths = 120), cran = TRUE)
  expect_no_error(suppressMessages(result <- tt_to_tlgrtf(tbl, file = tempfile())))
  expect_true(is.null(result[[1]]))

  lsting <- as_listing(ex_adsl[1:30, 1:10])
  expect_snapshot_file(rtf_out_wrapper(lsting, "listing1"), cran = TRUE)

  badlyt <- basic_table() |>
    split_rows_by("ARM") |>
    summarize_row_groups()

  badtbl <- build_table(badlyt, ex_adsl)

  ## this test is largely meaningless because it doesn't get caught
  ##  when calling tt_to_tlgrtf directly....
  expect_error(tt_to_tbldf(badtbl))

  lyt_pgby <- basic_table() |>
    split_cols_by("ARM") |>
    split_cols_by("STRATA1") |>
    split_cols_by("SEX") |>
    split_rows_by("RACE", page_by = TRUE, split_fun = keep_split_levels(levels(ex_adsl$RACE)[1:2])) |>
    summarize_row_groups() |>
    analyze("AGE")

  tbl_pgby <- build_table(lyt_pgby, ex_adsl)
  expect_silent(suppressMessages(res_pgby <- rtf_out_wrapper(tbl_pgby, "testpageby", part = NA)))
  for (fl in res_pgby) {
    expect_snapshot_file(fl, cran = TRUE)
  }
})

test_that("cwidths_final_adj calculates adjusted column widths correctly", {
  # Define test inputs
  labwidth_ins <- 2.5 ## use non-default value
  total_width <- 7
  colwidths <- c(10, 15, 20)

  # Calculate expected result
  proportion <- labwidth_ins / total_width
  expected_label_width <- floor(proportion / (1 - proportion) * sum(colwidths))
  expected <- c(expected_label_width, colwidths)

  # Run the function
  result <- cwidths_final_adj(labwidth_ins, total_width, colwidths)

  # Check the result
  expect_equal(result, expected)
  expect_equal(length(result), length(colwidths) + 1)
})

test_that("make_bordmat_row creates border matrix row correctly", {
  # Test 1: No spans > 1
  rowspns1 <- c(1, 1, 1, 1)
  result1 <- make_bordmat_row(rowspns1)
  expect_equal(result1, c(0, 0, 0, 0))

  # Test 2: With spans > 1
  rowspns2 <- c(2, 1, 3, 1)
  result2 <- make_bordmat_row(rowspns2)

  # Verify the length of the result first
  expect_length(result2, 5)

  # Match the actual output pattern
  expect_equal(result2, c(1, 1, 2, 2, 2))
})

test_that("tt_to_tlgrtf validates table structure correctly", {
  # Create an invalid table structure (similar to badtbl in previous test)
  data(ex_adsl)
  badlyt <- basic_table() |>
    split_rows_by("ARM") |>
    summarize_row_groups()

  badtbl <- build_table(badlyt, ex_adsl)

  # Test that a warning is issued when validate=TRUE
  expect_warning(
    tt_to_tlgrtf(badtbl, file = NULL, validate = TRUE),
    "Invalid table structure detected"
  )

  # Test that no warning is issued when validate=FALSE
  expect_no_warning(
    tt_to_tlgrtf(badtbl, file = NULL, validate = FALSE)
  )

  # Test that the default behavior (validate=TRUE) issues a warning
  expect_warning(
    tt_to_tlgrtf(badtbl, file = NULL),
    "Invalid table structure detected"
  )
})

test_that("tt_to_tlgrtf does not warn for valid table structures", {
  # Create a valid table structure
  data(ex_adsl)
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze("AGE")

  tbl <- build_table(lyt, ex_adsl)

  # Test that no warning is issued for a valid table
  expect_no_warning(
    tt_to_tlgrtf(tbl, file = NULL, validate = TRUE)
  )
})

test_that("tt_to_tlgrtf respects JUNCO_DISABLE_VALIDATION environment variable", {
  # Create an invalid table structure
  data(ex_adsl)
  badlyt <- basic_table() |>
    split_rows_by("ARM") |>
    summarize_row_groups()

  badtbl <- build_table(badlyt, ex_adsl)

  # Save current environment variable state
  old_env <- Sys.getenv("JUNCO_DISABLE_VALIDATION")

  # Set environment variable to disable validation
  Sys.setenv(JUNCO_DISABLE_VALIDATION = "TRUE")

  # Test that no warning is issued when environment variable is set
  expect_no_warning(
    tt_to_tlgrtf(badtbl, file = NULL, validate = TRUE)
  )

  # Reset environment variable
  if (old_env == "") {
    Sys.unsetenv("JUNCO_DISABLE_VALIDATION")
  } else {
    Sys.setenv(JUNCO_DISABLE_VALIDATION = old_env)
  }
})

test_that("more top left than col headers works", {
  data <- data.frame(
    TRT01A = "Dummy Treatment A",
    SEX = "M",
    AGEGR1 = "<65 years",
    RACE = "white",
    AGE = 65,
    empty_split = " "
  )

  lyt <- basic_table() |>
    split_cols_by("empty_split") |>
    split_cols_by("TRT01A") |>
    split_rows_by("SEX", label_pos = "topleft") |>
    split_rows_by("AGEGR1", label_pos = "topleft") |>
    split_rows_by("RACE", label_pos = "topleft") |>
    analyze("AGE")

  tbl <- build_table(lyt, data)
  tmpfile <- tempfile()
  expect_silent(tt_to_tlgrtf(tbl, file = tmpfile))
  expect_true(file.exists(paste0(tmpfile, ".rtf")))
  unlink(tmpfile)

})
