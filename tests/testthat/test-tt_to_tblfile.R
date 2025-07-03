library(rtables)
library(rlistings)
options(tidytlg.add_datetime = FALSE)
mk_part_names <- function(nfiles, fname) {
  if (nfiles > 1) {
    vapply(seq_len(nfiles), FUN.VALUE = "",
           function(i) {
             fmti <- paste0("%0", ceiling(log(nfiles, base = 10)), "d")
             paste0(fname, "part", sprintf(fmti, i), "of", nfiles)
           })
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
    if (!is.na(part))
      res <- res[part]
    res
  }
}

test_that("tt_to_tlgrtf converts table tree to tlg without error", {
  # Create a simple table for testing
  data(ex_adsl)
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE")

  tbl <- build_table(lyt, ex_adsl)

  # test that it runs without error
  expect_snapshot_file(rtf_out_wrapper(tbl, "test1"), cran = TRUE)
  expect_snapshot_file(rtf_out_wrapper(tbl, "test1b", colwidths = 120), cran = TRUE)
  expect_no_error(suppressMessages(result <- tt_to_tlgrtf(tbl)))
  expect_true(is.null(result[[1]]))

  ## wide enough for pagination:

  lyt_wide <- basic_table() |>
    split_cols_by("ARM") |>
    split_cols_by("STRATA1") |>
    split_cols_by("SEX") |>
    split_rows_by("RACE", split_fun = keep_split_levels(levels(ex_adsl$RACE)[1:2])) |>
    summarize_row_groups() |>
    analyze("AGE")

  tbl_wide <- build_table(lyt_wide, ex_adsl)
  expect_silent(suppressMessages(res_wide <- rtf_out_wrapper(tbl_wide, "test2", part = NA)))
  for (fl in res_wide) {
    expect_snapshot_file(fl, cran = TRUE)
    expect_snapshot_file(gsub("rtf$", "csv", fl))
  }
  expect_silent(suppressMessages(cmb_fl <- rtf_out_wrapper(tbl_wide, "test2", combined = TRUE)))
  expect_snapshot_file(cmb_fl, cran = TRUE)

  lsting <- as_listing(ex_adsl[1:30, 1:10])
  expect_snapshot_file(rtf_out_wrapper(lsting, "listing1"), cran = TRUE)

  badlyt <- basic_table() |>
    split_rows_by("ARM") |>
    summarize_row_groups()

  badtbl <- build_table(badlyt, ex_adsl)

  ## this test is largely meaningless because it doesn't get caught
  ##  when calling tt_to_tlgrtf directly....
  expect_error(tt_to_tbldf(badtbl))

  empty_lsting <-  as_listing(ex_adsl[numeric(), 1:10])
  expect_snapshot_file(rtf_out_wrapper(empty_lsting, "testemptylisting"), cran = TRUE)
  expect_error(tt_to_tlgrtf("hi"),
               "unable to determine tlg type")

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
