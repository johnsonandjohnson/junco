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

rtf_transform <- function(vec) {
  gsub("\r", "", fixed = TRUE, vec)
}

read_write_hax <- function(fl) {
  txt <- suppressWarnings(readLines(fl))
  writeLines(txt, fl)
}

rtf_out_wrapper <- function(
  tt,
  filnm,
  ...,
  part = 1,
  combined = FALSE,
  round_type = obj_round_type(tt)
) {
  fullfl <- file.path(tempdir(), filnm)
  res <- tt_to_tlgrtf(
    tt,
    file = fullfl,
    ...,
    combined_rtf = combined,
    round_type = round_type
  )
  nf <- length(res)
  if (combined) {
    res <- paste0(fullfl, "allparts.rtf")
  } else {
    fpaths <- mk_part_names(nf, fullfl)
    res <- paste0(fpaths, ".rtf")
    if (!is.na(part)) {
      res <- res[part]
    }
  }
  res
}

# all elements result in different rounding sas vs iec with format xx.xx
# third element only results in different rounding iec_mod vs iec with format xx.xx
vals_round_type <- c(1.865, 2.985, -0.001)

vals_round_type_fmt <- function(vals = vals_round_type, round_type = "sas") {
  mapply(format_value, x = vals, format = "xx.xx", round_type = round_type)
}

tt_to_test_round_type <- function(vals = vals_round_type, round_type = "iec") {
  require(dplyr, quietly = TRUE)
  txtvals_iec <- vals_round_type_fmt(vals = vals, round_type = "iec")
  txtvals_sas <- vals_round_type_fmt(vals = vals, round_type = "sas")

  # confirmation that at least one of the values result in different format presentation
  expect_true(any(txtvals_iec != txtvals_sas))

  adsl <- ex_adsl

  adsl <- adsl |>
    mutate(new_var = case_when(
      ARMCD == "ARM A" ~ vals[1],
      ARMCD == "ARM B" ~ vals[2],
      ARMCD == "ARM C" ~ vals[3]
    ))

  lyt <- basic_table(show_colcounts = FALSE, round_type = round_type) |>
    split_cols_by("ARMCD") |>
    analyze(c("new_var"), function(x) {
      in_rows(
        mean = mean(x),
        .formats = c("xx.xx"),
        .labels = c("Mean")
      )
    })

  tbl <- build_table(lyt, adsl)
}

listingdf_to_test_round_type <- function(vals = vals_round_type, round_type = "iec") {
  require(dplyr, quietly = TRUE)
  txtvals_iec <- vals_round_type_fmt(vals = vals, round_type = "iec")
  txtvals_sas <- vals_round_type_fmt(vals = vals, round_type = "sas")

  # confirmation that at least one of the values result in different format presentation
  expect_true(any(txtvals_iec != txtvals_sas))

  lsting <- ex_adae |>
    dplyr::select(USUBJID, AGE, SEX, RACE, ARM, BMRKR1) |>
    dplyr::distinct() |>
    dplyr::group_by(ARM) |>
    dplyr::slice_head(n = 2) |>
    dplyr::ungroup()

  lsting[1, "BMRKR1"] <- 1.865
  lsting[2, "BMRKR1"] <- 2.985
  lsting[3, "BMRKR1"] <- -0.001

  lsting <- lsting |>
    dplyr::mutate(
      AGE = tern::explicit_na(as.character(AGE), ""),
      SEX = tern::explicit_na(SEX, ""),
      RACE = explicit_na(RACE, ""),
      COL0 = explicit_na(.data[["ARM"]], ""),
      COL1 = explicit_na(USUBJID, ""),
      COL2 = paste(AGE, SEX, RACE, sep = " / "),
      COL3 = BMRKR1
    ) |>
    arrange(COL0, COL1)

  lsting <- formatters::var_relabel(
    lsting,
    COL0 = "Treatment Group",
    COL1 = "Subject ID",
    COL2 = paste("Age (years)", "Sex", "Race", sep = " / "),
    COL3 = "Biomarker 1"
  )

  ls1 <- rlistings::as_listing(
    df = lsting,
    key_cols = c("COL0", "COL1"),
    disp_cols = c("COL0", "COL1", "COL2", "COL3"),
    col_formatting = list(COL3 = formatters::fmt_config(format = "xx.xx"))
  )

  list(
    df = lsting,
    lsting = ls1
  )
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
  expect_snapshot_file(transform = rtf_transform, rtf_out_wrapper(empty_lsting, "testemptylisting"), cran = TRUE, )
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
    expect_snapshot_file(transform = rtf_transform, fl, cran = TRUE)
    expect_snapshot_file(transform = rtf_transform, gsub("rtf$", "csv", fl))
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
  expect_snapshot_file(transform = rtf_transform, cmb_fl, cran = TRUE)
  res_nullfl <- expect_silent(tt_to_tlgrtf(tbl_wide, file = NULL))
  expect_equal(length(res_nullfl), 7)
  ## extraneous empty line when we turn off timestamp line fix in next release
  expect_equal(sapply(res_nullfl, nrow), rep(nrow(tbl_wide) + nlines(col_info(tbl_wide)), 7) + 1)
})

test_that("tt_to_tlgrtf converts table tree to tlg without error", {
  # Create a simple table for testing
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze("AGE")

  tbl <- build_table(lyt, ex_adsl)

  # test that it runs without error
  expect_snapshot_file(transform = rtf_transform, rtf_out_wrapper(tbl, "test1"), cran = TRUE)
  expect_snapshot_file(transform = rtf_transform, rtf_out_wrapper(tbl, "test1b", colwidths = 120), cran = TRUE)
  expect_no_error(suppressMessages(result <- tt_to_tlgrtf(tbl, file = tempfile())))
  expect_true(is.null(result[[1]]))

  lsting <- as_listing(ex_adsl[1:30, 1:10])
  expect_snapshot_file(transform = rtf_transform, rtf_out_wrapper(lsting, "listing1"), cran = TRUE)

  badlyt <- basic_table() |>
    split_rows_by("ARM") |>
    summarize_row_groups()

  badtbl <- build_table(badlyt, ex_adsl)

  ## Test that an error is issued when validate=TRUE (default behavior)
  expect_error(tt_to_tbldf(badtbl))
  expect_error(tt_to_tbldf(badtbl, validate = TRUE))

  ## Test that a message is issued when validate=FALSE
  expect_message(
    tt_to_tbldf(badtbl, validate = FALSE),
    "Invalid table structure detected"
  )

  ## Test that a different message is issued for valid tables when validate=FALSE
  expect_message(
    tt_to_tbldf(tbl, validate = FALSE),
    "Table structure validation succeeded"
  )

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
    expect_snapshot_file(transform = rtf_transform, fl, cran = TRUE)
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

  # Test that a message is issued when validate=TRUE
  expect_message(
    tt_to_tlgrtf(badtbl, file = NULL, validate = TRUE),
    "Invalid table structure detected"
  )

  # Test that no message is issued when validate=FALSE
  expect_no_message(
    tt_to_tlgrtf(badtbl, file = NULL, validate = FALSE)
  )

  # Test that the default behavior (validate=TRUE) issues a message
  expect_message(
    tt_to_tlgrtf(badtbl, file = NULL),
    "Invalid table structure detected"
  )
})

test_that("tt_to_tlgrtf does not message for valid table structures with validate=TRUE", {
  # Create a valid table structure
  data(ex_adsl)
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze("AGE")

  tbl <- build_table(lyt, ex_adsl)

  # Test that no message is issued for a valid table with validate=TRUE
  expect_no_message(
    tt_to_tlgrtf(tbl, file = NULL, validate = TRUE)
  )
})

test_that("tt_to_tlgrtf validates valid table structures correctly", {
  # Create a valid table structure
  data(ex_adsl)
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze("AGE")

  tbl <- build_table(lyt, ex_adsl)

  # Test that a message is issued for valid tables when validate=FALSE
  expect_message(
    tt_to_tlgrtf(tbl, file = NULL, validate = FALSE),
    "Table structure validation succeeded"
  )

  # Test that no message is issued for valid tables when validate=TRUE
  expect_no_message(
    tt_to_tlgrtf(tbl, file = NULL, validate = TRUE)
  )
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

test_that("round_type in tt_to_tbldf works as expected for tabletree object", {
  tbl_iec <- tt_to_test_round_type(round_type = "iec")
  tbldf <- tt_to_tbldf(tbl_iec)
  tbldf_sas <- tt_to_tbldf(tbl_iec, round_type = "sas")
  expect_true(any(tbldf != tbldf_sas))
  expect_true(all(
    tbldf[, c("col 1", "col 2", "col 3")] ==
      vals_round_type_fmt(
        vals = vals_round_type,
        round_type = "iec"
      )
  ))
  expect_true(all(
    tbldf_sas[, c("col 1", "col 2", "col 3")] ==
      vals_round_type_fmt(
        vals = vals_round_type,
        round_type = "sas"
      )
  ))
})

test_that("round_type in tt_to_tlgrtf works as expected for tabletree object", {
  tbl_iec <- tt_to_test_round_type(round_type = "iec")
  expect_silent(suppressMessages(rtf_sas <- rtf_out_wrapper(tbl_iec, "test4sas", round_type = "sas")))
  expect_snapshot_file(transform = rtf_transform, rtf_sas, cran = TRUE)
  expect_silent(suppressMessages(rtf_iec_mod <- rtf_out_wrapper(tbl_iec, "test4iecmod", round_type = "iec_mod")))
  expect_snapshot_file(transform = rtf_transform, rtf_iec_mod, cran = TRUE)
  expect_silent(suppressMessages(rtf_iec <- rtf_out_wrapper(tbl_iec, "test4iec", round_type = "iec")))
  expect_snapshot_file(transform = rtf_transform, rtf_iec, cran = TRUE)

  # test actual values for sas rounding
  res_nullfl <- expect_silent(tt_to_tlgrtf(tbl_iec, round_type = "sas", file = NULL))

  vals_from_res_nullfl <- res_nullfl[[1]][[1]]
  vals_from_res_nullfl <- unname(unlist(vals_from_res_nullfl[3, 2:4]))

  expect_true(all(
    vals_from_res_nullfl ==
      vals_round_type_fmt(
        vals = vals_round_type,
        round_type = "sas"
      )
  ))
})

test_that("round_type in tt_to_tlgrtf works as expected for listing object", {
  listdf_iec <- listingdf_to_test_round_type()
  list_iec <- listdf_iec$lsting
  df <- listdf_iec$df

  res_nullfl_sas <- expect_silent(tt_to_tlgrtf(list_iec, round_type = "sas", file = NULL))
  res_nullfl_iec <- expect_silent(tt_to_tlgrtf(list_iec, round_type = "iec", file = NULL))
  vals_from_res_nullfl_sas <- res_nullfl_sas[[1]][[4]][-c(1:2, 9)]
  vals_from_res_nullfl_iec <- res_nullfl_iec[[1]][[4]][-c(1:2, 9)]

  expect_identical(
    vals_round_type_fmt(vals = df[["BMRKR1"]], round_type = "sas"),
    vals_from_res_nullfl_sas
  )

  expect_identical(
    vals_round_type_fmt(vals = df[["BMRKR1"]], round_type = "iec"),
    vals_from_res_nullfl_iec
  )

  expect_true(any(vals_from_res_nullfl_iec != vals_from_res_nullfl_sas))
})
