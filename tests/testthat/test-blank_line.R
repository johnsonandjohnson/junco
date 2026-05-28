test_that("insert_blank_line works as expected", {
  ADSL <- ex_adsl

  lyt <- basic_table(round_type = "sas") |>
    split_cols_by("ARM") |>
    split_rows_by("STRATA1") |>
    analyze(vars = "AGE", afun = function(x) {
      in_rows(
        "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)")
      )
    }) |>
    insert_blank_line() |>
    analyze(vars = "AGE", table_names = "AGE_Range", afun = function(x) {
      in_rows(
        "Range" = rcell(range(x), format = "xx.xx - xx.xx")
      )
    }) |>
    insert_blank_line() |>
    analyze(vars = "AGE", table_names = "AGE_Median", afun = function(x) {
      in_rows(
        "Median" = rcell(median(x), format = "xx.xx")
      )
    })

  # We don't want to see any warning about duplicate table names here.
  tbl <- expect_silent(build_table(lyt, ADSL))
  tbl

  # We expect 6 blank lines: after mean and range, for each of the
  # 3 STRATA1 levels' row splits.
  expect_snapshot(cran = TRUE, mf_strings(matrix_form(tbl)))
})

test_that("insert_blank_line optionally uses custom table names", {
  ADSL <- ex_adsl

  lyt <- basic_table(round_type = "sas") |>
    split_cols_by("ARM") |>
    split_rows_by("STRATA1") |>
    analyze(vars = "AGE", afun = function(x) {
      in_rows(
        "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)")
      )
    }) |>
    insert_blank_line(table_names = "Gap1") |>
    analyze(vars = "AGE", table_names = "AGE_Range", afun = function(x) {
      in_rows(
        "Range" = rcell(range(x), format = "xx.xx - xx.xx")
      )
    }) |>
    insert_blank_line(table_names = "Gap2") |>
    analyze(vars = "AGE", table_names = "AGE_Median", afun = function(x) {
      in_rows(
        "Median" = rcell(median(x), format = "xx.xx")
      )
    })

  # We don't want to see any warning about duplicate table names here.
  tbl <- expect_silent(build_table(lyt, ADSL))
  tbl

  # We expect 6 blank lines: after mean and range, for each of the
  # 3 STRATA1 levels' row splits.
  tbl_string <- mf_strings(matrix_form(tbl))
  tbl_row_paths <- row_paths(tbl)
  for (row in c(5, 8, 14, 17, 23, 26)) {
    expect_identical(tbl_string[row, ], rep("", length.out = 4))
  }
  for (row in c(4, 13, 22)) {
    checkmate::expect_subset("Gap1", tbl_row_paths[[row]])
  }
  for (row in c(7, 16, 25)) {
    checkmate::expect_subset("Gap2", tbl_row_paths[[row]])
  }
})

test_that("prepend_label_cell works with list of CellValues input", {
  cv1 <- rtables::rcell(NULL, label = "a")
  cv2 <- rtables::rcell(NULL, label = "b")
  res <- prepend_label_cell(list(cv1, cv2), label = "Section")
  expect_type(res, "list")
  expect_length(res, 3)
  expect_equal(attr(res[[1]], "label"), "Section")
  expect_identical(res[[2]], cv1)
  expect_identical(res[[3]], cv2)
})

test_that("prepend_label_cell works with RowsVerticalSection input", {
  rvs <- rtables::in_rows(Mean = rtables::rcell(5), Range = rtables::rcell(c(1, 8)))
  res <- prepend_label_cell(rvs, label = "Descriptive Statistics", label_indent_mod = 1L)
  expect_s3_class(res, "RowsVerticalSection")
  expect_equal(length(res), 3)
  expect_equal(attr(res, "row_labels")[1], "Descriptive Statistics")
  expect_equal(attr(res, "row_labels")[2:3], c("Mean", "Range"))
})
