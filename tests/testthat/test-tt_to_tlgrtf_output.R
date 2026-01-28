## Tests for tt_to_tlgrtf() export behavior

mk_tbl <- function() {
  lyt <- rtables::basic_table() |>
    rtables::split_rows_by("Species") |>
    rtables::analyze("Sepal.Length", mean)
  rtables::build_table(lyt, iris)
}

testthat::test_that("tt_to_tlgrtf .csv export behaviors", {

  tt <- mk_tbl()

  # 1) Default: csv next to rtf
  rtf_file <- file.path(tempdir(), "t01")
  res <- tt_to_tlgrtf(tt, file = rtf_file, export_csv = TRUE)
  testthat::expect_true(file.exists(paste0(rtf_file, ".rtf")))
  testthat::expect_true(file.exists(paste0(rtf_file, ".csv")))

  # 2) export_csv = TRUE, explicit csv dir
  rtf_file2 <- file.path(tempdir(), "t02")
  csv_dir <- tempdir()
  res <- suppressMessages(tt_to_tlgrtf(tt, file = rtf_file2, export_csv = TRUE, output_csv_directory = csv_dir))

  testthat::expect_true(file.exists(paste0(rtf_file2, ".csv")))

  # 3) export_csv = FALSE
  rtf_file3 <- file.path(tempdir(), "t03")
  res <- tt_to_tlgrtf(tt, file = rtf_file3, export_csv = FALSE)
  testthat::expect_false(file.exists(paste0(rtf_file3, ".csv")))

  # 4) Non-existent csv dir -> fallback next to rtf with message
  bad_csv_dir <- file.path(tempdir(), "does_not_exist")
  rtf_file4 <- file.path(tempdir(), "t04")
  testthat::expect_message(
    tt_to_tlgrtf(tt, file = rtf_file4, export_csv = TRUE, output_csv_directory = bad_csv_dir),
    regexp = "does not exist; csv will be saved"
  )
  testthat::expect_true(file.exists(paste0(rtf_file4, ".csv")))
})
