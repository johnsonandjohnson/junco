library(rtables)

test_that("summarize_row_counts works as expected without alt counts", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    summarize_row_counts(label_fstr = "Race: %s", alt_counts = FALSE) %>%
    analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx")

  res <- expect_silent(build_table(lyt, formatters::DM))
  mat <- matrix_form(res)$string

  expect_identical(
    mat[3, ],
    c("Race: ASIAN", "79", "68", "84")
  )
})

test_that("summarize_row_counts works as expected with alt counts", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    summarize_row_counts(label_fstr = "RACE value - %s") %>%
    analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx")

  res <- expect_silent(build_table(
    lyt,
    formatters::DM,
    alt_counts_df = rbind(DM, DM)
  ))
  mat <- matrix_form(res)$string

  expect_identical(
    mat[3, ],
    c("RACE value - ASIAN", "158", "136", "168")
  )
})
