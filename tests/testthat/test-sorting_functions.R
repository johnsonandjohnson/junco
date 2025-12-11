library(dplyr)
library(rtables)
library(tern)
library(tidytlg)

# Pre-processing the table
DM2 <- formatters::DM
DM2$spanhead <- factor(
  ifelse(DM2$ARM == "C: Combination", " ", "This is a Spanning Header"),
  levels = c("This is a Spanning Header", " ")
)

tab <- basic_table(round_type = "sas") |>
  split_cols_by("spanhead", split_fun = trim_levels_in_group("ARM")) |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1") |>
  summarize_row_groups() |>
  analyze_vars("COUNTRY", .stats = "count_fraction") |>
  build_table(DM2)

#### Tests for jj_complex_scorefun function ####
testthat::test_that("jj_complex_scorefun is identical to standard sorting: spanningheadercolvar=NA", {
  result <- sort_at_path(
    tab,
    path = c("root", "STRATA1"),
    scorefun = jj_complex_scorefun(spanningheadercolvar = NA)
  )
  result <- sort_at_path(
    tab,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(spanningheadercolvar = NA)
  )

  expected <- sort_at_path(
    tab,
    c("root", "STRATA1"),
    scorefun = score_occurrences_cont_cols(
      col_names = c("This is a Spanning Header.A: Drug X")
    )
  )
  expected <- sort_at_path(
    tab,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = score_occurrences_cols(
      col_names = c("This is a Spanning Header.A: Drug X")
    )
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("jj_complex_scorefun is identical to standard sorting: spanningheadercolvar=spanhead", {
  result <- sort_at_path(
    tab,
    path = c("root", "STRATA1"),
    scorefun = jj_complex_scorefun(spanningheadercolvar = "spanhead")
  )
  result <- sort_at_path(
    tab,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(spanningheadercolvar = "spanhead")
  )

  expected <- sort_at_path(
    tab,
    c("root", "STRATA1"),
    scorefun = score_occurrences_cont_cols(
      col_names = c("This is a Spanning Header.B: Placebo")
    )
  )
  expected <- sort_at_path(
    tab,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = score_occurrences_cols(
      col_names = c("This is a Spanning Header.B: Placebo")
    )
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("jj_complex_scorefun sorts correctly with colpath", {
  result <- sort_at_path(
    tab,
    path = c("root", "STRATA1"),
    scorefun = jj_complex_scorefun(
      colpath = c("spanhead", " ", "ARM", "C: Combination")
    )
  )
  result <- sort_at_path(
    tab,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(
      colpath = c("spanhead", " ", "ARM", "C: Combination")
    )
  )

  expected <- sort_at_path(
    tab,
    c("root", "STRATA1"),
    scorefun = score_occurrences_cont_cols(col_names = c(" .C: Combination"))
  )
  expected <- sort_at_path(
    tab,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = score_occurrences_cols(col_names = c(" .C: Combination"))
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("jj_complex_scorefun places specified category at the start: firstcat", {
  result <- sort_at_path(
    tab,
    path = c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(spanningheadercolvar = NA, firstcat = "count_fraction.BRA")
  )
  result <- result[2, ] # First record to check if it's BRA
  expected <- tab[4, ] # BRA record from original table
  testthat::expect_identical(result, expected)
})

testthat::test_that("jj_complex_scorefun places specified category at the end: lastcat", {
  result <- sort_at_path(
    tab,
    path = c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(spanningheadercolvar = NA, lastcat = "count_fraction.PAK")
  )
  result <- result[33, ] # Last record to check if it's PAK
  expected <- tab[27, ] # PAK record from original table
  testthat::expect_identical(result, expected)
})

tab2 <- basic_table(round_type = "sas") |>
  split_cols_by("spanhead", split_fun = trim_levels_in_group("ARM")) |>
  split_cols_by("ARM") |>
  split_cols_by("RACE") |>
  split_rows_by("STRATA1") |>
  summarize_row_groups() |>
  analyze_vars("COUNTRY", .stats = "count_fraction") |>
  build_table(DM2)

testthat::test_that("jj_complex_scorefun uses first column to sort: usefirstcol", {
  result <- sort_at_path(
    tab2,
    path = c("root", "STRATA1"),
    scorefun = jj_complex_scorefun(
      spanningheadercolvar = "spanhead",
      usefirstcol = TRUE
    )
  )
  result <- sort_at_path(
    tab2,
    path = c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(
      spanningheadercolvar = "spanhead",
      usefirstcol = TRUE
    )
  )

  expected <- sort_at_path(
    tab2,
    c("root", "STRATA1"),
    scorefun = score_occurrences_cont_cols(
      col_names = c("This is a Spanning Header.A: Drug X.ASIAN")
    )
  )
  expected <- sort_at_path(
    tab2,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = score_occurrences_cols(
      col_names = c("This is a Spanning Header.A: Drug X.ASIAN")
    )
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("jj_complex_scorefun uses first column when no spanning header: usefirstcol", {
  result <- sort_at_path(
    tab2,
    path = c("root", "STRATA1"),
    scorefun = jj_complex_scorefun(
      spanningheadercolvar = NA,
      usefirstcol = TRUE
    )
  )
  result <- sort_at_path(
    tab2,
    path = c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = jj_complex_scorefun(
      spanningheadercolvar = NA,
      usefirstcol = TRUE
    )
  )

  expected <- sort_at_path(
    tab2,
    c("root", "STRATA1"),
    scorefun = score_occurrences_cont_cols(
      col_names = c("This is a Spanning Header.A: Drug X.ASIAN")
    )
  )
  expected <- sort_at_path(
    tab2,
    c("root", "STRATA1", "*", "COUNTRY"),
    scorefun = score_occurrences_cols(
      col_names = c("This is a Spanning Header.A: Drug X.ASIAN")
    )
  )

  testthat::expect_identical(result, expected)
})
