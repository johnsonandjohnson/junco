library(testthat)
library(rtables)
library(dplyr)


test_that("get_ref_info works with a df analysis function", {
  dm <- formatters::DM
  dm$colspan_trt <- factor(
    ifelse(dm$ARM == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  colspan_trt_map <- create_colspan_map(
    dm,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  colspan_trt_var <- create_colspan_var(
    dm,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  # A standard analysis function which uses a reference group.
  standard_afun <- function(df, .ref_group, .in_ref_col) {
    in_rows(
      "Difference of Averages" = non_ref_rcell(
        mean(df$AGE) - mean(.ref_group$AGE),
        is_ref = .in_ref_col,
        format = "xx.xx"
      )
    )
  }

  # The custom analysis function which can work with a global reference group.
  result_afun <- function(df, ref_path, .spl_context) {
    ref <- get_ref_info(ref_path, .spl_context)
    standard_afun(df, .ref_group = ref$ref_group, .in_ref_col = ref$in_ref_col)
  }

  # Define the global reference group.
  ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

  lyt <- basic_table() |>
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) |>
    split_cols_by("ARM") |>
    analyze(
      "AGE",
      extra_args = list(ref_path = ref_path),
      afun = result_afun
    )
  result <- build_table(lyt, dm)
  expect_snapshot(result)

  # Compare with non-hierarchical layout.
  std_lyt <- basic_table() |>
    split_cols_by("ARM", ref_group = "B: Placebo") |>
    analyze(
      "AGE",
      extra_args = list(ref_path = ref_path),
      afun = standard_afun
    )
  std_result <- build_table(std_lyt, dm)
  expect_snapshot(std_result)
})


test_that("get_ref_info works with a vector analysis function", {
  dm <- formatters::DM
  dm$colspan_trt <- factor(
    ifelse(dm$ARM == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  colspan_trt_map <- create_colspan_map(
    dm,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  # A standard analysis function which uses a reference group.
  standard_afun <- function(x, .ref_group, .in_ref_col) {
    in_rows(
      "Difference of Averages" = non_ref_rcell(
        mean(x) - mean(.ref_group),
        is_ref = .in_ref_col,
        format = "xx.xx"
      )
    )
  }

  # The custom analysis function which can work with a global reference group.
  result_afun <- function(x, ref_path, .spl_context, .var) {
    ref <- get_ref_info(ref_path, .spl_context, .var)
    standard_afun(x, .ref_group = ref$ref_group, .in_ref_col = ref$in_ref_col)
  }

  # Define the global reference group.
  ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

  lyt <- basic_table() |>
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) |>
    split_cols_by("ARM") |>
    analyze(
      c("AGE", "BMRKR1"),
      extra_args = list(ref_path = ref_path),
      afun = result_afun
    )
  result <- build_table(lyt, dm)
  expect_snapshot(result)

  # Compare with non-hierarchical layout.
  std_lyt <- basic_table() |>
    split_cols_by("ARM", ref_group = "B: Placebo") |>
    analyze(
      c("AGE", "BMRKR1"),
      extra_args = list(ref_path = ref_path),
      afun = standard_afun
    )
  std_result <- build_table(std_lyt, dm)
  expect_snapshot(std_result)

  # Keep one explicit check to verify the relationship between the two outputs
  result_matrix <- matrix_form(result)$strings
  std_result_matrix <- matrix_form(std_result)$strings
  expect_identical(
    result_matrix[-1, c(1, 2, 4, 3)],
    std_result_matrix
  )
})
