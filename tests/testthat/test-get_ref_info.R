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

  lyt <- basic_table(round_type = "sas") |>
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
  expect_snapshot(cran = TRUE, result)

  # Compare with non-hierarchical layout.
  std_lyt <- basic_table(round_type = "sas") |>
    split_cols_by("ARM", ref_group = "B: Placebo") |>
    analyze(
      "AGE",
      extra_args = list(ref_path = ref_path),
      afun = standard_afun
    )
  std_result <- build_table(std_lyt, dm)
  expect_snapshot(cran = TRUE, std_result)
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

  lyt <- basic_table(round_type = "sas") |>
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
  expect_snapshot(cran = TRUE, result)

  # Compare with non-hierarchical layout.
  std_lyt <- basic_table(round_type = "sas") |>
    split_cols_by("ARM", ref_group = "B: Placebo") |>
    analyze(
      c("AGE", "BMRKR1"),
      extra_args = list(ref_path = ref_path),
      afun = standard_afun
    )
  std_result <- build_table(std_lyt, dm)
  expect_snapshot(cran = TRUE, std_result)

  # Keep one explicit check to verify the relationship between the two outputs
  result_matrix <- matrix_form(result)$strings
  std_result_matrix <- matrix_form(std_result)$strings
  expect_identical(
    result_matrix[-1, c(1, 2, 4, 3)],
    std_result_matrix
  )
})

test_that("get_ref_info works with a df in the presence of the overall column", {
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
    diff_means <- if (!missing(.in_ref_col) && isFALSE(.in_ref_col)) {
      mean(x) - mean(.ref_group)
    } else {
      NULL
    }
    in_rows(
      m = rcell(mean(x), label = "Mean"),
      dm = rcell(diff_means, label = "Difference in Means vs Placebo"),
      .formats = "xx.xx"
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
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(colspan_trt_map)) |>
    split_cols_by("ARM") |>
    add_overall_col("Total") |>
    analyze("AGE", afun = result_afun, extra_args = list(ref_path = ref_path))

  result <- expect_silent(build_table(lyt, dm))
  expect_snapshot(cran = TRUE, result)

  # Compare with non-hierarchical layout.
  std_lyt <- basic_table(round_type = "sas") |>
    split_cols_by("ARM", ref_group = "B: Placebo") |>
    add_overall_col("Total") |>
    analyze("AGE", afun = standard_afun, extra_args = list(ref_path = ref_path))

  std_result <- expect_silent(build_table(std_lyt, dm))
  expect_snapshot(cran = TRUE, std_result)

  result_matrix <- matrix_form(result)$strings
  std_result_matrix <- matrix_form(std_result)$strings
  expect_identical(
    result_matrix[-1, c(1, 2, 4, 3, 5)],
    std_result_matrix
  )
})

test_that("get_ref_info returns NULL values when ref_path is NULL", {
  res <- get_ref_info(NULL, .spl_context = data.frame())

  expect_null(res)
})

test_that("get_ref_info returns reference information for matching column splits", {
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

  ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

  introspect_ref_info <- function(df, ref_path, .spl_context) {
    ref_info <- get_ref_info(ref_path, .spl_context)
    in_rows(
      "Reference Group Size" = rcell(nrow(ref_info$ref_group)),
      "In Reference Column" = rcell(ref_info$in_ref_col)
    )
  }

  lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by("ARM") |>
    analyze("AGE", afun = introspect_ref_info, extra_args = list(ref_path = ref_path))

  result <- build_table(lyt, dm)

  expect_snapshot(
    cran = TRUE,
    cat(sub("[[:space:]]+$", "", capture.output(result)), sep = "\n")
  )
})

test_that("get_ref_info returns NULL reference information in risk-diff columns", {
  dm <- formatters::DM
  dm$colspan_trt <- factor(
    ifelse(dm$ARM == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  dm$rrisk_header <- "Risk Difference (95% CI)"
  dm$rrisk_label <- paste(dm$ARM, "vs B: Placebo")

  colspan_trt_map <- create_colspan_map(
    dm,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

  captured <- list()
  spy_afun <- function(df, ref_path, .spl_context) {
    colid <- .spl_context$cur_col_id[[1L]]
    if (grepl("difference", tolower(colid), fixed = TRUE)) {
      captured[[length(captured) + 1L]] <<- get_ref_info(ref_path, .spl_context)
    }
    in_rows("x" = rcell(1, format = "xx"))
  }

  lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by("ARM") |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by("ARM",
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("B: Placebo")
    ) |>
    analyze("AGE", afun = spy_afun, extra_args = list(ref_path = ref_path))

  build_table(lyt, dm)

  expect_length(captured, 2L)
  for (res in captured) {
    expect_null(res$ref_group)
    expect_null(res$in_ref_col)
  }
})

test_that("h_get_trtvar_refpath returns the expected shape and values in a risk-diff column", {
  dm <- formatters::DM
  dm$colspan_trt <- factor(
    ifelse(dm$ARM == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  dm$rrisk_header <- "Risk Difference (95% CI)"
  dm$rrisk_label <- paste(dm$ARM, "vs B: Placebo")

  colspan_trt_map <- create_colspan_map(
    dm,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

  captured <- list()
  spy_afun <- function(df, ref_path, .spl_context) {
    colid <- .spl_context$cur_col_id[[1L]]
    if (grepl("difference", tolower(colid), fixed = TRUE)) {
      res <- h_get_trtvar_refpath(ref_path, .spl_context, df, trt_var_pos = 3L)
      captured[[length(captured) + 1L]] <<- res
    }
    in_rows("x" = rcell(1, format = "xx"))
  }

  lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by("ARM") |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by("ARM",
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("B: Placebo")
    ) |>
    analyze("AGE", afun = spy_afun, extra_args = list(ref_path = ref_path))

  build_table(lyt, dm)

  expect_length(captured, 2L)
  for (res in captured) {
    expect_identical(res[["cur_trt_var"]], "ARM")
    expect_identical(res[["ref_trt_grp"]], "B: Placebo")
    expect_false(is.null(res[["cur_trt_grp"]])) # cur_trt_grp is the active arm value
  }
})

test_that("h_get_trtvar_refpath uses the requested trt_var position", {
  df <- data.frame(
    ARM = factor("A", levels = c("A", "B", "Placebo")),
    stringsAsFactors = FALSE
  )
  spl_context <- data.frame(
    cur_col_split = I(list(c("ARM", "stat", "ARM"))),
    cur_col_split_val = I(list(c("A", "N", "B")))
  )
  ref_path <- c("ARM", "Placebo")

  result <- h_get_trtvar_refpath(ref_path, spl_context, df, trt_var_pos = 5L)
  expect_identical(result[["cur_trt_grp"]], "B")
  expect_error(
    h_get_trtvar_refpath(ref_path, spl_context, df, trt_var_pos = 3L),
    "does not match"
  )
  expect_error(
    h_get_trtvar_refpath(ref_path, spl_context, df, trt_var_pos = 4L),
    "Must be TRUE"
  )
  expect_error(
    h_get_trtvar_refpath(ref_path, spl_context, df, trt_var_pos = 7L),
    "not <= 5"
  )
})
