# S01 appears twice in ARM=A to test that sum_unique keeps unique rows correctly
df <- data.frame(
  USUBJID = c("S01", "S01", "S02", "S03"),
  ARM = factor(c("A", "A", "A", "B")),
  EVENTS = c(1, 1, 0, 1),
  DAYS = c(10, 10, 20, 15)
)

test_that("a_sum_ratio_j produces all 4 stats in a layout", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(
        .stats = c("sum", "sum_unique", "ratio", "ratio_unique"),
        denom_by = "DAYS",
        id_var = "USUBJID"
      )
    )

  res <- expect_silent(build_table(lyt, df))
  res_act <- matrix_form(res)$string

  expect_identical(trimws(res_act[2, 2]), "2")
  expect_identical(trimws(res_act[2, 3]), "1")

  expect_identical(trimws(res_act[3, 2]), "1")
  expect_identical(trimws(res_act[3, 3]), "1")

  expect_true(nzchar(trimws(res_act[4, 2])))
  expect_true(nzchar(trimws(res_act[5, 2])))
})

test_that("a_sum_ratio_j produces correct result for sum only", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(.stats = "sum")
    )

  res <- expect_silent(build_table(lyt, df))
  res_act <- matrix_form(res)$string

  expect_identical(trimws(res_act[2, 2]), "2")
  expect_identical(trimws(res_act[2, 3]), "1")
  expect_identical(nrow(res_act), 2L)
})

test_that("a_sum_ratio_j produces correct cell values for ratio", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(
        .stats = "ratio",
        denom_by = "DAYS"
      )
    )

  res <- expect_silent(build_table(lyt, df))

  val_a <- unlist(cell_values(res)[[1]])
  expect_equal(val_a, c(2, 2 / 40), tolerance = 1e-10)

  val_b <- unlist(cell_values(res)[[2]])
  expect_equal(val_b, c(1, 1 / 15), tolerance = 1e-10)
})

test_that("a_sum_ratio_j respects custom .labels and .formats", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(
        .stats = c("sum", "ratio"),
        denom_by = "DAYS",
        .labels = c(sum = "Total Events", ratio = "Event Rate"),
        .formats = c(sum = "xx.", ratio = "xx.xx (xx.xx%)")
      )
    )

  res <- expect_silent(build_table(lyt, df))
  res_act <- matrix_form(res)$string

  expect_identical(trimws(res_act[2, 1]), "Total Events")
  expect_identical(trimws(res_act[3, 1]), "Event Rate")
})

test_that("a_sum_ratio_j handles empty data frame without error", {
  df_empty <- df[0, ]

  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(
        .stats = c("sum", "ratio"),
        denom_by = "DAYS"
      )
    )

  res <- expect_silent(build_table(lyt, df_empty))
  res_act <- matrix_form(res)$string

  expect_identical(trimws(res_act[2, 2]), "0")
  expect_identical(trimws(res_act[2, 3]), "0")
})

test_that("a_sum_ratio_j returns 0 when all values in .var are NA", {
  df_na <- data.frame(
    USUBJID = c("S01", "S02"),
    ARM = factor(c("A", "B")),
    EVENTS = c(NA_real_, NA_real_),
    DAYS = c(10, 15)
  )

  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(.stats = "sum")
    )

  res <- expect_silent(build_table(lyt, df_na))
  res_act <- matrix_form(res)$string

  expect_identical(trimws(res_act[2, 2]), "0")
  expect_identical(trimws(res_act[2, 3]), "0")
})

test_that("a_sum_ratio_j returns NA fraction when denominator is zero", {
  df_zero <- data.frame(
    USUBJID = c("S01", "S02"),
    ARM = factor(c("A", "A")),
    EVENTS = c(5, 3),
    DAYS = c(0, 0)
  )

  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    analyze(
      "EVENTS",
      afun = a_sum_ratio_j,
      extra_args = list(
        .stats = "ratio",
        denom_by = "DAYS"
      )
    )

  res <- expect_silent(build_table(lyt, df_zero))

  val <- cell_values(res)[[1]][[1]]
  expect_identical(val[1], 8)
  expect_true(is.na(val[2]))
})

test_that("a_sum_ratio_j throws an error for non-numeric .var column", {
  df_char <- data.frame(
    ARM = factor("A"),
    EVENTS = "not_a_number",
    DAYS = 10
  )
  expect_error(
    a_sum_ratio_j(df_char, .var = "EVENTS"),
    regexp = "numeric"
  )
})

test_that("a_sum_ratio_j throws an error for non-numeric denom_by column", {
  df_char_denom <- data.frame(
    ARM = factor("A"),
    EVENTS = 1,
    DAYS = "ten"
  )
  expect_error(
    a_sum_ratio_j(df_char_denom, .var = "EVENTS", .stats = "ratio", denom_by = "DAYS"),
    regexp = "numeric"
  )
})

test_that("a_sum_ratio_j throws an error when ratio requested without denom_by", {
  expect_error(
    a_sum_ratio_j(df, .var = "EVENTS", .stats = "ratio", denom_by = NULL),
    regexp = "denom_by"
  )
})

test_that("a_sum_ratio_j throws an error when sum_unique requested without id_var", {
  expect_error(
    a_sum_ratio_j(df, .var = "EVENTS", .stats = "sum_unique", id_var = NULL),
    regexp = "id_var"
  )
})

test_that("a_sum_ratio_j throws an error for invalid stat name", {
  expect_error(
    a_sum_ratio_j(df, .var = "EVENTS", .stats = "bogus"),
    regexp = "bogus"
  )
})
