# data setup ----
# S01 appears twice in ARM=A — this tests that sum_unique deduplicates correctly

df <- data.frame(
  USUBJID = c("S01", "S01", "S02", "S03"),
  ARM = factor(c("A", "A", "A", "B")),
  EVENTS = c(1, 1, 0, 1),
  DAYS = c(10, 10, 20, 15)
)

# Expected results per ARM:
#
#          |   A          |   B
# ---------+--------------+---------
# sum      |   2          |   1
# sum_uniq |   1          |   1
# ratio    |   2/40=0.05  |   1/15=0.067
# rat_uniq |   1/30=0.033 |   1/15=0.067

# Start of tests ----

# Happy path: all 4 stats together in one layout
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

  # sum: A = 1+1+0 = 2, B = 1
  expect_identical(trimws(res_act[2, 2]), "2")
  expect_identical(trimws(res_act[2, 3]), "1")

  # sum_unique: A = 1+0 = 1 (S01 deduped), B = 1
  expect_identical(trimws(res_act[3, 2]), "1")
  expect_identical(trimws(res_act[3, 3]), "1")

  # ratio and ratio_unique rows should be non-empty
  expect_true(nzchar(trimws(res_act[4, 2])))
  expect_true(nzchar(trimws(res_act[5, 2])))
})

# Single stat: verify only one row is produced
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

  # A: 1+1+0=2, B: 1
  expect_identical(trimws(res_act[2, 2]), "2")
  expect_identical(trimws(res_act[2, 3]), "1")

  # Header + 1 stat row = 2 rows total
  expect_identical(nrow(res_act), 2L)
})

# Numeric cell values: ratio returns c(numerator, fraction)
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

  # A: sum(EVENTS)=2, sum(DAYS)=40 → c(2, 0.05)
  val_a <- unlist(cell_values(res)[[1]])
  expect_equal(val_a, c(2, 2 / 40), tolerance = 1e-10)

  # B: sum(EVENTS)=1, sum(DAYS)=15 → c(1, 1/15)
  val_b <- unlist(cell_values(res)[[2]])
  expect_equal(val_b, c(1, 1 / 15), tolerance = 1e-10)
})

# Caller overrides: custom labels and formats replace defaults
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

  # Row labels should reflect caller-supplied labels
  expect_identical(trimws(res_act[2, 1]), "Total Events")
  expect_identical(trimws(res_act[3, 1]), "Event Rate")
})

# Edge case: 0-row data frame should build without error
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

  # sum(numeric(0), na.rm=TRUE) = 0
  expect_identical(trimws(res_act[2, 2]), "0")
  expect_identical(trimws(res_act[2, 3]), "0")
})

# Edge case: all NA values — sum should be 0 (na.rm = TRUE)
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

# Edge case: zero denominator — safe_ratio should return c(n, NA)
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

  # sum(EVENTS)=8, sum(DAYS)=0 → c(8, NA)
  val <- cell_values(res)[[1]][[1]]
  expect_identical(val[1], 8)
  expect_true(is.na(val[2]))
})

# Validation: .var must be numeric
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

# Validation: denom_by must be numeric
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

# Validation: ratio needs denom_by
test_that("a_sum_ratio_j throws an error when ratio requested without denom_by", {
  expect_error(
    a_sum_ratio_j(df, .var = "EVENTS", .stats = "ratio", denom_by = NULL),
    regexp = "denom_by"
  )
})

# Validation: sum_unique needs id_var
test_that("a_sum_ratio_j throws an error when sum_unique requested without id_var", {
  expect_error(
    a_sum_ratio_j(df, .var = "EVENTS", .stats = "sum_unique", id_var = NULL),
    regexp = "id_var"
  )
})

# Validation: only valid stat names accepted
test_that("a_sum_ratio_j throws an error for invalid stat name", {
  expect_error(
    a_sum_ratio_j(df, .var = "EVENTS", .stats = "bogus"),
    regexp = "bogus"
  )
})
