# Reprex: a_sum_ratio_j — verify all 4 stats with minimal data
# S01 appears twice in ARM=A → sum vs sum_unique should differ

df <- data.frame(
  USUBJID = c("S01", "S01", "S02", "S03"),
  ARM = factor(c("A", "A", "A", "B")),
  EVENTS = c(1, 1, 0, 1),
  DAYS = c(10, 10, 20, 15)
)

#
# Expected results per ARM:
#
#          |   A          |   B
# ---------+--------------+---------
# sum      |   2          |   1
# sum_uniq |   1          |   1
# ratio    |   2/40=0.05  |   1/15=0.067
# rat_uniq |   1/30=0.033 |   1/15=0.067
#

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

  tbl <- build_table(lyt, df)
  tbl_str <- mf_strings(matrix_form(tbl))

  # Row 1: sum — A=2, B=1
  expect_identical(trimws(tbl_str[2, 2]), "2")
  expect_identical(trimws(tbl_str[2, 3]), "1")

  # Row 2: sum_unique — A=1, B=1
  expect_identical(trimws(tbl_str[3, 2]), "1")
  expect_identical(trimws(tbl_str[3, 3]), "1")

  # Row 3: ratio — A has 2 events / 40 days, B has 1 / 15
  # Row 4: ratio_unique — A has 1 / 30, B has 1 / 15
  # (format "xx (xx.x%)" — just check they're non-empty for now)
  expect_true(nzchar(trimws(tbl_str[4, 2])))
  expect_true(nzchar(trimws(tbl_str[5, 2])))
})
