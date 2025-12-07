aesevall_spf <- make_combo_splitfun(
  nm = "AESEV_ALL",
  label = "Any AE",
  levels = NULL,
)

# Start of tests ----

test_that("a_maxlev produces correct numbers for single treatment per subject", {
  my_adsl <- ex_adsl[, c("USUBJID", "ARM", "ACTARM")]
  my_adae <- ex_adae[, c("USUBJID", "ARM", "ACTARM", "AESEV")]

  my_adae <- my_adae |>
    mutate(AESEV := string_to_title(AESEV)) |>
    mutate(
      AESEV := ordered(AESEV, levels = c("Missing", "Mild", "Moderate", "Severe"))
    )

  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    add_overall_col("Total") |>
    split_rows_by("AESEV", split_fun = aesevall_spf) |>
    summarize_row_groups(
      "AESEV",
      cfun = a_maxlev, extra_args = list(any_level = TRUE)
    ) |>
    analyze("AESEV", afun = a_maxlev)

  res <- expect_silent(build_table(lyt, my_adae, alt_counts_df = my_adsl))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "Any AE", "Missing", "Mild", "Moderate", "Severe",
      "A: Drug X", "122 (91.0%)", "0", "7 (5.2%)", "24 (17.9%)", "91 (67.9%)",
      "B: Placebo", "123 (91.8%)", "0", "9 (6.7%)", "24 (17.9%)", "90 (67.2%)",
      "C: Combination", "120 (90.9%)", "0", "4 (3.0%)", "23 (17.4%)", "93 (70.5%)",
      "Total", "365 (91.3%)", "0", "20 (5.0%)", "71 (17.8%)", "274 (68.5%)"
    ),
    dim = 6:5
  )
  expect_identical(res_act, res_exp)
})

test_that("a_maxlev produces correct numbers for sequence of treatments (missing values)", {
  treatments <- factor(c("a", "b", "c"))
  ae_severities <- c("Missing", "Mild", "Moderate", "Severe")
  ae_severities <- ordered(ae_severities, levels = ae_severities)
  my_adae <- data.frame(
    ID = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4),
    TRT = factor(c("a", "b", "b", "b", "c", "c", "a", "c", "b", "b")),
    AESEV = ae_severities[c(4L, 1L, 2L, 1L, 2L, 1L, 2L, 3L, 1L, 2L)]
  )
  my_adsl <- data.frame(
    ID = rep(1:5, each = 3),
    TRT = factor(rep(c("a", "b", "c"), times = 5))
  )

  lyt <- basic_table() |>
    split_cols_by("TRT") |>
    add_overall_col("Total") |>
    split_rows_by("AESEV", split_fun = aesevall_spf) |>
    summarize_row_groups(
      "AESEV",
      cfun = a_maxlev,
      extra_args = list(id = "ID", any_level = TRUE)
    ) |>
    analyze(
      "AESEV",
      afun = a_maxlev,
      extra_args = list(id = "ID")
    )

  res <- expect_silent(build_table(lyt, my_adae, alt_counts_df = my_adsl))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "Any AE", "Missing", "Mild", "Moderate", "Severe",
      "a", "2 (40.0%)", "0", "1 (20.0%)", "0", "1 (20.0%)",
      "b", "2 (40.0%)", "2 (40.0%)", "2 (40.0%)", "0", "0",
      "c", "2 (40.0%)", "0", "1 (20.0%)", "1 (20.0%)", "0",
      "Total", "4 (80.0%)", "0", "2 (40.0%)", "1 (20.0%)", "1 (20.0%)"
    ),
    dim = 6:5
  )
  expect_identical(res_act, res_exp)
})

test_that("a_maxlev produces correct numbers for sequence of treatments (all values are missing for one treatement)", {
  treatments <- factor(c("a", "b", "c"))
  ae_severities <- c("Missing", "Mild", "Moderate", "Severe")
  ae_severities <- ordered(ae_severities, levels = ae_severities)
  my_adae <- data.frame(
    ID = c(rep(1L, 3L), rep(2L, 7L)),
    TRT = factor(c("a", "b", "b", rep("c", 7L))),
    AESEV = ae_severities[c(2L, 2L, 3L, rep(1L, 7L))]
  )
  my_adsl <- data.frame(
    ID = rep(1:3, each = 3),
    TRT = factor(rep(c("a", "b", "c"), times = 3))
  )

  lyt <- basic_table() |>
    split_cols_by("TRT") |>
    add_overall_col("Total") |>
    split_rows_by("AESEV", split_fun = aesevall_spf) |>
    summarize_row_groups(
      "AESEV",
      cfun = a_maxlev,
      extra_args = list(id = "ID", any_level = TRUE)
    ) |>
    analyze(
      "AESEV",
      afun = a_maxlev,
      extra_args = list(id = "ID")
    )

  res <- expect_silent(build_table(lyt, my_adae, alt_counts_df = my_adsl))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "Any AE", "Missing", "Mild", "Moderate", "Severe",
      "a", "1 (33.3%)", "0", "1 (33.3%)", "0", "0",
      "b", "1 (33.3%)", "0", "0", "1 (33.3%)", "0",
      "c", "0", "1 (33.3%)", "0", "0", "0",
      "Total", "1 (33.3%)", "1 (33.3%)", "0", "1 (33.3%)", "0"
    ),
    dim = 6:5
  )
  expect_identical(res_act, res_exp)
})

test_that("a_maxlev produces correct numbers when any_level is active for custom level excl.)", {
  treatments <- factor(c("a", "b", "c"))
  ae_severities <- c("Missing", "Mild", "Moderate", "Severe")
  ae_severities <- ordered(ae_severities, levels = ae_severities)
  my_adae <- data.frame(
    ID = c(rep(1L, 3L), rep(2L, 7L)),
    TRT = factor(c("a", "b", "b", rep("c", 7L))),
    AESEV = ae_severities[c(2L, 2L, 3L, rep(1L, 7L))]
  )
  my_adsl <- data.frame(
    ID = rep(1:3, each = 3),
    TRT = factor(rep(c("a", "b", "c"), times = 3))
  )

  lyt <- basic_table() |>
    split_cols_by("TRT") |>
    add_overall_col("Total") |>
    split_rows_by("AESEV", split_fun = aesevall_spf) |>
    summarize_row_groups(
      "AESEV",
      cfun = a_maxlev,
      extra_args = list(id = "ID", any_level = TRUE, any_level_exclude = "Mild")
    )

  res <- expect_silent(build_table(lyt, my_adae, alt_counts_df = my_adsl))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c("", "Any AE", "a", "0", "b", "1 (33.3%)", "c", "1 (33.3%)", "Total", "2 (66.7%)"),
    dim = c(2L, 5L)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_maxlev throws an error for not ordered variable", {
  treatments <- factor(c("a", "b", "c"))
  ae_severities <- c("Missing", "MILD", "Moderate", "Severe")
  ae_severities <- factor(ae_severities, levels = ae_severities)
  my_adae <- data.frame(
    ID = c(1, 1, 1, 2, 2),
    TRT = factor(c("a", "b", "b", "c", "c")),
    AESEV = ae_severities[c(2L, 2L, 3L, 1L, 1)]
  )
  my_adsl <- data.frame(
    ID = rep(1:5, each = 3),
    TRT = factor(rep(c("a", "b", "c"), times = 5))
  )

  lyt <- basic_table() |>
    split_cols_by("TRT") |>
    add_overall_col("Total") |>
    split_rows_by("AESEV", split_fun = aesevall_spf) |>
    summarize_row_groups(
      "AESEV",
      cfun = a_maxlev,
      extra_args = list(id = "ID", any_level = TRUE)
    ) |>
    analyze(
      "AESEV",
      afun = a_maxlev,
      extra_args = list(id = "ID")
    )

  res <- expect_error(
    build_table(lyt, my_adae, alt_counts_df = my_adsl),
    regexp = "*.var.*.[Mm]ust be an ordered factor*."
  )
})
