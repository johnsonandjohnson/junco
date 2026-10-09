# s_diff_means ----

## data setup ----

diff_means_stat_names <- paste0(
  "diff_means_", c("n1", "n2", "est", "se", "est_se", "ci", "est_ci")
)

## tests ----

test_that("s_diff_means works for unpaired data", {
  df1 <- data.frame(x = c(1, 2, 2, 3))
  df2 <- data.frame(x = c(4, 5, 4, 8, 10))

  res <- s_diff_means(df1, df2, "x", conf.level = 0.8)

  est <- mean(df1$x) - mean(df2$x)
  se <- sqrt(var(df1$x) / 4 + var(df2$x) / 5)
  ci <- t.test(df1$x, df2$x, conf.level = 0.8)$conf.int

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 4L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 5L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, est, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, se, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(est, se), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, ci, ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(est, ci), ignore_attr = TRUE)
})

test_that("s_diff_means works for unpaired data (NA, NaN)", {
  df1 <- data.frame(x = c(1, 2, NA, 3))
  df2 <- data.frame(x = c(4, 5, NaN, 8, 10))

  res <- s_diff_means(df1, df2, "x", conf.level = 0.8)

  est <- mean(1:3, na.rm = TRUE) - mean(c(4, 5, 8, 10), na.rm = TRUE)
  se <- sqrt(var(c(1, 2, 3)) / 3 + var(c(4, 5, 8, 10)) / 4)
  ci <- t.test(c(1, 2, 3), df2$x, conf.level = 0.8)$conf.int

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 4L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, est, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, se, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(est, se), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, ci, ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(est, ci), ignore_attr = TRUE)
})

test_that("s_diff_means works for paired data", {
  df1 <- data.frame(id = c("A", "B", "C", "D", "E"), x = c(1, 2, 3, 21, 5))
  df2 <- data.frame(id = c("D", "G", "B", "A"), x = c(12, 31, 3, 5))

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id", conf.level = 0.7)

  x <- c(1, 2, 21)
  y <- c(5, 3, 12)
  est <- mean(x - y)
  se <- sd(x - y) / sqrt(3)
  ci <- t.test(x, y, paired = TRUE, conf.level = 0.7)$conf.int

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, est, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, se, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(est, se), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, ci, ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(est, ci), ignore_attr = TRUE)
})

test_that("s_diff_means works for paired data (NA, NaN)", {
  df1 <- data.frame(id = c("A", "B", "C", "D", "E"), x = c(1, 2, 3, NA, 5))
  df2 <- data.frame(id = c("D", "E", "C", "B", "A", "W"), x = c(9, 14, 31, 3, NaN, 8))

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id", conf.level = 0.7)

  x <- c(2, 3, 5)
  y <- c(3, 31, 14)
  est <- mean(x - y)
  se <- sd(x - y) / sqrt(3)
  ci <- t.test(x, y, paired = TRUE, conf.level = 0.7)$conf.int

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, est, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, se, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(est, se), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, ci, ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(est, ci), ignore_attr = TRUE)
})

test_that("s_diff_means handles identical paired vectors", {
  df1 <- data.frame(id = 1:3, x = c(1, 8, 23))
  df2 <- df1

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, 0, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, 0, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(0, 0), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NaN, NaN), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(0, NaN, NaN), ignore_attr = TRUE)
})

test_that("s_diff_means handles single observation groups", {
  df1 <- data.frame(x = 10, NA, NA)
  df2 <- data.frame(x = 5, NA)

  res <- s_diff_means(df1, df2, "x")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 1L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 1L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, 5, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(5, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(5, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles single observation groups (paired)", {
  df1 <- data.frame(id = c("A", "B", "C"), x = c(10, NA, NA))
  df2 <- data.frame(id = c("C", "A"), x = c(NA, 5))

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 1L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 1L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, 5, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(5, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(5, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles empty data", {
  df1 <- data.frame(x = numeric())
  df2 <- df1

  res <- s_diff_means(df1, df2, "x")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles empty data (paired)", {
  df1 <- data.frame(id = factor(), x = numeric())
  df2 <- df1

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles empty df2", {
  df1 <- data.frame(x = c(2, 2, 2))
  df2 <- data.frame(x = numeric())

  res <- s_diff_means(df1, df2, "x")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles empty df2 (paired)", {
  df1 <- data.frame(id = c("A", "B", "C"), x = c(2, 2, 2))
  df2 <- data.frame(id = factor(), x = numeric())

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles empty df1", {
  df1 <- data.frame(x = numeric())
  df2 <- data.frame(x = c(2, 2, 2))

  res <- s_diff_means(df1, df2, "x")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles empty df1 (paired)", {
  df1 <- data.frame(id = factor(), x = numeric())
  df2 <- data.frame(id = c("A", "B", "C"), x = c(2, 2, 2))

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means returns NA when no matching IDs", {
  df1 <- data.frame(id = c("A", "B"), x = 1:2)
  df2 <- data.frame(id = c("C", "D"), x = c(4, 6))

  res <- s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 0L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(NA_real_, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})

test_that("s_diff_means handles zero variance", {
  df1 <- data.frame(x = c(2, 2, 2))
  df2 <- data.frame(x = c(1, 1, 1, NA))

  res <- s_diff_means(df1, df2, "x")

  expect_named(res, diff_means_stat_names)
  expect_identical(res$diff_means_n1, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_n2, 3L, ignore_attr = TRUE)
  expect_identical(res$diff_means_est, 1, ignore_attr = TRUE)
  expect_equal(res$diff_means_se, NA_real_, ignore_attr = TRUE)
  expect_equal(res$diff_means_est_se, c(1, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_ci, c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_identical(res$diff_means_est_ci, c(1, c(NA_real_, NA_real_)), ignore_attr = TRUE)
})


test_that("s_diff_means errors on duplicate paired IDs", {
  df1 <- data.frame(id = c("A", "A"), x = 1:2)
  df2 <- data.frame(id = c("A", "B"), x = 3:4)

  expect_error(
    s_diff_means(df1, df2, "x", paired = TRUE, paired_by = "id"),
    "[d|D]uplicate"
  )
})

test_that("s_diff_means errors if paired_by missing in paired mode", {
  df1 <- data.frame(id = "A", x = 1)
  df2 <- data.frame(id = "A", x = 2)

  expect_error(s_diff_means(df1, df2, "x", paired = TRUE), "paired_by")
})

test_that("s_diff_means errors for missing variable", {
  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(x = 4:6)

  expect_error(s_diff_means(df1, df2, "y"), "df1.*y")
})

test_that("s_diff_means errors for non-numeric input (char)", {
  df1 <- data.frame(x = letters[1:3])
  df2 <- data.frame(x = letters[4:6])

  expect_error(s_diff_means(df1, df2, "x"), "\\.var.*numeric")
})

test_that("s_diff_means errors for Inf values", {
  df1 <- data.frame(x = c(1, Inf))
  df2 <- data.frame(x = 2)

  expect_error(s_diff_means(df1, df2, "x"), "finite")
})

test_that("s_diff_means errors for non-numeric input (char)", {
  df1 <- data.frame(x = letters[1:3])
  df2 <- data.frame(x = letters[4:6])

  expect_error(s_diff_means(df1, df2, "x"), "\\.var.*numeric")
})

test_that("s_diff_means errors for non-numeric input (factor)", {
  df1 <- data.frame(x = factor(letters[1:3]))
  df2 <- data.frame(x = factor(letters[4:6]))

  expect_error(s_diff_means(df1, df2, "x"), "\\.var.*numeric")
})

# a_diff_means ----

## data setup ----

advs_bp_blw12 <- local({
  res <- subset(
    ex_advs,
    PARAMCD %in% c("DIABP", "SYSBP") & AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")
  )
  res$PARAMCD <- droplevels(res$PARAMCD)
  res$AVISIT <- droplevels(res$AVISIT)
  res
})

advs2 <- local({
  set.seed(123)
  df <- expand.grid(
    ID = c("01", "02", "03", "04"),
    ARM = c("Placebo", "Drug X", "Drug Y"),
    AVISIT = c("Baseline", "Week 12", "Week 24")
  )
  # remove some Week 24 records (irregular missing structure)
  drop_w24 <- which(df$AVISIT == "Week 24")[c(2, 5, 9)]
  df <- df[-drop_w24, ]
  df$AVAL <- sample(110:140, nrow(df), replace = TRUE)
  df$AVAL[c(4, 11)] <- NA
  df$ARM <- ordered(df$ARM, levels = c("Drug X", "Drug Y", "Placebo"))
  df
})

## tests ----

test_that("a_diff_means works in a table layout with ex_advs data", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    split_rows_by("PARAMCD") |>
    split_rows_by("AVISIT") |>
    analyze("AVAL",
      afun = a_diff_means,
      extra_args = list(ref_path = c("ARM", "B: Placebo"))
    )

  res <- expect_silent(build_table(lyt, advs_bp_blw12))
  res_matrix <- matrix_form(res)$string

  expect_snapshot(res_matrix)
})

test_that("a_diff_means works in a table layout (stacked header) with ex_advs data", {
  advs_bp_blw12$colspan_trt <- factor(
    ifelse(advs_bp_blw12$ARM == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  colsp_trt_map <- create_colspan_map(
    advs_bp_blw12,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colsp_trt_map)) |>
    split_cols_by("ARM") |>
    split_rows_by("PARAMCD") |>
    split_rows_by("AVISIT") |>
    analyze("AVAL",
      afun = a_diff_means,
      extra_args = list(ref_path = c("colspan_trt", " ", "ARM", "B: Placebo"))
    )

  res <- expect_silent(build_table(lyt, advs_bp_blw12))
  res_matrix <- matrix_form(res)$string

  expect_snapshot(res_matrix)
})

test_that("a_diff_means works in a table layout with synthetic paired data containing NA values", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    split_rows_by("AVISIT") |>
    analyze("AVAL",
      afun = a_diff_means,
      extra_args = list(ref_path = c("ARM", "Placebo"), paired = TRUE, paired_by = "ID")
    )

  res <- expect_silent(build_table(lyt, advs2))
  res_matrix <- matrix_form(res)$string

  expect_snapshot(res_matrix)
})

test_that("a_diff_means works with custom .stats, .formats, .labels, .indents", {
  df <- advs_bp_blw12[advs_bp_blw12$ARM == "A: Drug X", ]
  ref_path <- c("ARM", "B: Placebo")
  spl_context <- data.frame(
    full_parent_df = I(list(advs_bp_blw12)),
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("A: Drug X")),
    "B: Placebo" = I(list(advs_bp_blw12$ARM == "B: Placebo")),
    check.names = FALSE
  )
  .formats <- c(diff_means_est_ci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"))
  .labels <- c(diff_means_est_ci = "diff mean est + CI")

  res <- expect_silent(
    a_diff_means(
      df = df,
      .var = "AVAL",
      .spl_context = spl_context,
      ref_path = ref_path,
      .stats = "diff_means_est_ci",
      .formats = .formats,
      .labels = .labels,
      .indent_mods = 4L
    )
  )

  # expected
  exp_est <- list(
    diff_means_est_ci = c(
      diff_means_est = 0.076020,
      diff_means_ci_lwr = -0.723132,
      diff_means_ci_upr = 0.875172
    )
  )
  attr(exp_est$diff_means_est_ci, "conf.level") <- 0.95
  attr(exp_est$diff_means_est_ci, "label") <- "Difference in Means (95% CI)"

  exp <- in_rows(
    .list = exp_est,
    .formats = .formats,
    .names = "diff_means_est_ci",
    .labels = .labels,
    .indent_mods = 4L
  )

  expect_equal(res, exp, tolerance = 10^-6)
})

test_that("a_diff_means works when in ref column", {
  df <- advs_bp_blw12[advs_bp_blw12$ARM == "A: Drug X", ]
  ref_path <- c("ARM", "B: Placebo")
  spl_context <- data.frame(
    full_parent_df = I(list(advs_bp_blw12)),
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("B: Placebo")),
    "B: Placebo" = I(list(advs_bp_blw12$ARM == "B: Placebo")),
    check.names = FALSE
  )

  res <- expect_silent(
    a_diff_means(
      df = df,
      .var = "AVAL",
      .spl_context = spl_context,
      ref_path = ref_path,
    )
  )

  expect_snapshot(res)
})

test_that("a_diff_means works with no observations in the column-split sample", {
  df <- advs_bp_blw12[FALSE, ]
  ref_path <- c("ARM", "B: Placebo")
  spl_context <- data.frame(
    full_parent_df = I(list(advs_bp_blw12)),
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("A: Drug X")),
    "B: Placebo" = I(list(advs_bp_blw12$ARM == "B: Placebo")),
    check.names = FALSE
  )

  res <- expect_silent(
    a_diff_means(
      df = df,
      .var = "AVAL",
      .spl_context = spl_context,
      ref_path = ref_path,
    )
  )

  expect_snapshot(res)
})

test_that("a_diff_means works with no observations in the ref sample", {
  df <- advs_bp_blw12[advs_bp_blw12$ARM == "A: Drug X", ]
  full_parent_df <- advs_bp_blw12[advs_bp_blw12$ARM != "B: Placebo", ]
  ref_path <- c("ARM", "B: Placebo")
  spl_context <- data.frame(
    full_parent_df = I(list(full_parent_df)),
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("A: Drug X")),
    "B: Placebo" = I(list(full_parent_df$ARM == "B: Placebo")),
    check.names = FALSE
  )

  res <- expect_silent(
    a_diff_means(
      df = df,
      .var = "AVAL",
      .spl_context = spl_context,
      ref_path = ref_path,
    )
  )

  expect_snapshot(res)
})

test_that("a_diff_means works with no observations in both samples", {
  df <- advs_bp_blw12[FALSE, ]
  full_parent_df <- advs_bp_blw12[advs_bp_blw12$ARM != "B: Placebo", ]
  ref_path <- c("ARM", "B: Placebo")
  spl_context <- data.frame(
    full_parent_df = I(list(full_parent_df)),
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("A: Drug X")),
    "B: Placebo" = I(list(full_parent_df$ARM == "B: Placebo")),
    check.names = FALSE
  )

  res <- expect_silent(
    a_diff_means(
      df = df,
      .var = "AVAL",
      .spl_context = spl_context,
      ref_path = ref_path,
    )
  )

  expect_snapshot(res)
})
