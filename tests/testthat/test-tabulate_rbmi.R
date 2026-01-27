library(broom)

# rbmi_test_data ----

rbmi_test_data_ancova <- structure(
  list(
    pars = list(
      var_4 = list(
        est = 1,
        ci = c(2, 3),
        se = 5,
        pvalue = 0.6,
        df = 10
      ),
      trt_B_4 = list(
        est = -0.0918064463781925,
        ci = c(-1.43949684096095, 1.25588394820457),
        se = 0.68262790574849,
        pvalue = 0.893177242657404,
        df = 10
      ),
      lsm_A_4 = list(
        est = -1.61581995766697,
        ci = c(-2.57577141468279, -0.655868500651147),
        se = 0.486231596928312,
        pvalue = 0.00109370836836806,
        df = 10
      ),
      lsm_B_4 = list(
        est = -1.70762640404516,
        ci = c(-2.64531930504159, -0.769933503048726),
        se = 0.474957263044525,
        pvalue = 0.000426214829197688,
        df = 10
      ),
      var_5 = list(
        est = 1,
        ci = c(2, 3),
        se = 5,
        pvalue = 0.6,
        df = 10
      ),
      trt_B_5 = list(
        est = 1.35132885626642,
        ci = c(-0.46983381835952, 3.17249153089236),
        se = 0.921893782421201,
        pvalue = 0.144732721791534,
        df = 10
      ),
      lsm_A_5 = list(
        est = -4.22512642519727,
        ci = c(-5.52033399517686, -2.92991885521768),
        se = 0.655676842047498,
        pvalue = 1.3935203627143e-09,
        df = 10
      ),
      lsm_B_5 = list(
        est = -2.87379756893085,
        ci = c(-4.15418494506937, -1.59341019279232),
        se = 0.647945143653854,
        pvalue = 1.77994352083364e-05,
        df = 10
      )
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 20L,
    method = "rubin"
  ),
  class = "pool"
)

rbmi_test_data_mmrm <- structure(
  list(
    pars = list(
      var_B_4 = list(
        est = 1,
        ci = c(2, 3),
        se = 5,
        pvalue = 0.6,
        df = 10
      ),
      var_C_4 = list(
        est = 12,
        ci = c(4, 6),
        se = 8,
        pvalue = 0.4,
        df = 12
      ),
      trt_B_4 = list(
        est = -0.0918064463781925,
        ci = c(-1.43949684096095, 1.25588394820457),
        se = 0.68262790574849,
        pvalue = 0.893177242657404,
        df = 10
      ),
      trt_C_4 = list(
        est = -0.08064463781925,
        ci = c(-1.949684096095, 1.588394820457),
        se = 0.262790574849,
        pvalue = 0.3177242657404,
        df = 1
      ),
      lsm_A_4 = list(
        est = -1.61581995766697,
        ci = c(-2.57577141468279, -0.655868500651147),
        se = 0.486231596928312,
        pvalue = 0.00109370836836806,
        df = 10
      ),
      lsm_B_4 = list(
        est = -1.70762640404516,
        ci = c(-2.64531930504159, -0.769933503048726),
        se = 0.474957263044525,
        pvalue = 0.000426214829197688,
        df = 10
      ),
      lsm_C_4 = list(
        est = -1.762640404516,
        ci = c(-2.531930504159, -0.9933503048726),
        se = 0.4957263044525,
        pvalue = 0.0426214829197688,
        df = 1
      )
    ),
    conf.level = 0.95,
    alternative = "two.sided",
    N = 20L,
    method = "rubin"
  ),
  class = "pool"
)

# h_tidy_pool ----

test_that("h_tidy_pool is produced correctly for ANCOVA case", {
  result <- h_tidy_pool(
    rbmi_test_data_ancova$pars[1:4],
    visit_name = "4",
    group_names = c("A", "B")
  )
  expected <- data.frame(
    visit = c("4", "4"),
    group = c("A", "B"),
    est = c(-1.61581995766697, -1.70762640404516),
    se_est = c(0.486231596928312, 0.474957263044525),
    lower_cl_est = c(-2.57577141468279, -2.64531930504159),
    upper_cl_est = c(-0.655868500651147, -0.769933503048726),
    est_contr = c(NA, -0.0918064463781925),
    se_contr = c(NA, 0.68262790574849),
    lower_cl_contr = c(NA, -1.43949684096095),
    upper_cl_contr = c(NA, 1.25588394820457),
    p_value = c(NA, 0.893177242657404),
    relative_reduc = c(NA, 0.056817249930957),
    mse = c(NA, 1),
    df = c(NA, 10)
  )
  expect_identical(result, expected, tolerance = 0.000001)
})

test_that("h_tidy_pool is produced correctly for MMRM case", {
  result <- h_tidy_pool(
    rbmi_test_data_mmrm$pars,
    visit_name = "4",
    group_names = c("A", "B", "C")
  )
  checkmate::expect_data_frame(result, nrows = 3)
  expect_equal(result$mse, c(NA, 1, 12))
  expect_equal(result$df, c(NA, 10, 12))
})

# tidy.pool ----

test_that("tidy.pool is produced correctly for ANCOVA case", {
  result <- broom::tidy(rbmi_test_data_ancova, visits = c("4", "5"))

  expected <- data.frame(
    visit = factor(c("4", "4", "5", "5")),
    group = factor(c("A", "B", "A", "B")),
    est = c(
      -1.61581995766697,
      -1.70762640404516,
      -4.22512642519727,
      -2.87379756893085
    ),
    se_est = c(
      0.486231596928312,
      0.474957263044525,
      0.655676842047498,
      0.647945143653854
    ),
    lower_cl_est = c(
      -2.57577141468279,
      -2.64531930504159,
      -5.52033399517686,
      -4.15418494506937
    ),
    upper_cl_est = c(
      -0.655868500651147,
      -0.769933503048726,
      -2.92991885521768,
      -1.59341019279232
    ),
    est_contr = c(NA, -0.0918064463781925, NA, 1.35132885626642),
    se_contr = c(NA, 0.68262790574849, NA, 0.921893782421201),
    lower_cl_contr = c(NA, -1.43949684096095, NA, -0.46983381835952),
    upper_cl_contr = c(NA, 1.25588394820457, NA, 3.17249153089236),
    p_value = c(NA, 0.893177242657404, NA, 0.144732721791534),
    relative_reduc = c(NA, 0.056817249930957, NA, -0.319831579052295),
    mse = c(NA, 1, NA, 1),
    df = c(NA, 10, NA, 10),
    conf_level = c(0.95, 0.95, 0.95, 0.95)
  )
  expect_identical(result, expected, tolerance = 0.000001)
})

test_that("tidy.pool is produced correctly for MMRM case", {
  result <- broom::tidy(rbmi_test_data_mmrm, visits = "4")

  checkmate::expect_data_frame(result, nrows = 3)
  expect_equal(result$mse, c(NA, 1, 12))
  expect_equal(result$df, c(NA, 10, 12))
})

# s_rbmi_lsmeans ----

test_that("s_rbmi_lsmeans works with as expected with .in_ref_col = TRUE", {
  df <- tidy(rbmi_test_data_ancova, visits = c("4", "5"))
  result <- s_rbmi_lsmeans(
    df = df[1, ],
    .in_ref_col = TRUE
  )
  expected <- list(
    adj_mean_se = c(-1.6158200, 0.4862316),
    adj_mean_ci = formatters::with_label(c(-2.5757714, -0.6558685), "95% CI"),
    diff_mean_se = character(0),
    diff_mean_ci = formatters::with_label(character(0), "95% CI"),
    change = formatters::with_label(character(0), "Relative Reduction (%)"),
    p_value = character(0),
    additional_title_row = NULL
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("s_rbmi_lsmeans works with as expected with .in_ref_col = FALSE", {
  df <- tidy(rbmi_test_data_ancova, visits = c("4", "5"))
  result <- s_rbmi_lsmeans(
    df = df[2, ],
    .in_ref_col = FALSE
  )
  expected <- list(
    adj_mean_se = c(-1.7076264, 0.4749573),
    adj_mean_ci = formatters::with_label(c(-2.6453193, -0.7699335), "95% CI"),
    diff_mean_se = c(-0.09180645, 0.68262791),
    diff_mean_ci = formatters::with_label(c(-1.439497, 1.255884), "95% CI"),
    change = formatters::with_label(0.05681725, "Relative Reduction (%)"),
    p_value = 0.8931772,
    additional_title_row = NULL
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("s_rbmi_lsmeans also works with show_relative = increase", {
  df <- tidy(rbmi_test_data_ancova, visits = c("4", "5"))
  result <- s_rbmi_lsmeans(
    df = df[2, ],
    .in_ref_col = FALSE,
    show_relative = "increase"
  )
  expected <- list(
    adj_mean_se = c(-1.7076264, 0.4749573),
    adj_mean_ci = formatters::with_label(c(-2.6453193, -0.7699335), "95% CI"),
    diff_mean_se = c(-0.09180645, 0.68262791),
    diff_mean_ci = formatters::with_label(c(-1.439497, 1.255884), "95% CI"),
    change = formatters::with_label(-0.05681725, "Relative Increase (%)"),
    p_value = 0.8931772,
    additional_title_row = NULL
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

# a_rbmi_lsmeans ----

test_that("a_rbmi_lsmeans works as expected in table layout", {
  df <- tidy(rbmi_test_data_ancova, visits = c("4", "5"))

  lyt <- basic_table() %>%
    split_cols_by("group") %>%
    split_rows_by("visit", split_label = "Visit", label_pos = "topleft") %>%
    analyze(
      "group",
      afun = a_rbmi_lsmeans,
      extra_args = list(
        ref_path = c("group", levels(df$group)[1]),
        .stats = c(
          "adj_mean_se",
          "adj_mean_ci",
          "diff_mean_se",
          "diff_mean_ci",
          "change",
          "p_value"
        ),
        .labels = c(p_value = "p-value (RBMI)")
      )
    )
  result <- expect_silent(build_table(lyt, df))

  expect_snapshot(result)
})
