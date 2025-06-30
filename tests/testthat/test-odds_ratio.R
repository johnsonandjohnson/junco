test_that("s_odds_ratio_j returns NA when one group has no responders and option set", {
  dta <- data.frame(
    rsp = c(rep(FALSE, 50), TRUE, rep(FALSE, 49)),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  result <- expect_silent(s_odds_ratio_j(
    df = subset(dta, grp == "B"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp"),
    na_if_no_events = TRUE # Specific option activated.
  ))
  expected <- list(
    or_ci = formatters::with_label(
      c(est = NA_real_, lcl = 0, ucl = Inf),
      "Odds Ratio (95% CI)"
    ),
    n_tot = c(n_tot = 100L),
    pval = 0.9966
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("s_odds_ratio_j returns NA when one group has no responders and option set, with strata", {
  set.seed(12)
  dta <- data.frame(
    rsp = c(rep(FALSE, 50), TRUE, rep(FALSE, 49)),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )
  expect_warning(
    result <- s_odds_ratio_j(
      df = subset(dta, grp == "B"),
      .df_row = dta,
      .var = "rsp",
      .ref_group = subset(dta, grp == "A"),
      .in_ref_col = FALSE,
      variables = list(arm = "grp", strata = "strata"),
      na_if_no_events = TRUE # Specific option activated.
    ),
    "infinite"
  )
  expected <- list(
    or_ci = formatters::with_label(
      c(est = NA_real_, lcl = 0, ucl = Inf),
      "Odds Ratio (95% CI)"
    ),
    n_tot = c(n_tot = 100L),
    pval = 0.999
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("s_odds_ratio_j does not fail with only non-responders", {
  dta <- data.frame(
    rsp = rep(FALSE, 100),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  result <- expect_silent(s_odds_ratio_j(
    df = subset(dta, grp == "B"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp")
  ))
  expected <- list(
    or_ci = formatters::with_label(
      c(est = NA_real_, lcl = 0, ucl = Inf),
      "Odds Ratio (95% CI)"
    ),
    n_tot = c(n_tot = 100L),
    pval = 1
  )
  expect_equal(result, expected)
})

test_that("s_odds_ratio_j does not fail with only one grp", {
  dta <- data.frame(
    rsp = rep(FALSE, 100),
    grp = factor(rep("A", 100), levels = c("A", "B"))
  )
  result <- expect_silent(s_odds_ratio_j(
    df = subset(dta, grp == "B"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp")
  ))
  expected <- list(
    or_ci = formatters::with_label(
      c(est = NA_real_, lcl = 0, ucl = Inf),
      "Odds Ratio (95% CI)"
    ),
    n_tot = c(n_tot = 100L),
    pval = 1
  )
  expect_equal(result, expected)
})

test_that("or_glm does not fail with only non-responders", {
  dta <- data.frame(
    rsp = rep(FALSE, 100),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  result <- expect_silent(or_glm(data = dta, conf_level = 0.9))
  expected <- list(
    or_ci = c(est = 1, lcl = 0, ucl = Inf),
    n_tot = c(n_tot = 100L),
    pval = 1
  )
  expect_equal(result, expected)
})

test_that("a_odds_ratio_j works in a table layout as expected for unstratified analysis", {
  set.seed(12)
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  lyt <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      "rsp",
      afun = a_odds_ratio_j,
      show_labels = "hidden",
      extra_args = list(
        ref_path = c("grp", "A"),
        .stats = c("or_ci", "pval")
      )
    )
  res <- expect_silent(build_table(lyt, df = dta))
  expect_snapshot(res)

  first_row <- as.list(res[c("rsp", "or_ci"), c("grp", "B")])
  second_row <- as.list(res[c("rsp", "pval"), c("grp", "B")])

  model_fit <- stats::glm(formula = rsp ~ grp, data = dta, family = binomial)
  model_est <- exp(coef(model_fit)[2])
  model_ci <- exp(confint.default(model_fit, )[2, ]) # Wald CI instead of Profile Likelihood CI.
  expected_first_row <- list(
    B.est = unname(model_est),
    B.lcl = unname(model_ci[1]),
    B.ucl = unname(model_ci[2])
  )
  expected_second_row <- list(B = summary(model_fit)$coef[2, "Pr(>|z|)"])

  expect_equal(first_row, expected_first_row)
  expect_equal(second_row, expected_second_row)
})

test_that("a_odds_ratio_j works in a table layout as expected for CMH analysis", {
  set.seed(12)
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  lyt <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      "rsp",
      afun = a_odds_ratio_j,
      show_labels = "hidden",
      extra_args = list(
        variables = list(arm = "grp", strata = "strata"),
        method = "cmh",
        ref_path = c("grp", "A"),
        .stats = c("or_ci", "pval")
      )
    )
  res <- expect_silent(build_table(lyt, df = dta))
  expect_snapshot(res)

  first_row <- as.list(res[c("rsp", "or_ci"), c("grp", "B")])
  second_row <- as.list(res[c("rsp", "pval"), c("grp", "B")])

  tbl <- table(
    factor(dta$grp, levels = c("B", "A")),
    factor(dta$rsp, levels = c("TRUE", "FALSE")),
    dta$strata
  )
  expected <- stats::mantelhaen.test(tbl, correct = FALSE)

  expected_first_row <- list(
    B.est = unname(expected$estimate),
    B.lcl = expected$conf.int[1],
    B.ucl = expected$conf.int[2]
  )
  expected_second_row <- list(B = expected$p.value)

  expect_equal(first_row, expected_first_row)
  expect_equal(second_row, expected_second_row)
})
