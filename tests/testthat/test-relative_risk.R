test_that("s_relative_risk returns NA when one group has no responders", {
  set.seed(12)
  dta <- data.frame(
    rsp = c(rep(FALSE, 50), TRUE, rep(FALSE, 49)),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )
  result <- expect_silent(s_relative_risk(
    df = subset(dta, grp == "B"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "A"),
    .in_ref_col = FALSE,
    variables = list(arm = "grp", strata = "strata")
  ))
  expected <- list(
    rel_risk_ci = formatters::with_label(
      c(est = NA_real_, lcl = 0, ucl = Inf),
      "Relative risk (95% CI)"
    ),
    pval = 0.3006
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("prop_ratio_cmh works as expected", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
  grp <- factor(grp, levels = c("Placebo", "Treatment"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    "f2" = sample(c("x", "y", "z"), 100, TRUE),
    stringsAsFactors = TRUE
  )

  result <- expect_silent(prop_ratio_cmh(
    rsp = rsp,
    grp = grp,
    strata = interaction(strata_data),
    conf_level = 0.90
  ))
  expected <- list(
    rel_risk_ci = c(
      est = 0.7417,
      lcl = 0.4908,
      ucl = 1.1210
    ),
    pval = 0.1892
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("prop_ratio_cmh works also with a single stratum", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
  grp <- factor(grp, levels = c("Placebo", "Treatment"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 100, TRUE),
    stringsAsFactors = TRUE
  )

  result <- expect_silent(prop_ratio_cmh(
    rsp = rsp,
    grp = grp,
    strata = interaction(strata_data),
    conf_level = 0.90
  ))
  expected <- list(
    rel_risk_ci = c(
      est = 0.7333,
      lcl = 0.5100,
      ucl = 1.0543
    ),
    pval = 0.1489
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("a_relative_risk in table layout gives same results as with SAS", {
  dat <- data.frame(
    Patient_ID = c(
      6L,
      8L,
      14L,
      16L,
      22L,
      24L,
      30L,
      32L,
      38L,
      40L,
      46L,
      48L,
      3L,
      4L,
      11L,
      12L,
      19L,
      20L,
      27L,
      28L,
      35L,
      36L,
      43L,
      44L,
      50L,
      5L,
      7L,
      13L,
      15L,
      21L,
      23L,
      29L,
      31L,
      37L,
      39L,
      45L,
      47L,
      1L,
      2L,
      9L,
      10L,
      17L,
      18L,
      25L,
      26L,
      33L,
      34L,
      41L,
      42L,
      49L
    ),
    Response = c(
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      TRUE
    ),
    Treatment = as.factor(c(
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A",
      "A",
      "B",
      "B",
      "A"
    )),
    Gender = as.factor(c(
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male"
    )),
    Age_Group = as.factor(c(
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Old",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young",
      "Young"
    ))
  )

  lyt <- basic_table(round_type = "sas") |>
    split_cols_by("Treatment") |>
    analyze(
      vars = "Response",
      afun = a_relative_risk,
      show_labels = "hidden",
      extra_args = list(
        variables = list(strata = c("Gender", "Age_Group"), arm = "Treatment"),
        ref_path = c("Treatment", "B"),
        .stats = c("rel_risk_ci", "pval")
      )
    )

  result <- build_table(lyt, dat)
  first_row <- as.list(result[
    c("Response", "rel_risk_ci"),
    c("Treatment", "A")
  ])
  second_row <- as.list(result[c("Response", "pval"), c("Treatment", "A")])

  # From sas_comparison/relative_risk.html:
  expected_first_row <- list(
    A.est = 1.1912,
    A.lcl = 0.6498,
    A.ucl = 2.1835
  )
  expected_second_row <- list(A = 0.5811)

  expect_equal(first_row, expected_first_row, tolerance = 1e-4)
  expect_equal(second_row, expected_second_row, tolerance = 1e-4)
})
