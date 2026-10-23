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

test_that("Wald confidence limits agree with CAMIS examples", {
  # CAMIS, "Relative Risk", example code for ratesci:
  # https://psiaims.github.io/CAMIS/R/ci_for_2indep_prop.html#relative-risk
  # Active: 36/154; placebo (reference): 12/77; 95% confidence limits.
  # Published Katz log and Adjusted log limits correspond to our wald and
  # waldcc methods. Compare limits at the precision printed on the website.
  # CAMIS reports the unadjusted estimate (1.5) for both methods; our waldcc
  # estimate applies the same 0.5 adjustment as its confidence limits.
  rsp <- rep(c(TRUE, FALSE, TRUE, FALSE), c(12, 65, 36, 118))
  grp <- factor(rep(c("ref", "trt"), c(77, 154)), levels = c("ref", "trt"))
  expected_ci <- list(
    c(lcl = 0.828758, ucl = 2.71490),
    c(lcl = 0.818876, ucl = 2.61996)
  )
  for (correction in c(FALSE, TRUE)) {
    result <- prop_ratio_wald(rsp, grp, correction = correction)
    expect_equal(result$rel_risk_ci[c("lcl", "ucl")], expected_ci[[1L + correction]], tolerance = 5e-6)
    if (!correction) expect_equal(unname(result$rel_risk_ci["est"]), 1.5)
    # Reversing reference group inverts the estimate and limits.
    reverse <- prop_ratio_wald(rsp, factor(grp, levels = rev(levels(grp))), correction = correction)
    expect_equal(unname(reverse$rel_risk_ci), 1 / unname(result$rel_risk_ci[c(1, 3, 2)]))
    expect_equal(reverse$pval, result$pval)
  }
})

test_that("Wald methods handle boundary response counts", {
  grp <- factor(rep(c("ref", "trt"), each = 10), levels = c("ref", "trt"))
  for (counts in list(c(0, 3), c(3, 0), c(0, 0))) {
    rsp <- c(
      rep(c(TRUE, FALSE), c(counts[1], 10 - counts[1])),
      rep(c(TRUE, FALSE), c(counts[2], 10 - counts[2]))
    )
    result <- expect_silent(prop_ratio_wald(rsp, grp))
    expect_equal(unname(result$rel_risk_ci[1]), if (all(counts == 0)) NA_real_ else if (counts[1] == 0) Inf else 0)
    expect_true(all(is.na(result$rel_risk_ci[2:3])))
    expect_true(is.na(result$pval))
    corrected <- expect_silent(prop_ratio_wald(rsp, grp, correction = TRUE))
    expect_true(all(is.finite(corrected$rel_risk_ci)))
    expect_true(is.finite(corrected$pval))
    expect_equal(unname(corrected$rel_risk_ci[1]), (counts[2] + 0.5) / (counts[1] + 0.5))
  }
  for (correction in c(FALSE, TRUE)) {
    result <- expect_silent(prop_ratio_wald(rep(TRUE, 20), grp, correction = correction))
    expect_equal(result, list(rel_risk_ci = c(est = 1, lcl = 1, ucl = 1), pval = 1))
  }
})

test_that("s_relative_risk supports unstratified Wald methods", {
  ref <- data.frame(rsp = rep(c(TRUE, FALSE), c(8, 32)))
  trt <- data.frame(rsp = rep(c(TRUE, FALSE), c(12, 18)))
  for (method in c("wald", "waldcc")) {
    result <- s_relative_risk(trt, "rsp", ref, FALSE, conf_level = 0.9, method = method)
    expected <- prop_ratio_wald(
      c(ref$rsp, trt$rsp), factor(rep(c("ref", "trt"), c(40, 30)), levels = c("ref", "trt")),
      conf_level = 0.9, correction = method == "waldcc"
    )
    expected$rel_risk_ci <- with_label(expected$rel_risk_ci, "Relative risk (90% CI)")
    expect_equal(result, expected)
    expect_equal(
      s_relative_risk(
        trt, "rsp", ref, FALSE,
        variables = list(strata = "unused"),
        conf_level = 0.9, method = method
      ),
      result
    )
    reference <- s_relative_risk(ref, "rsp", ref, TRUE, method = method)
    expect_length(reference$rel_risk_ci, 0)
    expect_length(reference$pval, 0)
  }
  expect_error(s_relative_risk(trt, "rsp", ref, FALSE), "strata_colnames")
  expect_error(s_relative_risk(trt, "rsp", ref, FALSE, method = "unknown"), "arg")
})

test_that("prop_ratio_wald validates inputs", {
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("ref", "ref", "trt", "trt"))
  expect_error(prop_ratio_wald(rsp, grp, correction = NA), "correction")
  expect_error(prop_ratio_wald(rsp, grp, conf_level = 1.1), "conf_level")
  expect_error(prop_ratio_wald(c(TRUE, NA, TRUE, FALSE), grp), "missing")
  expect_error(prop_ratio_wald(as.integer(rsp), grp), "logical")
  expect_error(prop_ratio_wald(rsp, factor(rep("ref", 4))), "levels")
})

test_that("a_relative_risk renders unstratified Wald results", {
  dat <- data.frame(
    rsp = rep(c(TRUE, FALSE, TRUE, FALSE), c(8, 32, 12, 18)),
    arm = factor(rep(c("ref", "trt"), c(40, 30)), levels = c("ref", "trt"))
  )
  for (method in c("wald", "waldcc")) {
    lyt <- basic_table() |>
      split_cols_by("arm") |>
      analyze("rsp", afun = a_relative_risk, extra_args = list(
        ref_path = c("arm", "ref"), method = method
      ))
    result <- build_table(lyt, dat)
    expected <- prop_ratio_wald(dat$rsp, dat$arm, correction = method == "waldcc")
    expect_equal(
      unname(unlist(as.list(result[c("rsp", "rel_risk_ci"), c("arm", "trt")]))),
      unname(expected$rel_risk_ci)
    )
    expect_equal(
      unname(unlist(as.list(result[c("rsp", "pval"), c("arm", "trt")]))),
      expected$pval
    )
  }
})
