test_that("s_test_proportion_diff works as expected", {
  set.seed(123, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  result <- s_test_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    method = "cmh",
    variables = list(strata = "strata")
  )
  expect_snapshot(result)

  result <- s_test_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .ref_group = subset(dta, grp == "B"),
    .in_ref_col = FALSE,
    method = "cmh",
    variables = list(strata = "strata"),
    alternative = "greater"
  )
  expect_snapshot(result)

  result <- s_test_proportion_diff(
    df = subset(dta, grp == "A"),
    .var = "rsp",
    .in_ref_col = TRUE,
    method = "cmh",
    alternative = "greater"
  )
  expect_snapshot(result)
})

test_that("a_test_proportion_diff works as expected in table layout", {
  set.seed(123, kind = "Mersenne-Twister")
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50)),
    strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  )

  l <- basic_table() |>
    split_cols_by(var = "grp") |>
    analyze(
      vars = "rsp",
      afun = a_test_proportion_diff,
      show_labels = "hidden",
      extra_args = list(
        method = "cmh",
        variables = list(strata = "strata"),
        ref_path = c("grp", "B")
      )
    )

  result <- expect_silent(build_table(l, df = dta))
  expect_snapshot(result)
})


# Tests below have been amended from tern/tests/testthat/test-test_proportion_diff.R

test_that("prop_chisq returns right result", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)

  result <- prop_chisq(tbl, alternative = "two.sided")
  res <- expect_silent(result)
  expect_snapshot(res)

  # New part:
  result <- prop_chisq(tbl, alternative = "less")
  res <- expect_silent(result)
  expect_snapshot(res)

  result <- prop_chisq(tbl, alternative = "greater")
  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("prop_cmh returns right result", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- sample(c(TRUE, FALSE), 100, TRUE)
  grp <- factor(rep(c("A", "B"), each = 50))
  strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
  tbl <- table(grp, rsp, strata)
  result <- prop_cmh(tbl, alternative = "two.sided")

  res <- expect_silent(result)
  expect_snapshot(res)

  # New part:
  result <- prop_cmh(tbl, alternative = "less")
  res <- expect_silent(result)
  expect_snapshot(res)

  result <- prop_cmh(tbl, alternative = "greater")
  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("prop_cmh also works when there are strata with just one observation", {
  tbl <- structure(
    c(
      20L,
      17L,
      7L,
      10L,
      0L,
      0L,
      2L,
      3L,
      21L,
      14L,
      3L,
      0L,
      0L,
      0L,
      1L,
      0L,
      21L,
      18L,
      3L,
      4L,
      79L,
      16L,
      30L,
      9L,
      1L,
      0L,
      13L,
      4L
    ),
    .Dim = c(2L, 2L, 7L),
    .Dimnames = list(
      grp = c("Placebo", "Treatment"),
      x = c("no", "yes"),
      strata = c("A", "B", "C", "D", "E", "F", "G")
    ),
    class = "table"
  )

  expect_warning(
    result <- prop_cmh(tbl, alternative = "two.sided"),
    "<5 data points in some strata. CMH test may be incorrect."
  )

  res <- expect_silent(result)
  expect_snapshot(res)

  # New part:
  expect_warning(result <- prop_cmh(tbl, alternative = "less"))
  res <- expect_silent(result)
  expect_snapshot(res)

  expect_warning(result <- prop_cmh(tbl, alternative = "greater"))
  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("prop_fisher returns right result", {
  set.seed(1, kind = "Mersenne-Twister")
  rsp <- c(
    sample(c(TRUE, FALSE), size = 20, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 20, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- c(rep("A", 20), rep("B", 20))
  tbl <- table(grp, rsp)
  result <- prop_fisher(tbl, alternative = "two.sided")

  res <- expect_silent(result)
  expect_snapshot(res)

  # New part:
  result <- prop_fisher(tbl, alternative = "less")
  res <- expect_silent(result)
  expect_snapshot(res)

  result <- prop_fisher(tbl, alternative = "greater")
  res <- expect_silent(result)
  expect_snapshot(res)
})
