# New tests inspired by tern/tests/testthat/test-utils_default_stats_formats_labels.R
#
# Note that these tests are minimal, given that the junco functions are merely wrappers
# of the tern functions, pointing to junco defaults.

test_that("get_stats works as expected", {
  res <- junco_get_stats("kaplan_meier")
  expect_snapshot(res)
})

test_that("get_formats_from_stats works as expected", {
  sts <- c("quantiles_upper", "range_with_cens_info")
  res <- junco_get_formats_from_stats(sts)

  environment(res[["quantiles_upper"]]) <- baseenv()
  expect_snapshot_value(res$quantiles_upper, style = "json")
  environment(res[["range_with_cens_info"]]) <- baseenv()
  expect_snapshot_value(res$range_with_cens_info, style = "json")


})

test_that("get_labels_from_stats works as expected", {
  sts <- c("quantiles_upper", "range_with_cens_info")
  res <- junco_get_labels_from_stats(sts)
  expect_snapshot(res)
})

test_that("get_label_attr_from_stats works as expected", {
  x_stats <- list(
    stats1 = structure(1, label = "bla"),
    stats2 = structure(c(2, 3), label = "boo")
  )
  res <- expect_silent(get_label_attr_from_stats(x_stats))
  expect_snapshot(res)
})

test_that("get_indents_from_stats works as expected", {
  sts <- c("quantiles_upper", "range_with_cens_info")
  res <- junco_get_indents_from_stats(sts)
  expect_snapshot(res)
})
