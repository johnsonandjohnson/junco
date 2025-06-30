# Copied from tern/tests/testthat/test-utils_default_stats_formats_labels.R
#
# The code was taken from tern GitHub development version 0.9.7.9017.
# Names have been modified with tern_ prefix as needed to avoid name conflicts with the junco
# functions.
#
# From `Note: New tests` onwards, new tests have been added in order to test the tern functions more
# thoroughly. These can be proposed to add to the tern package in a PR.

# Slight modifications have been applied to enhance functionality, as described in detail below.
# All these modifications should be proposed as PRs to the tern package.
# Hence eventually this file could be removed from the junco package, and the functions could be used
# from the tern namespace directly.

testthat::test_that("tern_get_stats works as expected for defaults", {
  # Defaults are not changing
  res <- testthat::expect_silent(tern_get_stats("count_occurrences"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(tern_get_stats("summarize_num_patients"))
  testthat::expect_snapshot(res)

  # Change depending on type
  res <- testthat::expect_silent(tern_get_stats("analyze_vars_counts"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(tern_get_stats("analyze_vars_numeric"))
  testthat::expect_snapshot(res)

  testthat::expect_error(
    tern_get_stats("dont_exist"),
    regexp = "The selected method group \\(dont_exist\\) has no default statistical method."
  )

  # Here they are different, and overlap only for n
  testthat::expect_identical(
    intersect(
      tern_get_stats("analyze_vars_counts"),
      tern_get_stats("analyze_vars_numeric")
    ),
    "n"
  )

  # Test multiples
  testthat::expect_identical(
    tern_get_stats(c("count_occurrences", "analyze_vars")),
    unique(c(
      tern_get_stats("count_occurrences"),
      tern_get_stats("analyze_vars_numeric")
    ))
  )
})
testthat::test_that("tern_get_stats works well with pval", {
  # pval is added correctly
  testthat::expect_true(
    "pval" %in% tern_get_stats("analyze_vars_numeric", add_pval = TRUE)
  )
  testthat::expect_true(
    "pval_counts" %in% tern_get_stats("analyze_vars_counts", add_pval = TRUE)
  )
  testthat::expect_true(
    "pval" %in% tern_get_stats("count_occurrences", add_pval = TRUE)
  )

  # Errors
  testthat::expect_error(tern_get_stats(
    "analyze_vars_counts",
    stats_in = c("pval", "pval_counts")
  ))
  testthat::expect_error(
    tern_get_stats("analyze_vars_counts", stats_in = c("n", "pval")),
    "Inserted p-value \\(pval\\) is not valid for type counts*"
  )
  testthat::expect_error(
    tern_get_stats("analyze_vars_numeric", stats_in = c("n", "pval_counts")),
    "Inserted p-value \\(pval_counts\\) is not valid for type numeric*"
  )
})

testthat::test_that("tern_get_stats works as expected for selection of stats", {
  sts_in <- c("mean", "n")
  res <- testthat::expect_silent(tern_get_stats(
    "analyze_vars",
    stats_in = sts_in
  ))
  testthat::expect_identical(res, sts_in)

  # False insertion
  testthat::expect_error(
    tern_get_stats("analyze_vars", stats_in = "unique"),
    regexp = "*unique"
  )

  # False insertion
  testthat::expect_error(
    tern_get_stats("count_occurrences", stats_in = "unique"),
    regexp = "*unique"
  )
})

testthat::test_that("tern_get_formats_from_stats works as expected", {
  sts <- tern_get_stats("count_occurrences")
  res <- testthat::expect_silent(tern_get_formats_from_stats(sts))
  testthat::expect_equal(names(res), sts)
  testthat::expect_equal(res[[1]], "xx.")

  testthat::expect_null(tern_get_formats_from_stats(c("nothing", "n"))[[
    "nothing"
  ]])
  testthat::expect_identical(
    tern_get_labels_from_stats(c("nothing", "n"))[["nothing"]],
    "nothing"
  )

  # list check
  stats_to_do <- c("not_a_stat" = function(x) as.character(x), "mean" = "xx.")
  testthat::expect_equal(
    tern_get_formats_from_stats(names(stats_to_do), formats_in = stats_to_do),
    stats_to_do
  )

  # Works also if we had a not present format
  testthat::expect_identical(
    tern_get_formats_from_stats(
      names(stats_to_do),
      formats_in = c(stats_to_do, "catch_me" = "xx")
    ),
    stats_to_do
  )

  # character vector is the same -> default have functions, so it is casted to list
  stats_to_do <- c("not_a_stat" = "xx", "mean" = "xx")
  testthat::expect_identical(
    tern_get_formats_from_stats(
      names(stats_to_do),
      formats_in = c(stats_to_do, "catch_me" = "xx")
    ),
    as.list(stats_to_do)
  )
})

testthat::test_that("tern_get_labels_from_stats works as expected", {
  sts <- tern_get_stats("count_occurrences")
  res <- testthat::expect_silent(tern_get_labels_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_identical(
    tern_get_labels_from_stats(c("nothing", "n"))[["nothing"]],
    "nothing"
  )

  testthat::expect_identical(
    tern_get_labels_from_stats(c("nothing", "unique"))[["unique"]],
    tern_default_labels[["unique"]]
  )

  # list check
  stats_to_do <- c("not_a_stat" = function(x) as.character(x), "mean" = "xx.")
  testthat::expect_equal(
    tern_get_labels_from_stats(names(stats_to_do), labels_in = stats_to_do),
    stats_to_do
  )

  testthat::expect_identical(
    tern_get_labels_from_stats(
      names(stats_to_do),
      labels_in = c(stats_to_do, "catch_me" = "xx")
    ),
    stats_to_do
  )

  # character vector
  stats_to_do <- c("not_a_stat" = "xx", "mean" = "xx")
  testthat::expect_identical(
    tern_get_labels_from_stats(
      names(stats_to_do),
      labels_in = c(stats_to_do, "catch_me" = "xx")
    ),
    stats_to_do %>% as.list()
  )
})

testthat::test_that("tern_get_labels_from_stats with labels in works when adding levels to stats", {
  labels_custom <- c(
    "c" = "Lvl c:",
    "a" = "any A",
    "count" = "COUNT",
    "count_fraction.b" = "CF: B"
  )
  levels_per_stats <- list(
    count = c("a", "b", "c"),
    count_fraction = c("a", "b", "c")
  )

  # with levels_per_stats
  testthat::expect_equal(
    tern_get_labels_from_stats(
      stats = c("count", "count_fraction"),
      labels_in = labels_custom,
      levels_per_stats = levels_per_stats
    ),
    c(
      "count.a" = "any A",
      "count.b" = "COUNT",
      "count.c" = "Lvl c:",
      "count_fraction.a" = "any A",
      "count_fraction.b" = "CF: B",
      "count_fraction.c" = "Lvl c:"
    ) %>%
      as.list()
  )
})

testthat::test_that("tern_get_labels_from_stats works fine for cases with levels", {
  x_stats <- list(
    n = list(
      n = c(n = 5)
    ),
    count_fraction = list(
      a = c(count = 1.0, p = 0.2),
      b = c(count = 1.0, p = 0.2),
      c = c(count = 1.0, p = 0.2),
      d = c(count = 1.0, p = 0.2),
      e = c(count = 1.0, p = 0.2)
    ),
    a_zero = 0,
    a_null = NULL
  )
  .stats <- names(x_stats)
  .labels <- list("n" = "N=", "a" = "AAAA", "a_zero" = "A_ZERO")

  out <- tern_get_labels_from_stats(
    .stats,
    .labels,
    levels_per_stats = lapply(x_stats, names)
  )

  testthat::expect_equal(
    .unlist_keep_nulls(out, recursive = TRUE),
    c(
      n = "N=",
      count_fraction.a = "AAAA",
      count_fraction.b = "b",
      count_fraction.c = "c",
      count_fraction.d = "d",
      count_fraction.e = "e",
      a_zero = "A_ZERO",
      a_null = "a_null"
    )
  )
})

testthat::test_that("tern_get_indents_from_stats works as expected", {
  sts <- tern_get_stats("count_occurrences")
  res <- testthat::expect_silent(tern_get_indents_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_identical(
    tern_get_indents_from_stats("count", NULL)[["count"]],
    0L
  )
  testthat::expect_identical(tern_get_indents_from_stats(c("count"), 3L), 3L)

  # integer vector
  stats_to_do <- c("count" = 3L, "mean" = 6L)
  testthat::expect_identical(
    tern_get_indents_from_stats(
      c(names(stats_to_do), "n"),
      indents_in = stats_to_do
    ),
    c(stats_to_do, n = 0L) %>% as.list()
  )
})

# Note: New tests

test_that("tern_get_labels_from_stats correctly combines labels_in and label_attr_from_stats", {
  result <- tern_get_labels_from_stats(
    stats = c("a", "b"),
    labels_in = c("a" = "A"),
    label_attr_from_stats = c("b" = "B"),
    tern_defaults = list("a" = "X", "b" = "Y")
  )
  expected <- list("a" = "A", "b" = "B")
  expect_identical(result, expected)
})

test_that(".unlist_keep_nulls works as expected non-recursively", {
  result <- .unlist_keep_nulls(list(a = NULL, b = c(10, 20, 30)))
  expected <- c(a = "NULL", b1 = "10", b2 = "20", b3 = "30")
  expect_identical(result, expected)
})

test_that(".unlist_keep_nulls works as expected non-recursively with NA", {
  result <- .unlist_keep_nulls(
    list(a = NULL, b = c(10, 20, 30)),
    null_placeholder = NA
  )
  expected <- c(a = NA, b1 = 10, b2 = 20, b3 = 30)
  expect_identical(result, expected)
})

test_that(".split_std_from_custom_stats works as expected with a list", {
  result <- .split_std_from_custom_stats(list(
    a = function(x) {
      x + 2
    },
    b = "default"
  ))
  expect_identical(result$default_stats, c(b = "default"))
  checkmate::expect_function(result$custom_stats$a)
  expect_identical(result$all_stats, c("a", "default"))
})

test_that(".split_std_from_custom_stats works as expected with a single input", {
  result <- .split_std_from_custom_stats("default")
  expect_snapshot(result)
})

test_that(".apply_stat_functions works as expected without custom stat functions", {
  result <- .apply_stat_functions(
    default_stat_fnc = function(x) {
      min(x)
    },
    custom_stat_fnc_list = NULL,
    args_list = list(x = 1:10)
  )
  expected <- list(1L)
  expect_identical(result, expected)
})

test_that(".apply_stat_functions works as expected with custom stat functions", {
  result <- .apply_stat_functions(
    default_stat_fnc = function(x) {
      min(x)
    },
    custom_stat_fnc_list = list(a = function(x, ...) {
      max(x)
    }),
    args_list = list(x = 1:10)
  )
  expected <- list(1L, a = 10L)
  expect_identical(result, expected)
})
