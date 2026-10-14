test_that("prepend_label_cell() prepends a label row as expected", {
  x <- rtables::in_rows(Mean = rcell(NULL), Range = rcell(c(1, 8)))

  res <- prepend_label_cell(x)
  exp <- rtables::in_rows(
    "__label" = rcell(NULL, label = ""),
    Mean = x[[1]],
    Range = x[[2]],
    .labels = c("", names(x))
  )

  expect_identical(res, exp)
})

test_that("prepend_label_cell() prepends a label row with a custom label and indentation", {
  x <- rtables::in_rows(Mean = rcell(5), Range = rcell(c(1, 8)))

  res <- prepend_label_cell(x, "LABEL", 3L)
  exp <- rtables::in_rows(
    "__label" = rcell(NULL, label = "LABEL", indent_mod = 3L),
    Mean = x[[1]],
    Range = x[[2]],
    .labels = c("LABEL", names(x))
  )

  expect_identical(res, exp)
})

test_that("prepend_label_cell() preserves the rcell format, label, indent_mod", {
  x <- rtables::in_rows(
    Mean = rcell(5, format = "xx.xx", label = "rcell_label", indent_mod = 10L)
  )

  res <- prepend_label_cell(x, "LABEL", 2L)
  exp <- rtables::in_rows(
    "__label" = rcell(NULL, label = "LABEL", indent_mod = 2L),
    Mean = x[[1]]
  )

  expect_identical(res, exp)
})

test_that("prepend_label_cell() preserves the in_rows .formats, .labels, .indent_mods", {
  x <- rtables::in_rows(
    Mean = rcell(5),
    .formats = "xx.xxxx",
    .labels = "in_rows_label",
    .indent_mods = 20L
  )

  res <- prepend_label_cell(x, "LABEL", 2L)
  exp <- rtables::in_rows(
    "__label" = rcell(NULL, label = "LABEL", indent_mod = 2L),
    Mean = x[[1]],
    .formats = c(list("__label" = NULL), as.list(attr(x, "row_formats"))),
    .labels = c("LABEL", attr(x, "row_labels")),
    .indent_mods = c(2L, attr(x, "indent_mods"))
  )

  expect_identical(res, exp)
})

test_that("prepend_label_cell() preserves the in_rows, named .formats, .labels, .indent_mods", {
  x <- rtables::in_rows(
    Mean = rcell(5),
    Range = rcell(c(1, 6)),
    .formats = c(Mean = "xx.xxxx", Range = "xx.xx - xx.xx"),
    .labels = c(Mean = "in_rows_mean", Range = "in_rows_range"),
    .indent_mods = c(Mean = 15L, Range = 20L)
  )

  res <- prepend_label_cell(x, "LABEL", 2L)
  exp <- rtables::in_rows(
    "__label" = rcell(NULL, label = "LABEL", indent_mod = 2L),
    Mean = x[[1]],
    Range = x[[2]],
    .formats = c(list("__label" = NULL), as.list(attr(x, "row_formats"))),
    .labels = c("__label" = "LABEL", attr(x, "row_labels")),
    .indent_mods = c("__label" = 2L, attr(x, "indent_mods"))
  )

  expect_identical(res, exp)
})

test_that("prepend_label_cell() preserves the rcell (f, l, im), in_rows (.f, .l, .im)", {
  x <- rtables::in_rows(
    Mean = rcell(5, format = "xx.xx", label = "rcell_label", indent_mod = 10L),
    .formats = "xx.xxxx",
    .labels = "in_rows_label",
    .indent_mods = 20L
  )

  res <- prepend_label_cell(x, "LABEL", 2L)
  exp <- rtables::in_rows(
    "__label" = rcell(NULL, label = "LABEL", indent_mod = 2L),
    Mean = x[[1]],
    .formats = c(list("__label" = NULL), as.list(attr(x, "row_formats"))),
    .labels = c("LABEL", attr(x, "row_labels")),
    .indent_mods = c(2L, attr(x, "indent_mods"))
  )

  expect_identical(res, exp)
})
