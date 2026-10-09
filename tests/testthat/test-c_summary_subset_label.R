# data setup ----

df <- data.frame(
  ID = 1:8,
  AGE = c(34, NA, 52, 45, 50, NA, 41, 29),
  AVAL = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
)

# c_summary_subset_label ----

test_that("c_summary_subset_label works as expected", {
  res <- c_summary_subset_label(df, "", "AVAL", expression(AGE > 45))

  x <- df[df$AGE > 45, "AVAL"]
  asum <- tern::a_summary(x)
  label_rcell <- rcell(NULL, label = "", indent_mod = 0L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(0L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label works as expected for custom label and indent", {
  res <- c_summary_subset_label(df, "", "AVAL", expression(AGE > 45), "MYLAB", 5L)

  x <- df[df$AGE > 45, "AVAL"]
  asum <- tern::a_summary(x)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label works as expected for custom .stats, .formats, .labels", {
  stats <- c("n", "sum")
  formats <- c("xx.xx", "xx.xxxx")
  labels <- c("N", "Mean")

  res <- c_summary_subset_label(
    df,
    "",
    "AVAL",
    expression(AGE > 45),
    "MYLAB",
    5L,
    .stats = stats,
    .formats = formats,
    .labels = labels
  )

  x <- df[df$AGE > 45, "AVAL"]
  asum <- tern::a_summary(x, .stats = stats, .formats = formats, .labels = labels)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label works as expected with fully filtered data", {
  res <- c_summary_subset_label(df, "", "AVAL", expression(rep(FALSE, 8L)), "MYLAB", 5L)

  x <- df[FALSE, "AVAL"]
  asum <- tern::a_summary(x)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label works as expected with no filtering", {
  res <- c_summary_subset_label(df, "", "AVAL", expression(rep(TRUE, 8L)), "MYLAB", 5L)

  asum <- tern::a_summary(df$AVAL)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label handles NA values in filter correctly", {
  res <- c_summary_subset_label(df, "", "AVAL", expression(AGE > 40), "MYLAB", 5L)

  x <- df[df$AGE > 40, "AVAL"]
  asum <- tern::a_summary(x)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label handles NA values in analysis variable", {
  df2 <- df
  df2$AVAL[1] <- NA

  res <- c_summary_subset_label(df2, "", "AVAL", expression(rep(TRUE, 8L)), "MYLAB", 5L)

  asum <- tern::a_summary(df2$AVAL)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label handles empty subset", {
  res <- c_summary_subset_label(df, "", "AVAL", expression(AGE > 1000), "MYLAB", 5L)

  x <- df[df$AGE > 1000, "AVAL"]
  asum <- tern::a_summary(x)
  label_rcell <- rcell(NULL, label = "MYLAB", indent_mod = 5L)
  exp <- rtables::in_rows(
    .list = c(list("__label" = label_rcell), asum)
  )
  attr(exp, "row_labels") <- c("__label" = "MYLAB", attr(asum, "row_labels"))
  attr(exp, "row_formats") <- c("__label" = list(NULL), attr(asum, "row_formats"))
  attr(exp, "indent_mods") <- c(5L, attr(asum, "indent_mods"))

  expect_identical(res, exp)
})

test_that("c_summary_subset_label errors if column does not exist", {
  expect_error(
    c_summary_subset_label(df, "", "AVAL", expression(NOT_A_COLUMN > 1000), "MYLAB", 5L),
    "NOT_A_COLUMN"
  )
})

test_that("c_summary_subset_label errors if expression is not logical", {
  expect_error(
    c_summary_subset_label(df, "", "AVAL", expression(AGE + 1), "MYLAB", 5L),
    "logical"
  )

  expect_error(
    c_summary_subset_label(df, "", "AVAL", expression(1), "MYLAB", 5L),
    "logical"
  )
})


test_that("c_summary_subset_label errors for logical filter that is too short", {
  expect_error(
    c_summary_subset_label(df, "", "AVAL", expression(TRUE), "MYLAB", 5L),
    "length"
  )
})

test_that("c_summary_subset_label errors for multiple expressions", {
  expect_error(
    c_summary_subset_label(df, "", "AVAL", expression(AGE > 40, AVAL < 5), "MYLAB", 5L),
    "length"
  )
})

test_that("c_summary_subset_label errors for missing .var", {
  expect_error(
    c_summary_subset_label(df, "", "NOT_THERE", expression(AGE >= 1), "MYLAB", 5L),
    "NOT_THERE"
  )
})
