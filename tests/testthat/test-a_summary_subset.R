# data setup ----

df <- data.frame(
  ID = 1:8,
  AGE = c(34, NA, 52, 45, 50, NA, 41, 29),
  AVAL = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
)

# a_summary_subset ----

test_that("a_summary_subset works as expected", {
  res <- a_summary_subset(df, "AVAL", expression(!is.na(AGE)))
  x <- df[!is.na(df$AGE), "AVAL"]
  exp <- tern::a_summary(x)

  expect_identical(res, exp)
})

test_that("a_summary_subset works as expected for custom .stats, .formats, .labels", {
  stats <- c("n", "sum")
  formats <- c("xx.xx", "xx.xxxx")
  labels <- c("N", "Mean")

  res <- a_summary_subset(
    df,
    "AVAL",
    expression(!is.na(AGE)),
    .stats = stats,
    .formats = formats,
    .labels = labels
  )

  x <- df[!is.na(df$AGE), "AVAL"]
  exp <- tern::a_summary(x, .stats = stats, .formats = formats, .labels = labels)

  expect_identical(res, exp)
})

test_that("a_summary_subset works as expected with fully filtered data", {
  res <- a_summary_subset(df, "AVAL", expression(rep(FALSE, 8L)))

  x <- df[FALSE, "AVAL"]
  exp <- tern::a_summary(x)

  expect_identical(res, exp)
})

test_that("a_summary_subset works as expected with no filtering", {
  res <- a_summary_subset(df, "AVAL", expression(rep(TRUE, 8L)))
  exp <- tern::a_summary(df$AVAL)

  expect_identical(res, exp)
})

test_that("a_summary_subset handles NA values in filter correctly", {
  res <- a_summary_subset(df, "AVAL", expression(AGE > 40))

  x <- df[df$AGE > 40, "AVAL"]
  exp <- tern::a_summary(x)

  expect_identical(res, exp)
})

test_that("a_summary_subset handles NA values in analysis variable", {
  df2 <- df
  df2$AVAL[1] <- NA

  res <- a_summary_subset(df2, "AVAL", expression(rep(TRUE, 8L)))
  exp <- tern::a_summary(df2$AVAL)

  expect_identical(res, exp)
})

test_that("a_summary_subset handles empty subset", {
  res <- a_summary_subset(df, "AVAL", expression(AGE > 1000))

  x <- df[df$AGE > 1000, "AVAL"]
  exp <- tern::a_summary(x)

  expect_identical(res, exp)
})

test_that("a_summary_subset errors if column does not exist", {
  expect_error(
    a_summary_subset(df, "AVAL", expression(NOT_A_COLUMN > 1)),
    "NOT_A_COLUMN"
  )
})

test_that("a_summary_subset errors if expression is not logical", {
  expect_error(
    a_summary_subset(df, "AVAL", expression(AGE + 1)), "logical"
  )

  expect_error(
    a_summary_subset(df, "AVAL", expression(1)), "logical"
  )
})


test_that("a_summary_subset errors for logical filter that is too short", {
  expect_error(
    a_summary_subset(df, "AVAL", expression(TRUE)),
    "length"
  )
})

test_that("a_summary_subset errors for multiple expressions", {
  expect_error(
    a_summary_subset(df, "AVAL", expression(AGE > 40, AVAL < 5)),
    "length"
  )
})

test_that("a_summary_subset errors for missing .var", {
  expect_error(
    a_summary_subset(df, "NOT_THERE", expression(AGE >= 1)),
    "NOT_THERE"
  )
})
