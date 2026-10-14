test_that("transpose_named_list() works for basic 2x2 case", {
  x <- list(
    a = list(x = 1, y = 2),
    b = list(x = 3, y = 4)
  )
  result <- transpose_named_list(x)
  expected <- list(
    x = list(a = 1, b = 3),
    y = list(a = 2, b = 4)
  )
  expect_identical(result, expected)
})

test_that("transpose_named_list() preserves outer and inner names", {
  x <- list(
    trt = list(n = 10L, pct = 0.5),
    pbo = list(n = 8L, pct = 0.4)
  )
  result <- transpose_named_list(x)

  expect_named(result, c("n", "pct"))
  expect_named(result$n, c("trt", "pbo"))
  expect_named(result$pct, c("trt", "pbo"))
})

test_that("transpose_named_list() works with more than 2 outer elements", {
  x <- list(
    a = list(x = 1, y = 2),
    b = list(x = 3, y = 4),
    c = list(x = 5, y = 6)
  )
  result <- transpose_named_list(x)
  expected <- list(
    x = list(a = 1, b = 3, c = 5),
    y = list(a = 2, b = 4, c = 6)
  )
  expect_identical(result, expected)
})

test_that("transpose_named_list() works with more than 2 inner keys", {
  x <- list(
    a = list(p = 1, q = 2, r = 3),
    b = list(p = 4, q = 5, r = 6)
  )
  result <- transpose_named_list(x)
  expected <- list(
    p = list(a = 1, b = 4),
    q = list(a = 2, b = 5),
    r = list(a = 3, b = 6)
  )
  expect_identical(result, expected)
})

test_that("transpose_named_list() errors when inner names differ across elements", {
  x <- list(
    a = list(x = 1, y = 2),
    b = list(x = 3, z = 4)
  )
  expect_error(
    transpose_named_list(x),
    "must have same names on all sublists"
  )
})

test_that("transpose_named_list() is its own inverse", {
  x <- list(
    a = list(x = 1, y = 2),
    b = list(x = 3, y = 4)
  )
  expect_identical(transpose_named_list(transpose_named_list(x)), x)
})
