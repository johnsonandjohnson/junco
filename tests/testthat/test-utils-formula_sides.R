test_that("rightside returns the right-hand side as a scalar character", {
  expect_identical(rightside(y ~ x), "x")
  expect_identical(rightside(y ~ x1 + x2), "x1 + x2")
})
