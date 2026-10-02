test_that("rightside returns the right-hand side as a scalar character", {
  expect_identical(rightside(y ~ x), "x")
  expect_identical(rightside(y ~ x1 + x2), "x1 + x2")
})

test_that("rightside works with a right hand side with blank", {
  expect_identical(rightside("bla" ~ "bli blu"), "bli blu")
})
