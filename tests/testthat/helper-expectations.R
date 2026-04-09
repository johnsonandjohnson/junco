expect_any_difference <- function(object, expected, ...) {
  is_identical <- isTRUE(all.equal(object, expected, ...))
  expect_false(
    is_identical,
    info = "Objects are identical; expected at least one difference"
  )
}