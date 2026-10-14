test_that("strict_match works at odd positions", {
  expect_equal(strict_match("A", c("A", "Placebo")), 1)
  expect_equal(strict_match("SEX", c("SomeVar", "SomeVal", "SEX", "Male")), 3)
  expect_equal(strict_match("ARM", c("SEX", "M", "ARM", "Placebo", "multivars", "AVAL")), 3)
})

test_that("strict_match works at even positions", {
  expect_equal(strict_match("Placebo", c("ARM", "Placebo"), odd = FALSE), 2)
  expect_equal(strict_match("Male", c("SomeVar", "SomeVal", "SEX", "Male"), odd = FALSE), 4)
})

test_that("strict_match errors at value not found", {
  expect_error(strict_match("ARM", c("SEX", "Male")), "not found")
})

test_that("strict_match errors at value is at wrong parity position", {
  expect_error(strict_match("Male", c("SEX", "Male")), "not found")
})

test_that("strict_match errors on duplicate matches", {
  expect_error(strict_match("ARM", c("ARM", "Placebo", "ARM", "Active")), "must be unique")
})
