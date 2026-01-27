library(testthat)

test_that("get_visit_levels returns visit levels in correct order", {
  # Test standard case with ordered numeric values
  visit_cat <- c("Week 1", "Week 2", "Week 4", "Week 12")
  visit_n <- c(1, 2, 4, 12)

  result <- get_visit_levels(visit_cat, visit_n)

  expect_equal(result, c("Week 1", "Week 2", "Week 4", "Week 12"))

  # Test with unordered input
  visit_cat2 <- c("Week 12", "Week 1", "Week 4", "Week 2")
  visit_n2 <- c(12, 1, 4, 2)

  result2 <- get_visit_levels(visit_cat2, visit_n2)

  expect_equal(result2, c("Week 1", "Week 2", "Week 4", "Week 12"))

  # Test with duplicate visits that have same numeric value (should be returned once)
  visit_cat3 <- c("Week 1", "Week 2", "Week 1", "Week 4")
  visit_n3 <- c(1, 2, 1, 4)

  result3 <- get_visit_levels(visit_cat3, visit_n3)

  expect_equal(result3, c("Week 1", "Week 2", "Week 4"))

  # Test with non-standard visit names
  visit_cat4 <- c("Baseline", "End of Treatment", "Follow-up")
  visit_n4 <- c(0, 10, 20)

  result4 <- get_visit_levels(visit_cat4, visit_n4)

  expect_equal(result4, c("Baseline", "End of Treatment", "Follow-up"))
})

test_that("get_visit_levels validates input correctly", {
  # Test with invalid inputs
  expect_error(get_visit_levels(1:3, 1:3), "Assertion on 'visit_cat' failed")
  expect_error(
    get_visit_levels(c("Week 1", "Week 2"), c("1", "2")),
    "Assertion on 'visit_n' failed"
  )

  # Test with different length inputs (should still work as long as they match correctly)
  visit_cat5 <- c("Week 1", "Week 2", "Week 1")
  visit_n5 <- c(1, 2, 1)

  result5 <- get_visit_levels(visit_cat5, visit_n5)
  expect_equal(result5, c("Week 1", "Week 2"))

  # Test with mismatched cat-numeric pairs (non-unique result)
  visit_cat6 <- c("Week 1", "Week 2", "Week 1")
  visit_n6 <- c(1, 2, 3)

  expect_error(
    get_visit_levels(visit_cat6, visit_n6),
    "Assertion on 'visit_levels' failed"
  )
})
