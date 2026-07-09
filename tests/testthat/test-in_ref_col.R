test_that("in_ref_col works for a single-level split", {
  .spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("Placebo"))
  )

  res <- in_ref_col(c("ARM", "Placebo"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "Placebo1"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col matches a single-level split with a wildcard", {
  .spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("Placebo"))
  )

  res <- in_ref_col(c("ARM", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "Placebo"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "Placebo1"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col uses the leaf row split for a single-level split", {
  .spl_context <- data.frame(
    cur_col_split = I(list("ARM_0", "ARM")),
    cur_col_split_val = I(list("Placebo_0", "Placebo"))
  )

  res <- in_ref_col(c("ARM", "Placebo"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "Placebo1"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col uses the leaf row split for a single-level split with a wildcard", {
  .spl_context <- data.frame(
    cur_col_split = I(list("ARM_0", "ARM")),
    cur_col_split_val = I(list("Placebo_0", "Placebo"))
  )

  res <- in_ref_col(c("ARM", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "Placebo"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "Placebo1"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col works for a two-level split", {
  .spl_context <- data.frame(
    cur_col_split = I(list(c("ARM", "desc_stat"))),
    cur_col_split_val = I(list(c("Placebo", "N")))
  )

  res <- in_ref_col(c("ARM", "Placebo", "desc_stat", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "Placebo", "desc_stat", "N1"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "Placebo"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col works for a two-level split with a wildcard", {
  .spl_context <- data.frame(
    cur_col_split = I(list(c("ARM", "desc_stat"))),
    cur_col_split_val = I(list(c("Placebo", "N")))
  )

  # TRUE
  res <- in_ref_col(c("ARM", "Placebo", "desc_stat", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "Placebo", "*", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "*", "desc_stat", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "Placebo", "desc_stat", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "*", "desc_stat", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "*", "desc_stat", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "*", "*", "*"), .spl_context)
  expect_identical(res, TRUE)

  # FALSE
  res <- in_ref_col(c("", "Placebo", "desc_stat", "*"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "Placebo1", "*", "N"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "*", "desc_stat1", "N"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("*", "Placebo", "desc_stat", "N1"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "*", "desc_stat1", "*"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("*", "*", "desc_stat ", "*"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col uses the leaf row split for a two-level split", {
  .spl_context <- data.frame(
    cur_col_split = I(list(c("ARM_0", "desc_stat_0"), c("ARM", "desc_stat"))),
    cur_col_split_val = I(list(c("Placebo_0", "N_0"), c("Placebo", "N")))
  )

  res <- in_ref_col(c("ARM", "Placebo", "desc_stat", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "Placebo", "desc_stat", "N1"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "Placebo"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col uses the leaf row split for a two-level split with a wildcard", {
  .spl_context <- data.frame(
    cur_col_split = I(list(c("ARM_0", "desc_stat_0"), c("ARM", "desc_stat"))),
    cur_col_split_val = I(list(c("Placebo_0", "N_0"), c("Placebo", "N")))
  )

  # TRUE
  res <- in_ref_col(c("ARM", "Placebo", "desc_stat", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "Placebo", "*", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "*", "desc_stat", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "Placebo", "desc_stat", "N"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("ARM", "*", "desc_stat", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "*", "desc_stat", "*"), .spl_context)
  expect_identical(res, TRUE)

  res <- in_ref_col(c("*", "*", "*", "*"), .spl_context)
  expect_identical(res, TRUE)

  # FALSE
  res <- in_ref_col(c("", "Placebo", "desc_stat", "*"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "Placebo1", "*", "N"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "*", "desc_stat1", "N"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("*", "Placebo", "desc_stat", "N1"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("ARM", "*", "desc_stat1", "*"), .spl_context)
  expect_identical(res, FALSE)

  res <- in_ref_col(c("*", "*", "desc_stat ", "*"), .spl_context)
  expect_identical(res, FALSE)
})

test_that("in_ref_col fails for invalid ref_path", {
  .spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("Placebo"))
  )

  expect_error(in_ref_col(character(0), .spl_context), regexp = "length")
  expect_error(in_ref_col("ARM", .spl_context), regexp = "length")
  expect_error(in_ref_col(c("ARM", "Placebo", ""), .spl_context), regexp = "length")
})

test_that("in_ref_col fails for invalid .spl_context", {
  .spl_context <- data.frame()
  expect_error(in_ref_col(c("ARM", "Placebo"), .spl_context), regexp = "length")

  .spl_context <- c(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("Placebo"))
  )
  expect_error(in_ref_col(c("ARM", "Placebo"), .spl_context), regexp = "data.frame")
})
