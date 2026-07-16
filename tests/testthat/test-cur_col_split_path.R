test_that("cur_col_split_path() works for a single-level split", {
  spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list("Placebo"))
  )

  res <- cur_col_split_path(spl_context)
  exp <- c("ARM", "Placebo")

  expect_identical(res, exp)
})

test_that("cur_col_split_path() uses the leaf row split for a single-level split", {
  spl_context <- data.frame(
    cur_col_split = I(list("ARM_0", "ARM")),
    cur_col_split_val = I(list("Placebo_0", "Placebo"))
  )

  res <- cur_col_split_path(spl_context)
  exp <- c("ARM", "Placebo")

  expect_identical(res, exp)
})

test_that("cur_col_split_path() works for a two-level split", {
  spl_context <- data.frame(
    cur_col_split = I(list(c("ARM", "desc_stat"))),
    cur_col_split_val = I(list(c("Placebo", "N")))
  )

  res <- cur_col_split_path(spl_context)
  exp <- c("ARM", "Placebo", "desc_stat", "N")

  expect_identical(res, exp)
})

test_that("cur_col_split_path() uses the leaf row split for a two-level split", {
  spl_context <- data.frame(
    cur_col_split = I(list(c("ARM_0", "desc_stat_0"), c("ARM", "desc_stat"))),
    cur_col_split_val = I(list(c("Placebo_0", "N_0"), c("Placebo", "N")))
  )

  res <- cur_col_split_path(spl_context)
  exp <- c("ARM", "Placebo", "desc_stat", "N")

  expect_identical(res, exp)
})

test_that("cur_col_split_path() fails for invalid .spl_context (missing cur_col_split/cur_col_split_val)", {
  spl_context <- data.frame(
    cur_col_split_val = I(list("Placebo"))
  )
  expect_error(cur_col_split_path(spl_context), regexp = "cur_col_split")

  spl_context <- data.frame(
    cur_col_split = I(list("ARM"))
  )
  expect_error(cur_col_split_path(spl_context), regexp = "cur_col_split_val")
})

test_that("cur_col_split_path() fails for invalid .spl_context (cur_col_split/cur_col_split_val are not lists)", {
  spl_context <- data.frame(
    cur_col_split = c("ARM"),
    cur_col_split_val = I(list("Placebo"))
  )
  expect_error(cur_col_split_path(spl_context), "list")

  spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = c("Placebo")
  )
  expect_error(cur_col_split_path(spl_context), "list")
})

test_that("cur_col_split_path() fails for invalid .spl_context (cur_col_split/_val[[1]] are not character)", {
  spl_context <- data.frame(
    cur_col_split = I(list(5)),
    cur_col_split_val = I(list("Placebo"))
  )
  expect_error(cur_col_split_path(spl_context), "character")

  spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list(c(5)))
  )
  expect_error(cur_col_split_path(spl_context), "character")
})

test_that("cur_col_split_path() fails for invalid .spl_context (cur_col_split/_val[[1]] have different lengths)", {
  spl_context <- data.frame(
    cur_col_split = I(list("ARM")),
    cur_col_split_val = I(list(c("Placebo", "X")))
  )
  expect_error(cur_col_split_path(spl_context), "length")

  spl_context <- data.frame(
    cur_col_split = I(list(c("ARM", "VAR2"))),
    cur_col_split_val = I(list(c("Placebo")))
  )
  expect_error(cur_col_split_path(spl_context), "length")
})
