test_that("copy_attributes() copies missing attributes", {
  x <- LETTERS[10:14]
  attr(x, "label") <- c("X", "x")

  y <- letters[1:5]
  y_with_attr <- copy_attributes(x, y)

  y_label <- attr(y_with_attr, "label")
  expect_identical(y_label, c("X", "x"))
})

test_that("copy_attributes() does not alter values of target", {
  x <- LETTERS[10:14]
  attr(x, "label") <- "X"

  y <- letters[1:5]
  y_with_attr <- copy_attributes(x, y)

  expect_equal(y_with_attr, y, ignore_attr = TRUE)
})

test_that("copy_attributes() does not copy attributes already present in target", {
  x <- LETTERS[10:14]
  attr(x, "label") <- "X"

  y <- letters[1:5]
  attr(y, "label") <- "Y"
  y_with_attr <- copy_attributes(x, y)

  y_label <- attr(y_with_attr, "label")
  expect_identical(y_label, "Y")
})

test_that("copy_attributes() preserves existing attributes and copies missing attributes", {
  x <- LETTERS[10:14]
  attr(x, "label") <- "X"
  attr(x, "precision") <- 15

  y <- letters[1:5]
  attr(y, "label") <- "Y"
  y_with_attr <- copy_attributes(x, y)

  expect_identical(attr(y_with_attr, "label"), "Y")
  expect_identical(attr(y_with_attr, "precision"), 15)
})

test_that("copy_attributes() works as expected for factors", {
  x <- factor(LETTERS[10:14])
  attr(x, "label") <- "X"
  attr(x, "precision") <- 15

  y <- factor(letters[1:5])
  attr(y, "label") <- "Y"
  y_with_attr <- copy_attributes(x, y)

  expect_s3_class(y_with_attr, "factor")
  expect_equal(y_with_attr, y, ignore_attr = TRUE)
  expect_identical(levels(y_with_attr), levels(y))
  expect_identical(attr(y_with_attr, "label"), "Y")
  expect_identical(attr(y_with_attr, "precision"), 15)
})

test_that("copy_attributes() returns target unchanged if source has no attributes", {
  x <- LETTERS[10:14]
  y <- letters[1:5]

  expect_identical(copy_attributes(x, y), y)
})
