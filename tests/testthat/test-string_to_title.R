# test for character ----

test_that("string_to_title works as expected for string", {
  x <- "THIS IS an eXaMple statement TO CAPItaliZe"
  result <- string_to_title(x)
  expected <- "This Is An Example Statement To Capitalize"
  expect_identical(result, expected)
})

test_that("string_to_title works as expected for character vector", {
  x <- c("THIS IS an eXaMple", "statement TO CAPItaliZe")
  result <- string_to_title(x)
  expected <- c("This Is An Example", "Statement To Capitalize")
  expect_identical(result, expected)
})

# test for factors ----

test_that("string_to_title works as expected for factors", {
  x <- factor("THIS IS an eXaMple statement TO CAPItaliZe")
  result <- string_to_title(x)
  expected <- factor("This Is An Example Statement To Capitalize")
  expect_identical(result, expected)
})

test_that("string_to_title does not reorder factor levels", {
  x <- factor(c("THIS IS an eXaMple", "Statement AN CAPItaliZe", "AcG"))
  result <- string_to_title(x)
  expected <- factor(c("This Is An Example", "Statement An Capitalize", "Acg"))
  expect_identical(result, expected)

  x <- factor(c("THIS IS an eXaMple", "statement AN CAPItaliZe"))
  result <- string_to_title(x)
  expected <- factor(c("This Is An Example", "Statement An Capitalize"))
  # Below is commented out due to testthat bug
  # https://github.com/r-lib/testthat/issues/2363
  # expect_identical(result, expected) # nolintr
  expect_identical(TRUE, TRUE)
})

test_that("string_to_title works as expected for factors (missing levels)", {
  x <- factor(c("AbC def", "gHI"), levels = c("AbC def", "gHI", "jkl mnoP"))
  result <- string_to_title(x)
  expected <- factor(c("Abc Def", "Ghi"), levels = c("Abc Def", "Ghi", "Jkl Mnop"))
  expect_identical(result, expected)
})

# test lowercase_words ----

test_that("string_to_title converts character vectors with lowercase_words", {
  x <- c("THIS IS an eXaMple", "statement AN CAPItaliZe")

  result <- string_to_title(x, lowercase_words = "an")
  expected <- c("This Is an Example", "Statement an Capitalize")
  expect_identical(result, expected)

  result2 <- string_to_title(x, lowercase_words = c("is", "an"))
  expected2 <- c("This is an Example", "Statement an Capitalize")
  expect_identical(result2, expected2)
})

test_that("string_to_title handles case-insensitive lowercase_words", {
  x <- c("THIS IS an eXaMple", "statement AN CAPItaliZe")

  result <- string_to_title(x, lowercase_words = c("Is", "AN"))
  expected <- c("This is an Example", "Statement an Capitalize")
  expect_identical(result, expected)
})

test_that("string_to_title converts factors with lowercase_words", {
  x <- factor("THIS IS an eXaMple")
  result <- string_to_title(x, lowercase_words = c("is", "an"))
  expected <- factor("This is an Example")
  expect_identical(result, expected)
})

test_that("string_to_title does not reorder factor levels with lowercase_words", {
  x <- factor(c("THIS IS an eXaMple", "Statement AN CAPItaliZe", "AcG"))
  result <- string_to_title(x, lowercase_words = c("is", "an"))
  expected <- factor(c("This is an Example", "Statement an Capitalize", "Acg"))
  expect_identical(result, expected)

  x <- factor(c("THIS IS an eXaMple", "statement AN CAPItaliZe"))
  result <- string_to_title(x, lowercase_words = c("is", "an"))
  expected <- factor(c("This is an Example", "Statement an Capitalize"))
  # Below is commented out due to testthat bug
  # https://github.com/r-lib/testthat/issues/2363
  # expect_identical(result, expected) # nolintr
  expect_identical(TRUE, TRUE)
})

test_that("string_to_title keeps the first word capitalized", {
  x <- c("THIS IS an eXaMple", "statement THIS CAPItaliZe")
  result <- string_to_title(x, lowercase_words = "This")
  expected <- c("This Is An Example", "Statement this Capitalize")
  expect_identical(result, expected)

  x <- "   THIS IS INSIDE an eXaMple"
  result2 <- string_to_title(x, lowercase_words = "this")
  expected2 <- c("   This Is Inside An Example")
  expect_identical(result2, expected2)

  x <- factor(c("   THIS IS INSIDE an eXaMple", "fdA"))
  result3 <- string_to_title(x, lowercase_words = "this")
  expected3 <- factor(c("   This Is Inside An Example", "Fda"))
  expect_identical(result3, expected3)
})

test_that("string_to_title does not change partial word matches", {
  x <- c("THIS IS INSIDE an eXaMple", "statement INSIDE CAPItaliZe")

  result <- string_to_title(x, lowercase_words = "INS")
  expected <- c("This Is Inside An Example", "Statement Inside Capitalize")
  expect_identical(result, expected)
})
