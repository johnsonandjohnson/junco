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

test_that("string_to_title works as expected for factors", {
  x <- factor("THIS IS an eXaMple statement TO CAPItaliZe")
  result <- string_to_title(x)
  expected <- factor("This Is An Example Statement To Capitalize")
  expect_identical(result, expected)
})

test_that("string_to_title works as expected for factors (missing levels)", {
  x <- factor(c("AbC def", "gHI"), levels = c("AbC def", "gHI", "jkl mnoP"))
  result <- string_to_title(x)
  expected <- factor(c("Abc Def", "Ghi"), levels = c("Abc Def", "Ghi", "Jkl Mnop"))
  expect_identical(result, expected)
})
