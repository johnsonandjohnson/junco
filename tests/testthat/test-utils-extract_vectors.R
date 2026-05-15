test_that("extract_vectors() works for unpaired data (int,num)", {
  df1 <- data.frame(id = LETTERS[1:8], value = 1:8)
  df2 <- data.frame(id = LETTERS[c(1, 3:8, 10)], value = c(11, 13:18, NA))

  result <- extract_vectors(df1, df2, "value")
  expected <- list(x1 = df1$value, x2 = c(11, 13:18))

  expect_identical(result, expected)
})

test_that("extract_vectors() works for paired data (int,int)", {
  df1 <- data.frame(id = LETTERS[1:8], value = 1:8)
  df2 <- data.frame(id = LETTERS[c(1, 3:8, 10)], value = c(11L, 13:18, NA))

  result <- extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
  expected <- list(x1 = c(1L, 3:8), x2 = c(11L, 13:18))

  expect_identical(result, expected)
})

test_that("extract_vectors() works for paired data (char,char)", {
  df1 <- data.frame(id = c("A", "B", "C"), value = c("x", "y", "z"))
  df2 <- data.frame(id = c("A", "B", "C"), value = c("a", "b", "c"))

  result <- extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
  expected <- list(x1 = df1$value, x2 = df2$value)

  expect_identical(result, expected)
})

test_that("extract_vectors() removes missing values independently for unpaired data", {
  df1 <- data.frame(id = 1:8, value = c(1, NA, 3, NaN, 5, 6, NA, 8))
  df2 <- data.frame(id = 1:8, value = c(NA, 12, 13, 14, NaN, 16, 17, 18))

  result <- extract_vectors(df1, df2, "value")
  expected <- list(x1 = c(1, 3, 5, 6, 8), x2 = c(12, 13, 14, 16, 17, 18))

  expect_identical(result, expected)
})

test_that("extract_vectors() keeps only complete pairs", {
  df1 <- data.frame(id = LETTERS[1:8], value = c(1, 2, NA, 4, 5, NA, 7, 8))
  df2 <- data.frame(
    id = LETTERS[c(8, 2, 1, 5, 6, 4, 7, 3)],
    value = c(11, NA, 13, 14, 15, 16, NA, 18)
  )

  result <- extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
  expected <- list(x1 = c(1, 4, 5, 8), x2 = c(13, 16, 14, 11))

  expect_identical(result, expected)
})

test_that("extract_vectors() excludes missing pairing keys", {
  df1 <- data.frame(id = c("A", "B", NA, "D", "E", NA, "G", "H"), value = 1:8)
  df2 <- data.frame(id = c("A", "D", "B", NA, "E", "F", NA, "H"), value = 11:18)

  result <- extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
  expected <- list(x1 = c(1L, 2L, 4L, 5L, 8L), x2 = c(11L, 13L, 12L, 15L, 18L))

  expect_identical(result, expected)
})

test_that("extract_vectors() supports multiple factor pairing variables", {
  df1 <- data.frame(
    id = factor(c("A", "A", "B", "B", "C", "C", "D", "D")),
    visit = factor(c(rep(c("baseline", "week1"), times = 4))),
    value = c(10, 20, 30, 40, 50, 60, 70, 80)
  )

  df2 <- data.frame(
    id = factor(c("A", "A", "B", "B", "C", "C", "D", "E")),
    visit = factor(c(
      "baseline", "week1",
      "baseline", "week2",
      "baseline", "week1",
      "baseline", "week1"
    )),
    value = c(110, 120, 130, 140, 150, 160, 170, 180)
  )

  result <- extract_vectors(df1, df2, "value", paired = TRUE, paired_by = c("id", "visit"))
  expected <- list(
    x1 = c(10, 20, 30, 50, 60, 70),
    x2 = c(110, 120, 130, 150, 160, 170)
  )

  expect_identical(result, expected)
})

test_that("extract_vectors() returns empty vectors when no pairs match", {
  df1 <- data.frame(id = LETTERS[1:8], value = 1:8)
  df2 <- data.frame(id = LETTERS[9:16], value = 11:18)

  result <- extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
  expected <- list(x1 = integer(0), x2 = integer(0))

  expect_identical(result, expected)
})

test_that("extract_vectors() errors when .var is missing", {
  df1 <- data.frame(id = "A", x = 1)
  df2 <- data.frame(id = "A", x = 2)

  expect_error(
    extract_vectors(df1, df2, "value"),
    "must include"
  )
})

test_that("extract_vectors() errors when paired_by is missing", {
  df1 <- data.frame(id = "A", value = 1)
  df2 <- data.frame(other = "A", value = 2)

  expect_error(
    extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
  )
  expect_error(
    extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "some_id")
  )

  expect_error(
    extract_vectors(df2, df1, "value", paired = TRUE, paired_by = "id")
  )
  expect_error(
    extract_vectors(df2, df1, "value", paired = TRUE, paired_by = "some_id")
  )
})

test_that("extract_vectors() ignores paired_by when paired = FALSE", {
  df1 <- data.frame(id = c("A", "B", "C"), value = 1:3)
  df2 <- data.frame(id = c("B", "A"), value = 11:12)

  result <- extract_vectors(df1, df2, "value", paired = FALSE, paired_by = "id")
  expected <- list(x1 = df1$value, x2 = df2$value)

  expect_identical(result, expected)
})

test_that("extract_vectors() ignores multi-column paired_by when paired = FALSE", {
  df1 <- data.frame(id = c("A", "B"), visit = c("x", "y"), value = 1:2)
  df2 <- data.frame(id = c("B", "A"), visit = c("x", "y"), value = 10:11)

  result <- extract_vectors(
    df1, df2, "value",
    paired = FALSE, paired_by = c("id", "visit")
  )
  expected <- list(x1 = df1$value, x2 = df2$value)

  expect_identical(result, expected)
})

test_that("extract_vectors() errors when df1 pairing keys are duplicated", {
  df1 <- data.frame(id = c("A", "A", "B", "C", "D", "E"), value = 1:6)
  df2 <- data.frame(id = LETTERS[1:8], value = 11:18)

  expect_error(
    extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id"),
    "Duplicate values in 'paired_by' columns in df1 \\(complete cases only\\)"
  )
})

test_that("extract_vectors() errors when df1/df2 pairing keys are duplicated with NA", {
  df1 <- data.frame(id = c("A", "A", NA, "B"), value = 1:4)
  df2 <- data.frame(id = c("A", "B", "C", "D"), value = 11:14)

  expect_error(
    extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id"),
    "Duplicate values in 'paired_by' columns in df1"
  )

  expect_error(
    extract_vectors(df2, df1, "value", paired = TRUE, paired_by = "id"),
    "Duplicate values in 'paired_by' columns in df2"
  )
})

# Note that this tests is different than
# "extract_vectors() errors when df1/df2 pairing keys are duplicated with NA",
# because here we do not have duplicateds in keys on complete data.
test_that("extract_vectors() errors when df1/df2 pairing keys are duplicated on NA", {
  df1 <- data.frame(id = c("A", "A", NA, "B"), value = 1:4)
  df2 <- data.frame(id = c("A", NA, "C", "D"), value = 11:14)

  expect_error(
    extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id"),
    "Duplicate values in 'paired_by' columns in df1"
  )

  expect_error(
    extract_vectors(df2, df1, "value", paired = TRUE, paired_by = "id"),
    "Duplicate values in 'paired_by' columns in df2"
  )
})
