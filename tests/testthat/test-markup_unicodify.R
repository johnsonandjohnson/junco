test_that("dps markup works with ~{ and ~[", {
  skip_if(Sys.getenv("COVR") == "TRUE", "Skipping test when COVR is TRUE")
  strs <- c(
    "~{super a} and some ~{optional [a,b]} more st~{super a,b}uff",
    "~[super a] and some ~[optional {a,b}] more st~[super a,b]uff"
  )
  out <- prep_strs_for_rtf(strs)
  expect_snapshot(cran = TRUE, out)
})

test_that("var_relabel_list replaces existing labels when replace_existing = TRUE", {
  df <- formatters::var_relabel(iris, Sepal.Length = "Old Label")
  res <- var_relabel_list(df, list(Sepal.Length = "New Label", Sepal.Width = "Width"))
  lbls <- formatters::var_labels(res)
  expect_equal(lbls[["Sepal.Length"]], "New Label")
  expect_equal(lbls[["Sepal.Width"]], "Width")
})

test_that("var_relabel_list ignores variables not in lbl_list", {
  df <- iris
  res <- var_relabel_list(df, list(Sepal.Length = "SL"))
  lbls <- formatters::var_labels(res)
  expect_equal(lbls[["Sepal.Length"]], "SL")
  expect_true(is.na(lbls[["Sepal.Width"]]))
})

test_that("var_relabel_list only fills unlabelled vars when replace_existing = FALSE", {
  df <- formatters::var_relabel(iris, Sepal.Length = "Keep This")
  res <- var_relabel_list(
    df,
    list(Sepal.Length = "Should Not Replace", Sepal.Width = "Fill This"),
    replace_existing = FALSE
  )
  lbls <- formatters::var_labels(res)
  expect_equal(lbls[["Sepal.Length"]], "Keep This")
  expect_equal(lbls[["Sepal.Width"]], "Fill This")
})
