test_that("dps markup works with ~{ and ~[", {
  skip_on_covr()
  strs <- c(
    "~{super a} and some ~{optional [a,b]} more st~{super a,b}uff",
    "~[super a] and some ~[optional {a,b}] more st~[super a,b]uff"
  )
  out <- prep_strs_for_rtf(strs)
  expect_snapshot(cran = TRUE, out)
})
