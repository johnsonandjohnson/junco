test_that("dps markup works with ~{ and ~[", {
  skip_if(Sys.getenv("COVR") == "TRUE", "Skipping test when COVR is TRUE")
  strs <- c(
    "~{super a} and some ~{optional [a,b]} more st~{super a,b}uff",
    "~[super a] and some ~[optional {a,b}] more st~[super a,b]uff"
  )
  out <- prep_strs_for_rtf(strs)
  expect_snapshot(out)
})
