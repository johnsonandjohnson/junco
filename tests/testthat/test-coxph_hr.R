test_that("a_coxph_hr is deprecated", {
  expect_error(
    lifecycle::expect_deprecated(
      a_coxph_hr(df = data.frame(), .var = "x", ref_path = NULL, .spl_context = NULL)
    )
  )
})


test_that("s_coxph_hr is deprecated", {
  expect_error(
    lifecycle::expect_deprecated(
      s_coxph_hr(df = data.frame(), .var = "x", ref_path = NULL, .spl_context = NULL)
    )
  )
})
