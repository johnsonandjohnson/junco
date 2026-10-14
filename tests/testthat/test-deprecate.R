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

test_that("a_kaplan_meier is deprecated", {
  expect_error(
    lifecycle::expect_deprecated(
      a_kaplan_meier(df = data.frame(), .var = "x", is_event = "y")
    )
  )
})

test_that("make_rbmi_cluster is deprecated", {
  expect_error(
    lifecycle::expect_deprecated(
      make_rbmi_cluster(cluster_or_cores = 1)
    )
  )
})

test_that("par_lapply is deprecated", {
  expect_error(
    lifecycle::expect_deprecated(
      par_lapply(NULL, function(x) x^2, 1:3)
    )
  )
})

test_that("rbmi_analyse is deprecated", {
  expect_error(
    lifecycle::expect_deprecated(
      rbmi_analyse(imputations = list())
    )
  )
})
