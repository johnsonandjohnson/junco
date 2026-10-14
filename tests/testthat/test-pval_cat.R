pvalcat <- list(
  "<0.001" = c(0, 0.001),
  "0.001 to <0.05" = c(0.001, 0.05),
  ">=0.05" = c(0.05, 1)
)

test_that("h_normalize_pvalcat returns category bounds and labels", {
  expect_equal(
    h_normalize_pvalcat(pvalcat),
    list(
      bounds = rbind(c(0, 0.001), c(0.001, 0.05), c(0.05, 1)),
      cats = names(pvalcat)
    )
  )
})

test_that("categorize_pval respects category boundaries", {
  expect_equal(
    categorize_pval(c(0, 0.0009, 0.001, 0.0499, 0.05, 1, NA), pvalcat),
    c("<0.001", "<0.001", "0.001 to <0.05", "0.001 to <0.05", ">=0.05", ">=0.05", NA)
  )
})

test_that("categorize_pval returns NA outside configured categories", {
  custom_pvalcat <- list("small" = c(0, 0.01), "large" = c(0.05, 0.1))

  expect_equal(
    categorize_pval(c(-0.001, 0.02, 0.1, 0.2), custom_pvalcat),
    c(NA, NA, "large", NA)
  )
})

test_that("p-value category inputs are validated", {
  expect_error(h_normalize_pvalcat(list(c(0, 0.05))))
  expect_error(h_normalize_pvalcat(list("invalid" = c(0.05, 0))))
  expect_error(categorize_pval("0.05", pvalcat))
})
