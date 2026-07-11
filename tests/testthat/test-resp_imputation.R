binary_imputation_data <- function() {
  data.frame(
    arm = rep(c("Control", "Treatment"), each = 8),
    response = c(
      TRUE,
      FALSE,
      NA,
      TRUE,
      FALSE,
      NA,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      NA,
      TRUE,
      TRUE,
      NA,
      FALSE,
      TRUE
    ),
    strata = rep(c("A", "B"), 8)
  )
}

test_that("h_impute_analyze_resp returns one result for each independent imputation", {
  set.seed(42)
  result <- h_impute_analyze_resp(
    dat = binary_imputation_data(),
    p_ctrl = 0.2,
    p_trt = 0.8,
    trtvar = "arm",
    ctrlab = "Control",
    trtlab = "Treatment",
    respvar = "response",
    stratvar = "strata",
    n_imputations = 3
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("rd_est", "rd_se", "rd_var", "p_cmh", "z_stat"))
  expect_equal(nrow(result), 3)
  expect_equal(result$rd_var, result$rd_se^2)
  expect_true(all(is.finite(unlist(result))))
})

test_that("resp_multiple_imputation pools paired response-probability scenarios", {
  pvalcat <- list("<0.05" = c(0, 0.05), ">=0.05" = c(0.05, 1))

  set.seed(123)
  result <- resp_multiple_imputation(
    dat = binary_imputation_data(),
    p_ctrl = c(0, 0.2),
    p_trt = c(1, 0.8),
    trtvar = "arm",
    ctrlab = "Control",
    trtlab = "Treatment",
    respvar = "response",
    stratvar = "strata",
    n_imputations = 3,
    pvalcat = pvalcat
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("p_ctrl", "p_trt", "m", "rd", "rd_se", "p_value", "p_cat", "marker_flag", "effect_label")
  )
  expect_equal(result$m, c(1, 3))
  expect_equal(result$marker_flag[[1]], "*")
  expect_match(result$effect_label[[1]], "\\*$")
  expect_true(all(is.finite(unlist(result[c("rd", "rd_se", "p_value")]))))
  expect_true(all(result$p_cat %in% names(pvalcat)))
})
