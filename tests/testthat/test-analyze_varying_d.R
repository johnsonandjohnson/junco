testthat::test_that("a_summary_d_j work with healthy input.", {
  options("width" = 100)
  
  # numeric input
  set.seed(1)
  df <- tibble(x = rnorm(10), decimal1 = 0, decimal2 = 1)
 
  
  # numeric input - a_summary_d_j
  result <- a_summary_d_j(
    df, .var = "x", .stats = c("mean", "mean_sd", "mean_ci_3d")
  )
  testthat::expect_snapshot(result)
  
  # d from variable name
  result2 <- a_summary_d_j(
    df, .var = "x", .stats = c("mean", "mean_sd", "mean_ci_3d"), d = "decimal2", .df_row = df
  )
  testthat::expect_snapshot(result2)  
  
  # d = 2
  result3 <- a_summary_d_j(
    df, .var = "x", .stats = c("mean", "mean_sd", "mean_ci_3d"), d = 2
  )
  testthat::expect_snapshot(result3)    
  
  # d = 0 with custom format 
  result4 <- a_summary_d_j(
    df, .var = "x", .stats = c("mean", "mean_sd", "mean_ci_3d"), d = 0, .formats = c("mean_sd" = "xx.d (xx.d)")
  )
  testthat::expect_snapshot(result4)      
})


testthat::test_that("a_summarize_ancova_d_j work with healthy input.", {

  # default values for d
  lyt <- basic_table() %>% 
    split_cols_by("Species") %>% 
    analyze(vars = "Sepal.Length", afun = a_summarize_ancova_d_j,
            extra_args = list(
              variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width")),
              ref_path =  c("Species", "setosa"),
              conf_level = 0.99
            )
    )
  
  result <- build_table(lyt, iris)
  
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  
  # d = 0 
  lyt2 <- basic_table() %>% 
    split_cols_by("Species") %>% 
    analyze(vars = "Sepal.Length", afun = a_summarize_ancova_d_j,
            extra_args = list(
              variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width")),
              ref_path =  c("Species", "setosa"),
              conf_level = 0.99,
              d = 0
            )
    )
  
  result2 <- build_table(lyt2, iris)
  
  res2 <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res2)  
  
  # d = 0 with custom format for adjusted mean (99%CI)
  lyt3 <- basic_table() %>% 
    split_cols_by("Species") %>% 
    analyze(vars = "Sepal.Length", afun = a_summarize_ancova_d_j,
            extra_args = list(
              variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width")),
              ref_path =  c("Species", "setosa"),
              conf_level = 0.99,
              d = 0,
              .formats = c(lsmean_ci = "xx.dxx (xx.dxx, xx.dx)")
            )
    )
  
  result3 <- build_table(lyt3, iris)
  
  res3 <- testthat::expect_silent(result3)
  testthat::expect_snapshot(res3)    
  
})







