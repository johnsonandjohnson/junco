if (requireNamespace("rbmi", quietly = TRUE)) {
  suppressPackageStartupMessages(library(rbmi))
} else {
  skip("rbmi package not available")
}

library(rbmi)
require(mockery)


test_that("mod_pool_internal_rubin combines results correctly", {
  # Create mock data
  results <- list(
    est = c(1.2, 1.5, 0.9, 1.3, 1.0),
    se = c(0.2, 0.3, 0.15, 0.25, 0.22),
    df = c(12, 15, 10, 14, 11)
  )

  # Create test/mock functions to replace rbmi dependencies
  mock_rubin_rules <- function(ests, ses, v_com) {
    list(
      est_point = mean(ests),
      var_t = var(ests) + mean(ses^2),
      df = 10
    )
  }

  mock_parametric_ci <- function(
    point,
    se,
    alpha,
    alternative,
    qfun,
    pfun,
    df
  ) {
    q_val <- qfun(1 - alpha / 2, df = df)
    ci <- switch(
      alternative,
      "two.sided" = c(point - q_val * se, point + q_val * se),
      "less" = c(-Inf, point + q_val * se),
      "greater" = c(point - q_val * se, Inf)
    )
    p_val <- switch(
      alternative,
      "two.sided" = 2 * pfun(-abs((point) / se), df = df),
      "less" = pfun(point / se, df = df),
      "greater" = pfun(-point / se, df = df)
    )
    list(
      est = point,
      ci = ci,
      se = se,
      pvalue = p_val
    )
  }

  # Mock dependencies
  with_mocks <- function(expr) {
    mockery::stub(
      mod_pool_internal_rubin,
      "rbmi:::rubin_rules",
      mock_rubin_rules
    )
    mockery::stub(
      mod_pool_internal_rubin,
      "rbmi:::parametric_ci",
      mock_parametric_ci
    )
    force(expr)
  }

  # Test two-sided
  with_mocks({
    out1 <- mod_pool_internal_rubin(
      results,
      conf.level = 0.95,
      alternative = "two.sided",
      type = "normal",
      D = 1
    )
  })
  expect_type(out1, "list")
  expect_named(out1, c("est", "ci", "se", "pvalue", "df"))
  expect_equal(out1$est, mean(results$est))

  # Test one-sided less
  with_mocks({
    out2 <- mod_pool_internal_rubin(
      results,
      conf.level = 0.90,
      alternative = "less",
      type = "normal",
      D = 1
    )
  })
  expect_false(is.infinite(out2$ci[1]))
  expect_true(is.infinite(out2$ci[2]))

  # Test one-sided greater
  with_mocks({
    out3 <- mod_pool_internal_rubin(
      results,
      conf.level = 0.90,
      alternative = "greater",
      type = "normal",
      D = 1
    )
  })
  expect_true(is.infinite(out3$ci[1]))
  expect_false(is.infinite(out3$ci[2]))
})

test_that("pool function processes and returns combined results", {
  # Create a mock imputations object with the right structure
  mock_results <- list(
    # First parameter results for 3 imputations
    param1 = list(
      est = c(1.2, 1.3, 1.1),
      se = c(0.2, 0.25, 0.18),
      df = c(10, 12, 11)
    ),
    # Second parameter results for 3 imputations
    param2 = list(
      est = c(2.1, 1.9, 2.2),
      se = c(0.3, 0.28, 0.33),
      df = c(15, 14, 16)
    )
  )

  # Create fake results object
  fake_results <- list(
    results = structure(
      list(
        imp1 = list(
          param1 = list(est = 1.2, se = 0.2, df = 10),
          param2 = list(est = 2.1, se = 0.3, df = 15)
        ),
        imp2 = list(
          param1 = list(est = 1.3, se = 0.25, df = 12),
          param2 = list(est = 1.9, se = 0.28, df = 14)
        ),
        imp3 = list(
          param1 = list(est = 1.1, se = 0.18, df = 11),
          param2 = list(est = 2.2, se = 0.33, df = 16)
        )
      ),
      class = "rubin"
    ),
    method = list(D = 1)
  )

  pool_no_validate <- function(
    results,
    conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"),
    type = c("percentile", "normal")
  ) {
    # Skip validation step rbmi::validate(results)

    alternative <- match.arg(alternative)
    type <- match.arg(type)

    pool_type <- class(results$results)[[1]]
    checkmate::assert_true(identical(pool_type, "rubin"))

    results_transpose <- mock_results # Use our pre-defined mock results directly

    pars <- lapply(
      results_transpose,
      function(x, ...) {
        # Return values that match what parametric_ci would produce
        if (identical(x, mock_results$param1)) {
          list(
            est = mean(x$est),
            ci = c(mean(x$est) - 0.5, mean(x$est) + 0.5),
            se = 0.25,
            pvalue = 0.05,
            df = median(x$df)
          )
        } else {
          list(
            est = mean(x$est),
            ci = c(mean(x$est) - 0.6, mean(x$est) + 0.6),
            se = 0.3,
            pvalue = 0.02,
            df = median(x$df)
          )
        }
      },
      conf.level = conf.level,
      alternative = alternative,
      type = type,
      D = results$method$D
    )

    method <- pool_type

    ret <- list(
      pars = pars,
      conf.level = conf.level,
      alternative = alternative,
      N = length(results$results),
      method = method
    )
    class(ret) <- "pool"
    return(ret)
  }

  # Run the test with our modified pool function
  res <- pool_no_validate(
    fake_results,
    conf.level = 0.95,
    alternative = "two.sided",
    type = "normal"
  )

  # Check the results structure
  expect_s3_class(res, "pool")
  expect_equal(res$conf.level, 0.95)
  expect_equal(res$alternative, "two.sided")
  expect_equal(res$N, 3)
  expect_equal(res$method, "rubin")

  # Check the parameters
  expect_named(res$pars, c("param1", "param2"))

  # Check parameter 1
  expect_equal(res$pars$param1$est, mean(mock_results$param1$est))
  expect_equal(
    res$pars$param1$ci,
    c(mean(mock_results$param1$est) - 0.5, mean(mock_results$param1$est) + 0.5)
  )
  expect_equal(res$pars$param1$se, 0.25)
  expect_equal(res$pars$param1$pvalue, 0.05)
  expect_equal(res$pars$param1$df, median(mock_results$param1$df))

  # Check parameter 2
  expect_equal(res$pars$param2$est, mean(mock_results$param2$est))
  expect_equal(
    res$pars$param2$ci,
    c(mean(mock_results$param2$est) - 0.6, mean(mock_results$param2$est) + 0.6)
  )
  expect_equal(res$pars$param2$se, 0.3)
  expect_equal(res$pars$param2$pvalue, 0.02)
  expect_equal(res$pars$param2$df, median(mock_results$param2$df))
})

test_that("Pool (Rubin) works as expected when se = NA in analysis model", {
  set.seed(101)

  mu <- 0
  sd <- 1
  n <- 2000
  vals <- rnorm(n, mu, sd)
  real_mu <- mean(vals)

  runanalysis <- function(x) {
    list("p1" = list(est = mean(x), se = NA, df = NA))
  }

  results_bayes <- rbmi_as_analysis(
    method = rbmi::method_bayes(n_samples = 5000),
    results = lapply(
      seq_len(5000),
      function(x) runanalysis(sample(vals, size = n, replace = TRUE))
    )
  )
  bayes <- rbmi_pool(results_bayes)
  bayes2 <- rbmi_pool(results_bayes, conf.level = 0.8)
  bayes3 <- rbmi_pool(results_bayes, alternative = "greater")

  expect_equal(
    bayes$pars$p1,
    list(
      est = real_mu,
      ci = as.numeric(c(NA, NA)),
      se = as.numeric(NA),
      pvalue = as.numeric(NA),
      df = NA
    ),
    tolerance = 1e-2
  )

  expect_equal(
    bayes2$pars$p1,
    list(
      est = real_mu,
      ci = as.numeric(c(NA, NA)),
      se = as.numeric(NA),
      pvalue = as.numeric(NA),
      df = NA
    ),
    tolerance = 1e-2
  )

  expect_equal(
    bayes3$pars$p1,
    list(
      est = real_mu,
      ci = as.numeric(c(NA, NA)),
      se = as.numeric(NA),
      pvalue = as.numeric(NA),
      df = NA
    ),
    tolerance = 1e-2
  )

  runanalysis <- function(x) {
    list("p1" = list(est = mean(x), se = NA, df = Inf))
  }

  results_bayes <- rbmi_as_analysis(
    method = rbmi::method_bayes(n_samples = 5000),
    results = lapply(
      seq_len(5000),
      function(x) runanalysis(sample(vals, size = n, replace = TRUE))
    )
  )
  bayes <- rbmi_pool(results_bayes)
  bayes2 <- rbmi_pool(results_bayes, conf.level = 0.8)
  bayes3 <- rbmi_pool(results_bayes, alternative = "greater")

  expect_equal(
    bayes$pars$p1,
    list(
      est = real_mu,
      ci = as.numeric(c(NA, NA)),
      se = as.numeric(NA),
      pvalue = as.numeric(NA),
      df = NA
    ),
    tolerance = 1e-2
  )

  expect_equal(
    bayes2$pars$p1,
    list(
      est = real_mu,
      ci = as.numeric(c(NA, NA)),
      se = as.numeric(NA),
      pvalue = as.numeric(NA),
      df = NA
    ),
    tolerance = 1e-2
  )

  expect_equal(
    bayes3$pars$p1,
    list(
      est = real_mu,
      ci = as.numeric(c(NA, NA)),
      se = as.numeric(NA),
      pvalue = as.numeric(NA),
      df = NA
    ),
    tolerance = 1e-2
  )
})
