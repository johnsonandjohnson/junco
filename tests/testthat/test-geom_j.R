suppressPackageStartupMessages({
  library(ggplot2)
})

test_that("StatBoxplotQuantile computes type = 2 quantiles", {

  vec <- c(1, 2, 3, 4, 100) # force outlier
  df <- data.frame(x = 1, y = vec)

  res <- junco:::StatBoxplotQuantile$compute_group(
    data = df, scales = NULL, width = NULL, na.rm = FALSE, coef = 1.5
  )

  qs <- stats::quantile(vec, probs = c(0, 0.25, 0.5, 0.75, 1), type = 2)
  iqr <- qs[4] - qs[2]
  lower_fence <- qs[2] - 1.5 * iqr
  upper_fence <- qs[4] + 1.5 * iqr
  ymin_exp <- min(vec[vec >= lower_fence])
  ymax_exp <- max(vec[vec <= upper_fence])
  out_exp <- vec[vec < ymin_exp | vec > ymax_exp]

  expect_equal(res$lower, qs[[2]])
  expect_equal(res$middle, qs[[3]])
  expect_equal(res$upper, qs[[4]])
  expect_equal(res$ymin, ymin_exp)
  expect_equal(res$ymax, ymax_exp)
  expect_setequal(unlist(res$outliers), out_exp)
})

test_that("geom_boxplot_j uses type = 2 quantiles", {

  set.seed(123)
  df <- data.frame(
    g = factor(rep(c("a", "b"), each = 10)),
    y = c(sample(1:20, 10), sample(30:60, 10))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(g, y)) + geom_boxplot_j()
  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  d <- built$data[[1]]

  # quartiles per group using type = 2
  split_quantile_2 <- lapply(
    split(df$y, df$g),
    stats::quantile,
    probs = c(0.25, 0.5, 0.75), type = 2
  )

  exp_lower <- vapply(split_quantile_2, function(q) q[[1]], numeric(1))
  exp_middle <- vapply(split_quantile_2, function(q) q[[2]], numeric(1))
  exp_upper <- vapply(split_quantile_2, function(q) q[[3]], numeric(1))

  expect_equal(d$lower, as.numeric(exp_lower))
  expect_equal(d$middle, as.numeric(exp_middle))
  expect_equal(d$upper, as.numeric(exp_upper))
})

test_that("StatBoxplotQuantile differs from default type = 7", {
  df <- data.frame(x = 1, y = 1:8)

  res <- junco:::StatBoxplotQuantile$compute_group(
    data = df, scales = NULL, width = NULL, na.rm = FALSE, coef = 1.5
  )

  q2 <- stats::quantile(1:8, probs = c(0.25, 0.5, 0.75), type = 2)
  q7 <- stats::quantile(1:8, probs = c(0.25, 0.5, 0.75), type = 7) #default R

  # sanity check matches type=2
  expect_equal(c(res$lower, res$middle, res$upper), as.numeric(q2))

  #doesn't match type=7
  expect_true(any(as.numeric(q2) != as.numeric(q7)))
})

test_that("geom_boxplot_j differs from default type = 7", {

  df <- data.frame(g = factor("a"), y = 1:8)

  p <- ggplot2::ggplot(df, ggplot2::aes(g, y)) + geom_boxplot_j()
  built <- ggplot2::ggplot_build(p)
  d <- built$data[[1]]

  q2 <- stats::quantile(df$y, probs = c(0.25, 0.5, 0.75), type = 2)
  q7 <- stats::quantile(df$y, probs = c(0.25, 0.5, 0.75), type = 7) #default R

  # sanity check matches type=2
  expect_equal(c(d$lower, d$middle, d$upper), as.numeric(q2))

  #doesn't match type=7
  expect_true(any(as.numeric(q2) != as.numeric(q7)))
})

test_that("StatBoxplotQuantile supports custom quantile_type (type = 7)", {
  df <- data.frame(x = 1, y = 1:9)

  res <- junco:::StatBoxplotQuantile$compute_group(
    data = df, scales = NULL, width = NULL, na.rm = FALSE, coef = 1.5,
    quantile_type = 7
  )

  q7_full <- stats::quantile(df$y, probs = c(0, 0.25, 0.5, 0.75, 1), type = 7)
  iqr7 <- q7_full[4] - q7_full[2]
  lf7 <- q7_full[2] - 1.5 * iqr7
  uf7 <- q7_full[4] + 1.5 * iqr7
  ymin_exp <- min(df$y[df$y >= lf7])
  ymax_exp <- max(df$y[df$y <= uf7])

  expect_equal(res$lower, q7_full[[2]])
  expect_equal(res$middle, q7_full[[3]])
  expect_equal(res$upper, q7_full[[4]])
  expect_equal(res$ymin, ymin_exp)
  expect_equal(res$ymax, ymax_exp)
})

test_that("geom_boxplot_j supports custom quantile_type (type = 7)", {
  df <- data.frame(g = factor(rep(c("a", "b"), each = 8)),
                   y = c(1:8, 11:18))

  p <- ggplot2::ggplot(df, ggplot2::aes(g, y)) + geom_boxplot_j(quantile_type = 7)
  built <- ggplot2::ggplot_build(p)
  d <- built$data[[1]]

  split_q7 <- lapply(
    split(df$y, df$g),
    stats::quantile,
    probs = c(0.25, 0.5, 0.75), type = 7
  )
  exp_lower <- vapply(split_q7, function(q) q[[1]], numeric(1))
  exp_middle <- vapply(split_q7, function(q) q[[2]], numeric(1))
  exp_upper <- vapply(split_q7, function(q) q[[3]], numeric(1))

  expect_equal(d$lower, as.numeric(exp_lower))
  expect_equal(d$middle, as.numeric(exp_middle))
  expect_equal(d$upper, as.numeric(exp_upper))
})
