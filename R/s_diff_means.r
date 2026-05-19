#' @title Difference in Means Statistics
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes estimates and associated inferential statistics for the difference
#' in population means between two underlying distributions.
#'
#' Supports both non-paired and paired samples. For non-paired samples, the
#' estimator is defined as the difference between sample means. For paired
#' samples, the estimator is defined as the mean of within-pair differences.
#'
#' Inferential statistics, including standard errors and confidence intervals,
#' are obtained using a t-distribution framework via the internal
#' `safe_t_test()` function.
#'
#' Only complete (non-missing) observations are used in statistical computations.
#' Non-finite values (i.e., `Inf`, `-Inf`) are not allowed.
#'
#' The following quantities are computed:
#' \describe{
#'   \item{diff_means_n1}{Sample size of Group 1.}
#'   \item{diff_means_n2}{Sample size of Group 2.}
#'   \item{diff_means_est}{Point estimate of the difference in population means.}
#'   \item{diff_means_se}{Standard error of the estimator of the difference in population means.}
#'   \item{diff_means_est_se}{The point estimate and the standard error.}
#'   \item{diff_means_ci}{Confidence interval for the difference in population means.}
#'   \item{diff_means_est_ci}{The point estimate and the confidence interval.}
#' }
#'
#' @details
#' The first sample is taken from `df1[[.var]]` and the second from `df2[[.var]]`.
#'
#' If `paired = FALSE`, missing values are removed separately from each sample
#' prior to statistical computations.
#'
#' If `paired = TRUE`, observations are matched using `paired_by` prior to
#' statistical analysis, and only complete pairs are used.
#'
#' Data extraction, alignment, and missing-value handling are delegated to the
#' internal `extract_vectors()` function, which prepares cleaned numeric
#' vectors for both paired and unpaired settings.
#'
#' Inferential statistics are then computed using `safe_t_test()` applied to
#' the processed vectors.
#'
#' @param df1 (`data.frame`)\cr Dataset for the first sample.
#' @param df2 (`data.frame`)\cr Dataset for the second sample.
#' @param .var (`character(1)`)\cr Column name in `df1` and `df2` containing
#'   numeric values.
#' @param paired (`logical(1)`)\cr Whether the samples are paired.
#' @param paired_by (`character` or `NULL`)\cr Column name(s) in `df1` and `df2`
#'   used to match observations between datasets. Required when `paired = TRUE`
#'   and must uniquely identify each pair in both datasets.
#' @param ... Additional named arguments passed to `safe_t_test()`.
#'
#' @return
#' A named `list` containing the quantities described in the Description section.
#'
#' @seealso `safe_t_test()`, `extract_vectors()`
#'
#' @importFrom tern f_conf_level
#' @importFrom formatters with_label
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
#' df2 <- data.frame(id = c("A", "C", "D", "E", "F"), value = c(3:1, NA, 16))
#' df1
#' df2
#'
#' # Unpaired
#' s_diff_means(df1, df2, "value", conf.level = .8)
#'
#' # Paired
#' s_diff_means(df1, df2, "value", paired = TRUE, paired_by = "id")
#'
s_diff_means <- function(df1,
                         df2,
                         .var,
                         paired = FALSE,
                         paired_by = NULL,
                         ...) {
  checkmate::assert_data_frame(df1)
  checkmate::assert_data_frame(df2)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df1), must.include = .var)
  checkmate::assert_names(colnames(df2), must.include = .var)
  checkmate::assert_numeric(df1[[.var]], finite = TRUE)
  checkmate::assert_numeric(df2[[.var]], finite = TRUE)
  checkmate::assert_flag(paired, null.ok = FALSE)
  if (paired) {
    checkmate::assert_character(paired_by)
    checkmate::assert_names(colnames(df1), must.include = paired_by)
    checkmate::assert_names(colnames(df2), must.include = paired_by)
  }

  vecs <- extract_vectors(df1, df2, .var, paired = paired, paired_by = paired_by)
  n1 <- length(vecs$x1)
  n2 <- length(vecs$x2)

  # Get estimates.
  ttest_res <- safe_t_test(vecs$x1, vecs$x2, paired = paired, ...)
  est <- if (paired) {
    ttest_res$estimate
  } else {
    ttest_res$estimate[1] - ttest_res$estimate[2]
  }
  ci <- ttest_res$conf.int
  se <- ttest_res$stderr
  conf.level <- attr(ci, "conf.level")

  names(n1) <- "diff_means_n1"
  names(n2) <- "diff_means_n2"
  names(est) <- "diff_means_est"
  names(ci) <- c("diff_means_ci_lwr", "diff_means_ci_upr")
  names(se) <- "diff_means_se"
  est_ci <- c(est, ci)
  attr(est_ci, "conf.level") <- conf.level

  label <- "Difference in Means"
  cl <- tern::f_conf_level(conf.level)

  y <- list()
  y$diff_means_n1 <- formatters::with_label(n1, paste(label, "Sample Size (Group 1)"))
  y$diff_means_n2 <- formatters::with_label(n2, paste(label, "Sample Size (Group 2)"))
  y$diff_means_est <- formatters::with_label(est, paste(label, "Estimate"))
  y$diff_means_se <- formatters::with_label(se, paste(label, "SE"))
  y$diff_means_est_se <- formatters::with_label(c(est, se), paste(label, "Estimate + SE"))
  y$diff_means_ci <- formatters::with_label(ci, paste(label, cl))
  y$diff_means_est_ci <- formatters::with_label(est_ci, paste(label, "Estimate +", cl))

  y
}
