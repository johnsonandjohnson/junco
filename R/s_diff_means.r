#' @title Difference in Means Statistics
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Computes estimates and associated inferential statistics for the difference
#' in population means between two underlying distributions.
#'
#' Supports both non-paired and paired samples. For non-paired samples, the
#' estimator is defined as the difference between sample means. For paired
#' samples, the estimator is defined as the mean of within-pair differences.
#'
#' Inferential statistics, including standard errors and confidence intervals,
#' are obtained using a t-distribution framework via [safe_t_test()].
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
#' If `paired = TRUE`, observations are matched using `paired_by`, and all
#' statistics are computed on within-pair differences (i.e., differences between
#' matched observations).
#'
#' Moreover, any `NA` or `NaN` values in columns specified by `paired_by` are
#' excluded from matching (see `merge(..., incomparables = c(NA, NaN))`).
#'
#' For paired samples, only complete pairs are passed to [safe_t_test()].
#' For non-paired samples, missing values are removed separately from each
#' sample prior to passing them to [safe_t_test()].
#'
#' @param df1 (`data.frame`)\cr Dataset for the first sample.
#' @param df2 (`data.frame`)\cr Dataset for the second sample.
#' @param .var (`character(1)`)\cr Column name in `df1` and `df2` containing
#'   numeric values.
#' @param paired (`logical(1)`)\cr Whether the samples are paired.
#' @param paired_by (`character` or `NULL`)\cr Column name(s) in `df1` and `df2`
#'   used to match observations between datasets. Required when `paired = TRUE`
#'   and must uniquely identify each pair in both datasets.
#' @param conf.level (`proportion`)\cr Confidence level for the interval.
#' @param ... Additional arguments passed to [safe_t_test()].
#'
#' @return
#' A named `list` containing the quantities described in the Description section.
#'
#' @importFrom stats complete.cases
#' @importFrom tern f_conf_level
#' @importFrom formatters with_label
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(
#'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#'   CHG = c(4, 1, -1, 9, -2)
#' )
#' df2 <- data.frame(
#'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#'   CHG = c(-2, 4, NA, 5, 2)
#' )
#'
#' # Paired
#' s_diff_means(df1, df2, "CHG", paired = TRUE, paired_by = "USUBJID")
#'
#' # Unpaired
#' s_diff_means(df1, df2, "CHG")
#'
s_diff_means <- function(df1,
                         df2,
                         .var,
                         paired = FALSE,
                         paired_by = NULL,
                         conf.level = 0.95,
                         ...) {
  checkmate::assert_data_frame(df1, null.ok = FALSE)
  checkmate::assert_data_frame(df2, null.ok = FALSE)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df1), must.include = .var)
  checkmate::assert_names(colnames(df2), must.include = .var)
  checkmate::assert_flag(paired, null.ok = FALSE)

  if (paired) {
    checkmate::assert_character(paired_by, null.ok = FALSE)
    checkmate::assert_names(colnames(df1), must.include = paired_by)
    checkmate::assert_names(colnames(df2), must.include = paired_by)
    if (any(duplicated(df1[, paired_by]))) {
      stop("df1: 'paired_by' must uniquely identify rows.")
    }
    if (any(duplicated(df2[, paired_by]))) {
      stop("df2: 'paired_by' must uniquely identify rows.")
    }

    pvcols <- c(paired_by, .var)
    df <- merge(
      df1[, pvcols, drop = FALSE],
      df2[, pvcols, drop = FALSE],
      by = paired_by,
      suffixes = c("_df1", "_df2"),
      incomparables = c(NA, NaN)
    )

    cols_var <- c(paste0(.var, "_df1"), paste0(.var, "_df2"))
    df <- df[complete.cases(df[, cols_var]), ]

    x1 <- df[[cols_var[1]]]
    x2 <- df[[cols_var[2]]]
  } else {
    x1 <- df1[[.var]]
    x2 <- df2[[.var]]

    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
  }

  n1 <- length(x1)
  n2 <- length(x2)
  ttest_res <- safe_t_test(x1, x2, paired = paired, conf.level = conf.level, ...)
  est <- if (paired) {
    ttest_res$estimate
  } else {
    ttest_res$estimate[1] - ttest_res$estimate[2]
  }
  ci <- ttest_res$conf
  se <- ttest_res$stderr

  names(n1) <- "diff_means_n1"
  names(n2) <- "diff_means_n2"
  names(est) <- "diff_means_est"
  names(ci) <- c("diff_means_ci_lwr", "diff_means_ci_upr")
  names(se) <- "diff_means_se"
  est_ci <- c(est, ci)
  attr(est_ci, "conf.level") <- attr(ci, "conf.level")

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
