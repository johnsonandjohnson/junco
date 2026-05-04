#' @title Difference in Means with Confidence Interval
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Computes the difference in means between two samples along with a confidence
#' interval. The interval is computed using a t-distribution framework via
#' [safe_t_test()].
#'
#' Supports both independent and paired samples. For paired data, observations
#' are matched using `paired_by`, and the inference is based on within-pair
#' differences using a paired t-distribution framework.
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
#' A named `list` with a single element `diff_mean_ci`, containing the difference
#' in means and confidence interval estimates.
#'
#' @details
#' The first sample is taken from `df1[[.var]]` and the second from `df2[[.var]]`.
#'
#' If `paired = TRUE`, observations are matched using `paired_by`. In this case,
#' the difference in means and its confidence interval are computed using a
#' t-statistic for paired data (based on within-pair differences). Otherwise,
#' a t-statistic for two independent samples is used.
#'
#' Any `NA` or `NaN` values in columns specified by `paired_by` are ignored and
#' excluded from matching (see `merge(..., incomparables = c(NA, NaN))`).
#'
#' When `paired = TRUE`, only complete pairs are passed to [safe_t_test()]
#' (i.e., rows with missing values in `.var` are removed prior to computation).
#' For unpaired cases, missing values are removed separately from each sample
#' before computation.
#'
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
#'   CHG = c(-2, 4, -2, 5, 2)
#' )
#'
#' # Paired
#' s_diff_mean_ci(df1, df2, "CHG", paired = TRUE, paired_by = "USUBJID")
#'
#' # Unpaired
#' s_diff_mean_ci(df1, df2, "CHG")
#'
s_diff_mean_ci <- function(df1,
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
    checkmate::assert_subset(paired_by, colnames(df1), empty.ok = FALSE)
    checkmate::assert_subset(paired_by, colnames(df2), empty.ok = FALSE)
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

    cols_out <- c(paste0(.var, "_df1"), paste0(.var, "_df2"))
    df <- df[complete.cases(df[, cols_out]), ]

    x1 <- df[[cols_out[1]]]
    x2 <- df[[cols_out[2]]]
  } else {
    x1 <- df1[[.var]]
    x2 <- df2[[.var]]

    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
  }

  ttest_res <- safe_t_test(x1, x2, paired = paired, conf.level = conf.level, ...)
  est <- ttest_res$estimate
  ci <- ttest_res$conf

  pest <- if (paired) {
    est
  } else {
    est[1] - est[2]
  }

  names(pest) <- "diff_mean"
  names(ci) <- c("ci_lwr", "ci_upr")
  estimates <- c(pest, ci)

  label <- paste("Difference in Means +", tern::f_conf_level(conf.level))
  list(diff_mean_ci = formatters::with_label(estimates, label))
}
