#' @title Calculated Difference in Means with the Confidence-Interval.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A statistical function for differences in mean point and interval estimates
#' for two samples. The interval estimates are obtained from the t-statistic
#' (see [safe_t_test]).
#' The first sample is `df2[[.var]]`, while the second, `df2[[.var]]`.
#' The key for paired samples is specified by `paired_by`.
#' The fact whethwet the samples are paired or not impacts only the way the
#' confinde interval estaimtes are computed, that is, for paired samples,
#' t-statistic for paired data is used.
#'
#' @param df1 (`data.frame`)\cr A dataset containing data values of the first sample.
#' @param df2 (`data.frame`)\cr A dataset containing data values of the second sample.
#' @param .var (`character(1)`)\cr The name of the column in `df1`, `df2`, containing the data values.
#' @param paired (`logical(1)`)\cr Are the samples paired?
#' @param paired_by (`character` or `NULL`)\cr Column names in `df1` and `df2`.
#'   Variables whose (unique) values define a single pair
#'   It is used if and only if `paired` is `TRUE`, and in this case, if must be
#'   not `NULL`.
#' @param conf.level (`proportion`)\cr Confidence level of the interval.
#' @param ... Further arguments to be passed to [safe_t_test()].
#'
#' @returns `list` with `diff_mean_ci` statistic.
#' @export
#'
#' @examples
#' df1 <- data.frame(
#'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#'   TRT01A = rep("ARM_A", 5),
#'   PARAMCD = rep("SYSBP", 5),
#'   AVISIT = rep("Visit 1", 5),
#'   CHG = c(4, 1, -1, 9, -2)
#' )
#' df2 <- data.frame(
#'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#'   TRT01A = rep("Placebo", 5),
#'   PARAMCD = rep("SYSBP", 5),
#'   AVISIT = rep("Visit 1", 5),
#'   CHG = c(-2, 6, -2, 5, 2)
#' )
#'
#' s_diff_mean_ci(df1, df2, "CHG", paired = TRUE, paired_by = "USUBJID")
#' s_diff_mean_ci(df1, df2, "CHG") # notice different CI estimates.
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
  checkmate::assert_choice(.var, choices = colnames(df1))
  checkmate::assert_choice(.var, choices = colnames(df2))
  checkmate::assert_flag(paired, null.ok = FALSE)

  if (paired) {
    checkmate::assert_character(paired_by, null.ok = FALSE)
    checkmate::assert_subset(paired_by, colnames(df1), empty.ok = FALSE)
    checkmate::assert_subset(paired_by, colnames(df2), empty.ok = FALSE)
    if (any(duplicated(df1[, paired_by])) || any(duplicated(df2[, paired_by]))) {
      stop("df1/df2 rows for paired_by columns must be unique.")
    }

    df1_pv <- df1[, c(paired_by, .var), drop = FALSE]
    df2_pv <- df2[, c(paired_by, .var), drop = FALSE]
    df <- merge(df1_pv, df2_pv, by = paired_by, suffixes = c("_df1", "_df2"))
    x1 <- df[[paste0(.var, "_df1")]]
    x2 <- df[[paste0(.var, "_df2")]]
  } else {
    x1 <- df1[[.var]]
    x2 <- df2[[.var]]
  }
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]

  ttest_res <- safe_t_test(x1, x2, paired = paired, conf.level = conf.level, ...)
  stat_diff <- ifelse(paired, ttest_res$estimate, ttest_res$estimate[1] - ttest_res$estimate[2])
  stats <- c(diff_mean = stat_diff, setNames(ttest_res$conf.int, c("ci_lwr", "ci_upr")))

  list(
    diff_mean_ci = with_label(
      stats,
      paste("Difference in Means +", tern::f_conf_level(conf.level))
    )
  )
}
