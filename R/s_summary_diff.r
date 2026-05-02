#' @title Descriptive Statistics for Univariate Numerical (Quantitative) Data.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A statistical function for descriptive statistics for numerical data. It uses
#' [tern:::s_summary.numeric()] and [s_diff_mean_ci()]. It computes the values of
#' statistics for a univariate data values in `df[[.var]]`.
#' It can also compute the statistics for two univariate data values (for example
#' difference in means): `df[[.var]]` and the reference group `.ref_group[[.var]]`.
#'
#' @inheritParams proposal_argument_convention
#' @param .var (`character(1)`)\cr The name of the column in `df`. Contains
#'   data values for which that statistic are computed.
#' @param .stats (`character` or `NULL`)\cr names of the statistics for be computed.
#'   The list of names of supported statistics is given by
#'   [tern::get_stats(method_groups = "analyze_vars_numeric", custom_stats_in  = "diff_mean_ci")].
#'   If `NULL` then all the stats are computed.
#' @param control (`list`)\cr relevant list of control options.
#'   Passed to [tern:::s_summary.numeric()].
#'   If `diff_mean_ci` is computed, it must containt an element named
#'   `conf_level` with the specification of the confidence level of the interval.
#' @param ... Additional arguments passed to [tern:::s_summary.numeric()] and to
#'   [junco::s_diff_mean_ci()], if `diff_mean_ci` is computed.
#'
#' @importFrom tern s_summary get_stats
#' @returns `list` with computed values of chosen statistics.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#'   TRT01A = rep("ARM_A", 5),
#'   PARAMCD = rep("SYSBP", 5),
#'   AVISIT = rep("Visit 1", 5),
#'   CHG = c(4, 1, -1, 9, -2)
#' )
#' .ref_group <- data.frame(
#'   USUBJID = c("X06", "X07", "X08", "X09", "X10"),
#'   TRT01A = rep("Placebo", 5),
#'   PARAMCD = rep("SYSBP", 5),
#'   AVISIT = rep("Visit 1", 5),
#'   CHG = c(-2, 6, -2, 5, 2)
#' )
#'
#' s_summary_diff(df, "CHG", c("n", "mean_sd", "diff_mean_ci"), .ref_group)
#' s_summary_diff(df, "CHG", c("n", "mean_sd", "diff_mean_ci"), df, .in_ref_col = TRUE)
#'
s_summary_diff <- function(df,
                           .var,
                           .stats = NULL,
                           .ref_group,
                           .in_ref_col = FALSE,
                           control = tern::control_analyze_vars(),
                           ...) {
  checkmate::assert_data_frame(df, null.ok = FALSE)
  checkmate::assert_string(.var)
  checkmate::assert_choice(.var, choices = colnames(df))
  checkmate::assert_character(.stats, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  checkmate::assert_flag(.in_ref_col, null.ok = FALSE)
  checkmate::assert_list(control, null.ok = FALSE)
  checkmate::assert_choice("conf_level", choices = names(control))

  if (is.null(.stats)) {
    .stats <- c(tern::get_stats(), "diff_mean_ci")
  }

  y <- tern::s_summary(df[[.var]], control = control)

  if ("diff_mean_ci" %in% .stats) {
    dm <- if (!.in_ref_col) {
      s_diff_mean_ci(df, .ref_group, .var, conf.level = control$conf_level, ...)
    } else {
      list(diff_mean_ci = NULL)
    }
    y <- c(y, dm)
  }

  y[.stats]
}
