#' @title Descriptive Statistics for Univariate Data with Optional Reference Comparison
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Computes descriptive statistics for a single variable `df[[.var]]` using
#' [tern::s_summary()], which dispatches type-specific methods depending on the
#' S3 class of the input (e.g., character, factor, logical, numeric).
#'
#' Optionally, it computes a difference in means with confidence interval
#' between `df[[.var]]` and `.ref_group[[.var]]` using [s_diff_mean_ci()].
#' This statistic is applicable only to numeric variables.
#'
#' @inheritParams proposal_argument_convention
#' @param .var (`character(1)`)\cr Name of the column in `df` containing the
#'   values for which statistics are computed. The variable type is handled by
#'   the corresponding methods of [tern::s_summary()].
#'   The `diff_mean_ci` statistic is only valid when `df[[.var]]` is numeric.
#' @param .stats (`character` or `NULL`)\cr Names of statistics to be computed.
#'   For numerical data, supported statistics are listed via
#'   `tern::get_stats(method_groups = "analyze_vars_numeric", custom_stats_in = "diff_mean_ci")`.
#'   If `NULL`, all available statistics for numerical data are computed.
#' @param control (`list`)\cr List of control options passed to [tern::s_summary()].
#'   If \emph{diff_mean_ci} statistic is requested, `control$conf_level`
#'   specifies the confidence level used for the interval.
#' @param ... Additional arguments passed to [tern::s_summary()] and to
#'   [s_diff_mean_ci()] when \emph{diff_mean_ci} is computed.
#'
#' @importFrom tern control_analyze_vars s_summary get_stats
#'
#' @returns A named `list` with the requested statistics.
#'
#' @export

#' @examples
#' df <- data.frame(
#'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#'   TRT01A = rep("ARM_A", 5),
#'   PARAMCD = rep("SYSBP", 5),
#'   AVISIT = rep("Visit 1", 5),
#'   CHG = c(4, 1, -1, 9, -2)
#' )
#' df
#'
#' rg <- data.frame(
#'   USUBJID = c("X06", "X07", "X08", "X09", "X10"),
#'   TRT01A = rep("Placebo", 5),
#'   PARAMCD = rep("SYSBP", 5),
#'   AVISIT = rep("Visit 1", 5),
#'   CHG = c(-2, 6, -2, 5, 2)
#' )
#' rg
#'
#' .stats <- c("n", "mean_sd", "diff_mean_ci")
#'
#' # With reference group.
#' s_summary_diff(df, "CHG", .stats, rg)
#'
#' # Using df as reference.
#' s_summary_diff(df, "CHG", .stats, df, .in_ref_col = TRUE)
#'
s_summary_diff <- function(df,
                           .var,
                           .stats = NULL,
                           .ref_group = NULL,
                           .in_ref_col = FALSE,
                           control = tern::control_analyze_vars(),
                           ...) {
  checkmate::assert_data_frame(df, null.ok = FALSE)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df), must.include = .var)
  checkmate::assert_character(.stats, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  checkmate::assert_flag(.in_ref_col, null.ok = FALSE)
  checkmate::assert_list(control, null.ok = FALSE)

  if (is.null(.stats)) {
    .stats <- unique(c(tern::get_stats(), "diff_mean_ci"))
  }

  if ("diff_mean_ci" %in% .stats && !.in_ref_col) {
    if (!checkmate::test_data_frame(.ref_group, null.ok = FALSE)) {
      stop(".ref_group must be a data.frame when diff_mean_ci is computed and .in_ref_col is FALSE")
    }
    checkmate::assert_names(names(control), must.include = "conf_level")
  }

  y <- tern::s_summary(df[[.var]], control = control)

  if ("diff_mean_ci" %in% .stats) {
    dm <- if (.in_ref_col) {
      list(diff_mean_ci = NULL)
    } else {
      s_diff_mean_ci(df, .ref_group, .var, conf.level = control$conf_level, ...)
    }
    y <- c(y, dm)
  }

  y[.stats]
}
