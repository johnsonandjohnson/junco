#' Proportion difference estimation
#'
#' The analysis function [a_proportion_diff_j()] can be used to create a layout element to estimate
#' the difference in proportion of responders within a studied population. The primary analysis variable,
#' `vars`, is a logical variable indicating whether a response has occurred for each record. See the `method`
#' parameter for options of methods to use when constructing the confidence interval of the proportion difference.
#' A stratification variable can be supplied via the `strata` element of the `variables` argument.
#'
#' @param df (`data.frame`)\cr input data frame.
#' @param .var (`string`)\cr name of the response variable.
#' @param ref_path (`character`)\cr path to the reference group.
#' @param .spl_context (`environment`)\cr split context environment.
#' @param ... Additional arguments passed to the statistics function.
#' @param .stats (`character`)\cr statistics to calculate.
#' @param .formats (`list`)\cr formats for the statistics.
#' @param .labels (`list`)\cr labels for the statistics.
#' @param .indent_mods (`list`)\cr indentation modifications for the statistics.
#' @param .ref_group (`data.frame`)\cr reference group data frame.
#' @param .in_ref_col (`logical`)\cr whether the current column is the reference column.
#' @param variables (`list`)\cr list with strata variable names.
#' @param conf_level (`numeric`)\cr confidence level for the confidence interval.
#' @param method (`string`)\cr method to use for confidence interval calculation.
#' @param weights_method (`string`)\cr method to use for weights calculation in stratified analysis.
#' @param label (`string`)\cr label for the `diff_est_ci` statistic. Defaults to
#'   `"% Difference (conf_level CI)"`. Can be overridden by the caller.
#'
#' @name prop_diff
#' @order 1
#'
#' @note The [a_proportion_diff_j()] function has the `_j` suffix to distinguish it
#'   from [tern::a_proportion_diff()]. It is a thin wrapper around
#'   `tern::s_proportion_diff()` with the following additions:
#'
#'   * The `diff_est_ci` statistic (returned natively by tern since tern#1523) is
#'     relabelled with a shorter junco-style label (`"% Difference (conf_level CI)"`).
#'   * `ref_path` needs to be provided as extra argument to specify the control group column.
#'
NULL

#' @describeIn prop_diff Formatted analysis function which is used as `afun` in `estimate_proportion_diff()`.
#'
#' @return
#' * `a_proportion_diff_j()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' nex <- 100
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1" = sample(c("a1", "a2"), nex, TRUE),
#'   "f2" = sample(c("x", "y", "z"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' l <- basic_table() |>
#'   split_cols_by(var = "grp") |>
#'   analyze(
#'     vars = "rsp",
#'     afun = a_proportion_diff_j,
#'     show_labels = "hidden",
#'     na_str = tern::default_na_str(),
#'     extra_args = list(
#'       conf_level = 0.9,
#'       method = "ha",
#'       ref_path = c("grp", "B")
#'     )
#'   )
#'
#' build_table(l, df = dta)
#' @export
#' @order 2
a_proportion_diff_j <- function(
  df,
  .var,
  ref_path,
  .spl_context,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  # Obtain reference column information
  ref <- get_ref_info(ref_path, .spl_context)

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_proportion_diff_j,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      .var = .var,
      .ref_group = list(ref$ref_group),
      .in_ref_col = ref$in_ref_col,
      dots_extra_args
    )
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "proportion_diff",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
