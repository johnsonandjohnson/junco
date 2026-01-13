#' Difference test for two proportions
#'
#' @description `r lifecycle::badge('stable')`
#'
#' The analysis function [a_test_proportion_diff()] can be used to create a layout element to test
#' the difference between two proportions. The primary analysis variable, `vars`, indicates whether a
#' response has occurred for each record. See the `method` parameter for options of methods to use
#' to calculate the p-value. Additionally, a stratification variable can be supplied via the `strata`
#' element of the `variables` argument. The argument `alternative` specifies the direction of the
#' alternative hypothesis.
#'
#' @inheritParams proposal_argument_convention
#' @param ... Additional arguments passed to [tern::s_test_proportion_diff()], including:
#'   * `method` (`string`)\cr one of `chisq`, `cmh`, `cmh_wh`, `fisher` or `schouten`;
#'     specifies the test used to calculate the p-value.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#' @note This function has been forked from the `tern` package. Additional features are:
#'
#'   * Additional `ref_path` argument for flexible reference column path specification.
#'
#' @name prop_diff_test
#' @order 1
NULL

#' @describeIn prop_diff_test Formatted analysis function which is used as `afun`
#'
#' @return
#' * `a_test_proportion_diff()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50)),
#'   strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' )
#'
#' l <- basic_table() |>
#'   split_cols_by(var = "grp") |>
#'   analyze(
#'     vars = "rsp",
#'     afun = a_test_proportion_diff,
#'     show_labels = "hidden",
#'     extra_args = list(
#'       method = "cmh",
#'       variables = list(strata = "strata"),
#'       ref_path = c("grp", "B")
#'     )
#'   )
#'
#' build_table(l, df = dta)
#'
#' @export
#' @order 1
a_test_proportion_diff <- function(
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
    default_stat_fnc = s_test_proportion_diff,
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
    method_groups = "test_proportion_diff",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
