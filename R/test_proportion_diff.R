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
#' @param method (`string`)\cr one of `chisq`, `cmh`, `fisher`; specifies the test used
#'   to calculate the p-value.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#' @seealso [h_prop_diff_test]
#'
#' @note These functions have been forked from the `tern` package. Additional features are:
#'
#'   * Additional `alternative` argument for the sidedness of the test.
#'   * Additional `ref_path` argument for flexible reference column path specification.
#'
#' @name prop_diff_test
#' @order 1
NULL

#' @describeIn prop_diff_test Statistics function which tests the difference between two proportions.
#'
#' @return
#' * `s_test_proportion_diff()` returns a named `list` with a single item `pval` with an attribute `label`
#'   describing the method used. The p-value tests the null hypothesis that proportions in two groups are the same.
#'
#' @keywords internal
s_test_proportion_diff <- function(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  method = c("chisq", "fisher", "cmh"),
  alternative = c("two.sided", "less", "greater")
) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  y <- list(pval = list())

  if (!.in_ref_col) {
    assert_df_with_variables(df, list(rsp = .var))
    assert_df_with_variables(.ref_group, list(rsp = .var))
    rsp <- factor(c(.ref_group[[.var]], df[[.var]]), levels = c("TRUE", "FALSE"))
    grp <- factor(rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "Not-ref"))

    if (!is.null(variables$strata) || method == "cmh") {
      strata <- variables$strata
      checkmate::assert_false(is.null(strata))
      strata_vars <- stats::setNames(as.list(strata), strata)
      assert_df_with_variables(df, strata_vars)
      assert_df_with_variables(.ref_group, strata_vars)
      strata <- c(interaction(.ref_group[strata]), interaction(df[strata]))
    }

    tbl <- switch(method, cmh = table(grp, rsp, strata), table(grp, rsp))

    y$pval <- switch(
      method,
      chisq = prop_chisq(tbl, alternative),
      cmh = prop_cmh(tbl, alternative),
      fisher = prop_fisher(tbl, alternative)
    )
  }

  y$pval <- with_label(y$pval, d_test_proportion_diff(method, alternative))
  y
}

#' Description of the difference test between two proportions
#'
#' @description `r lifecycle::badge('stable')`
#'
#' This is an auxiliary function that describes the analysis in `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#'
#' @return A `string` describing the test from which the p-value is derived.
#'
#' @export
d_test_proportion_diff <- function(method, alternative) {
  checkmate::assert_string(method)
  meth_part <- switch(
    method,
    chisq = "Chi-Squared Test",
    cmh = "Cochran-Mantel-Haenszel Test",
    fisher = "Fisher's Exact Test",
    stop(paste(method, "does not have a description"))
  )
  alt_part <- switch(
    alternative,
    two.sided = "",
    less = ", 1-sided, direction less",
    greater = ", 1-sided, direction greater"
  )
  paste0("p-value (", meth_part, alt_part, ")")
}

#' @describeIn prop_diff_test Formatted analysis function which is used as `afun`
#'
#' @return
#' * `a_test_proportion_diff()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c('A', 'B'), each = 50)),
#'   strata = factor(rep(c('V', 'W', 'X', 'Y', 'Z'), each = 20))
#' )
#'
#' # With `rtables` pipelines.
#' l <- basic_table() |>
#'   split_cols_by(var = 'grp') |>
#'   analyze(
#'     vars = 'rsp',
#'     afun = a_test_proportion_diff,
#'     show_labels = 'hidden',
#'     extra_args = list(
#'       method = 'cmh',
#'       variables = list(strata = 'strata'),
#'       ref_path = c('grp', 'B')
#'     )
#'   )
#'
#' build_table(l, df = dta)
#'
#' @export
#' @order 2
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

#' Helper functions to test proportion differences
#'
#' Helper functions to implement various tests on the difference between two proportions.
#'
#' @param tbl (`matrix`)\cr matrix with two groups in rows and the binary response (`TRUE`/`FALSE`) in columns.
#'
#' @return A p-value.
#'
#' @seealso [prop_diff_test()] for implementation of these helper functions.
#'
#' @name h_prop_diff_test
NULL

#' @describeIn h_prop_diff_test Performs Chi-Squared test. Internally calls [stats::prop.test()].
#'
#' @keywords internal
prop_chisq <- function(tbl, alternative) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  if (any(colSums(tbl) == 0)) {
    return(1)
  }
  stats::prop.test(tbl, correct = FALSE, alternative = alternative)$p.value
}

#' @describeIn h_prop_diff_test Performs stratified Cochran-Mantel-Haenszel test.
#'  Internally calls [stats::mantelhaen.test()].
#'
#' @note strata with less than five observations will result in a warning and
#'  possibly incorrect results; strata with less than two observations are
#'  automatically discarded.
#'
#' @param ary (`array`, 3 dimensions)\cr array with two groups in rows, the binary response
#'   (`TRUE`/`FALSE`) in columns, and the strata in the third dimension.
#'
#' @keywords internal
prop_cmh <- function(ary, alternative) {
  checkmate::assert_array(ary)
  checkmate::assert_integer(c(ncol(ary), nrow(ary)), lower = 2, upper = 2)
  checkmate::assert_integer(length(dim(ary)), lower = 3, upper = 3)
  strata_sizes <- apply(ary, MARGIN = 3, sum)
  if (any(strata_sizes < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
    ary <- ary[, , strata_sizes > 1]
  }
  stats::mantelhaen.test(ary, correct = FALSE, alternative = alternative)$p.value
}

#' @describeIn h_prop_diff_test Performs the Fisher's exact test. Internally calls [stats::fisher.test()].
#'
#' @keywords internal
prop_fisher <- function(tbl, alternative) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  stats::fisher.test(tbl, alternative = alternative)$p.value
}
