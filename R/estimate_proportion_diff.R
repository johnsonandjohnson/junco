#' @noRd
#'
#' @title Helper Function to Prepare Data for Proportion Analyses
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Prepares response, group, and optional strata vectors for use in
#' proportion-based analyses. The function extracts the variables from an
#' analysis dataset and, optionally, a reference dataset, and combines them
#' into vectors suitable for downstream statistical functions provided by the
#' [tern::h_prop_diff] family.
#'
#' @param df (`data.frame`)\cr
#'   A data frame containing the observations for the non-reference group.
#' @param df_ref (`data.frame` or `NULL`)\cr
#'   An optional data frame containing the observations for the reference group.
#' @param var (`character(1)`)\cr
#'   The column name in `df` (and, if supplied, `df_ref`) specifying the
#'   response variable. The response is converted to a logical vector by
#'   comparing its values with `val`.
#' @param val (`character(1)` or `logical(1)`)\cr
#'   The value of `df[[var]]` (and, if supplied, `df_ref[[var]]`) that defines
#'   a positive response. Observations matching this value are returned as
#'   `TRUE` in the `rsp` vector; all other observations are returned as `FALSE`.
#' @param strata_vars (`character` or `NULL`)\cr
#'   Optional column names in `df` (and, if supplied, `df_ref`) specifying the
#'   strata variables. The specified columns must all be factors.
#' @param complete_cases (`logical(1)`)\cr
#'   Whether incomplete observations should be removed from
#'   `df[c(var, strata_vars)]` (and, if supplied, `df_ref[c(var, strata_vars)]`).
#'   This is done using [get_complete_cases()].
#' @param quiet (`logical(1)`)\cr
#'   Passed to [get_complete_cases()], controlling whether messages about
#'   removed incomplete observations are displayed.
#'
#' @return A named `list` containing:
#' \describe{
#'   \item{rsp}{A logical vector indicating whether each observation has the
#'   value specified by `val` in `df[[var]]` (and, if supplied, `df_ref`).}
#'   \item{grp}{A factor identifying the group of each observation. The levels
#'   are always `"ref"` and `"Not-ref"` (in this order), corresponding to
#'   observations from `df_ref` and `df`, respectively. If `df_ref` is `NULL`,
#'   all observations belong to `"Not-ref"`.}
#'   \item{strata}{A factor defining the analysis strata when `strata_vars` is
#'   supplied, or `NULL` otherwise. When multiple stratification variables are
#'   supplied, their combinations, using [interaction()], are used to define
#'   the strata.}
#' }
#'
#' @details
#' The function centralizes the preparation of response, group, and optional
#' strata vectors required by proportion-based analyses in the `tern` package.
#' See the proportion difference documentation [tern::h_prop_diff] for related
#' functions.
#'
#' If `complete_cases = TRUE`, incomplete observations are removed separately
#' from `df` and `df_ref` (if supplied) before the vectors are constructed.
#' Completeness is assessed jointly across the `var` and `strata_vars` columns
#' when `strata_vars` is supplied, and only across `var` otherwise. This is
#' performed using [get_complete_cases()].
#'
#' The response variable specified by `var`, and optionally the strata variables
#' specified by `strata_vars`, are extracted independently from `df` and
#' `df_ref` (if supplied). The response vectors are then combined into a single
#' vector and converted to a logical vector by comparing each value with `val`,
#' such that observations matching `val` are `TRUE` and all other observations
#' are `FALSE`.
#'
#' When multiple stratification variables are provided, their combinations are
#' collapsed into a single factor using [interaction()]. This is done
#' independently for `df` and `df_ref` (if supplied), after which the resulting
#' strata vectors are combined into a single factor.
#'
#' A group factor is constructed to identify the source of each observation.
#' Observations from `df` are assigned to the `"Not-ref"` group, while
#' observations from `df_ref` are assigned to the `"ref"` group. The factor
#' always has `"ref"` and `"Not-ref"` as its levels, in this order.
#' The level order is important because proportion-difference calculations in
#' `tern` use the first level as the reference group and calculate the
#' difference as `"Not-ref"` - `"ref"`.
#'
#' The function returns a named list containing the three resulting vectors:
#' `rsp`, `grp`, and `strata`.
#'
#' @seealso [tern::h_prop_diff]
#' @author WW
#' @keywords internal
#'
#' @examples
#'
#' set.seed(123)
#' n <- 28
#' dta <- data.frame(
#'   "rsp" = sample(c("Y", "N"), n, TRUE),
#'   "grp" = sample(c("X", "Placebo"), n, TRUE),
#'   "f1" = sample(c("a1", "a2"), n, TRUE),
#'   "f2" = sample(c("x", "y"), n, TRUE),
#'   stringsAsFactors = TRUE
#' )
#' head(dta)
#'
#' rgs <- h_prepare_prop_data(
#'   df = subset(dta, grp == "X"),
#'   df_ref = subset(dta, grp == "Placebo"),
#'   var = "rsp",
#'   val = "Y",
#'   strata_vars = c("f1", "f2")
#' )
#'
#' rbind(
#'   subset(dta, grp == "X"),
#'   subset(dta, grp == "Placebo"),
#'   make.row.names = FALSE
#' )
#'
#' rgs$rsp
#' rgs$grp
#' rgs$strata
#'
#' table(rgs$grp, rgs$rsp, rgs$strata)
#' tern::prop_diff_cmh(rgs$rsp, rgs$grp, rgs$strata)
#'
h_prepare_prop_data <- function(df,
                                df_ref = NULL,
                                var,
                                val,
                                strata_vars = NULL,
                                complete_cases = FALSE,
                                quiet = FALSE) {
  checkmate::assert_data_frame(df)
  checkmate::assert_data_frame(df_ref, null.ok = TRUE)
  checkmate::assert_string(var)
  checkmate::assert_true(
    checkmate::test_string(val) || checkmate::test_flag(val)
  )
  checkmate::assert_subset(var, colnames(df), empty.ok = FALSE)
  if (!is.null(df_ref)) {
    checkmate::assert_subset(var, colnames(df_ref), empty.ok = FALSE)
  }
  if (!is.null(strata_vars)) {
    checkmate::assert_subset(strata_vars, colnames(df), empty.ok = FALSE)
    checkmate::assert_data_frame(df[strata_vars], types = "factor")
    if (!is.null(df_ref)) {
      checkmate::assert_subset(strata_vars, colnames(df_ref), empty.ok = FALSE)
      checkmate::assert_data_frame(df_ref[strata_vars], types = "factor")
    }
  }
  checkmate::assert_flag(complete_cases)
  checkmate::assert_flag(quiet)

  # Optionally remove incomplete cases.
  if (complete_cases) {
    vars <- c(var, strata_vars)
    df <- get_complete_cases(df[, vars, drop = FALSE], quiet = quiet)
    if (!is.null(df_ref)) {
      df_ref <- get_complete_cases(df_ref[, vars, drop = FALSE], quiet = quiet)
    }
  }

  # NOTE: The order of group levels is important and must not be changed.
  # `tern` proportion-difference functions use the first level as the
  # reference group and calculate the difference as "Not-ref" - "ref".
  grp_levels <- c("ref", "Not-ref")

  # Extract response, group and strata data for non-reference group.
  rsp <- df[[var]]
  grp <- factor(rep(grp_levels[2], nrow(df)), levels = grp_levels)
  strata <- if (!is.null(strata_vars)) {
    interaction(df[strata_vars])
  } else {
    NULL
  }

  # Add reference group data, if supplied.
  if (!is.null(df_ref)) {
    rsp <- c(rsp, df_ref[[var]])
    grp <- c(grp, factor(rep(grp_levels[1], nrow(df_ref)), levels = grp_levels))
    strata <- if (!is.null(strata_vars)) {
      c(strata, interaction(df_ref[strata_vars]))
    } else {
      NULL
    }
  }

  rsp_logical <- rsp == val

  list(
    rsp = rsp_logical,
    grp = grp,
    strata = strata
  )
}

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
#'
#' @name prop_diff
#' @order 1
#'
#' @note The [a_proportion_diff_j()] function has the `_j` suffix to distinguish it
#'   from [tern::a_proportion_diff()]. The functions here are a copy from the `tern` package
#'   with additional features:
#'
#'   * Additional statistic `diff_est_ci` is returned.
#'   * `ref_path` needs to be provided as extra argument to specify the control group column.
#'
NULL

#' @describeIn prop_diff Statistics function estimating the difference
#'   in terms of responder proportion.
#'
#' @return
#' * `s_proportion_diff_j()` returns a named list of elements `diff`,
#'    `diff_ci`, `diff_est_ci` and `diff_ci_3d`.
#'
#' @note When performing an unstratified analysis, methods `'cmh'`, `'cmh_sato'`, `'cmh_mn'`,
#'   `'strat_newcombe'`, and `'strat_newcombecc'` are not permitted.
#'
#' @examples
#'
#' s_proportion_diff_j(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   conf_level = 0.90,
#'   method = "ha"
#' )
#'
#' s_proportion_diff_j(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = c("f1", "f2")),
#'   conf_level = 0.90,
#'   method = "cmh"
#' )
#'
#' @export
#' @order 3
s_proportion_diff_j <- function(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c(
    "waldcc", "wald", "cmh", "cmh_sato", "cmh_mn", "ha",
    "newcombe", "newcombecc", "strat_newcombe", "strat_newcombecc",
    "cmh_sato", "cmh_mn", "uncond_exact_diff"
  ),
  weights_method = "cmh"
) {
  start <- s_proportion_diff(
    df = df,
    .var = .var,
    .ref_group = .ref_group,
    .in_ref_col = .in_ref_col,
    variables = variables,
    conf_level = conf_level,
    method = method,
    weights_method = weights_method
  )

  c(
    start,
    list(
      diff_est_ci = with_label(
        c(start$diff, start$diff_ci),
        paste0("% Difference (", f_conf_level(conf_level), ")")
      ),
      diff_ci_3d = with_label(
        c(start$diff, start$diff_ci),
        paste0("Relative Risk (", f_conf_level(conf_level), ")")
      )
    )
  )
}

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
