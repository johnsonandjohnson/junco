#' @name a_summary_diff_mvars
#'
#' @title Descriptive Statistics for Multiple Univariate Variables with Optional Reference-Based Comparison
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' An analysis function for computing descriptive statistics for multiple
#' univariate variables. It uses [s_summary_diff()] to compute statistics
#' defined in `stats_vars$stat` for variables specified in `stats_vars$var`.
#' Essentially, each row in `stats_vars` defines one (variable, statistic) pair.
#' The output is ordered according to `stats_vars`.
#'
#' Optionally, statistics may be computed with respect to a reference group,
#' enabling differences in means (for numeric variables) via a reference dataset
#' derived from `ref_path` and `.spl_context`.
#'
#' @inheritParams proposal_argument_convention
#'
#' @param .var (`character(1)` or `NULL`)\cr Column name in `df` used only for
#'   row filtering when `na_rm_var = TRUE`. It does not define the variables
#'   for which statistics are computed (these are specified in `stats_vars$var`).
#'   If `na_rm_var = TRUE`, `.var` must be provided and exist in `df`;
#'   otherwise, it is ignored.
#'
#' @param .spl_context (`data.frame` or `NULL`)\cr Information about ancestor
#'   split states passed by **rtables**. It is ignored if `ref_path` is `NULL`.
#'
#' @param ref_path (`character` or `NULL`)\cr Global reference group specification,
#'   see [get_ref_info()]. It is used to construct `.ref_group` and
#'   `.in_ref_col`, which are passed to [s_summary_diff()] to compute the
#'   comparison statistics for variables in `df` and the reference `.ref_group`
#'   data set.
#'
#' @param na_rm_var (`logical(1)`)\cr If `TRUE`, rows with missing values in
#'   `.var` are removed from `df` before computing statistics defined in
#'   `stats_vars`. In this case, `.var` must be provided and must exist in `df`.
#'
#' @param stats_vars (`data.frame`)\cr Specification of statistics to compute
#'   for each variable. Must contain two columns:
#'   \itemize{
#'     \item `stat` - statistics to compute (allowed values defined by
#'       [s_summary_diff()]).
#'     \item `var` - variable names in `df` for which the statistics are computed.
#'   }
#'
#' @param labels_vars (`list` or `NULL`)\cr Optional custom labels for statistics.
#'   Must be a named list with names matching a subset of `stats_vars$var`.
#'   The format of each element is defined by the `labels_in` argument in
#'   [junco_get_labels_from_stats()].
#'
#' @param formats_vars (`list` or `NULL`)\cr Optional custom formats for statistics.
#'   Must be a named list with names matching a subset of `stats_vars$var`.
#'   The format of each element is defined by the `formats_in` argument in
#'   [junco_get_formats_from_stats()].
#'
#' @param indent_mods_vars (`list` or `NULL`)\cr Optional custom indentation
#'   modifiers for statistics. Must be a named list with names matching a subset
#'   of `stats_vars$var`.
#'   The format of each element is defined by the `indents_in` argument in
#'   [junco_get_indents_from_stats()].
#'
#' @param ... Additional arguments passed on to [s_summary_diff()]
#'
#' @returns
#'   `RowsVerticalSection` with computed values of chosen statistics for the
#'   specified variables.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = rep(1:6, each = 2),
#'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32),
#'   ABLFL = rep(c(TRUE, FALSE), 6),
#'   BASE = rep(c(1, 2, 13, 15, 43, 24), each = 2),
#'   CHG = c(0, 2, 0, 7, 0, 6, 0, 8, 0, 13, 0, 8)
#' )
#' df
#'
#' stats_vars <- data.frame(
#'   stat = c("n", "mean_sd", "mean_sd"),
#'   var = c("CHG", "BASE", "CHG")
#' )
#'
#' labels_vars <- list(BASE = c(mean_sd = "Baseline Mean (SD)"))
#'
#' a_summary_diff_mvars(
#'   df,
#'   stats_vars = stats_vars,
#'   na_rm_var = FALSE,
#'   labels_vars = labels_vars
#' )
a_summary_diff_mvars <- function(df,
                                 .var = NULL,
                                 .spl_context = NULL,
                                 ref_path = NULL,
                                 na_rm_var = TRUE,
                                 stats_vars,
                                 labels_vars = NULL,
                                 formats_vars = NULL,
                                 indent_mods_vars = NULL,
                                 ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_flag(na_rm_var)
  if (na_rm_var) {
    checkmate::assert_string(.var)
    checkmate::assert_names(colnames(df), must.include = .var)
  }
  checkmate::assert_data_frame(
    x = stats_vars, types = rep("character", 2), any.missing = FALSE, min.rows = 1, ncols = 2, col.names = "unique"
  )
  checkmate::assert_names(colnames(stats_vars), permutation.of = c("stat", "var"))
  checkmate::assert_character(stats_vars$stat, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  checkmate::assert_subset(stats_vars$var, colnames(df), empty.ok = FALSE)
  checkmate::assert_list(
    x = labels_vars, any.missing = FALSE, max.len = length(unique(stats_vars$var)), null.ok = TRUE
  )
  if (!is.null(labels_vars)) {
    checkmate::assert_names(names(labels_vars), type = "unique", subset.of = stats_vars$var)
  }
  checkmate::assert_list(
    x = formats_vars, any.missing = FALSE, max.len = length(unique(stats_vars$var)), null.ok = TRUE
  )
  if (!is.null(formats_vars)) {
    checkmate::assert_names(names(formats_vars), type = "unique", subset.of = stats_vars$var)
  }
  checkmate::assert_list(
    x = indent_mods_vars, any.missing = FALSE, max.len = length(unique(stats_vars$var)), null.ok = TRUE
  )
  if (!is.null(indent_mods_vars)) {
    checkmate::assert_names(names(indent_mods_vars), type = "unique", subset.of = stats_vars$var)
  }

  if (na_rm_var) {
    df <- df[!is.na(df[[.var]]), ]
  }

  ## issue with junco::get_ref_info(ref_path, .spl_context) in column produced by add_overall_col
  ## need to check if we are in such column!!!
  if (!is.null(ref_path)) {
    ref <- junco::get_ref_info(ref_path, .spl_context)
    .ref_group <- ref$ref_group
    .in_ref_col <- ref$in_ref_col
  } else {
    .ref_group <- NULL
    .in_ref_col <- FALSE
  }

  vars <- unique(stats_vars$var)
  stats_values <- NULL
  for (v in vars) {
    stats_v <- stats_vars[stats_vars$var == v, "stat"]
    stats_values[[v]] <- s_summary_diff(df, v, stats_v, .ref_group, .in_ref_col, ...)
    labels_vars[[v]] <- junco_get_labels_from_stats(stats_v, labels_in = labels_vars[[v]])
    formats_vars[[v]] <- junco_get_formats_from_stats(stats_v, formats_in = formats_vars[[v]])
    indent_mods_vars[[v]] <- junco_get_indents_from_stats(stats_v, indents_in = indent_mods_vars[[v]])
  }

  stats_order <- paste(stats_vars$var, stats_vars$stat, sep = ".")
  stats_values <- do.call(c, stats_values)[stats_order]
  labels <- unlist(do.call(c, labels_vars))[stats_order]
  formats <- do.call(c, formats_vars)[stats_order]
  indent_mods <- do.call(c, indent_mods_vars)[stats_order]

  in_rows(
    .list = stats_values,
    .labels = labels,
    .formats = formats,
    .indent_mods = indent_mods,
    .format_na_strs = "-"
  )
}

#' @describeIn a_summary_diff_mvars
#'
#'
#' A wrapper around [a_summary_diff_mvars()] that prepends a label to the
#' resulting table section containing the computed statistics.
#'
#' @inheritParams a_summary_diff_mvars
#' @inheritParams prepend_label_cell
#' @param label (`character(1)` or `function`)\cr
#'   Label to be displayed for the section. If a function, it must accept a
#'   single argument `.spl_context` and return a character string.
#' @param ... Additional arguments passed on to [a_summary_diff_mvars()].
#'
#' @returns
#' A `RowsVerticalSection` object with a prepended section label.
#'
#' @export
#'
#' @examples
#'
#' a_summary_diff_mvars_label(
#'   df,
#'   stats_vars = stats_vars,
#'   na_rm_var = FALSE,
#'   labels_vars = labels_vars,
#'   label = "Change from Baseline"
#' )
#'
#' label_change <- function(spl_cntxt) {
#'   last_split <- length(spl_cntxt$split)
#'   paste("Change from Baseline to", spl_cntxt$value[last_split])
#' }
#'
#' library(rtables)
#'
#' lyt <- basic_table() |>
#'   append_topleft("Parameter") |>
#'   split_cols_by("ARM") |>
#'   split_rows_by(
#'     "PARAMCD",
#'     split_fun = keep_split_levels(c("DIABP", "SYSBP")),
#'     labels_var = "PARAM",
#'     child_labels = "visible",
#'     section_div = " "
#'   ) |>
#'   summarize_row_groups(
#'     "AVAL",
#'     cfun = c_summary_subset_label,
#'     extra_args = list(
#'       subset_expr = expression(ABLFL == "Y"),
#'       .stats = c("n", "mean_sd"),
#'       .indent_mods = c(n = 1L, mean_sd = 1L),
#'       label = "BASELINE"
#'     )
#'   ) |>
#'   split_rows_by(
#'     "AVISIT",
#'     split_fun = keep_split_levels(c("WEEK 1 DAY 8", "WEEK 2 DAY 15")),
#'     indent_mod = -1
#'   ) |>
#'   analyze(
#'     "AVAL",
#'     afun = rtables::simple_analysis,
#'     show_labels = "hidden"
#'   ) |>
#'   analyze(
#'     "CHG",
#'     afun = a_summary_diff_mvars_label,
#'     extra_args = list(
#'       stats_vars = data.frame(
#'         stat = c("n", "mean_sd", "mean_sd"),
#'         var = c("CHG", "BASE", "CHG")
#'       ),
#'       labels_vars = list(BASE = c(mean_sd = "Baseline Mean (SD)")),
#'       label = label_change,
#'       label_indent_mod = -1L
#'     ),
#'     show_labels = "hidden"
#'   )
#'
#' tbl <- build_table(lyt, formatters::ex_advs)
#' tbl
a_summary_diff_mvars_label <- function(df,
                                       .var = NULL,
                                       .spl_context = NULL,
                                       ref_path = NULL,
                                       na_rm_var = TRUE,
                                       stats_vars,
                                       labels_vars = NULL,
                                       formats_vars = NULL,
                                       indent_mods_vars = NULL,
                                       label,
                                       label_indent_mod = 0L,
                                       ...) {
  checkmate::assert_true(checkmate::test_function(label) || checkmate::test_string(label))

  asummary_ret <- a_summary_diff_mvars(
    df = df,
    .var = .var,
    .spl_context = .spl_context,
    ref_path = ref_path,
    na_rm_var = na_rm_var,
    stats_vars = stats_vars,
    labels_vars = labels_vars,
    formats_vars = formats_vars,
    indent_mods_vars = indent_mods_vars,
    ...
  )

  section_label <- if (is.function(label)) {
    label(.spl_context)
  } else {
    label
  }

  prepend_label_cell(asummary_ret, label = section_label, label_indent_mod = label_indent_mod)
}
