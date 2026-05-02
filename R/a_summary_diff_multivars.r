#' @title Descriptive Statistics for Multiple Univariate Numerical (Quantitative) Data.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' An analysis function for descriptive statistics for
#' multivariate numerical data. It uses [s_summary_diff()] to compute
#' statistics specified in `.stats_vars$stat` for corresponding variable
#' specified `.stats_vars$vars`.
#'
#' @inheritParams proposal_argument_convention
#' @param .var (`character(1)` or `NULL`)\cr Column name in `df` containing the
#'   the data values for the analysis.
#'   It is not used if `na_rm_var` if `FALSE`.
#' @param .spl_context (`data.frame` or `NULL`)\cr gives information about
#'   ancestor split states that is passed by `rtables`.
#'   It is not used if `ref_path` is not `NULL`.
#' @param ref_path (`character` or `NULL`)\cr global reference group specification,
#'   see [get_ref_info()].
#'   It is used to construct `.ref_group` and `.in_ref_col` objects that are then
#'   passed to [s_summary_diff()] function for compute statistics for the
#'   a given variable from the `df` data set and from the reference variable from
#'   the `.ref_group` data set.
#' @param na_rm_var (`logical(1)`)\cr If `TRUE`, rows with missing values (`NA`)
#'   for `.var` column are removed from `df` before any computations.
#' @param .stats_vars (`data.frame`)\cr statistics to compute for a given variable.
#'   It should have two named columns: `stat` and `vars`.
#'   The `.stats_vars$stat` specifies the statistics to be computed. I
#'   Its allowable set of values is determined by the of `.stats` argument of
#'   [s_summary_diff()].
#'   Column `.stats_vars$vars` specifies names of columns in `df` for which
#'   corresponding statistics are computed.
#' @param .labels_vars (`list` or `NULL`)\cr Custom labels for the statistics.
#'   It must be a named `list` with names being a subset of `.stats_vars$vars`.
#'   The format and the set of allowable values of a given element of this list
#'   is determined by `labels_in` argument of [junco_get_labels_from_stats()].
#'   If unspecified for some statistic, then the default label will be used.
#' @param .formats_vars (`list` or `NULL`)\cr Custom formats for the statistics.
#'   It must be a named `list` with names being a subset of `.stats_vars$vars`.
#'   The format and the set of allowable values of a given element of this list
#'   is determined by `formats_in` argument of [junco_get_formats_from_stats()].
#'   If unspecified for some statistic, then the default formatting will be used.
#' @param .indent_mods_vars (`list` or `NULL`)\cr Custom indents for the statistics.
#'   It must be a named `list` with names being a subset of `.stats_vars$vars`.
#'   The format and the set of allowable values of a given element of this list
#'   is determined by `indents_in` argument of [junco_get_indents_from_stats()].
#'   If unspecified for some statistic, then the default indentation will be used.
#' @param ... Additional arguments passed on to [s_summary_diff()]
#'
#' @returns
#'   A `list` with computed values of chosen statistics for the specified variables.
#'
#' @export
#'
#' @examples
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 2),
#'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32)
#' ) |>
#'   mutate(ABLFL = AVISIT == "Baseline") |>
#'   group_by(USUBJID) |>
#'   mutate(
#'     BASE = AVAL[ABLFL],
#'     CHG = AVAL - BASE
#'   ) |>
#'   ungroup()
#'
#' .stats_vars <- data.frame(
#'   stat = c("n", "mean_sd", "mean_sd"),
#'   var = c("CHG", "BASE", "CHG")
#' )
#' .labels_vars <- list(BASE = c(mean_sd = "Baseline Mean (SD)"))
#'
#' a_summary_multivars_num(
#'   dta_test,
#'   .stats_vars = .stats_vars,
#'   na_rm_var = FALSE,
#'   .labels_vars = .labels_vars
#' )
a_summary_diff_multivars <- function(df,
                                     .var = NULL,
                                     .spl_context = NULL,
                                     ref_path = NULL,
                                     na_rm_var = TRUE,
                                     .stats_vars,
                                     .labels_vars = NULL,
                                     .formats_vars = NULL,
                                     .indent_mods_vars = NULL,
                                     ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var, null.ok = TRUE)
  checkmate::assert_choice(.var, colnames(df), null.ok = TRUE)
  checkmate::assert_flag(na_rm_var)
  checkmate::assert_data_frame(
    .stats_vars,
    rep("character", 2),
    any.missing = FALSE, min.rows = 1, ncols = 2, col.names = "unique"
  )
  checkmate::assert_permutation(colnames(.stats_vars), c("stat", "var"), na.ok = FALSE)
  checkmate::assert_character(.stats_vars$stat, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  checkmate::assert_subset(.stats_vars$var, colnames(df), empty.ok = FALSE)
  checkmate::assert_list(
    .labels_vars,
    any.missing = FALSE, max.len = length(unique(.stats_vars$var)), null.ok = TRUE
  )
  checkmate::assert_subset(names(.labels_vars), .stats_vars$var, empty.ok = TRUE)
  checkmate::assert_list(
    .formats_vars,
    any.missing = FALSE, max.len = length(unique(.stats_vars$var)), null.ok = TRUE
  )
  checkmate::assert_subset(names(.formats_vars), .stats_vars$var, empty.ok = TRUE)
  checkmate::assert_list(
    .indent_mods_vars,
    any.missing = FALSE, max.len = length(unique(.stats_vars$var)), null.ok = TRUE
  )
  checkmate::assert_subset(names(.indent_mods_vars), .stats_vars$var, empty.ok = TRUE)

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

  vars <- unique(.stats_vars$var)
  stats_values <- NULL
  for (v in vars) {
    stats_v <- .stats_vars[.stats_vars$var == v, "stat"]
    stats_values[[v]] <- s_summary_diff(df, v, stats_v, .ref_group, .in_ref_col, ...)
    .labels_vars[[v]] <- junco_get_labels_from_stats(stats_v, labels_in = .labels_vars[[v]])
    .formats_vars[[v]] <- junco_get_formats_from_stats(stats_v, formats_in = .formats_vars[[v]])
    .indent_mods_vars[[v]] <- junco_get_indents_from_stats(stats_v, indents_in = .indent_mods_vars[[v]])
  }

  stats_order <- paste(.stats_vars$var, .stats_vars$stat, sep = ".")
  stats_values <- do.call(c, stats_values)[stats_order]
  labels <- unlist(do.call(c, .labels_vars))[stats_order]
  formats <- do.call(c, .formats_vars)[stats_order]
  indent_mods <- do.call(c, .indent_mods_vars)[stats_order]

  in_rows(
    .list = stats_values,
    .labels = labels,
    .formats = formats,
    .indent_mods = indent_mods,
    .format_na_strs = "-"
  )
}

#' @title Descriptive Statistics for Multiple Univariate Numerical (Quantitative)
#    Data with Label
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A wrapper function of [a_summary_multivars_num()] function that prepends the
#' label for the section with statistics.
#'
#' @inheritParams a_summary_multivars_num
#' @inheritParams prepend_label_cell
#' @param label (`string` or `function`)\cr A label to be appended or the function
#'   the creates the label. If a `function`, then is must accept exactly one
#'   arguemnt which is a `.spl_context`.
#' @inheritDotParams a_summary_multivars_num
#'
#' @returns
#' @export
#'
#' @examples
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 2),
#'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32)
#' ) |>
#'   mutate(ABLFL = AVISIT == "Baseline") |>
#'   group_by(USUBJID) |>
#'   mutate(
#'     BASE = AVAL[ABLFL],
#'     CHG = AVAL - BASE
#'   ) |>
#'   ungroup()
#'
#' .stats_vars <- data.frame(
#'   stat = c("n", "mean_sd", "mean_sd"),
#'   var = c("CHG", "BASE", "CHG")
#' )
#' .labels_vars <- list(BASE = c(mean_sd = "Baseline Mean (SD)"))
#'
#' a_summary_multivars_num_label(
#'   dta_test,
#'   .stats_vars = .stats_vars,
#'   na_rm_var = FALSE,
#'   .labels_vars = .labels_vars,
#'   label = "Change from Baseline"
#' )
#'
#' label_change <- function(spl_cntxt) {
#'   last_split <- length(spl_cntxt$split)
#'   paste("Change from baseline to", spl_cntxt$value[last_split])
#' }
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
#'     afun = a_summary_multivars_num_label,
#'     extra_args = list(
#'       .stats_vars = data.frame(
#'         stat = c("n", "mean_sd", "mean_sd"),
#'         var = c("CHG", "BASE", "CHG")
#'       ),
#'       .labels_vars = list(BASE = c(mean_sd = "Baseline Mean (SD)")),
#'       label = label_change,
#'       label_indent_mod = -1L
#'     ),
#'     show_labels = "hidden"
#'   )
#'
#' tbl <- build_table(lyt, formatters::ex_advs)
#' tbl
a_summary_diff_multivars_label <- function(df,
                                           .var = NULL,
                                           .spl_context = NULL,
                                           ref_path = NULL,
                                           na_rm_var = TRUE,
                                           .stats_vars,
                                           .labels_vars = NULL,
                                           .formats_vars = NULL,
                                           .indent_mods_vars = NULL,
                                           label,
                                           label_indent_mod = 0L,
                                           ...) {
  checkmate::assert_true(checkmate::test_function(label) || checkmate::test_string(label))

  asummary_ret <- a_summary_diff_multivars(
    df = df,
    .var = .var,
    .spl_context = .spl_context,
    ref_path = ref_path,
    na_rm_var = na_rm_var,
    .stats_vars = .stats_vars,
    .labels_vars = .labels_vars,
    .formats_vars = .formats_vars,
    .indent_mods_vars = .indent_mods_vars,
    ...
  )

  section_label <- if (is.function(label)) {
    label(.spl_context)
  } else {
    label
  }

  prepend_label_cell(asummary_ret, label = section_label, label_indent = label_indent_mod)
}
