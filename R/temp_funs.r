#' #' @title Calculated Difference in Means with the Confidence-Interval.
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' A statistical function for differences in mean point and interval estimates
#' #' for two samples. The interval estimates are obtained from the t-statistic
#' #' (see [safe_t_test]).
#' #' The first sample is `df2[[.var]]`, while the second, `df2[[.var]]`.
#' #' The key for paired samples is specified by `paired_by`.
#' #' The fact whethwet the samples are paired or not impacts only the way the
#' #' confinde interval estaimtes are computed, that is, for paired samples,
#' #' t-statistic for paired data is used.
#' #'
#' #' @param df1 (`data.frame`)\cr A dataset containing data values of the first sample.
#' #' @param df2 (`data.frame`)\cr A dataset containing data values of the second sample.
#' #' @param .var (`character(1)`)\cr The name of the column in `df1`, `df2`, containing the data values.
#' #' @param paired (`logical(1)`)\cr Are the samples paired?
#' #' @param paired_by (`character` or `NULL`)\cr Column names in `df1` and `df2`.
#' #'   Variables whose (unique) values define a single pair
#' #'   It is used if and only if `paired` is `TRUE`, and in this case, if must be
#' #'   not `NULL`.
#' #' @param conf.level (`proportion`)\cr Confidence level of the interval.
#' #' @param ... Further arguments to be passed to [safe_t_test()].
#' #'
#' #' @returns `list` with `diff_mean_ci` statistic.
#' #' @export
#' #'
#' #' @examples
#' #' df1 <- data.frame(
#' #'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#' #'   TRT01A = rep("ARM_A", 5),
#' #'   PARAMCD = rep("SYSBP", 5),
#' #'   AVISIT = rep("Visit 1", 5),
#' #'   CHG = c(4, 1, -1, 9, -2)
#' #' )
#' #' df2 <- data.frame(
#' #'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#' #'   TRT01A = rep("Placebo", 5),
#' #'   PARAMCD = rep("SYSBP", 5),
#' #'   AVISIT = rep("Visit 1", 5),
#' #'   CHG = c(-2, 6, -2, 5, 2)
#' #' )
#' #'
#' #' s_diff_mean_ci(df1, df2, "CHG", paired = TRUE, paired_by = "USUBJID")
#' #' s_diff_mean_ci(df1, df2, "CHG") # notice different CI estimates.
#' s_diff_mean_ci <- function(df1,
#'                            df2,
#'                            .var,
#'                            paired = FALSE,
#'                            paired_by = NULL,
#'                            conf.level = 0.95,
#'                            ...) {
#'   checkmate::assert_data_frame(df1, null.ok = FALSE)
#'   checkmate::assert_data_frame(df2, null.ok = FALSE)
#'   checkmate::assert_string(.var)
#'   checkmate::assert_choice(.var, choices = colnames(df1))
#'   checkmate::assert_choice(.var, choices = colnames(df2))
#'   checkmate::assert_flag(paired, null.ok = FALSE)
#' 
#'   if (paired) {
#'     checkmate::assert_character(paired_by, null.ok = FALSE)
#'     checkmate::assert_subset(paired_by, colnames(df1), empty.ok = FALSE)
#'     checkmate::assert_subset(paired_by, colnames(df2), empty.ok = FALSE)
#'     if (any(duplicated(df1[, paired_by])) || any(duplicated(df2[, paired_by]))) {
#'       stop("df1/df2 rows for paired_by columns must be unique.")
#'     }
#' 
#'     df1_pv <- df1[, c(paired_by, .var), drop = FALSE]
#'     df2_pv <- df2[, c(paired_by, .var), drop = FALSE]
#'     df <- merge(df1_pv, df2_pv, by = paired_by, suffixes = c("_df1", "_df2"))
#'     x1 <- df[[paste0(.var, "_df1")]]
#'     x2 <- df[[paste0(.var, "_df2")]]
#'   } else {
#'     x1 <- df1[[.var]]
#'     x2 <- df2[[.var]]
#'   }
#'   x1 <- x1[!is.na(x1)]
#'   x2 <- x2[!is.na(x2)]
#' 
#'   ttest_res <- safe_t_test(x1, x2, paired = paired, conf.level = conf.level, ...)
#'   stat_diff <- ifelse(paired, ttest_res$estimate, ttest_res$estimate[1] - ttest_res$estimate[2])
#'   stats <- c(diff_mean = stat_diff, setNames(ttest_res$conf.int, c("ci_lwr", "ci_upr")))
#' 
#'   list(
#'     diff_mean_ci = with_label(
#'       stats,
#'       paste("Difference in Means +", tern::f_conf_level(conf.level))
#'     )
#'   )
#' }
#' 
#' #' @title Descriptive Statistics for Univariate Numerical (Quantitative) Data.
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' A statistical function for descriptive statistics for numerical data. It uses
#' #' [tern:::s_summary.numeric()] and [s_diff_mean_ci()]. It computes the values of
#' #' statistics for a univariate data values in `df[[.var]]`.
#' #' It can also compute the statistics for two univariate data values (for example
#' #' difference in means): `df[[.var]]` and the reference group `.ref_group[[.var]]`.
#' #'
#' #' @inheritParams proposal_argument_convention
#' #' @param .var (`character(1)`)\cr The name of the column in `df`. Contains
#' #'   data values for which that statistic are computed.
#' #' @param .stats (`character` or `NULL`)\cr names of the statistics for be computed.
#' #'   The list of names of supported statistics is given by
#' #'   [tern::get_stats(method_groups = "analyze_vars_numeric", custom_stats_in  = "diff_mean_ci")].
#' #'   If `NULL` then all the stats are computed.
#' #' @param control (`list`)\cr relevant list of control options.
#' #'   Passed to [tern:::s_summary.numeric()].
#' #'   If `diff_mean_ci` is computed, it must containt an element named
#' #'   `conf_level` with the specification of the confidence level of the interval.
#' #' @param ... Additional arguments passed to [tern:::s_summary.numeric()] and to
#' #'   [junco::s_diff_mean_ci()], if `diff_mean_ci` is computed.
#' #'
#' #' @returns `list` with computed values of chosen statistics.
#' #' @export
#' #'
#' #' @examples
#' #' df <- data.frame(
#' #'   USUBJID = c("X01", "X02", "X03", "X04", "X05"),
#' #'   TRT01A = rep("ARM_A", 5),
#' #'   PARAMCD = rep("SYSBP", 5),
#' #'   AVISIT = rep("Visit 1", 5),
#' #'   CHG = c(4, 1, -1, 9, -2)
#' #' )
#' #' .ref_group <- data.frame(
#' #'   USUBJID = c("X06", "X07", "X08", "X09", "X10"),
#' #'   TRT01A = rep("Placebo", 5),
#' #'   PARAMCD = rep("SYSBP", 5),
#' #'   AVISIT = rep("Visit 1", 5),
#' #'   CHG = c(-2, 6, -2, 5, 2)
#' #' )
#' #'
#' #' s_summary_diff(df, "CHG", c("n", "mean_sd", "diff_mean_ci"), .ref_group)
#' #' s_summary_diff(df, "CHG", c("n", "mean_sd", "diff_mean_ci"), df, .in_ref_col = TRUE)
#' #'
#' s_summary_diff <- function(df,
#'                            .var,
#'                            .stats = NULL,
#'                            .ref_group,
#'                            .in_ref_col = FALSE,
#'                            control = tern::control_analyze_vars(),
#'                            ...) {
#'   checkmate::assert_data_frame(df, null.ok = FALSE)
#'   checkmate::assert_string(.var)
#'   checkmate::assert_choice(.var, choices = colnames(df))
#'   checkmate::assert_character(.stats, any.missing = FALSE, min.len = 1, null.ok = TRUE)
#'   checkmate::assert_flag(.in_ref_col, null.ok = FALSE)
#'   checkmate::assert_list(control, null.ok = FALSE)
#'   checkmate::assert_choice("conf_level", choices = names(control))
#' 
#'   if (is.null(.stats)) {
#'     .stats <- c(tern:::get_stats(), "diff_mean_ci")
#'   }
#' 
#'   y <- tern::s_summary(df[[.var]], control = control)
#' 
#'   if ("diff_mean_ci" %in% .stats) {
#'     md <- if (!.in_ref_col) {
#'       s_diff_mean_ci(df, .ref_group, .var, conf.level = control$conf_level, ...)
#'     } else {
#'       list(diff_mean_ci = NULL)
#'     }
#'     y <- c(y, md)
#'   }
#' 
#'   y[.stats]
#' }
#' 
#' #' @title Pre-pend Row with Label to the Results of the Analysis Function.
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' @note If `x` is of class `RowsVerticalSection`, attributes `row_formats`,
#' #'   `row_na_strs`, `row_footnotes` are not preserved (if present).
#' #'
#' #' @param x (`list` or `CellValue` or `RowsVerticalSection`)\cr
#' #'   Results of the Analysis Function.
#' #' @param label (`string`)\cr A label to be appended.
#' #' @param label_indent (`integer(1)`)\cr An indent for the row with the
#' #'   prepended label.
#' #'
#' #' @returns `RowsVerticalSection` class object.
#' #' @export
#' #'
#' #' @examples
#' #' rvs <- in_rows(Mean = rcell(5), SD = rcell(1))
#' #' prepend_label_cell(rvs, "Descriptive Statistics", label_indent = 1L)
#' #'
#' prepend_label_cell <- function(x, label = "", label_indent = 0L) {
#'   checkmate::check_multi_class(x, c("list", "CellValue", "RowsVerticalSection"))
#'   if (class(x) == "list") {
#'     checkmate::assert_list(x, types = "CellValue", any.missing = FALSE)
#'   }
#'   checkmate::assert_string(label, na.ok = TRUE)
#'   checkmate::assert_int(label_indent)
#' 
#'   if (class(x) == "CellValue") {
#'     label_rcell <- rcell(NULL, label = label, indent_mod = label_indent)
#'     list(label_rcell, x)
#'   } else if (class(x) == "list") {
#'     label_rcell <- rcell(NULL, label = label, indent_mod = label_indent)
#'     c(list(label_rcell), x)
#'   } else if (class(x) == "RowsVerticalSection") {
#'     ret <- in_rows(.list = c(list(NULL), x))
#'     if (is.null(attr(x, "indent_mods"))) {
#'       attr(ret[[1]], "indent_mod") <- label_indent
#'     } else {
#'       attr(ret, "indent_mods") <- c(label_indent, attr(x, "indent_mods"))
#'     }
#'     attr(ret, "row_labels") <- c(label, attr(x, "row_labels"))
#'     # rvs_attr <- c("row_formats", "row_na_strs", "row_footnotes")
#'     # rvs_attr_gr1 <- sapply(attributes(x)[rvs_attr], length) > 1
#'     # rvs_attr_gr1 <- names(rvs_attr_gr1)[rvs_attr_gr1]
#'     # for (a in rvs_attr_gr1) {
#'     #   attr(ret, a) <- c(NA, attr(x, a))
#'     # }
#'     ret
#'   }
#' }
#' 
#' #' @title Descriptive Statistics for Multiple Univariate Numerical (Quantitative) Data.
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' An analysis function for descriptive statistics for
#' #' multivariate numerical data. It uses [s_summary_diff()] to compute
#' #' statistics specified in `.stats_vars$stat` for corresponding variable
#' #' specified `.stats_vars$vars`.
#' #'
#' #' @inheritParams proposal_argument_convention
#' #' @param .var (`character(1)` or `NULL`)\cr Column name in `df` containing the
#' #'   the data values for the analysis.
#' #'   It is not used if `na_rm_var` if `FALSE`.
#' #' @param .spl_context (`data.frame` or `NULL`)\cr gives information about
#' #'   ancestor split states that is passed by `rtables`.
#' #'   It is not used if `ref_path` is not `NULL`.
#' #' @param ref_path (`character` or `NULL`)\cr global reference group specification,
#' #'   see [get_ref_info()].
#' #'   It is used to construct `.ref_group` and `.in_ref_col` objects that are then
#' #'   passed to [s_summary_diff()] function for compute statistics for the
#' #'   a given variable from the `df` data set and from the reference variable from
#' #'   the `.ref_group` data set.
#' #' @param na_rm_var (`logical(1)`)\cr If `TRUE`, rows with missing values (`NA`)
#' #'   for `.var` column are removed from `df` before any computations.
#' #' @param .stats_vars (`data.frame`)\cr statistics to compute for a given variable.
#' #'   It should have two named columns: `stat` and `vars`.
#' #'   The `.stats_vars$stat` specifies the statistics to be computed. I
#' #'   Its allowable set of values is determined by the of `.stats` argument of
#' #'   [s_summary_diff()].
#' #'   Column `.stats_vars$vars` specifies names of columns in `df` for which
#' #'   corresponding statistics are computed.
#' #' @param .labels_vars (`list` or `NULL`)\cr Custom labels for the statistics.
#' #'   It must be a named `list` with names being a subset of `.stats_vars$vars`.
#' #'   The format and the set of allowable values of a given element of this list
#' #'   is determined by `labels_in` argument of [junco_get_labels_from_stats()].
#' #'   If unspecified for some statistic, then the default label will be used.
#' #' @param .formats_vars (`list` or `NULL`)\cr Custom formats for the statistics.
#' #'   It must be a named `list` with names being a subset of `.stats_vars$vars`.
#' #'   The format and the set of allowable values of a given element of this list
#' #'   is determined by `formats_in` argument of [junco_get_formats_from_stats()].
#' #'   If unspecified for some statistic, then the default formatting will be used.
#' #' @param .indent_mods_vars (`list` or `NULL`)\cr Custom indents for the statistics.
#' #'   It must be a named `list` with names being a subset of `.stats_vars$vars`.
#' #'   The format and the set of allowable values of a given element of this list
#' #'   is determined by `indents_in` argument of [junco_get_indents_from_stats()].
#' #'   If unspecified for some statistic, then the default indentation will be used.
#' #' @param ... Additional arguments passed to [s_summary_diff()]
#' #'
#' #' @returns
#' #'   A `list` with computed values of chosen statistics for the specified variables.
#' #'
#' #' @importFrom tern s_summary
#' #' @export
#' #'
#' #' @examples
#' #' dta_test <- data.frame(
#' #'   USUBJID = rep(1:6, each = 2),
#' #'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#' #'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32)
#' #' ) |>
#' #'   mutate(ABLFL = AVISIT == "Baseline") |>
#' #'   group_by(USUBJID) |>
#' #'   mutate(
#' #'     BASE = AVAL[ABLFL],
#' #'     CHG = AVAL - BASE
#' #'   ) |>
#' #'   ungroup()
#' #'
#' #' .stats_vars <- data.frame(
#' #'   stat = c("n", "mean_sd", "mean_sd"),
#' #'   var = c("CHG", "BASE", "CHG")
#' #' )
#' #' .labels_vars <- list(BASE = c(mean_sd = "Baseline Mean (SD)"))
#' #'
#' #' a_summary_multivars_num(
#' #'   dta_test,
#' #'   .stats_vars = .stats_vars,
#' #'   na_rm_var = FALSE,
#' #'   .labels_vars = .labels_vars
#' #' )
#' a_summary_diff_mvars <- function(df,
#'                                  .var = NULL,
#'                                  .spl_context = NULL,
#'                                  ref_path = NULL,
#'                                  na_rm_var = TRUE,
#'                                  .stats_vars,
#'                                  .labels_vars = NULL,
#'                                  .formats_vars = NULL,
#'                                  .indent_mods_vars = NULL,
#'                                  ...) {
#'   checkmate::assert_data_frame(df)
#'   checkmate::assert_string(.var, null.ok = TRUE)
#'   checkmate::assert_choice(.var, colnames(df), null.ok = TRUE)
#'   checkmate::assert_flag(na_rm_var)
#'   checkmate::assert_data_frame(
#'     .stats_vars,
#'     rep("character", 2),
#'     any.missing = FALSE, min.rows = 1, ncols = 2, col.names = "unique"
#'   )
#'   checkmate::assert_permutation(colnames(.stats_vars), c("stat", "var"), na.ok = FALSE)
#'   checkmate::assert_character(.stats_vars$stat, any.missing = FALSE, min.len = 1, null.ok = TRUE)
#'   checkmate::assert_subset(.stats_vars$var, colnames(df), empty.ok = FALSE)
#'   checkmate::assert_list(
#'     .labels_vars,
#'     any.missing = FALSE, max.len = length(unique(.stats_vars$var)), null.ok = TRUE
#'   )
#'   checkmate::assert_subset(names(.labels_vars), .stats_vars$var, empty.ok = TRUE)
#'   checkmate::assert_list(
#'     .formats_vars,
#'     any.missing = FALSE, max.len = length(unique(.stats_vars$var)), null.ok = TRUE
#'   )
#'   checkmate::assert_subset(names(.formats_vars), .stats_vars$var, empty.ok = TRUE)
#'   checkmate::assert_list(
#'     .indent_mods_vars,
#'     any.missing = FALSE, max.len = length(unique(.stats_vars$var)), null.ok = TRUE
#'   )
#'   checkmate::assert_subset(names(.indent_mods_vars), .stats_vars$var, empty.ok = TRUE)
#' 
#'   if (na_rm_var) {
#'     df <- df[!is.na(df[[.var]]), ]
#'   }
#' 
#'   ## issue with junco::get_ref_info(ref_path, .spl_context) in column produced by add_overall_col
#'   ## need to check if we are in such column!!!
#'   if (!is.null(ref_path)) {
#'     ref <- junco::get_ref_info(ref_path, .spl_context)
#'     .ref_group <- ref$ref_group
#'     .in_ref_col <- ref$in_ref_col
#'   } else {
#'     .ref_group <- NULL
#'     .in_ref_col <- FALSE
#'   }
#' 
#'   vars <- unique(.stats_vars$var)
#'   stats_values <- NULL
#'   for (v in vars) {
#'     stats_v <- .stats_vars[.stats_vars$var == v, "stat"]
#'     stats_values[[v]] <- s_summary_diff(df, v, stats_v, .ref_group, .in_ref_col, ...)
#'     .labels_vars[[v]] <- junco_get_labels_from_stats(stats_v, labels_in = .labels_vars[[v]])
#'     .formats_vars[[v]] <- junco_get_formats_from_stats(stats_v, formats_in = .formats_vars[[v]])
#'     .indent_mods_vars[[v]] <- junco_get_indents_from_stats(stats_v, indents_in = .indent_mods_vars[[v]])
#'   }
#' 
#'   stats_order <- paste(.stats_vars$var, .stats_vars$stat, sep = ".")
#'   stats_values <- do.call(c, stats_values)[stats_order]
#'   labels <- unlist(do.call(c, .labels_vars))[stats_order]
#'   formats <- do.call(c, .formats_vars)[stats_order]
#'   indent_mods <- do.call(c, .indent_mods_vars)[stats_order]
#' 
#'   in_rows(
#'     .list = stats_values,
#'     .labels = labels,
#'     .formats = formats,
#'     .indent_mods = indent_mods,
#'     .format_na_strs = "-"
#'   )
#' }
#' 
#' #' @title Descriptive Statistics for Multiple Univariate Numerical (Quantitative)
#' #    Data with Label
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' A wrapper function of [a_summary_multivars_num()] function that prepends the
#' #' label for the section with statistics.
#' #'
#' #' @inheritParams a_summary_multivars_num
#' #' @inheritParams prepend_label_cell
#' #' @param label (`string` or `function`)\cr A label to be appended or the function
#' #'   the creates the label. If a `function`, then is must accept exactly one
#' #'   arguemnt which is a `.spl_context`.
#' #' @inheritDotParams a_summary_multivars_num
#' #'
#' #' @returns
#' #' @export
#' #'
#' #' @examples
#' #' dta_test <- data.frame(
#' #'   USUBJID = rep(1:6, each = 2),
#' #'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#' #'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32)
#' #' ) |>
#' #'   mutate(ABLFL = AVISIT == "Baseline") |>
#' #'   group_by(USUBJID) |>
#' #'   mutate(
#' #'     BASE = AVAL[ABLFL],
#' #'     CHG = AVAL - BASE
#' #'   ) |>
#' #'   ungroup()
#' #'
#' #' .stats_vars <- data.frame(
#' #'   stat = c("n", "mean_sd", "mean_sd"),
#' #'   var = c("CHG", "BASE", "CHG")
#' #' )
#' #' .labels_vars <- list(BASE = c(mean_sd = "Baseline Mean (SD)"))
#' #'
#' #' a_summary_multivars_num_label(
#' #'   dta_test,
#' #'   .stats_vars = .stats_vars,
#' #'   na_rm_var = FALSE,
#' #'   .labels_vars = .labels_vars,
#' #'   label = "Change from Baseline"
#' #' )
#' #'
#' #' label_change <- function(spl_cntxt) {
#' #'   last_split <- length(spl_cntxt$split)
#' #'   paste("Change from baseline to", spl_cntxt$value[last_split])
#' #' }
#' #'
#' #' lyt <- basic_table() |>
#' #'   append_topleft("Parameter") |>
#' #'   split_cols_by("ARM") |>
#' #'   split_rows_by(
#' #'     "PARAMCD",
#' #'     split_fun = keep_split_levels(c("DIABP", "SYSBP")),
#' #'     labels_var = "PARAM",
#' #'     child_labels = "visible",
#' #'     section_div = " "
#' #'   ) |>
#' #'   summarize_row_groups(
#' #'     "AVAL",
#' #'     cfun = c_summary_subset_label,
#' #'     extra_args = list(
#' #'       subset_expr = expression(ABLFL == "Y"),
#' #'       .stats = c("n", "mean_sd"),
#' #'       .indent_mods = c(n = 1L, mean_sd = 1L),
#' #'       label = "BASELINE"
#' #'     )
#' #'   ) |>
#' #'   split_rows_by(
#' #'     "AVISIT",
#' #'     split_fun = keep_split_levels(c("WEEK 1 DAY 8", "WEEK 2 DAY 15")),
#' #'     indent_mod = -1
#' #'   ) |>
#' #'   analyze(
#' #'     "AVAL",
#' #'     afun = rtables::simple_analysis,
#' #'     show_labels = "hidden"
#' #'   ) |>
#' #'   analyze(
#' #'     "CHG",
#' #'     afun = a_summary_multivars_num_label,
#' #'     extra_args = list(
#' #'       .stats_vars = data.frame(
#' #'         stat = c("n", "mean_sd", "mean_sd"),
#' #'         var = c("CHG", "BASE", "CHG")
#' #'       ),
#' #'       .labels_vars = list(BASE = c(mean_sd = "Baseline Mean (SD)")),
#' #'       label = label_change,
#' #'       label_indent_mod = -1L
#' #'     ),
#' #'     show_labels = "hidden"
#' #'   )
#' #'
#' #' tbl <- build_table(lyt, formatters::ex_advs)
#' #' tbl
#' a_summary_diff_mvars_label <- function(df,
#'                                        .var = NULL,
#'                                        .spl_context = NULL,
#'                                        ref_path = NULL,
#'                                        na_rm_var = TRUE,
#'                                        .stats_vars,
#'                                        .labels_vars = NULL,
#'                                        .formats_vars = NULL,
#'                                        .indent_mods_vars = NULL,
#'                                        label,
#'                                        label_indent_mod = 0L,
#'                                        ...) {
#'   checkmate::assert_true(checkmate::test_function(label) || checkmate::test_string(label))
#' 
#'   asummary_ret <- a_summary_diff_mvars(
#'     df = df,
#'     .var = .var,
#'     .spl_context = .spl_context,
#'     ref_path = ref_path,
#'     na_rm_var = na_rm_var,
#'     .stats_vars = .stats_vars,
#'     .labels_vars = .labels_vars,
#'     .formats_vars = .formats_vars,
#'     .indent_mods_vars = .indent_mods_vars,
#'     ...
#'   )
#' 
#'   section_label <- if (is.function(label)) {
#'     label(.spl_context)
#'   } else {
#'     label
#'   }
#' 
#'   prepend_label_cell(asummary_ret, label = section_label, label_indent = label_indent_mod)
#' }
#' 
#' #' @title Summary Statistics for the Filtered Data.
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' Formatted analysis function for summary statistics for optionally
#' #' row-filtered data.
#' #' It uses [a_summary_j()] under the hood.
#' #'
#' #' @inheritParams proposal_argument_convention
#' #' @param subset_expr (`expression` or `NULL`)\cr
#' #'   logical expression indicating rows in `df` to keep for the analysis.
#' #'   Filtering is performed prior to analysis.
#' #' @param ... Additional arguments passed to [a_summary_j()]
#' #'
#' #' @returns an object created by [a_summary_j()] for a given input arguments.
#' #' @export
#' #'
#' #' @examples
#' a_summary_subset <- function(df,
#'                              .var,
#'                              subset_expr,
#'                              ...) {
#'   checkmate::assert_data_frame(df)
#'   checkmate::assert_string(.var)
#'   checkmate::assert_choice(.var, colnames(df))
#'   checkmate::assert_logical(with(df, eval(subset_expr)), len = nrow(df), any.missing = FALSE)
#' 
#'   row_mask <- with(df, eval(subset_expr))
#'   x <- df[row_mask, .var, drop = TRUE]
#'   a_summary_j(x, ...)
#' }
#' 
#' #' @title Summary Statistics for the Filtered Data with Label
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' A wrapper function of [c_summary_subset()] function that prepends the
#' #' label for the section with statistics.
#' #'
#' #' @inheritParams proposal_argument_convention
#' #' @inheritParams c_summary_subset
#' #' @inheritParams prepend_label_cell
#' #' @param label (`string` or `function`)\cr A label to be appended or the function
#' #'   the creates the label. If a `function`, then is must accept exactly one
#' #'   arguemnt which is a `.spl_context`.
#' #' @param ... Additional arguments passed to [a_summary_subset()]
#' #'
#' #' @returns an object created by [a_summary_subset()] for a given input arguments.
#' #' @export
#' #'
#' #' @examples
#' c_summary_subset_label <- function(df,
#'                                    labelstr,
#'                                    .var,
#'                                    .spl_context,
#'                                    subset_expr,
#'                                    label,
#'                                    label_indent_mod = 0L,
#'                                    ...) {
#'   checkmate::assert_true(
#'     checkmate::test_function(label) || checkmate::test_string(label, na.ok = TRUE, null.ok = TRUE)
#'   )
#' 
#'   if (is.function(label)) {
#'     checkmate::assert_true(length(formals(label)) >= 1L)
#'     label <- label(.spl_context)
#'   }
#' 
#'   y <- a_summary_subset(df, .var, subset_expr, ...)
#' 
#'   prepend_label_cell(y, label = label, label_indent = label_indent_mod)
#' }
#' 
#' #' @title Wrapper around `tern::a_summary()` with junco-specific defaults
#' #'
#' #' @description `r lifecycle::badge("experimental")`
#' #'
#' #' This function wraps [tern::a_summary()] and applies junco-specific defaults
#' #' to selected formatting arguments: `.labels`, `.formats`, and `.indent_mods`.
#' #'
#' #' @details
#' #' If `.labels`, `.formats`, or `.indent_mods` are supplied with non-`NULL`
#' #' values, they are passed through to [tern::a_summary()] unchanged. If they
#' #' are `NULL` or not supplied, default values are generated using:
#' #' \itemize{
#' #'   \item [junco_get_labels_from_stats()] for `.labels`
#' #'   \item [junco_get_formats_from_stats()] for `.formats`
#' #'   \item [junco_get_indents_from_stats()] for `.indent_mods`.
#' #' }
#' #'
#' #' If the `.stats` argument is not explicitly provided, or is provided as `NULL`,
#' #' the default statistics from [tern::get_stats()] are used.
#' #'
#' #' @param ... Arguments passed on to [tern::a_summary()], including `.labels`,
#' #'   `.formats`, and `.indent_mods`.
#' #'
#' #' @return
#' #' Returns the same output as [tern::a_summary()].
#' #'
#' #' @seealso [tern::a_summary()]
#' #'
#' #' @importFrom tern a_summary get_stats
#' #' @export
#' #' @examples
#' #' .stats <- c("n", "mean_sd", "median_range")
#' #' tern::a_summary(1:10, .stats = .stats)
#' #' a_summary_j(1:10, .stats = .stats)
#' #' a_summary_j(1:10, .stats = .stats, .formats = c(mean_sd = "xx (xx.x)"))
#' #' a_summary_j(1:10)
#' #'
#' a_summary_j <- function(...) {
#'   argsd <- list(...)
#' 
#'   if (is.null(argsd$.stats)) {
#'     argsd$.stats <- tern::get_stats()
#'   }
#'   s <- argsd$.stats
#'   argsd$.labels <- junco_get_labels_from_stats(s, labels_in = argsd$.labels)
#'   argsd$.formats <- junco_get_formats_from_stats(s, formats_in = argsd$.formats)
#'   argsd$.indent_mods <- junco_get_indents_from_stats(s, indents_in = argsd$.indent_mods)
#' 
#'   do.call(tern::a_summary, argsd)
#' }
