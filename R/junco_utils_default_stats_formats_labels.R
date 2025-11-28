#' Get default statistical methods and their associated formats, labels, and indent modifiers
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their associated formats (`.formats`), labels (`.labels`), and indent modifiers
#' (`.indent_mods`). This utility is used across `junco`, but some of its working principles can be
#' seen in [tern::analyze_vars()]. See notes to understand why this is experimental.
#'
#' @param stats (`character`)\cr statistical methods to return defaults for.
#' @param levels_per_stats (named `list` of `character` or `NULL`)\cr named list where the name of each element is a
#'   statistic from `stats` and each element is the levels of a `factor` or `character` variable (or variable name),
#'   each corresponding to a single row, for which the named statistic should be calculated for. If a statistic is only
#'   calculated once (one row), the element can be either `NULL` or the name of the statistic. Each list element will be
#'   flattened such that the names of the list elements returned by the function have the format `statistic.level` (or
#'   just `statistic` for statistics calculated for a single row). Defaults to `NULL`.
#'
#' @details
#' Current choices for `type` are `counts` and `numeric` for [tern::analyze_vars()] and affect `junco_get_stats()`.
#'
#' @note
#' These defaults are experimental because we use the names of functions to retrieve the default
#' statistics. This should be generalized in groups of methods according to more reasonable groupings.
#'
#' These functions have been modified from the `tern` file `utils_default_stats_formats_labels.R`.
#' This file contains `junco` specific wrappers of functions called within the `afun` functions,
#' in order to point to `junco` specific default statistics, formats and labels.
#'
#' @name default_stats_formats_labels
NULL

#' @describeIn default_stats_formats_labels Get statistics available for a given method
#'   group (analyze function). To check available defaults see `junco_default_stats` list.
#'
#' @param method_groups (`character`)\cr indicates the statistical method group (`junco` analyze function)
#'   to retrieve default statistics for. A character vector can be used to specify more than one statistical
#'   method group.
#' @param stats_in (`character`)\cr statistics to retrieve for the selected method group. If custom statistical
#'   functions are used, `stats_in` needs to have them in too.
#' @param custom_stats_in (`character`)\cr custom statistics to add to the default statistics.
#' @param add_pval (`flag`)\cr should `'pval'` (or `'pval_counts'` if `method_groups` contains
#'   `'analyze_vars_counts'`) be added to the statistical methods?
#'
#' @return
#' * `junco_get_stats()` returns a `character` vector of statistical methods.
#'
#' @export
junco_get_stats <- function(
    method_groups = "analyze_vars_numeric",
    stats_in = NULL,
    custom_stats_in = NULL,
    add_pval = FALSE) {
  tern_get_stats(
    method_groups = method_groups,
    stats_in = stats_in,
    custom_stats_in = custom_stats_in,
    add_pval = add_pval,
    tern_defaults = junco_default_stats
  )
}

#' @describeIn default_stats_formats_labels Get formats corresponding to a list of statistics.
#'   To check available defaults see list `junco_default_formats`.
#'
#' @param formats_in (named `vector`)\cr custom formats to use instead of defaults. Can be a character vector with
#'   values from [formatters::list_valid_format_labels()] or custom format functions. Defaults to `NULL` for any rows
#'   with no value is provided.
#'
#' @return
#' * `junco_get_formats_from_stats()` returns a named list of formats as strings or functions.
#'
#' @note Formats in `tern` or `junco` and `rtables` can be functions that take in the table cell value and
#'   return a string. This is well documented in `vignette('custom_appearance', package = 'rtables')`.
#'
#' @export
junco_get_formats_from_stats <- function(stats, formats_in = NULL, levels_per_stats = NULL) {
  tern_get_formats_from_stats(
    stats = stats,
    formats_in = formats_in,
    levels_per_stats = levels_per_stats,
    tern_defaults = junco_default_formats
  )
}

#' @describeIn default_stats_formats_labels Get labels corresponding to a list of statistics.
#'   To check for available defaults see list `junco_default_labels`.
#'
#' @param labels_in (named `character`)\cr custom labels to use instead of defaults. If no value is provided, the
#'   variable level (if rows correspond to levels of a variable) or statistic name will be used as label.
#' @param label_attr_from_stats (named `list`)\cr if `labels_in = NULL`, then this will be used instead. It is a list
#'   of values defined in statistical functions as default labels. Values are ignored if `labels_in` is provided or `''`
#'   values are provided.
#'
#' @return
#' * `junco_get_labels_from_stats()` returns a named list of labels as strings.
#'
#' @export
junco_get_labels_from_stats <- function(
    stats,
    labels_in = NULL,
    levels_per_stats = NULL,
    label_attr_from_stats = NULL) {
  tern_get_labels_from_stats(
    stats = stats,
    labels_in = labels_in,
    levels_per_stats = levels_per_stats,
    label_attr_from_stats = label_attr_from_stats,
    tern_defaults = junco_default_labels
  )
}

#' @describeIn default_stats_formats_labels Get label attributes from statistics list.
#' @param x_stats (`list`)\cr with the statistics results.
#' @keywords internal
get_label_attr_from_stats <- function(x_stats) {
  sapply(x_stats, obj_label)
}

#' @describeIn default_stats_formats_labels Get row indent modifiers corresponding to a list of statistics/rows.
#'
#' @param indents_in (named `integer`)\cr custom row indent modifiers to use instead of defaults. Defaults to `0L` for
#'   all values.
#'
#' @return
#' * `junco_get_indents_from_stats()` returns a named list of indentation modifiers as integers. By default all of the
#'   indentations will be zero.
#'
#' @export
junco_get_indents_from_stats <- function(stats, indents_in = NULL, levels_per_stats = NULL) {
  # For statistics still remaining without default indentation after looking in junco_default_indents, use
  # indentation 0 as default.
  remaining_stats <- setdiff(stats, names(junco_default_indents))
  default_indents <- c(
    junco_default_indents,
    as.list(rep(0L, length(remaining_stats))) |>
      stats::setNames(remaining_stats)
  )

  tern_get_indents_from_stats(
    stats = stats,
    indents_in = indents_in,
    levels_per_stats = levels_per_stats,
    tern_defaults = default_indents
  )
}

#' @describeIn default_stats_formats_labels Format statistics results according to format specifications.
#'
#' @return
#' * `format_stats()` returns the correspondingly formatted [rtables::in_rows()] result.
#'
#' @export
format_stats <- function(x_stats, method_groups, stats_in, formats_in, labels_in, indents_in) {
  .stats <- junco_get_stats(method_groups, stats_in = stats_in)

  .formats <- junco_get_formats_from_stats(stats = .stats, formats_in = formats_in)

  label_attr <- get_label_attr_from_stats(x_stats)
  .labels <- junco_get_labels_from_stats(stats = .stats, labels_in = labels_in, label_attr_from_stats = label_attr)
  .labels <- .unlist_keep_nulls(.labels)

  .indent_mods <- junco_get_indents_from_stats(stats = .stats, indents_in = indents_in)
  .indent_mods <- .unlist_keep_nulls(.indent_mods)

  x_stats <- x_stats[.stats]

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .labels = .labels,
    .indent_mods = .indent_mods
  )
}


# junco_default_stats -----------------------------------------------------------

#' @describeIn default_stats_formats_labels Named list of available statistics by method group for `junco`.
#'
#' @format
#' * `junco_default_stats` is a named list of available statistics, with each element
#'   named for their corresponding statistical method group.
#'
#' @export
junco_default_stats <- list(
  cmhrms = c("pval"),
  coxph_hr = c("n_tot", "n_tot_events", "hr", "hr_ci", "hr_ci_3d", "pvalue", "lr_stat_df"),
  event_free = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci", "event_free_ci"),
  kaplan_meier = c("quantiles_lower", "median_ci_3d", "quantiles_upper", "range_with_cens_info"),
  odds_ratio = c("n_tot", "or_ci", "pval"),
  proportion_diff = c("diff", "diff_ci", "diff_est_ci"),
  relative_risk = c("rel_risk_ci", "pval"),
  summarize_ancova_j = c(
    "n",
    "mean_sd",
    "median",
    "range",
    "quantiles",
    "lsmean_se",
    "lsmean_ci",
    "lsmean_diffci",
    "pval"
  ),
  summarize_mmrm = c(
    "n",
    "adj_mean_se",
    "adj_mean_ci",
    "adj_mean_est_ci",
    "diff_mean_se",
    "diff_mean_ci",
    "diff_mean_est_ci",
    "change",
    "p_value"
  ),
  tabulate_lsmeans = c(
    "n",
    "adj_mean_se",
    "adj_mean_ci",
    "adj_mean_est_ci",
    "diff_mean_se",
    "diff_mean_ci",
    "diff_mean_est_ci",
    "change",
    "p_value"
  ),
  tabulate_rbmi = c(
    "adj_mean_se",
    "adj_mean_ci",
    "diff_mean_se",
    "diff_mean_ci",
    "change",
    "p_value",
    "additional_title_row"
  ),
  test_proportion_diff = c("pval"),
  a_freq_j = c(
    "n_altdf",
    "n_df",
    "n_rowdf",
    "n_parentdf",
    "denom",
    "count",
    "count_unique",
    "count_unique_fraction",
    "count_unique_denom_fraction"
  ),
  a_patyrs_j = c("patyrs"),
  a_eair100_j = c("eair", "n_event", "person_years")
)

# junco_default_formats ---------------------------------------------------------
junco_default_formats_start <- c(
  adj_mean_se = jjcsformat_xx("xx.xxx (xx.xxx)"),
  adj_mean_ci = jjcsformat_xx("(xx.xxx, xx.xxx)"),
  adj_mean_est_ci = jjcsformat_xx("xx.xxx (xx.xxx, xx.xxx)"),
  change = "xx.x%",
  diff = jjcsformat_xx("xx.x"),
  diff_ci = jjcsformat_xx("(xx.x, xx.x)"),
  diff_est_ci = jjcsformat_xx("xx.x (xx.x, xx.x)"),
  diff_mean_se = jjcsformat_xx("xx.xxx (xx.xxx)"),
  diff_mean_ci = jjcsformat_xx("(xx.xxx, xx.xxx)"),
  diff_mean_est_ci = jjcsformat_xx("xx.xxx (xx.xxx, xx.xxx)"),
  event_free_ci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  event_free_rate = jjcsformat_xx("xx.xx"),
  hr = jjcsformat_xx("xx.xx"),
  hr_ci = jjcsformat_xx("(xx.xx, xx.xx)"),
  hr_ci_3d = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  quantiles_upper = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  lsmean = jjcsformat_xx("xx.xx"),
  lsmean_ci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  lsmean_diff = jjcsformat_xx("xx.xx"),
  lsmean_diffci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  lsmean_diff_ci = jjcsformat_xx("(xx.xx, xx.xx)"),
  lsmean_se = jjcsformat_xx("xx.xx (xx.xx)"),
  mean_sd = jjcsformat_xx("xx.xx (xx.xxx)"),
  median = jjcsformat_xx("xx.xx"),
  median_ci = jjcsformat_xx("(xx.xx, xx.xx)"),
  median_ci_3d = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  n = jjcsformat_xx("xx."),
  n_tot = jjcsformat_xx("xx."),
  n_tot_events = jjcsformat_xx("xx."),
  or_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
  pt_at_risk = "xx",
  pval = jjcsformat_pval_fct(0),
  pvalue = jjcsformat_pval_fct(0),
  p_value = jjcsformat_pval_fct(0),
  quantiles = jjcsformat_xx("xx.xx, xx.xx"),
  range = jjcsformat_xx("xx.xx, xx.xx"),
  range_with_cens_info = jjcsformat_range_fct("xx.xx"),
  rate_ci = jjcsformat_xx("(xx.xx, xx.xx)"),
  rate_se = jjcsformat_xx("xx.xx"),
  rel_risk_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
  quantiles_upper = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  n_altdf = "xx",
  n_df = "xx",
  n_rowdf = "xx",
  n_parentdf = "xx",
  denom = "xx",
  count = "xx",
  count_unique = "xx",
  count_unique_fraction = jjcsformat_count_fraction,
  count_unique_denom_fraction = jjcsformat_count_denom_fraction,
  rr_ci_3d = jjcsformat_xx("xx.x (xx.x, xx.x)"),
  patyrs = jjcsformat_xx("xx.x"),
  eair = jjcsformat_xx("xx.x"),
  eair_diff = jjcsformat_xx("xx.xx (xx.xx, xx.xx)"),
  n_event = "xx",
  person_years = jjcsformat_xx("xx.xx"),
  total_subject_years = jjcsformat_xx("xx.x (xx.x)")
)

tern_formats_only <- setdiff(names(tern_default_formats), names(junco_default_formats_start))
#' @describeIn default_stats_formats_labels Named vector of default formats for `junco`.
#'
#' @format
#' * `junco_default_formats` is a named vector of available default formats, with each element
#'   named for their corresponding statistic.
#'
#' @export
junco_default_formats <- c(junco_default_formats_start, tern_default_formats[tern_formats_only])

# junco_default_labels ----------------------------------------------------------
junco_default_labels_start <- c(
  additional_title_row = "Additional Title",
  adj_mean_se = "Adjusted Mean (SE)",
  adj_mean_est_ci = "Adjusted Mean (CI)",
  diff = "Difference in Response rate (%)",
  diff_mean_se = "Difference in Adjusted Means (SE)",
  diff_mean_est_ci = "Difference in Adjusted Means (CI)",
  hr = "Hazard Ratio",
  lr_stat_df = "Log-Rank Chi-Squared",
  mean_sd = "Mean (SD)",
  median = "Median",
  n_tot = "Total n",
  n_tot_events = "Total events",
  pval = "p-value",
  pvalue = "p-value",
  p_value = "p-value",
  range = "Min, max",
  range_with_cens_info = "Min, max",
  n_altdf = "N",
  n_df = "N",
  n_rowdf = "N",
  n_parentdf = "N",
  denom = "N",
  patyrs = "Patient years",
  n_event = "Number of events",
  person_years = "Person years",
  total_subject_years = "Total treatment (subject years)"
)
tern_labels_only <- setdiff(names(tern_default_labels), names(junco_default_labels_start))

#' @describeIn default_stats_formats_labels Named `character` vector of default labels for `junco`.
#'
#' @format
#' * `junco_default_labels` is a named `character` vector of available default labels, with each element
#'   named for their corresponding statistic.
#'
#' @export
junco_default_labels <- c(junco_default_labels_start, tern_default_labels[tern_labels_only])

#' @describeIn default_stats_formats_labels Named `integer` vector of default indents for `junco`.
#'
#' @format
#' * `junco_default_indents` is a named `integer` vector of available default indents, with each element
#'   named for their corresponding statistic. Only indentations different from zero need to be
#'   recorded here.
#'
#' @export
junco_default_indents <- c(
  additional_title_row = 1L,
  adj_mean_ci = 1L,
  adj_mean_est_ci = 1L,
  change = 1L,
  diff_ci = 1L,
  diff_mean_ci = 1L,
  diff_mean_est_ci = 1L,
  hr_ci = 1L,
  or_ci = 1L,
  pval = 1L,
  p_value = 1L,
  rate_se = 1L,
  rate_ci = 1L,
  rel_risk_ci = 1L
)
