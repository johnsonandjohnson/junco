#' @name diff_means
#'
#' @title Difference in Means
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes estimates and associated inferential statistics for the estimand
#' defined as the difference in population means between two distributions.
#'
#' The following quantities are available:
#' \describe{
#'   \item{diff_means_n1}{Sample size of Group 1.}
#'   \item{diff_means_n2}{Sample size of Group 2.}
#'   \item{diff_means_est}{Point estimate of the difference in population means.}
#'   \item{diff_means_se}{Standard error of the estimator of the difference in population means.}
#'   \item{diff_means_est_se}{The point estimate and the standard error.}
#'   \item{diff_means_ci}{Confidence interval for the difference in population means.}
#'   \item{diff_means_est_ci}{The point estimate and the confidence interval.}
#' }
#'
#' @details
#' Supports both non-paired and paired samples. For non-paired samples, the
#' estimator is defined as the difference between sample means. For paired
#' samples, the estimator is defined as the mean of within-pair differences.
#'
#' Inferential statistics, including standard errors and confidence intervals,
#' are obtained using a t-distribution framework via the internal
#' `safe_t_test()` function.
#'
#' It supports only numeric variables.
#' Only complete (non-missing) observations are used in statistical computations.
#' Non-finite values (i.e., `Inf`, `-Inf`) are not allowed.
#'
#' @inheritParams proposal_argument_convention
#' @param .var (`character(1)`)\cr Name of the column in the dataset(s)
#'   containing observed sample values.
#' @param ... Additional named arguments passed to `s_diff_means()` and
#'   `safe_t_test()`.
NULL

#' @describeIn diff_means Statistical function that computes estimates of the
#' difference in means.
#' The first sample is taken from `df1[[.var]]` and the second from
#' `df2[[.var]]`.
#'
#' If `paired = FALSE`, missing values are removed separately from each sample
#' prior to statistical computations.
#' If `paired = TRUE`, observations are matched using `paired_by` prior to
#' statistical analysis, and only complete pairs are used.
#'
#' Data extraction, alignment, and missing-value handling are delegated to the
#' internal `extract_vectors()` function, which prepares cleaned numeric
#' vectors for both paired and unpaired settings.
#'
#' Inferential statistics are then computed using `safe_t_test()` applied to
#' the processed vectors.
#'
#' @param df1 (`data.frame`)\cr Dataset for the first sample.
#' @param df2 (`data.frame`)\cr Dataset for the second sample.
#' @param paired (`logical(1)`)\cr Whether the samples are paired.
#' @param paired_by (`character` or `NULL`)\cr Column name(s) in `df1` and `df2`
#'   used to match observations between datasets.
#'   Required when `paired = TRUE` and must uniquely identify each pair in both
#'   datasets.
#'
#' @return
#' * `s_diff_means()` returns a named `list` containing the quantities described
#' in the Description section.
#'
#' @seealso `safe_t_test()`, `extract_vectors()`
#'
#' @importFrom tern f_conf_level
#' @importFrom formatters with_label
#'
#' @export
#'
#' @examples
#'
#' ## Statistics function:
#'
#' df1 <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
#' df2 <- data.frame(id = c("A", "C", "D", "E", "F"), value = c(3:1, NA, 16))
#'
#' # Unpaired samples.
#' s_diff_means(df1, df2, "value", conf.level = .8)
#'
#' # Paired samples.
#' s_diff_means(df1, df2, "value", paired = TRUE, paired_by = "id")
#'
s_diff_means <- function(df1,
                         df2,
                         .var,
                         paired = FALSE,
                         paired_by = NULL,
                         ...) {
  checkmate::assert_data_frame(df1)
  checkmate::assert_data_frame(df2)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df1), must.include = .var)
  checkmate::assert_names(colnames(df2), must.include = .var)
  checkmate::assert_numeric(df1[[.var]], finite = TRUE)
  checkmate::assert_numeric(df2[[.var]], finite = TRUE)
  checkmate::assert_flag(paired, null.ok = FALSE)
  if (paired) {
    checkmate::assert_character(paired_by)
    checkmate::assert_names(colnames(df1), must.include = paired_by)
    checkmate::assert_names(colnames(df2), must.include = paired_by)
  }

  vecs <- extract_vectors(df1, df2, .var, paired = paired, paired_by = paired_by)
  n1 <- length(vecs$x1)
  n2 <- length(vecs$x2)

  # Get estimates.
  ttest_res <- safe_t_test(vecs$x1, vecs$x2, paired = paired, ...)
  est <- if (paired) {
    ttest_res$estimate
  } else {
    ttest_res$estimate[1] - ttest_res$estimate[2]
  }
  ci <- ttest_res$conf.int
  se <- ttest_res$stderr
  conf.level <- attr(ci, "conf.level")

  names(n1) <- "diff_means_n1"
  names(n2) <- "diff_means_n2"
  names(est) <- "diff_means_est"
  names(ci) <- c("diff_means_ci_lwr", "diff_means_ci_upr")
  names(se) <- "diff_means_se"
  est_ci <- c(est, ci)
  attr(est_ci, "conf.level") <- conf.level

  label <- "Difference in Means"
  cl <- tern::f_conf_level(conf.level)

  y <- list()
  y$diff_means_n1 <- formatters::with_label(n1, paste(label, "Sample Size (Group 1)"))
  y$diff_means_n2 <- formatters::with_label(n2, paste(label, "Sample Size (Group 2)"))
  y$diff_means_est <- formatters::with_label(est, label)
  y$diff_means_se <- formatters::with_label(se, paste(label, "SE"))
  y$diff_means_est_se <- formatters::with_label(c(est, se), paste(label, "(SE)"))
  y$diff_means_ci <- formatters::with_label(ci, paste(label, cl))
  y$diff_means_est_ci <- formatters::with_label(est_ci, paste0(label, " (", cl, ")"))

  y
}

#' @describeIn diff_means Formatted analysis function which is used as `afun` in
#' [rtables::analyze()].
#'
#' The sample from the first distribution is `df[[.var]]`, and the sample from
#' the second distribution is `.ref_group[[.var]]`, where `.ref_group` is
#' computed by [get_ref_info()] using `.spl_context` and `ref_path`.
#'
#' If `ref_path = NULL`, then `a_diff_means()` returns an [rtables::in_rows()]
#' result with an empty `formatted_cell` for every statistic specified in
#' `.stats`.
#'
#' @param df (`data.frame`)\cr Dataset for the first sample.
#' @param ref_path (`character`)\cr Global reference group specification; see
#'   [get_ref_info()]. This specifies the column path for the second sample.
#'   Specifically, the second sample is `.ref_group[[.var]]`, where
#'   `.ref_group` is computed by [get_ref_info()] using `.spl_context` and
#'   `ref_path`.
#'
#'   This is a workaround for an [rtables] limitation in supporting
#'   `.ref_group` / `.in_ref_col` in the presence of stacked headers.
#'   Normally, `.ref_group` and `.in_ref_col` are provided by the [rtables]
#'   tabulation framework when the current column split is within the reference
#'   variable.
#' @param .stats (`character`)\cr Statistics to include in the table.
#'
#'   Available options are:
#'   ``r shQuote(junco_get_stats("diff_means"), type = "sh")``
#'
#'   If `NULL`, all statistics are included.
#'
#' @return
#' * `a_diff_means()` returns formatted [rtables::in_rows()] result with the
#' quantities requested in `.stats`.
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#'
#' ## Analysis  function:
#'
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_rows_by(
#'     "PARAMCD",
#'     labels_var = "PARAM",
#'     split_fun = keep_split_levels(c("SYSBP", "DIABP"))
#'   ) |>
#'   split_rows_by(
#'     "AVISIT",
#'     split_fun = keep_split_levels(c("BASELINE", "WEEK 1 DAY 8"))
#'   ) |>
#'   analyze(
#'     "AVAL",
#'     afun = tern::a_summary,
#'     extra_args = list(.stats = c("n", "mean")),
#'     show_labels = "hidden"
#'   ) |>
#'   analyze(
#'     "AVAL",
#'     afun = a_diff_means,
#'     table_names = "aval_diff",
#'     extra_args = list(
#'       .stats = "diff_means_est_ci",
#'       ref_path = c("ARM", "B: Placebo")
#'     ),
#'     show_labels = "hidden"
#'   )
#'
#' tbl <- build_table(lyt, formatters::ex_advs)
#' tbl
#'
a_diff_means <- function(
    df,
    .var,
    .spl_context,
    ref_path = NULL,
    ...,
    .stats = NULL,
    .formats = NULL,
    .labels = NULL,
    .indent_mods = NULL) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_data_frame(.spl_context)
  checkmate::assert_character(ref_path, min.len = 1L, null.ok = TRUE)
  checkmate::assert_character(.stats, null.ok = TRUE)
  if (!is.null(.stats)) {
    checkmate::assert_names(.stats, subset.of = junco_get_stats("diff_means"))
  }

  ref <- get_ref_info(ref_path, .spl_context)
  .ref_group <- ref$ref_group
  .in_ref_col <- ref$in_ref_col

  y <- if (is.null(.ref_group) || is.null(.in_ref_col) || .in_ref_col) {
    .stats <- junco_get_stats("diff_means", stats_in = .stats)
    setNames(vector(mode = "list", length = length(.stats)), .stats)
  } else {
    s_diff_means(df1 = df, df2 = .ref_group, .var = .var, ...)
  }

  format_stats(
    y,
    method_groups = "diff_means",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
