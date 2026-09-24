#' @name proportion_diff_mf
#'
#' @title Proportion Difference with Method Selection Based on the Mantel-Fleiss
#'   Criterion
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Performs an adaptive analysis of the difference in response proportions
#' between a non-reference group and a reference group for stratified data, with
#' the method selected based on the Mantel-Fleiss (MF) criterion.
#'
#' When the Mantel-Fleiss criterion is satisfied, a Cochran-Mantel-Haenszel
#' (CMH) analysis is performed using the method specified by `mf_method`.
#' When the criterion is not satisfied, an unconditional exact method is used
#' instead.
#'
#' @details
#' The data are prepared by [tern::h_prepare_rsp_table()], which constructs the
#' response vector, group indicator, optional strata variable, and the
#' corresponding 2 x 2 contingency table(s). When multiple strata variables are
#' supplied, the cross-combinations of their levels define the analysis strata.
#'
#' The response variable is converted to a binary response according to `val`.
#' The analysis compares the response proportion in the non-reference group with
#' that in the reference group. The difference is calculated as the
#' non-reference group minus the reference group.
#'
#' When strata are provided and the Mantel-Fleiss criterion is satisfied, the
#' selected CMH method [tern::prop_diff_cmh()] is used to obtain the point
#' estimate and confidence interval for the difference in response proportions.
#' The available Cochran-Mantel-Haenszel-type methods for estimating the
#' difference in response proportions and its confidence interval are:
#'
#' * `"cmh"`: CMH method.
#' * `"cmh_sato"`: CMH method using the Sato variance estimator.
#' * `"cmh_mn"`: CMH method using the Miettinen-Nurminen method.
#'
#' See [tern::prop_diff_cmh()] for more details and references.
#'
#' When strata are not specified, or when strata are specified but the
#' Mantel-Fleiss criterion is not satisfied or cannot be evaluated (e.g., when
#' all strata contain no observed values), the unconditional exact method
#' [tern::prop_diff_uncond_exact()] is used to obtain the point estimate and
#' confidence interval.
#'
#' The Mantel-Fleiss criterion is evaluated using [tern::mantel_fleiss_crit()].
#'
#' The statistics function `s_proportion_diff_mf()` follows the usage and output
#' of [tern::s_proportion_diff()] but removes the `method` argument and selects
#' internally between a Cochran-Mantel-Haenszel-type method and an unconditional
#' exact method based on the Mantel-Fleiss criterion.
#'
#' @inheritParams proposal_argument_convention
#' @param df (`data.frame`) \cr
#'   The data containing all analysis variables for the non-reference group.
#' @param .var (`character(1)`) \cr
#'   The column name in `df` (and, if supplied, `.ref_group`) specifying the
#'   response variable. The response is converted to a logical vector by
#'   comparing its values with `val`. `df[[var]]` (and `.ref_group[[var]]`, if
#'   supplied) must be an atomic vector as defined by
#'   `checkmate::check_atomic_vector()`, of one of the following types:
#'   `logical`, `integer`, `numeric`, or `character`.
#' @param .in_ref_col (`logical(1)` or `NULL`) \cr
#'   Indicates whether the function is being evaluated for the reference
#'   column. Set to `TRUE` for the reference column and to `FALSE` or `NULL`
#'   otherwise.
#'   For the statistics function, `TRUE` or `NULL` results in empty statistics,
#'   while for the analysis function, the result is a `RowsVerticalSection` with
#'   a `NULL` value for all selected statistics.
#' @param .ref_group (`data.frame` or `NULL`) \cr
#'   The data corresponding to the reference group.
#' @param val (`logical(1)` or `integer(1)` or `numeric(1)` or `character(1)`) \cr
#'   The value used to identify the response of interest. An observation is
#'   considered a response of interest when `df[[.var]] == val` (and, if the
#'   the reference group is supplied, `.ref_group[[.var]] == val`).
#'   If `df[[.var]]` is a `factor`, `val` must be a `character` value matching
#'   one of its levels. Otherwise, `val` must have the same class as `df[[.var]]`.
#' @param variables (`list` or `NULL`) \cr
#'   Optional strata variables specified as `list(strata = <strata_vars>)`,
#'   where `<strata_vars>` is a character vector containing column names in
#'   `df` (and, if supplied, `.ref_group`). All specified strata variables
#'   must be factors.
#' @param na.rm (`logical(1)`) \cr
#'   Whether to remove incomplete rows from `df` (and, if supplied, `.ref_group`)
#'   before constructing the response, group, and strata vectors.
#'   Completeness is assessed only for `.var` and the strata columns (if supplied).
#'   If `na.rm = TRUE`, rows containing missing values in any of these columns
#'   are removed. If `na.rm = FALSE`, the function fails if any of these columns
#'   contain missing values, rather than returning results containing `NA`
#'   values.
#'
#'   This argument is passed unchanged to the `complete_cases` argument of
#'   [tern::h_prepare_rsp_table()].
#' @param mf_method (`character(1)`) \cr
#'   The method used for estimating the difference in response proportions and
#'   its confidence interval when strata are provided and the Mantel-Fleiss
#'   criterion is satisfied. Available choices are: `"cmh"`, `"cmh_sato"`, and
#'   `"cmh_mn"`. See **Details** for more information.
#' @param .stats (`character`) \cr
#'   Statistics to select. Available choices are `"diff"`, `"diff_ci"`, and
#'   `"diff_est_ci"`. This parameter affects only the formatted analysis
#'   function and has no effect on the statistics function.
#' @param .formats (named `character` or `list`) \cr
#'   Formats for the statistics.
#'
#' @seealso [cond_proportion_j()]
#' @order 1
#' @author WW
#' @examples
#' n <- 100 # Number of observations.
#' set.seed(123)
#' dta <- data.frame(
#'   "rsp" = sample(c("Y", "N"), n, TRUE),
#'   "grp" = c(
#'     sample(c("A", "Placebo"), round(n - 0.1 * n), TRUE),
#'     rep("C", round(0.1 * n))
#'   ),
#'   "f1" = sample(c("a1", "a2"), n, TRUE),
#'   "f2" = sample(c("x", "y", "z"), n, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
NULL

#' @describeIn proportion_diff_mf Statistics function estimating the difference
#'   in response proportions and its confidence interval.
#'   When strata variables are provided and the Mantel-Fleiss criterion is
#'   satisfied, the CMH method [tern::prop_diff_cmh()] is used, with the
#'   `diff_se` argument determined by `mf_method`. Otherwise, i.e., when strata
#'   variables are not provided, or when strata variables are provided but the
#'   Mantel-Fleiss criterion is not satisfied or cannot be evaluated,
#'   the unconditional exact method [tern::prop_diff_uncond_exact()] is used.
#'
#' @return
#' * `s_proportion_diff_mf()` returns a list containing:
#'  \describe{
#'    \item{`diff` (`numeric(1)` or `numeric(0)`)}{Point estimate of the
#'    difference in response proportions, or `numeric(0)` when the function is
#'    evaluated for the reference column (`.in_ref_col` is NULL or `.in_ref_col`
#'    is `TRUE`).}
#'    \item{`diff_ci` (`numeric(2)` or `numeric(0)`)}{Confidence interval for
#'    the difference in response proportions, or `numeric(0)` when the function
#'    is evaluated for the reference column (`.in_ref_col` is NULL or
#'    `.in_ref_col` is `TRUE`).}
#'    \item{`diff_est_ci` (`numeric(3)` or `numeric(0)`)}{Point estimate and
#'    confidence interval for the difference in response proportions, or
#'    `numeric(0)` when the function is evaluated for the reference column
#'    (`.in_ref_col` is NULL or `.in_ref_col` is `TRUE`).}
#'    \item{`executed_method` (`character(1)`)}{Name of the method actually used
#'    for the analysis. Takes the value `NA_character_` when the function is
#'    evaluated for the reference column (`.in_ref_col` is `NULL` or
#'    `.in_ref_col` is `TRUE`). Otherwise, identifies the method used:
#'    `"cmh"`, `"cmh_sato"`, `"cmh_mn"`, or `"uncond_exact_diff"`.
#'    Formatted analysis functions can use this value to determine whether an
#'    appropriate footnote should be added.}
#'  }
#'
#' @order 2
#' @export
#' @examples
#' prop_d <- s_proportion_diff_mf(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "Placebo"),
#'   .in_ref_col = FALSE,
#'   val = "Y",
#'   variables = list(strata = c("f1", "f2")),
#'   conf_level = 0.90,
#'   mf_method = "cmh_mn"
#' )
#' prop_d
#'
s_proportion_diff_mf <- function(df,
                                 .var = NULL,
                                 .in_ref_col = NULL,
                                 .ref_group = NULL,
                                 val = TRUE,
                                 variables = list(strata = NULL),
                                 na.rm = FALSE,
                                 conf_level = 0.95,
                                 mf_method = c("cmh", "cmh_sato", "cmh_mn")) {
  assert_flag(.in_ref_col, null.ok = TRUE)
  assert_data_frame(.ref_group)
  assert_list(variables)
  assert_flag(na.rm)

  mf_method <- match.arg(mf_method)

  if (is.null(.in_ref_col) || .in_ref_col) {
    y <- list(
      diff = numeric(),
      diff_ci = numeric(),
      diff_est_ci = numeric(),
      executed_method = NA_character_
    )
  } else {
    assert_false(is.null(.ref_group))

    rsp_data <- h_prepare_rsp_table(
      df = df,
      df_ref = .ref_group,
      var = .var,
      val = val,
      strata_vars = variables$strata,
      complete_cases = na.rm
    )

    # Check the Mantel-Fleiss criterion for stratified data.
    is_mf_satisfied <- if (is.null(variables$strata)) {
      warning(
        "No strata variables were supplied; the Mantel-Fleiss criterion ",
        "cannot be checked. Falling back to unconditional exact analysis."
      )
      FALSE
    } else {
      isTRUE(mantel_fleiss_crit(rsp_data$tbl)) # Note: mantel_fleiss_crit() can return NA.
    }

    y <- if (is_mf_satisfied) {
      executed_method <- mf_method
      diff_se <- switch(mf_method,
        cmh = "standard",
        cmh_sato = "sato",
        cmh_mn = "miettinen_nurminen"
      )
      prop_diff_cmh(
        rsp = rsp_data$rsp,
        grp = rsp_data$grp,
        strata = rsp_data$strata,
        conf_level = conf_level,
        diff_se = diff_se
      )
    } else {
      executed_method <- "uncond_exact_diff"
      prop_diff_uncond_exact(
        rsp = rsp_data$rsp, grp = rsp_data$grp, conf_level = conf_level
      )
    }

    assert_subset(c("diff", "diff_ci"), names(y))
    y <- y[c("diff", "diff_ci")]
    y$diff <- setNames(y$diff * 100, paste0("diff_", executed_method))
    y$diff_ci <- setNames(y$diff_ci * 100, paste0("diff_ci_", executed_method, c("_l", "_u")))
    y$diff_est_ci <- c(y$diff, y$diff_ci)
    y$executed_method <- executed_method
  }

  h_set_labels_prop_diff_mf(y, mf_method = mf_method, conf_level = conf_level)
}

#' @describeIn proportion_diff_mf Formatted analysis function used as `afun`
#'   for the difference in response proportions with method selection based on
#'   the Mantel-Fleiss criterion.
#'
#' @param exact_footnote (`character(1)` or `RefFootnote`) \cr
#'   Footnote used to indicate that the unconditional exact method was actually
#'   used for the analysis.
#'
#' @return
#' * `a_proportion_diff_mf()` returns the corresponding `RowsVerticalSection`
#' object with formatted results.
#'
#' @order 3
#' @export
#' @examples
#' lyt <- basic_table() |>
#'   split_cols_by(var = "grp", ref_group = "Placebo") |>
#'   analyze(
#'     vars = "rsp",
#'     afun = a_proportion_diff_mf,
#'     extra_args = list(
#'       val = "Y",
#'       variables = list(strata = c("f1", "f2")),
#'       conf_level = 0.90,
#'       mf_method = "cmh_mn",
#'       .stats = "diff_est_ci"
#'     )
#'   )
#'
#' build_table(lyt, df = dta)
#'
a_proportion_diff_mf <- function(df,
                                 .var,
                                 .in_ref_col = NULL,
                                 .ref_group = NULL,
                                 val = TRUE,
                                 variables = list(strata = NULL),
                                 na.rm = FALSE,
                                 conf_level = 0.95,
                                 mf_method = c("cmh", "cmh_sato", "cmh_mn"),
                                 ...,
                                 .stats = NULL,
                                 .formats = NULL,
                                 .labels = NULL,
                                 .indent_mods = NULL,
                                 exact_footnote = rtables:::RefFootnote(
                                   tern::d_proportion_diff(
                                     method = "uncond_exact_diff", method_only = TRUE
                                   ),
                                   1L, "+"
                                 )) {
  assert_scalar(.var)
  assert_scalar(.in_ref_col, null.ok = TRUE)
  assert_scalar(val)
  assert_scalar(na.rm)
  assert_scalar(conf_level)
  assert_true(
    identical(class(exact_footnote), structure("RefFootnote", package = "rtables")) ||
      test_string(exact_footnote)
  )

  dots_extra_args <- list(...)

  # It supports only default stats, not custom stats.
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_proportion_diff_mf,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      .var = .var,
      .in_ref_col = .in_ref_col,
      .ref_group = list(.ref_group),
      val = val,
      variables = list(variables),
      na.rm = na.rm,
      conf_level = conf_level,
      mf_method = list(mf_method),
      dots_extra_args
    )
  )

  method <- x_stats$executed_method
  x_stats$executed_method <- NULL

  cell_footnotes <- if (identical(method, "uncond_exact_diff")) { # method can be NA.
    fn <- list(list(exact_footnote))
    setNames(rep(fn, length(x_stats)), names(x_stats))
  } else {
    list(NULL)
  }

  format_stats(
    x_stats,
    method_groups = "proportion_diff",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods,
    .cell_footnotes = cell_footnotes
  )
}

#' @title Helper function to set labels for `s_proportion_diff_mf()` statistics
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets descriptive labels on the statistics returned by `s_proportion_diff_mf()`.
#'
#' The label includes the detailed names of the two methods that may be selected
#' according to the Mantel-Fleiss criterion: `mf_method` is selected when the
#' criterion is satisfied, while `non_mf_method` is selected when the criterion
#' is not satisfied or cannot be evaluated.
#'
#' Specifically, the method part of the label has the form
#' `"(mf_method_label / non_mf_method_label)"`, where `mf_method_label` and
#' `non_mf_method_label` are the corresponding detailed method names.
#'
#' @param y (`list`) \cr
#'   The list of statistics returned by `s_proportion_diff_mf()` for which
#'   labels should be set. Must contain the named elements `"diff"`, `"diff_ci"`,
#'   and `"diff_est_ci"`.
#' @param mf_method (`character(1)`) \cr
#'   The method specified for the Mantel-Fleiss-based analysis.
#'   Available choices are those accepted by the `method` argument of
#'   [tern::d_proportion_diff()].
#' @param non_mf_method (`character(1)`) \cr
#'   The method used when the Mantel-Fleiss criterion is not satisfied or cannot
#'   be evaluated.
#'   Available choices are those accepted by the method argument of
#'   [tern::d_proportion_diff()].
#' @param conf_level (`numeric(1)`) \cr
#'   The confidence level used for the confidence interval.
#'
#' @return
#'   The input `y` with descriptive label attributes added to the `"diff"`,
#'   `"diff_ci"`, and `"diff_est_ci"` elements.
#'
#' @keywords internal
h_set_labels_prop_diff_mf <- function(y,
                                      mf_method,
                                      non_mf_method = "uncond_exact_diff",
                                      conf_level) {
  assert_list(y)
  assert_subset(c("diff", "diff_ci", "diff_est_ci"), choices = names(y))

  label_prefix <- "Difference in Response rate (%)"
  mf_method_label <- d_proportion_diff(method = mf_method, method_only = TRUE)
  non_mf_method_label <- d_proportion_diff(method = non_mf_method, method_only = TRUE)
  method_label <- paste0("(", mf_method_label, " / ", non_mf_method_label, ")")

  # Set labels.
  attr(y$diff, "label") <- paste(label_prefix, method_label)
  attr(y$diff_ci, "label") <- paste(
    label_prefix, tern::f_conf_level(conf_level), method_label
  )
  attr(y$diff_est_ci, "label") <- paste(
    label_prefix, "and", tern::f_conf_level(conf_level), method_label
  )
  y
}
