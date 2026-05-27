#' Deprecated Cox PH HR statistics and analysis functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions have been superseded by [tern::s_coxph_pairwise()] and
#' [tern::a_coxph_pairwise()], which now support the `alternative` argument
#' (one-sided p-values) via [tern::control_coxph()] and include the
#' `lr_stat_df` statistic.
#'
#' @inheritParams proposal_argument_convention
#'
#' @name coxph_hr
#' @return
#' * `s_coxph_hr` returns a list containing the same statistics returned by
#' [tern::s_coxph_pairwise] and the additional `lr_stat_df` statistic.
#' * `a_coxph_hr` returns a `VerticalRowsSection` object.
#' @order 1
NULL

#' @describeIn coxph_hr `r lifecycle::badge("deprecated")` Use
#'   [tern::s_coxph_pairwise()] instead, passing `alternative` via
#'   [tern::control_coxph()]. The `lr_stat_df` statistic is now also
#'   returned by [tern::s_coxph_pairwise()].
#' @export
#' @order 3
s_coxph_hr <- function(
    df,
    .ref_group,
    .in_ref_col,
    .var,
    is_event,
    strata = NULL,
    control = control_coxph(),
    alternative = c("two.sided", "less", "greater")) {
  lifecycle::deprecate_stop(
    when = "0.1.0",
    what = "s_coxph_hr()",
    with = "tern::s_coxph_pairwise()",
    details = paste(
      "Pass `alternative` via `tern::control_coxph(alternative = ...)` instead of as a direct argument.",
      "The `lr_stat_df` statistic is now included in `tern::s_coxph_pairwise()` output."
    )
  )
}

#' @describeIn coxph_hr `r lifecycle::badge("deprecated")` Use
#'   [tern::a_coxph_pairwise()] instead, and use [tern::coxph_pairwise()] as
#'   the layout-level wrapper.
#' @export
#' @order 2
a_coxph_hr <- function(
    df,
    .var,
    ref_path,
    .spl_context,
    ...,
    .stats = NULL,
    .formats = NULL,
    .labels = NULL,
    .indent_mods = NULL) {
  lifecycle::deprecate_stop(
    when = "0.1.0",
    what = "a_coxph_hr()",
    with = "tern::a_coxph_pairwise()",
    details = "Use `tern::coxph_pairwise()` as the layout-level wrapper."
  )
}
