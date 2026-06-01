#' @name  a_coxph_hr
#' @title a_coxph_hr
#' @description a_coxph_hr `r lifecycle::badge("deprecated")` Use
#'   tern:::a_coxph_pairwise()` instead, and use `tern::coxph_pairwise()` as
#'   the layout-level wrapper.
#' @inheritParams odds_ratio
#' @export
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
    when = "0.1.6",
    what = "a_coxph_hr()",
    with = "tern:::a_coxph_pairwise()",
    details = "Use `tern:::coxph_pairwise()` as the layout-level wrapper."
  )
}

#' @name  coxph_hr
#' @title coxph_hr
#' @description coxph_hr `r lifecycle::badge("deprecated")` Use
#'  `tern::s_coxph_pairwise`  instead, and use `tern::coxph_pairwise()` as
#'   the layout-level wrapper.
#' @inheritParams odds_ratio
#' @export
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
    when = "0.1.6",
    what = "s_coxph_hr()",
    with = "tern:::s_coxph_pairwise()",
    details = "Use `tern:::coxph_pairwise()` as the layout-level wrapper."
  )
}

