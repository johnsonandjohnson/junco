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
    when = "0.1.7",
    what = "a_coxph_hr()",
    with = "tern:::a_coxph_pairwise()",
    details = "Use `tern:::coxph_pairwise()` as the layout-level wrapper."
  )
}

#' @name  s_coxph_hr
#' @title s_coxph_hr
#' @description s_coxph_hr `r lifecycle::badge("deprecated")` Use
#'  `tern::s_coxph_pairwise`  instead, and use `tern::coxph_pairwise()` as
#'   the layout-level wrapper.
#' @inheritParams odds_ratio
#' @keywords internal
s_coxph_hr <- function(
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
    when = "0.1.7",
    what = "s_coxph_hr()",
    with = "tern:::s_coxph_pairwise()",
    details = "Use `tern:::coxph_pairwise()` as the layout-level wrapper."
  )
}

#' @title s_kaplan_meier
#' @description `r lifecycle::badge("deprecated")` Use [tern::s_surv_time()] instead.
#' @keywords internal
s_kaplan_meier <- function(df, .var, is_event, control = control_surv_time()) {
  lifecycle::deprecate_stop(
    when = "0.1.7",
    what = "s_kaplan_meier()",
    with = "tern::s_surv_time()",
    details = paste0(
      "Note: the label for `range_with_cens_info` has changed from ",
      "'Min, max' (junco) to 'Min - Max (with censoring)' (tern)."
    )
  )
}

#' @title a_kaplan_meier
#' @description `r lifecycle::badge("deprecated")` Use [tern::a_surv_time()] instead.
#' @export
a_kaplan_meier <- function(df, .var, ..., .stats = NULL, .formats = NULL, .labels = NULL, .indent_mods = NULL) {
  lifecycle::deprecate_stop(
    when = "0.1.7",
    what = "a_kaplan_meier()",
    with = "tern::a_surv_time()",
    details = paste0(
      "Note: the label for `range_with_cens_info` has changed from ",
      "'Min, max' (junco) to 'Min - Max (with censoring)' (tern)."
    )
  )
}

