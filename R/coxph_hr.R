#' @name  coxph_hr
#' @title coxph_hr
#' @description coxph_hr `r lifecycle::badge("deprecated")` Use
#'   `tern:::a_coxph_pairwise()` instead, and use `tern::coxph_pairwise()` as
#'   the layout-level wrapper.
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
    when = "0.1.0",
    what = "a_coxph_hr()",
    with = "tern:::a_coxph_pairwise()",
    details = "Use `tern:::coxph_pairwise()` as the layout-level wrapper."
  )
}
