#' @name a_summary_j
#'
#' @title Wrappers Around `tern::a_summary()` with Row Split Exclusion
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These wrappers around [tern::a_summary()] allow selected row split levels to
#' be excluded from producing the analysis.
#'
#' @details
#' [tern::a_summary()] is a formatted analysis function that computes summary
#' statistics for a single variable. In typical use within the [rtables]
#' framework, the analysis variable is first extracted from a data frame and
#' then passed to [tern::a_summary()] as a standalone vector.
#'
#' `a_summary_j_with_exclude()` extends this behavior by:
#' - accepting a data frame (`df`) as input,
#' - checking whether the current row split path should be excluded,
#' - returning `NULL` for excluded row split levels,
#' - extracting the analysis variable `.var` for all other row split levels,
#' - and passing the resulting vector to [tern::a_summary()].
#'
#' @inheritParams proposal_argument_convention
#' @param ... Additional arguments passed to [tern::a_summary()], excluding
#'   the `x` argument, which is constructed internally from `df[[.var]]`.
#'
#' @return
#' Returns the same type of object as [tern::a_summary()], or `NULL` when the
#' current row split path is excluded by `exclude_levels`.
#'
#' @seealso
#' [tern::a_summary()], [do_exclude_split()]
#'
NULL

#' @describeIn a_summary_j Analysis function which can exclude row split levels
#'   from producing the analysis. These have to be specified in the
#'   `exclude_levels` argument, see `?do_exclude_split` for details.
#'
#' @importFrom tern a_summary
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = 1:8,
#'   AVISIT = factor(rep(c("Baseline", "Week 1"), 4)),
#'   ARM = factor(rep(c("A", "B"), each = 4)),
#'   AVAL = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
#' )
#'
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_rows_by("AVISIT") |>
#'   analyze(
#'     "AVAL",
#'     afun = a_summary_j_with_exclude,
#'     extra_args = list(exclude_levels = list(AVISIT = "Baseline"))
#'   )
#'
#' build_table(lyt, df)
a_summary_j_with_exclude <- function(
  df,
  .var,
  exclude_levels,
  .spl_context,
  ...
) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df), must.include = .var)

  if (do_exclude_split(exclude_levels, .spl_context)) {
    NULL
  } else {
    tern::a_summary(df[[.var]], ...)
  }
}
