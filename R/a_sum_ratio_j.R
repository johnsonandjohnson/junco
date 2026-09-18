#' @title Analysis Function: Sum and Ratio of Sums
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes one or more of the following statistics for use with
#' [rtables::analyze()]:
#' - `"sum"`: `sum(.var)`
#' - `"sum_unique"`: `sum(.var)` after keeping unique rows by `id_var`
#' - `"ratio"`: `sum(.var) / sum(denom_by)`
#' - `"ratio_unique"`: same ratio after keeping unique rows by `id_var`
#'
#' Both `.var` and `denom_by` columns must be numeric (integer, double, or
#' logical coerced to numeric).
#'
#' @inheritParams proposal_argument_convention
#' @param .stats (`character`)\cr one or more of `"sum"`, `"sum_unique"`,
#'   `"ratio"`, `"ratio_unique"`. Default: `"sum"`.
#' @param denom_by (`string` or `NULL`)\cr denominator column; required for
#'   `"ratio"` and `"ratio_unique"`.
#' @param id_var (`string` or `NULL`)\cr subject-id column for unique row selection;
#'   required for `"sum_unique"` and `"ratio_unique"`.
#' @param .indent_mods (named `integer` or `NULL`)\cr indent modifiers for the
#'   row labels. Defaults to `NULL` (no modification).
#' @param na_str (`character` or `NULL`)\cr string(s) used to replace `NA`
#'   values in the formatted output. Defaults to `NULL` (no replacement).
#'
#' @return A `RowsVerticalSection` for use by rtables.
#'
#' @author VR
#'
#' @importFrom rtables in_rows
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = c("S01", "S01", "S02", "S03"),
#'   ARM     = factor(c("A", "A", "A", "B")),
#'   EVENTS  = c(1, 1, 0, 1),
#'   DAYS    = c(10, 10, 20, 15)
#' )
#'
#' # All 4 stats in a layout (S01 duplicate tests unique logic)
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   analyze(
#'     "EVENTS",
#'     afun = a_sum_ratio_j,
#'     extra_args = list(
#'       .stats   = c("sum", "sum_unique", "ratio", "ratio_unique"),
#'       denom_by = "DAYS",
#'       id_var   = "USUBJID"
#'     )
#'   )
#'
#' build_table(lyt, df)
a_sum_ratio_j <- function(
  df,
  .var,
  .stats = "sum",
  denom_by = NULL,
  id_var = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = NULL,
  ...
) {
  # Data Validation ------------------------------------------------------------
  valid_stats <- c("sum", "sum_unique", "ratio", "ratio_unique")

  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df), must.include = .var)
  checkmate::assert_numeric(df[[.var]], .var.name = .var)
  checkmate::assert_subset(.stats, choices = valid_stats)

  needs_ratio <- any(.stats %in% c("ratio", "ratio_unique"))
  needs_unique <- any(.stats %in% c("sum_unique", "ratio_unique"))

  if (needs_ratio) {
    checkmate::assert_string(denom_by)
    checkmate::assert_names(colnames(df), must.include = denom_by)
    checkmate::assert_numeric(df[[denom_by]], .var.name = denom_by)
  }
  if (needs_unique) {
    checkmate::assert_string(id_var)
    checkmate::assert_names(colnames(df), must.include = id_var)
  }

  # Helper funcs -----------------------------------------------
  sum_plain <- function(v) {
    sum(df[[v]], na.rm = TRUE)
  }

  sum_unique <- function(v) {
    d <- unique(df[!is.na(df[[v]]), c(id_var, v), drop = FALSE])
    sum(d[[v]], na.rm = TRUE)
  }

  safe_ratio <- function(n, d) {
    if (is.na(d) || d == 0) {
      c(n, NA_real_)
    } else {
      c(n, n / d)
    }
  }

  # Default formats and labels -------------------------------------------------
  default_fmt <- c(
    sum = "xx",
    sum_unique = "xx",
    ratio = "xx (xx.x%)",
    ratio_unique = "xx (xx.x%)"
  )
  default_lbl <- c(
    sum = "Sum",
    sum_unique = "Sum (unique)",
    ratio = "Ratio",
    ratio_unique = "Ratio (unique)"
  )

  # Compute stats ---------------------------------------------------------------
  n_plain <- if (any(.stats %in% c("sum", "ratio"))) {
    sum_plain(.var)
  } else {
    NULL
  }
  n_unique <- if (any(.stats %in% c("sum_unique", "ratio_unique"))) {
    sum_unique(.var)
  } else {
    NULL
  }
  d_plain <- if (any(.stats %in% c("ratio")) && !is.null(denom_by)) {
    sum_plain(denom_by)
  } else {
    NULL
  }
  d_unique <- if (any(.stats %in% c("ratio_unique")) && !is.null(denom_by)) {
    sum_unique(denom_by)
  } else {
    NULL
  }

  # Build result list ---------------------------------------------------------------
  x_stats <- list()
  if ("sum" %in% .stats) {
    x_stats[["sum"]] <- n_plain
  }
  if ("sum_unique" %in% .stats) {
    x_stats[["sum_unique"]] <- n_unique
  }
  if ("ratio" %in% .stats) {
    x_stats[["ratio"]] <- safe_ratio(n_plain, d_plain)
  }
  if ("ratio_unique" %in% .stats) {
    x_stats[["ratio_unique"]] <- safe_ratio(n_unique, d_unique)
  }

  # Resolve formats and labels ------------------------------------------
  fmts <- default_fmt[names(x_stats)]
  labels <- default_lbl[names(x_stats)]
  if (!is.null(.formats)) {
    fmts[names(.formats)] <- .formats
  }
  if (!is.null(.labels)) {
    labels[names(.labels)] <- .labels
  }

  .format_na_strs <- if (!is.null(na_str)) {
    lapply(names(fmts), function(x) na_str)
  } else {
    NULL
  }

  in_rows(
    .list = x_stats,
    .formats = as.list(fmts),
    .names = labels,
    .labels = labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )
}
