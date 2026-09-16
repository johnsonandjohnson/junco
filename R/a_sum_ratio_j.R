# TODO: move it to junco.
# Helper functions:
#' @title Analysis function: sum and ratio of sums (tern-style)
#'
#' @description
#' Computes one or more of the following statistics for use with [rtables::analyze()]:
#' - `"sum"`: `sum(.var)`
#' - `"sum_unique"`: `sum(.var)` after deduplicating rows by `id_var`
#' - `"ratio"`: `sum(.var) / sum(denom_by)`
#' - `"ratio_unique"`: same ratio after deduplicating rows by `id_var`
#'
#' @param df (`data.frame`) analysis data for the current table cell.
#' @param .var (`string`) name of the numerator column.
#' @param .stats (`character`) one or more of `"sum"`, `"sum_unique"`,
#'   `"ratio"`, `"ratio_unique"`. Default: `"sum"`.
#' @param denom_by (`string` or `NULL`) denominator column; required for
#'   `"ratio"` and `"ratio_unique"`.
#' @param id_var (`string` or `NULL`) subject-id column for deduplication;
#'   required for `"sum_unique"` and `"ratio_unique"`.
#' @param .formats (`named character` or `NULL`) format string per stat name.
#'   Defaults: `"xx"` for sum stats, `"xx (xx.x%)"` for ratio stats.
#' @param .labels (`named character` or `NULL`) row label per stat name.
#'   Defaults to the stat name itself.
#'
#' @return A `RowsVerticalSection` for use by rtables.
#'
a_sum_ratio_j <- function(
  df,
  .var,
  .stats = "sum",
  denom_by = NULL,
  id_var = NULL,
  .formats = NULL,
  .labels = NULL,
  ...
) {
  # test
  valid_stats <- c("sum", "sum_unique", "ratio", "ratio_unique")

  # Validation ---
  stopifnot(
    is.character(.var),
    length(.var) == 1,
    .var %in% names(df),
    all(.stats %in% valid_stats)
  )
  if (any(.stats %in% c("ratio", "ratio_unique"))) {
    stopifnot(!is.null(denom_by), denom_by %in% names(df))
  }
  if (any(.stats %in% c("sum_unique", "ratio_unique"))) {
    stopifnot(!is.null(id_var), id_var %in% names(df))
  }

  # Helper functions (NAs always removed) ---
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

  # Default formats and labels ---
  default_fmt <- c(
    sum = "xx",
    sum_unique = "xx",
    ratio = "xx (xx.x%)",
    ratio_unique = "xx (xx.x%)"
  )
  default_lbl <- c(
    sum = "sum",
    sum_unique = "sum (unique)",
    ratio = "ratio",
    ratio_unique = "ratio (unique)"
  )

  # Compute only what is needed (DRY: each block computed once) ---
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

  # Build result list ---
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

  # Resolve formats and labels (caller overrides defaults, NULL-safe) ---
  fmts <- default_fmt[names(x_stats)]
  labels <- default_lbl[names(x_stats)]
  if (!is.null(.formats)) {
    fmts[names(.formats)] <- .formats
  }
  if (!is.null(.labels)) {
    labels[names(.labels)] <- .labels
  }

  in_rows(
    .list = x_stats,
    .formats = as.list(fmts),
    .names = labels,
    .labels = labels
  )
}
