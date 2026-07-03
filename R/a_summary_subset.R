#' @name summary_subset
#'
#' @title Wrappers Around `tern::a_summary()` with Optional Subsetting and Labels
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These wrappers around [tern::a_summary()] allow optional row subsetting
#' using a logical expression evaluated in the context of the input data frame.
#'
#' `a_summary_subset()` is an [rtables] analysis function (see
#' [rtables::analyze()]), while `c_summary_subset_label()` is a content row
#' function (see [rtables::summarize_row_groups()]) that may optionally
#' append a label to the resulting content row.
#'
#' @details
#' [tern::a_summary()] is a formatted analysis function that computes summary
#' statistics for a single variable. In typical use within the [rtables]
#' framework, the analysis variable is first extracted from a data frame and
#' then passed to [tern::a_summary()] as a standalone vector.
#'
#' The `a_summary_subset()` and `c_summary_subset_label()` wrappers extend
#' this behavior by:
#' - accepting a data frame (`df`) as input,
#' - optionally subsetting rows using a logical expression,
#' - extracting the analysis variable `.var` after subsetting,
#' - and passing the resulting vector to [tern::a_summary()].
#'
#' @inheritParams proposal_argument_convention
#' @param filter_expr (`expression`)
#'   A single logical expression wrapped in `expression(...)` used to subset
#'   rows of `df` before summarization. Exactly one expression is allowed.
#'   The expression is evaluated using [eval()] in an environment containing
#'   the columns of `df`, meaning that all column names are available as
#'   variables.
#'
#'   In the current implementation, variables used in the expression can only
#'   be column names of `df`. That is, the following is not supported:
#'   `min_age <- 45;`
#'   `a_summary_subset(df, "AVAL", expression(AGE > min_age))`,
#'   given that `AVAL` is a column in `df` and `min_age` is not a column in `df`.
#'
#'   The expression must evaluate to a logical vector whose length is exactly
#'   equal to `nrow(df)`. No recycling is performed.
#'   In particular, scalar logical values such as `expression(TRUE)` or
#'   `expression(FALSE)` are not allowed when `nrow(df) > 1`.
#' @param ... Additional arguments passed to [tern::a_summary()], excluding
#'   the `x` argument, which is constructed internally from `df[[.var]]`
#'   after applying the filter.
#'
#' @return
#' Returns the same type of object as [tern::a_summary()].
#'
#' @seealso
#' [tern::a_summary()]
#'
NULL

#' @describeIn summary_subset Analysis function with optional subsetting.
#' @importFrom tern a_summary
#' @export
#' @examples
#'
#' ## Analysis function:
#'
#' df <- data.frame(
#'   ID = 1:8,
#'   AGE = c(34, NA, 52, 45, 50, NA, 41, 29),
#'   AVAL = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
#' )
#' df
#'
#' stats <- c("n", "sum")
#'
#' tern::a_summary(df$AVAL, .stats = stats)
#'
#' a_summary_subset(df, "AVAL", expression(!is.na(AGE)), .stats = stats)
#' a_summary_subset(df, "AVAL", expression(AGE > 30 & AVAL > 0), .stats = stats)
#'
a_summary_subset <- function(df, .var, filter_expr, ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df), must.include = .var)
  checkmate::assert_class(filter_expr, "expression")
  checkmate::assert_true(length(filter_expr) == 1)

  row_mask <- eval(filter_expr[[1]], envir = df)
  checkmate::assert_logical(row_mask, len = nrow(df))
  x <- df[row_mask, .var, drop = TRUE]

  tern::a_summary(x, ...)
}

#' @describeIn summary_subset Content row function with optional subsetting
#' and an optional label prepended to the resulting content row using
#' [prepend_label_cell()].
#'
#' @inheritParams prepend_label_cell
#'
#' @export
#' @examples
#'
#' ## Content row function:
#'
#' df <- data.frame(
#'   USUBJID = rep(1:6, each = 2),
#'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32),
#'   ABLFL = rep(c(TRUE, FALSE), 6),
#'   BASE = rep(c(1, 2, 13, 15, 43, 24), each = 2),
#'   CHG = c(0, 2, 0, 7, 0, 6, 0, 8, 0, 13, 0, 8)
#' )
#' df
#'
#' c_summary_subset_label(
#'   df = df,
#'   .var = "BASE",
#'   filter_expr = expression(ABLFL),
#'   label = "Baseline",
#'   .stats = c("n", "mean_sd")
#' )
c_summary_subset_label <- function(df,
                                   labelstr,
                                   .var,
                                   filter_expr,
                                   label = "",
                                   label_indent_mod = 0L,
                                   ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df), must.include = .var)
  checkmate::assert_class(filter_expr, "expression")
  checkmate::assert_true(length(filter_expr) == 1)
  checkmate::assert_string(label, na.ok = TRUE)
  checkmate::assert_int(label_indent_mod)

  y <- a_summary_subset(df = df, .var = .var, filter_expr = filter_expr, ...)
  prepend_label_cell(y, label = label, label_indent_mod = label_indent_mod)
}
