#' @title Wrapper around `tern::a_summary()` with junco-specific defaults
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function wraps [tern::a_summary()] and applies junco-specific defaults
#' for formatting-related arguments when they are not explicitly provided by the user.
#'
#' In particular, default values are generated for:
#' \itemize{
#'   \item `.labels` via [junco_get_labels_from_stats()]
#'   \item `.formats` via [junco_get_formats_from_stats()]
#'   \item `.indent_mods` via [junco_get_indents_from_stats()]
#' }
#'
#' If `.stats` is not provided or is `NULL`, the default statistics from
#' [tern::get_stats()] are used.
#'
#' @details
#' User-supplied values for `.labels`, `.formats`, and `.indent_mods` are used
#' as-is and only completed where needed by the corresponding junco helper
#' functions. No modification is performed if these arguments are fully specified.
#'
#' @inheritParams tern::a_summary
#'
#' @return
#' Returns the same type of output as [tern::a_summary()], with optional
#' junco-based default formatting applied.
#'
#' @seealso [tern::a_summary()], [tern::get_stats()]
#'
#' @importFrom tern a_summary get_stats
#'
#' @export
#'
#' @examples
#' .stats <- c("n", "mean_sd", "median_range")
#' tern::a_summary(1:10, .stats = .stats)
#' a_summary_j(1:10, .stats = .stats)
#' a_summary_j(1:10, .stats = .stats, .formats = c(mean_sd = "xx (xx.x)"))
#' a_summary_j(1:10)
#'
a_summary_j <- function(x,
                        ...,
                        .stats = NULL,
                        .stat_names = NULL,
                        .formats = NULL,
                        .labels = NULL,
                        .indent_mods = NULL) {
  # Arguments for a_summary.
  argsas <- list(...)

  if (is.null(.stats)) {
    .stats <- tern::get_stats()
  }
  .formats <- junco_get_formats_from_stats(.stats, formats_in = .formats)
  .labels <- junco_get_labels_from_stats(.stats, labels_in = .labels)
  .indent_mods <- junco_get_indents_from_stats(.stats, indents_in = .indent_mods)

  tern::a_summary(
    x = x,
    ...,
    .stats = .stats,
    .stat_names = .stat_names,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
}

#' @title Summary Statistics for Filtered Data with Label
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A wrapper around [a_summary_j()] that filters the data prior to execution
#' and prepends a label to the resulting summary statistics object.
#'
#' @details
#' The function first applies row filtering via [filter_df_prior_afun()],
#' then computes summary statistics using [a_summary_j()], and finally
#' attaches a label to the resulting object.
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams filter_df_prior_afun
#' @param label (`string` or `function`)\cr A label to be added to the output.
#'   If a function is provided, it must accept a single argument `.spl_context`
#'   and return a character string.
#' @inheritParams prepend_label_cell
#'
#' @param ... Additional arguments passed to [a_summary_j()].
#'
#' @returns
#' An object returned by [a_summary_j()] with an additional label applied.
#'
#' @export
#'
#' @examples
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
#'   .var = "CHG",
#'   subset_expr = expression(ABLFL),
#'   label = "Change from Baseline",
#'   .stats = c("n", "mean_sd")
#' )
c_summary_subset_label <- function(df,
                                   labelstr,
                                   .var,
                                   .spl_context,
                                   subset_expr,
                                   label,
                                   label_indent_mod = 0L,
                                   ...) {
  checkmate::assert_true(
    checkmate::test_function(label) || checkmate::test_string(label, na.ok = TRUE, null.ok = TRUE)
  )

  y <- filter_df_prior_afun(
    df = df, .var = .var, afun = a_summary_j, subset_expr = subset_expr, ...
  )

  if (is.function(label)) {
    checkmate::assert_true(length(formals(label)) >= 1L)
    label <- label(.spl_context)
  }

  prepend_label_cell(y, label = label, label_indent_mod = label_indent_mod)
}
