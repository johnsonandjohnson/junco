#' @title Wrapper around `tern::a_summary()` with junco-specific defaults
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function wraps [tern::a_summary()] and applies junco-specific defaults
#' to selected formatting arguments: `.labels`, `.formats`, and `.indent_mods`.
#'
#' @details
#' If `.labels`, `.formats`, or `.indent_mods` are supplied with non-`NULL`
#' values, they are passed through to [tern::a_summary()] unchanged. If they
#' are `NULL` or not supplied, default values are generated using:
#' \itemize{
#'   \item [junco_get_labels_from_stats()] for `.labels`
#'   \item [junco_get_formats_from_stats()] for `.formats`
#'   \item [junco_get_indents_from_stats()] for `.indent_mods`.
#' }
#'
#' If the `.stats` argument is not explicitly provided, or is provided as `NULL`,
#' the default statistics from [tern::get_stats()] are used.
#'
#' @param ... Arguments passed on to [tern::a_summary()], including `.labels`,
#'   `.formats`, and `.indent_mods`.
#'
#' @return
#' Returns the same output as [tern::a_summary()].
#'
#' @seealso [tern::a_summary()]
#'
#' @importFrom tern a_summary get_stats
#' @export
#' @examples
#' .stats <- c("n", "mean_sd", "median_range")
#' tern::a_summary(1:10, .stats = .stats)
#' a_summary_j(1:10, .stats = .stats)
#' a_summary_j(1:10, .stats = .stats, .formats = c(mean_sd = "xx (xx.x)"))
#' a_summary_j(1:10)
#'
a_summary_j <- function(...) {
  # Arguments for a_summary.
  argsas <- list(...)

  if (is.null(argsas$.stats)) {
    argsas$.stats <- tern:::get_stats()
  }
  s <- argsas$.stats
  argsas$.labels <- junco_get_labels_from_stats(s, labels_in = argsas$.labels)
  argsas$.formats <- junco_get_formats_from_stats(s, formats_in = argsas$.formats)
  argsas$.indent_mods <- junco_get_indents_from_stats(s, indents_in = argsas$.indent_mods)

  do.call(tern::a_summary, argsas)
}

#' @title Summary Statistics for the Filtered Data with Label
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A wrapper function of [a_summary_j()] function that filters the data prior
#' to the function execution and prepends the a single label for the object with
#' statistics values.
#'
#' @inheritParams proposal_argument_convention df labelstr .var .spl_context
#' @inheritParams filter_df_prior_afun subset_expr
#' @param label (`string` or `function`)\cr A label to be appended or the function
#'   the creates the label. If a `function`, then is must accept exactly one
#'   argument which is a `.spl_context`.
#' @inheritParams prepend_label_cell label_indent_mod
#' @param ... Additional arguments passed to [a_summary_j()].
#'
#' @returns an object created by [a_summary_j()] for a given input arguments.
#' @export
#'
#' @examples
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

  prepend_label_cell(y, label = label, label_indent = label_indent_mod)
}
