#' Simple Content Row Function to Count Rows
#'
#' @keywords internal
c_row_counts <- function(df, labelstr, label_fstr) {
  in_rows(count = nrow(df), .formats = "xx", .labels = sprintf(label_fstr, labelstr))
}

#' Simple Content Row Function to Count Rows from Alternative Data
#'
#' @keywords internal
c_row_counts_alt <- function(df, labelstr, label_fstr, .alt_df) {
  c_row_counts(df = .alt_df, labelstr = labelstr, label_fstr = label_fstr)
}

#' Layout Creating Function Adding Row Counts
#'
#' This is a simple wrapper of [rtables::summarize_row_groups()] and the main
#' additional value is that we can choose whether we want to use the alternative
#' (usually ADSL) data set for the counts (default) or use the original data set.
#'
#' @inheritParams proposal_argument_convention
#' @param label_fstr (`string`)\cr a `sprintf` style format string.
#'   It can contain up to one `%s` which takes the current split value and
#'   generates the row label.
#' @param alt_counts (`flag`)\cr whether row counts should be taken from
#'   `alt_counts_df` (`TRUE`) or from `df` (`FALSE`).
#'
#' @return A modified layout where the latest row split now contains the counts.
#' @export
#' @examples
#' basic_table() |>
#'   split_cols_by("ARM") |>
#'   add_colcounts() |>
#'   split_rows_by("RACE", split_fun = drop_split_levels) |>
#'   summarize_row_counts(label_fstr = "RACE value - %s") |>
#'   analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx") |>
#'   build_table(DM, alt_counts_df = rbind(DM, DM))
#'
summarize_row_counts <- function(lyt, label_fstr = "%s", alt_counts = TRUE) {
  checkmate::assert_flag(alt_counts)

  summarize_row_groups(
    lyt,
    cfun = if (alt_counts) c_row_counts_alt else c_row_counts,
    extra_args = list(label_fstr = label_fstr)
  )
}
