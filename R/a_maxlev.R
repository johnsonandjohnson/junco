#' Calculate Count and Percentage of the Maximum Level of an Ordered Factor per
#' Subject.
#'
#' @description A formatted analysis function used as an `afun` in
#'  \link[rtables]{analyze} and as a `cfun` in \link[rtables]{summarize_row_groups}.
#'
#'  It computes count and proportion statistics for the maximum level of an
#'  ordered factor, `df[[.var]]`, for each unique subject in `df[[id]]`.
#'  Specifically, for each subject, the function identifies the highest level
#'  of `df[[.var]]`, producing one value per subject.
#'  Then, if `any_level = TRUE`, the function reports the total number of
#'  maximum values, excluding those specified in `any_level_exclude`.
#'  Otherwise, it tabulates the frequency of each maximum level across all
#'  subjects.
#'
#'  This function is particularly useful for identifying the maximum severity of
#'  adverse events in a treatment sequence, where the most severe event
#'  experienced by a subject is used for reporting.
#'
#' @note The denominator for proportions is computed using the `denom_df`
#'  argument. This serves as a temporary workaround until the next version of
#'  `rtables` is released, which will support `.alt_count_df` for use in
#'  `afun`/`cfun`.
#'
#' @details For each unique subject, only the maximum level of the ordered factor
#'  `df[[.var]]` is included in the final count and percentage statistics.
#'
#' @inheritParams proposal_argument_convention
#' @param denom_df (`data.frame`)\cr
#'  A dataset used to compute the denominator for proportions. Required when
#'  the same subject appears multiple times in the dataset due to treatment
#'  sequences.
#' @param any_level (`flag`)\cr
#'  Should be set to `TRUE` when the function is used as a `cfun`.
#' @param any_level_exclude (`character`)\cr
#'  Applicable only when `any_level = TRUE`. Specifies levels of `df[[.var]]`
#'  to exclude from the statistic (default = "Missing").
#'
#' @returns A `RowsVerticalSection` object.
#' @export
#'
#' @examples
#' treatments <- factor(c("a", "b", "c"))
#' ae_severities <- c("Missing", "Mild", "Moderate", "Severe")
#' ae_severities <- ordered(ae_severities, levels = ae_severities)
#' my_adae <- data.frame(
#'   ID = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4),
#'   TRT = factor(c("a", "b", "b", "b", "c", "c", "a", "c", "b", "b")),
#'   AESEV = ae_severities[c(4L, 1L, 2L, 1L, 2L, 1L, 2L, 3L, 1L, 2L)]
#' )
#' my_adsl <- data.frame(
#'   ID = rep(1:5, each = 3),
#'   TRT = factor(rep(c("a", "b", "c"), times = 5))
#' )
#'
#' aesevall_spf <- make_combo_splitfun(
#'   nm = "AESEV_ALL",
#'   label = "Any AE",
#'   levels = NULL,
#' )
#'
#' lyt <- basic_table() |>
#'   split_cols_by("TRT") |>
#'   add_overall_col("Total") |>
#'   split_rows_by("AESEV", split_fun = aesevall_spf) |>
#'   summarize_row_groups(
#'     "AESEV",
#'     cfun = a_maxlev,
#'     extra_args = list(id = "ID", denom_df = my_adsl, any_level = TRUE)
#'   ) |>
#'   analyze(
#'     "AESEV",
#'     afun = a_maxlev,
#'     extra_args = list(id = "ID", denom_df = my_adsl)
#'   )
#' build_table(lyt, my_adae)
a_maxlev <- function(df,
                     labelstr = NULL,
                     .var,
                     .spl_context,
                     id = "USUBJID",
                     denom_df,
                     any_level = FALSE,
                     any_level_exclude = "Missing",
                     ...) {
  checkmate::assert_factor(df[[.var]], ordered = TRUE, any.missing = FALSE)
  checkmate::assert_string(id)
  checkmate::assert_subset(id, choices = colnames(df))
  checkmate::assert_subset(id, choices = colnames(denom_df))
  checkmate::assert_flag(any_level)
  checkmate::assert_character(any_level_exclude, any.missing = FALSE, unique = TRUE)

  varvec <- df[[.var]]
  levs <- levels(varvec)
  levs_lab <- if (any_level) {
    labelstr
  } else {
    levs
  }

  N_row <- nrow(df)
  count_pct <- if (N_row != 0) {
    max_per_id <- tapply(varvec, df[[id]], max)
    max_per_id <- ordered(levs[max_per_id], levels = levs) # tapply loses levels of varvec.

    count_max <- if (any_level) {
      sum(max_per_id != any_level_exclude)
    } else {
      table(max_per_id)
    }

    # TODO: N is currently computed using the `denom_df` extra argument.
    # This is a temporary placeholder until the new version of rtables is released,
    # after which `.alt_count_df` will be available for use in `afun`/`cfun`.
    cur_col <- subset(denom_df, eval(.spl_context$cur_col_expr[[1]]))
    N <- length(unique(cur_col[[id]]))
    as.list(data.frame(rbind(count_max, count_max / N)))
  } else {
    rep(list(NA), length(levs_lab))
  }

  count_pct <- setNames(count_pct, levs_lab)

  in_rows(
    .list = count_pct,
    .formats = jjcsformat_count_fraction,
    .format_na_strs = "-"
  )
}
