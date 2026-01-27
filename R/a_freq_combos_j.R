#' @name a_freq_combos_j
#'
#' @title Analysis function count and percentage in column design controlled by combosdf
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams a_freq_j
#'
#' @param combosdf The df which provides the mapping of column facets to produce cumulative counts for .N_col.\cr
#' In the cell facet, these cumulative records must then be removed from the numerator,
#' which can be done via the filter_var parameter
#' to avoid unwanted counting of events.
#' @param do_not_filter A vector of facets (i.e., column headers), identifying headers for which
#' no filtering of records should occur.
#' That is, the numerator should contain cumulative counts.  Generally, this will be used for a
#' "Total" column, or something similar.
#' @param filter_var The variable which identifies the records to count in the numerator for any given column.
#' Generally, this will contain text matching the column header for the column associated with a given record.
#' @param flag_var Variable which identifies the occurrence (or first occurrence) of an event.
#' The flag variable is expected to have a value of "Y" identifying that the event should be counted, or NA otherwise.
#' @param denom (`string`)\cr
#' One of \cr
#' \itemize{
#' \item \strong{N_col} Column count, \cr
#' \item \strong{n_df} Number of patients (based upon the main input dataframe `df`),\cr
#' \item \strong{n_altdf} Number of patients from the secondary dataframe (`.alt_df_full`),\cr
#' Note that argument `denom_by` will perform a row-split on the `.alt_df_full` dataframe.\cr
#' It is a requirement that variables specified in `denom_by` are part of the row split specifications. \cr
#' \item \strong{n_rowdf} Number of patients from the current row-level dataframe
#' (`.row_df` from the rtables splitting machinery).\cr
#' \item \strong{n_parentdf} Number of patients from a higher row-level split than the current split.\cr
#' This higher row-level split is specified in the argument `denom_by`.\cr
#' }
#' @param .formats (named 'character' or 'list')\cr
#' formats for the statistics.
#' @return list of requested statistics with formatted `rtables::CellValue()`.\cr
#'
#' @examples
#' library(dplyr)
#' ADSL <- ex_adsl |> select(USUBJID, ARM, EOSSTT, EOSDT, EOSDY, TRTSDTM)
#'
#' cutoffd <- as.Date("2023-09-24")
#'
#' ADSL <- ADSL |>
#'   mutate(
#'     TRTDURY = case_when(
#'       !is.na(EOSDY) ~ EOSDY,
#'       TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
#'     )
#'   ) |>
#'   mutate(ACAT1 = case_when(
#'     TRTDURY < 183 ~ "0-6 Months",
#'     TRTDURY < 366 ~ "6-12 Months",
#'     TRUE ~ "+12 Months"
#'   )) |>
#'   mutate(ACAT1 = factor(ACAT1, levels = c("0-6 Months", "6-12 Months", "+12 Months")))
#'
#'
#' ADAE <- ex_adae |> select(USUBJID, ARM, AEBODSYS, AEDECOD, ASTDY)
#'
#' ADAE <- ADAE |>
#'   mutate(TRTEMFL = "Y") |>
#'   mutate(ACAT1 = case_when(
#'     ASTDY < 183 ~ "0-6 Months",
#'     ASTDY < 366 ~ "6-12 Months",
#'     TRUE ~ "+12 Months"
#'   )) |>
#'   mutate(ACAT1 = factor(ACAT1, levels = c("0-6 Months", "6-12 Months", "+12 Months")))
#'
#' combodf <- tribble(
#'   ~valname, ~label, ~levelcombo, ~exargs,
#'   "Tot", "Total", c("0-6 Months", "6-12 Months", "+12 Months"), list(),
#'   "A_0-6 Months", "0-6 Months", c("0-6 Months", "6-12 Months", "+12 Months"), list(),
#'   "B_6-12 Months", "6-12 Months", c("6-12 Months", "+12 Months"), list(),
#'   "C_+12 Months", "+12 Months", c("+12 Months"), list()
#' )
#'
#'
#' lyt <- basic_table(show_colcounts = TRUE) |>
#'   split_cols_by("ARM") |>
#'   split_cols_by("ACAT1",
#'     split_fun = add_combo_levels(combosdf = combodf, trim = FALSE, keep_levels = combodf$valname)
#'   ) |>
#'   analyze("TRTEMFL",
#'     show_labels = "hidden",
#'     afun = a_freq_combos_j,
#'     extra_args = list(
#'       val = "Y",
#'       label = "Subjects with >= 1 AE",
#'       combosdf = combodf,
#'       filter_var = "ACAT1",
#'       do_not_filter = "Tot"
#'     )
#'   )
#'
#'
#' result <- build_table(lyt, df = ADAE, alt_counts_df = ADSL)
#'
#' result
#' @export
a_freq_combos_j <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    # arguments specific to a_freq_combos_j
    combosdf = NULL,
    do_not_filter = NULL,
    filter_var = NULL,
    flag_var = NULL,
    # arguments specific to a_freq_combos_j till here
    .df_row,
    .spl_context,
    .N_col,
    id = "USUBJID",
    denom = c("N_col", "n_df", "n_altdf", "n_rowdf", "n_parentdf"),
    label = NULL,
    label_fstr = NULL,
    label_map = NULL,
    .alt_df_full = NULL,
    denom_by = NULL,
    .stats = "count_unique_denom_fraction",
    .formats = NULL,
    .labels_n = NULL,
    .indent_mods = NULL,
    na_str = rep("NA", 3)) {
  denom <- match.arg(denom)

  check_alt_df_full(denom, "n_altdf", .alt_df_full)

  if (!is.null(combosdf) && !all(c("valname", "label") %in% names(combosdf))) {
    stop("a_freq_combos_j: combosdf must have variables valname and label.")
  }

  res_dataprep <- h_a_freq_dataprep(
    df = df,
    labelstr = labelstr,
    .var = .var,
    val = val,
    drop_levels = FALSE,
    excl_levels = NULL,
    new_levels = NULL,
    new_levels_after = FALSE,
    .df_row = .df_row,
    .spl_context = .spl_context,
    .N_col = .N_col,
    id = id,
    denom = denom,
    variables = NULL,
    label = label,
    label_fstr = label_fstr,
    label_map = label_map,
    .alt_df_full = .alt_df_full,
    denom_by = denom_by,
    .stats = .stats
  )
  # res_dataprep is list with elements
  # df .df_row val
  # drop_levels excl_levels
  # alt_df parentdf new_denomdf
  # .stats
  # make these elements available in current environment
  df <- res_dataprep$df
  .df_row <- res_dataprep$.df_row
  val <- res_dataprep$val
  drop_levels <- res_dataprep$drop_levels
  excl_levels <- res_dataprep$excl_levels
  alt_df <- res_dataprep$alt_df
  parentdf <- res_dataprep$parentdf
  new_denomdf <- res_dataprep$new_denomdf
  .stats <- res_dataprep$.stats

  ## colid can be used to figure out if we're in the combo column or not
  colid <- .spl_context$cur_col_id[[1]]

  ### this is the core code for subsetting to appropriate combo level
  df <- h_subset_combo(
    df = df,
    combosdf = combosdf,
    do_not_filter = do_not_filter,
    filter_var = filter_var,
    flag_var = flag_var,
    colid = colid
  )

  ## the same s-function can be used as in a_freq_j
  x_stats <- s_freq_j(
    df,
    .var = .var,
    .df_row = .df_row,
    val = val,
    alt_df = new_denomdf,
    parent_df = new_denomdf,
    id = id,
    denom = denom,
    .N_col = .N_col,
    countsource = "df"
  )

  .stats_adj <- .stats

  res_prepinrows <- h_a_freq_prepinrows(
    x_stats,
    .stats_adj,
    .formats,
    labelstr,
    label_fstr,
    label,
    .indent_mods,
    .labels_n,
    na_str
  )
  # res_prepinrows is list with elements
  # x_stats .formats .labels .indent_mods .format_na_strs
  # make these elements available in current environment
  x_stats <- res_prepinrows$x_stats
  .formats <- res_prepinrows$.formats
  .labels <- res_prepinrows$.labels
  .indent_mods <- res_prepinrows$.indent_mods
  .format_na_strs <- res_prepinrows$.format_na_strs

  ### final step: turn requested stats into rtables rows
  in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )
}
