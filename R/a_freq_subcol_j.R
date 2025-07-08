#' @name a_freq_subcol_j
#'
#' @title Analysis function count and percentage with extra column-subsetting in
#' selected columns (controlled by subcol_* arguments)
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams a_freq_j
#' @param subcol_split Text to search colid to determine whether further subsetting
#'                     should be performed.
#' @param subcol_var Name of variable containing to be searched for the text
#'                     identified in subcol_val argument.
#' @param subcol_val Value to use to perform further data sub-setting.

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
#'
#' @return list of requested statistics with formatted `rtables::CellValue()`.\cr
#' @export
#' 
#' @examples
#' library(dplyr)
#' 
#' ADSL <- ex_adsl |> 
#'   select(USUBJID, ARM)
#'
#' ADSL$COLSPAN_REL <- "AEs"
#'
#' ADAE <- ex_adae |> 
#'  select(USUBJID, ARM, AEDECOD, AREL)
#'
#' ADAE <- ADAE |>
#'   mutate(
#'     AEREL = case_when(AREL == "Y" ~ "RELATED",
#'                       AREL == "N" ~ "NOT RELATED"),
#'     AEREL = factor(AEREL),
#'     COLSPAN_REL = "AEs"
#'   )
#'
#' combodf <- tribble(
#'   ~valname,  ~label,        ~levelcombo, ~exargs,
#'   "RELATED", "Related AEs", c("AEs"),    list()
#' )
#'
#' lyt <- basic_table(show_colcounts = TRUE) |>
#'   split_cols_by("COLSPAN_REL", split_fun = add_combo_levels(combodf, trim = TRUE)) |>
#'   split_cols_by("ARM") |>
#'   analyze("AEDECOD", afun = a_freq_subcol_j, 
#'     extra_args = list(subcol_split = "RELATED", 
#'       subcol_var = "AEREL", 
#'       subcol_val = "RELATED"))
#'
#' result <- build_table(lyt, ADAE, alt_counts_df = ADSL)
#'
#' result

a_freq_subcol_j <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    # arguments specific to a_freq_subcol_j
    subcol_split = NULL,
    subcol_var = NULL,
    subcol_val = NULL,
    # arguments specific to a_freq_subcol_j till here
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
    .stats = c("count_unique_denom_fraction"),
    .formats = NULL,
    .labels_n = NULL,
    .indent_mods = NULL,
    na_str = rep("NA", 3)) {
  denom <- match.arg(denom)

  if (!is.null(labelstr) && is.na(.var)) {
    stop(
      "Argument var must be specified in call to summarize_row_groups when using cfun = a_freq_subcol_j."
    )
  }

  check_alt_df_full(denom, "n_altdf", .alt_df_full)

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
  alt_df <- res_dataprep$alt_df
  parentdf <- res_dataprep$parentdf
  new_denomdf <- res_dataprep$new_denomdf
  .stats <- .stats

  ## colid can be used to figure out if we're in subcolum
  colid <- .spl_context$cur_col_id[[1]]

  ### this is the core code for subsetting to appropriate subcol_val
  insubcol <- grepl(subcol_split, colid, fixed = TRUE)
  if (insubcol) {
    df <- subset(df, df[[subcol_var]] == subcol_val)
  }

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
  inrows <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )

  return(inrows)
}
