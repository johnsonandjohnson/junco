#' @name  response_by_var
#' @title Count denom fraction statistic
#'
#' @description Derives the count_denom_fraction statistic (i.e., 'xx /xx (xx.x percent)' )\cr
#'              Summarizes the number of unique subjects with a response = 'Y' for a given variable
#'              (e.g. TRTEMFL) within each category of another variable (e.g., SEX).
#'              Note that the denominator is derived using input df,
#'              in order to have these aligned with alt_source_df, it is expected that df includes all subjects.
#'
#' @details This is an analysis function for use within `analyze`. Arguments
#'          `df`, `.var` will be populated automatically by rtables during
#'          the tabulation process.
#'
#' Rows with `NA` in `.var` or `resp_var` are excluded from both
#' numerator and denominator. The `resp_var` column must be character
#' or factor with values limited to `"Y"`, `"N"`, or `NA`.
#'
#' @param df           (`data.frame`)\cr Name of dataframe being analyzed.
#' @param labelstr     (`character vector`)\cr Custom label for the variable being analyzed.
#' @param .var         (`character`)\cr Name of the variable being analyzed. Records with non-missing
#'                     values will be counted in the denominator.
#' @param .N_col       (`numeric`)\cr The total for the current column.
#' @param resp_var     (`character`)\cr Name of variable, for which, records with a value of 'Y'
#'                     will be counted in the numerator.
#' @param id           (`character`)\cr Name of column in df which will have patient identifiers
#' @param .format      (`character`)\cr Format for the count/denominator/fraction output.
#' @param ...          Additional arguments passed to the function.
#' @examples
#' library(dplyr)
#'
#' ADAE <- data.frame(
#'   USUBJID = c(
#'     "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
#'     "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
#'   ),
#'   SEX_DECODE = c(
#'     "Female", "Female", "Male", "Female", "Male",
#'     "Female", "Male", "Female", "Male", "Female"
#'   ),
#'   TRT01A = c(
#'     "ARMA", "ARMB", "ARMA", "ARMB", "ARMB",
#'     "Placebo", "Placebo", "Placebo", "ARMA", "ARMB"
#'   ),
#'   TRTEMFL = c("Y", "Y", "N", "Y", "Y", "Y", "Y", "N", "Y", "Y")
#' )
#'
#' ADAE <- ADAE |>
#'   mutate(
#'     TRT01A = as.factor(TRT01A),
#'     SEX_DECODE = as.factor(SEX_DECODE)
#'   )
#'
#' lyt <- basic_table() |>
#'   split_cols_by("TRT01A") |>
#'   analyze(
#'     vars = "SEX_DECODE",
#'     var_labels = "Sex, n/Ns (%)",
#'     show_labels = "visible",
#'     afun = response_by_var,
#'     extra_args = list(resp_var = "TRTEMFL"),
#'     nested = FALSE
#'   )
#'
#' result <- build_table(lyt, ADAE)
#'
#' result
#' @return a `RowsVerticalSection` for use by the internal tabulation machinery of `rtables`
#' @export
response_by_var <- function(
  df,
  labelstr = NULL,
  .var,
  .N_col,
  resp_var = NULL,
  id = "USUBJID",
  .format = jjcsformat_count_denom_fraction,
  ...
) {
  # Derive statistics: xx / xx (xx.x%)

  h_check_char_resp_var(df, resp_var, .var, id)

  # --- remove NAs --------------------------------------------------------------
  df <- df[!is.na(df[[.var]]) & !is.na(df[[resp_var]]), ]

  varvec <- df[[.var]]

  # For group summaries, varvec will be a singular value, equal to the current split, whereas for afun summaries,
  # varvec will consist of all possible values of var.
  if (!is.null(labelstr)) {
    levs <- labelstr
  } else {
    levs <- if (is.factor(varvec)) levels(varvec) else unique(varvec)
  }

  fn <- function(levii) {
    dfii <- df[df[[.var]] == levii, ]

    den <- NROW(unique(dfii[, id]))
    num <- NROW(unique(dfii[dfii[[resp_var]] == "Y", id]))

    rcell(c(num * 1, den * 1, num * 1 / den), format = .format)
  }

  cls <- lapply(levs, fn)
  names(cls) <- levs

  # Hand off results to analyze

  in_rows(.list = cls)
}

#' Check that the response variable is a valid character/factor Y/N column
#'
#' @param df (`data.frame`)\cr data to validate.
#' @param resp_var (`string`)\cr response variable name.
#' @param .var (`string`)\cr analysis variable name.
#' @param id (`string`)\cr subject identifier column name.
#'
#' @return `NULL` invisibly. Raises an error on invalid input.
#' @keywords internal
h_check_char_resp_var <- function(df, resp_var, .var, id) {
  checkmate::assert_data_frame(df, min.rows = 0L)
  checkmate::assert_string(resp_var)
  checkmate::assert_string(id)
  checkmate::assert_choice(resp_var, names(df))
  checkmate::assert_choice(.var, names(df))
  checkmate::assert_choice(id, names(df))
  checkmate::assert_multi_class(df[[resp_var]], c("character", "factor"))

  resp_vals <- as.character(unique(df[[resp_var]][!is.na(df[[resp_var]])]))
  if (length(resp_vals) > 0L) {
    checkmate::assert_subset(resp_vals, c("Y", "N"))
  }

  invisible(NULL)
}
